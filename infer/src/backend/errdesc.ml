(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Create descriptions of analysis errors *)

module L = Logging
module F = Format

(** Check whether the hpred is a |-> representing a resource in the Racquire state *)
let hpred_is_open_resource prop = function
  | Sil.Hpointsto(e, _, _) ->
      (match Prop.get_resource_attribute prop e with
       | Some (Sil.Aresource { Sil.ra_kind = Sil.Racquire; Sil.ra_res = res }) -> Some res
       | _ -> None)
  | _ ->
      None

(** Produce a description of a persistent reference to an Android Context *)
let explain_context_leak pname context_typ fieldname error_path =
  Localise.desc_context_leak pname context_typ fieldname error_path

(** Explain a deallocate stack variable error *)
let explain_deallocate_stack_var pvar ra =
  let pvar_str = Pvar.to_string pvar in
  Localise.desc_deallocate_stack_variable pvar_str ra.Sil.ra_pname ra.Sil.ra_loc

(** Explain a deallocate constant string error *)
let explain_deallocate_constant_string s ra =
  let const_str =
    let pp fmt () =
      Sil.pp_exp pe_text fmt (Sil.Const (Sil.Cstr s)) in
    pp_to_string pp () in
  Localise.desc_deallocate_static_memory const_str ra.Sil.ra_pname ra.Sil.ra_loc

let verbose = Config.trace_error

(** Find the Set instruction used to assign [id] to a program variable, if any *)
let find_variable_assigment node id : Sil.instr option =
  let res = ref None in
  let node_instrs = Cfg.Node.get_instrs node in
  let find_set instr = match instr with
    | Sil.Set (Sil.Lvar _, _, e, _) when Sil.exp_equal (Sil.Var id) e ->
        res := Some instr;
        true
    | _ -> false in
  ignore (IList.exists find_set node_instrs);
  !res

(** Check if a nullify instruction exists for the program variable after the given instruction *)
let find_nullify_after_instr node instr pvar : bool =
  let node_instrs = Cfg.Node.get_instrs node in
  let found_instr = ref false in
  let find_nullify = function
    | Sil.Nullify (pv, _, _) when !found_instr -> Pvar.equal pv pvar
    | _instr ->
        if instr = _instr then found_instr := true;
        false in
  IList.exists find_nullify node_instrs

(** Find the other prune node of a conditional
    (e.g. the false branch given the true branch of a conditional) *)
let find_other_prune_node node =
  match Cfg.Node.get_preds node with
  | [n_pre] ->
      (match Cfg.Node.get_succs n_pre with
       | [n1; n2] ->
           if Cfg.Node.equal n1 node then Some n2 else Some n1
       | _ -> None)
  | _ -> None

(** Return true if [id] is assigned to a program variable which is then nullified *)
let id_is_assigned_then_dead node id =
  match find_variable_assigment node id with
  | Some (Sil.Set (Sil.Lvar pvar, _, _, _) as instr)
    when Pvar.is_local pvar || Pvar.is_callee pvar ->
      let is_prune = match Cfg.Node.get_kind node with
        | Cfg.Node.Prune_node _ -> true
        | _ -> false in
      let prune_check = function
        (* if prune node, check that it's also nullified in the other branch *)
        | Some node' ->
            (match Cfg.Node.get_instrs node' with
             | instr':: _ -> find_nullify_after_instr node' instr' pvar
             | _ -> false)
        | _ -> false in
      find_nullify_after_instr node instr pvar
      && (not is_prune || prune_check (find_other_prune_node node))
  | _ -> false

(** Find the function call instruction used to initialize normal variable [id],
    and return the function name and arguments *)
let find_normal_variable_funcall
    (node: Cfg.Node.t)
    (id: Ident.t): (Sil.exp * (Sil.exp list) * Location.t * Sil.call_flags) option =
  let res = ref None in
  let node_instrs = Cfg.Node.get_instrs node in
  let find_declaration = function
    | Sil.Call ([id0], fun_exp, args, loc, call_flags) when Ident.equal id id0 ->
        res := Some (fun_exp, IList.map fst args, loc, call_flags);
        true
    | _ -> false in
  ignore (IList.exists find_declaration node_instrs);
  if !verbose && !res == None
  then
    (L.d_str
       ("find_normal_variable_funcall could not find " ^
        Ident.to_string id ^
        " in node " ^
        string_of_int (Cfg.Node.get_id node));
     L.d_ln ());
  !res

(** Find a program variable assignment in the current node or predecessors. *)
let find_program_variable_assignment node pvar : (Cfg.Node.t * Ident.t) option =
  let visited = ref Cfg.NodeSet.empty in
  let rec find node =
    if Cfg.NodeSet.mem node !visited then None
    else
      begin
        visited := Cfg.NodeSet.add node !visited;
        let res = ref None in
        let find_instr = function
          | Sil.Set (Sil.Lvar _pvar, _, Sil.Var id, _)
            when Pvar.equal pvar _pvar && Ident.is_normal id ->
              res := Some (node, id);
              true
          | _ -> false in
        if IList.exists find_instr (Cfg.Node.get_instrs node)
        then !res
        else match Cfg.Node.get_preds node with
          | [pred_node] ->
              find pred_node
          | [pn1; pn2] ->
              (match find pn1 with
               | None -> find pn2
               | x -> x)
          | _ -> None (* either 0 or >2 predecessors *)
      end in
  find node

(** Find a program variable assignment to id in the current node or predecessors. *)
let find_ident_assignment node id : (Cfg.Node.t * Sil.exp) option =
  let visited = ref Cfg.NodeSet.empty in
  let rec find node =
    if Cfg.NodeSet.mem node !visited then None
    else
      begin
        visited := Cfg.NodeSet.add node !visited;
        let res = ref None in
        let find_instr = function
          | Sil.Letderef(_id, e, _, _) when Ident.equal _id id ->
              res := Some (node, e);
              true
          | _ -> false in
        if IList.exists find_instr (Cfg.Node.get_instrs node)
        then !res
        else match Cfg.Node.get_preds node with
          | [pred_node] ->
              find pred_node
          | [pn1; pn2] ->
              (match find pn1 with
               | None -> find pn2
               | x -> x)
          | _ -> None (* either 0 or >2 predecessors *)
      end in
  find node

(** Find a boolean assignment to a temporary variable holding a boolean condition.
    The boolean parameter indicates whether the true or false branch is required. *)
let rec find_boolean_assignment node pvar true_branch : Cfg.Node.t option =
  let find_instr n =
    let filter = function
      | Sil.Set (Sil.Lvar _pvar, _, Sil.Const (Sil.Cint i), _) when Pvar.equal pvar _pvar ->
          Sil.Int.iszero i <> true_branch
      | _ -> false in
    IList.exists filter (Cfg.Node.get_instrs n) in
  match Cfg.Node.get_preds node with
  | [pred_node] -> find_boolean_assignment pred_node pvar true_branch
  | [n1; n2] ->
      if find_instr n1 then (Some n1)
      else if find_instr n2 then (Some n2)
      else None
  | _ -> None

(** Check whether the program variable is a temporary one generated by CIL *)
let pvar_is_cil_tmp pvar =
  Pvar.is_local pvar &&
  let name = Pvar.to_string pvar in
  string_is_prefix "_tmp" name

(** Check whether the program variable is a temporary one generated by sawja *)
let pvar_is_sawja_tmp pvar =
  Pvar.is_local pvar &&
  let name = Pvar.to_string pvar in
  string_is_prefix "$irvar" name ||
  string_is_prefix "$T" name ||
  string_is_prefix "$bc" name

(** Check whether the program variable is a temporary generated by the front-end *)
let pvar_is_frontend_tmp pvar =
  if !Config.curr_language = Config.Java then pvar_is_sawja_tmp pvar
  else pvar_is_cil_tmp pvar

(** Find the Letderef instruction used to declare normal variable [id],
    and return the expression dereferenced to initialize [id] *)
let rec _find_normal_variable_letderef (seen : Sil.ExpSet.t) node id : Sil.dexp option =
  let res = ref None in
  let node_instrs = Cfg.Node.get_instrs node in
  let find_declaration = function
    | Sil.Letderef (id0, e, _, _) when Ident.equal id id0 ->
        if !verbose
        then
          (L.d_str "find_normal_variable_letderef defining ";
           Sil.d_exp e; L.d_ln ());
        res := _exp_lv_dexp seen node e;
        true
    | Sil.Call ([id0], Sil.Const (Sil.Cfun pn), (e, _):: _, _, _)
      when Ident.equal id id0 && Procname.equal pn (Procname.from_string_c_fun "__cast") ->
        if !verbose
        then
          (L.d_str "find_normal_variable_letderef cast on ";
           Sil.d_exp e; L.d_ln ());
        res := _exp_rv_dexp seen node e;
        true
    | Sil.Call ([id0], (Sil.Const (Sil.Cfun pname) as fun_exp), args, loc, call_flags)
      when Ident.equal id id0 ->
        if !verbose
        then
          (L.d_str "find_normal_variable_letderef function call ";
           Sil.d_exp fun_exp; L.d_ln ());

        let fun_dexp = Sil.Dconst (Sil.Cfun pname) in
        let args_dexp =
          let args_dexpo = IList.map (fun (e, _) -> _exp_rv_dexp seen node e) args in
          if IList.exists (fun x -> x = None) args_dexpo
          then []
          else
            let unNone = function Some x -> x | None -> assert false in
            IList.map unNone args_dexpo in

        res := Some (Sil.Dretcall (fun_dexp, args_dexp, loc, call_flags));
        true
    | _ -> false in
  ignore (IList.exists find_declaration node_instrs);
  if !verbose && !res == None
  then
    (L.d_str
       ("find_normal_variable_letderef could not find " ^
        Ident.to_string id ^
        " in node " ^
        string_of_int (Cfg.Node.get_id node));
     L.d_ln ());
  !res

(** describe lvalue [e] as a dexp *)
and _exp_lv_dexp (_seen : Sil.ExpSet.t) node e : Sil.dexp option =
  if Sil.ExpSet.mem e _seen then
    (L.d_str "exp_lv_dexp: cycle detected"; Sil.d_exp e; L.d_ln (); None)
  else
    let seen = Sil.ExpSet.add e _seen in
    match Prop.exp_normalize_noabs Sil.sub_empty e with
    | Sil.Const c ->
        if !verbose then (L.d_str "exp_lv_dexp: constant "; Sil.d_exp e; L.d_ln ());
        Some (Sil.Dderef (Sil.Dconst c))
    | Sil.BinOp(Sil.PlusPI, e1, e2) ->
        if !verbose then (L.d_str "exp_lv_dexp: (e1 +PI e2) "; Sil.d_exp e; L.d_ln ());
        (match _exp_lv_dexp seen node e1, _exp_rv_dexp seen node e2 with
         | Some de1, Some de2 -> Some (Sil.Dbinop(Sil.PlusPI, de1, de2))
         | _ -> None)
    | Sil.Var id when Ident.is_normal id ->
        if !verbose then (L.d_str "exp_lv_dexp: normal var "; Sil.d_exp e; L.d_ln ());
        (match _find_normal_variable_letderef seen node id with
         | None -> None
         | Some de -> Some (Sil.Dderef de))
    | Sil.Lvar pvar ->
        if !verbose then (L.d_str "exp_lv_dexp: program var "; Sil.d_exp e; L.d_ln ());
        if pvar_is_frontend_tmp pvar then
          begin
            match find_program_variable_assignment node pvar with
            | None -> None
            | Some (node', id) ->
                begin
                  match find_normal_variable_funcall node' id with
                  | Some (fun_exp, eargs, loc, call_flags) ->
                      let fun_dexpo = _exp_rv_dexp seen node' fun_exp in
                      let blame_args = IList.map (_exp_rv_dexp seen node') eargs in
                      if IList.exists (fun x -> x = None) (fun_dexpo:: blame_args) then None
                      else
                        let unNone = function Some x -> x | None -> assert false in
                        let args = IList.map unNone blame_args in
                        Some (Sil.Dfcall (unNone fun_dexpo, args, loc, call_flags))
                  | None ->
                      _exp_rv_dexp seen node' (Sil.Var id)
                end
          end
        else Some (Sil.Dpvar pvar)
    | Sil.Lfield (Sil.Var id, f, _) when Ident.is_normal id ->
        if !verbose then
          begin
            L.d_str "exp_lv_dexp: Lfield with var ";
            Sil.d_exp (Sil.Var id);
            L.d_str (" " ^ Ident.fieldname_to_string f);
            L.d_ln ()
          end;
        (match _find_normal_variable_letderef seen node id with
         | None -> None
         | Some de -> Some (Sil.Darrow (de, f)))
    | Sil.Lfield (e1, f, _) ->
        if !verbose then
          begin
            L.d_str "exp_lv_dexp: Lfield ";
            Sil.d_exp e1;
            L.d_str (" " ^ Ident.fieldname_to_string f);
            L.d_ln ()
          end;
        (match _exp_lv_dexp seen node e1 with
         | None -> None
         | Some de -> Some (Sil.Ddot (de, f)))
    | Sil.Lindex (e1, e2) ->
        if !verbose then
          begin
            L.d_str "exp_lv_dexp: Lindex ";
            Sil.d_exp e1;
            L.d_str " ";
            Sil.d_exp e2;
            L.d_ln ()
          end;
        (match _exp_lv_dexp seen node e1, _exp_rv_dexp seen node e2 with
         | None, _ -> None
         | Some de1, None ->
             (* even if the index is unknown, the array info is useful for bound errors *)
             Some (Sil.Darray (de1, Sil.Dunknown))
         | Some de1, Some de2 -> Some (Sil.Darray (de1, de2)))
    | _ ->
        if !verbose then (L.d_str "exp_lv_dexp: no match for  "; Sil.d_exp e; L.d_ln ());
        None

(** describe rvalue [e] as a dexp *)
and _exp_rv_dexp (_seen : Sil.ExpSet.t) node e : Sil.dexp option =
  if Sil.ExpSet.mem e _seen then
    (L.d_str "exp_rv_dexp: cycle detected"; Sil.d_exp e; L.d_ln (); None)
  else
    let seen = Sil.ExpSet.add e _seen in
    match e with
    | Sil.Const c ->
        if !verbose then (L.d_str "exp_rv_dexp: constant "; Sil.d_exp e; L.d_ln ());
        Some (Sil.Dconst c)
    | Sil.Lvar pv ->
        if !verbose then (L.d_str "exp_rv_dexp: program var "; Sil.d_exp e; L.d_ln ());
        if pvar_is_frontend_tmp pv
        then _exp_lv_dexp _seen (* avoid spurious cycle detection *) node e
        else Some (Sil.Dpvaraddr pv)
    | Sil.Var id when Ident.is_normal id ->
        if !verbose then (L.d_str "exp_rv_dexp: normal var "; Sil.d_exp e; L.d_ln ());
        _find_normal_variable_letderef seen node id
    | Sil.Lfield (e1, f, _) ->
        if !verbose then
          begin
            L.d_str "exp_rv_dexp: Lfield ";
            Sil.d_exp e1;
            L.d_str (" " ^ Ident.fieldname_to_string f);
            L.d_ln ()
          end;
        (match _exp_rv_dexp seen node e1 with
         | None -> None
         | Some de -> Some (Sil.Ddot(de, f)))
    | Sil.Lindex (e1, e2) ->
        if !verbose then
          begin
            L.d_str "exp_rv_dexp: Lindex ";
            Sil.d_exp e1;
            L.d_str " ";
            Sil.d_exp e2;
            L.d_ln ()
          end;
        (match _exp_rv_dexp seen node e1, _exp_rv_dexp seen node e2 with
         | None, _ | _, None -> None
         | Some de1, Some de2 -> Some (Sil.Darray(de1, de2)))
    | Sil.BinOp (op, e1, e2) ->
        if !verbose then (L.d_str "exp_rv_dexp: BinOp "; Sil.d_exp e; L.d_ln ());
        (match _exp_rv_dexp seen node e1, _exp_rv_dexp seen node e2 with
         | None, _ | _, None -> None
         | Some de1, Some de2 -> Some (Sil.Dbinop (op, de1, de2)))
    | Sil.UnOp (op, e1, _) ->
        if !verbose then (L.d_str "exp_rv_dexp: UnOp "; Sil.d_exp e; L.d_ln ());
        (match _exp_rv_dexp seen node e1 with
         | None -> None
         | Some de1 -> Some (Sil.Dunop (op, de1)))
    | Sil.Cast (_, e1) ->
        if !verbose then (L.d_str "exp_rv_dexp: Cast "; Sil.d_exp e; L.d_ln ());
        _exp_rv_dexp seen node e1
    | Sil.Sizeof (typ, sub) ->
        if !verbose then (L.d_str "exp_rv_dexp: type "; Sil.d_exp e; L.d_ln ());
        Some (Sil.Dsizeof (typ, sub))
    | _ ->
        if !verbose then (L.d_str "exp_rv_dexp: no match for  "; Sil.d_exp e; L.d_ln ());
        None

let find_normal_variable_letderef = _find_normal_variable_letderef Sil.ExpSet.empty
let exp_lv_dexp = _exp_lv_dexp Sil.ExpSet.empty
let exp_rv_dexp = _exp_rv_dexp Sil.ExpSet.empty

(** Produce a description of a mismatch between an allocation function
    and a deallocation function *)
let explain_allocation_mismatch ra_alloc ra_dealloc =
  let get_primitive_called is_alloc ra =
    (* primitive alloc/dealloc function ultimately used, and function actually called  *)
    (* e.g. malloc and my_malloc *)
    let primitive = match ra.Sil.ra_res with
      | Sil.Rmemory mk_alloc ->
          (if is_alloc then Sil.mem_alloc_pname else Sil.mem_dealloc_pname) mk_alloc
      | _ -> ra_alloc.Sil.ra_pname in
    let called = ra.Sil.ra_pname in
    (primitive, called, ra.Sil.ra_loc) in
  Localise.desc_allocation_mismatch
    (get_primitive_called true ra_alloc) (get_primitive_called false ra_dealloc)

(** Produce a description of a pointer dangerously coerced to a boolean in a comparison *)
let explain_bad_pointer_comparison exp node loc =
  let dexp_opt = exp_rv_dexp node exp in
  Localise.desc_bad_pointer_comparison dexp_opt loc

(** check whether the type of leaked [hpred] appears as a predicate
    in an inductive predicate in [prop] *)
let leak_from_list_abstraction hpred prop =
  let hpred_type = function
    | Sil.Hpointsto (_, _, texp) ->
        Some texp
    | Sil.Hlseg (_, { Sil.body =[Sil.Hpointsto (_, _, texp)]}, _, _, _) ->
        Some texp
    | Sil.Hdllseg (_, { Sil.body_dll =[Sil.Hpointsto (_, _, texp)]}, _, _, _, _, _) ->
        Some texp
    | _ -> None in
  let found = ref false in
  let check_hpred texp hp = match hpred_type hp with
    | Some texp' when Sil.exp_equal texp texp' -> found := true
    | _ -> () in
  let check_hpara texp _ hpara =
    IList.iter (check_hpred texp) hpara.Sil.body in
  let check_hpara_dll texp _ hpara =
    IList.iter (check_hpred texp) hpara.Sil.body_dll in
  match hpred_type hpred with
  | Some texp ->
      let env = Prop.prop_pred_env prop in
      Sil.Predicates.iter env (check_hpara texp) (check_hpara_dll texp);
      if !found
      then
        (L.d_str "leak_from_list_abstraction of predicate of type ";
         Sil.d_texp_full texp; L.d_ln());
      !found
  | None -> false

(** find the type of hpred, if any *)
let find_hpred_typ hpred = match hpred with
  | Sil.Hpointsto (_, _, texp) -> Some texp
  | _ -> None

(** find the type of pvar and remove the pointer, if any *)
let find_typ_without_ptr prop pvar =
  let res = ref None in
  let do_hpred = function
    | Sil.Hpointsto (e, _, te) when Sil.exp_equal e (Sil.Lvar pvar) ->
        res := Some te
    | _ -> () in
  IList.iter do_hpred (Prop.get_sigma prop);
  !res

(** Produce a description of a leak by looking at the current state.
    If the current instruction is a variable nullify, blame the variable.
    If it is an abstraction, blame any variable nullify at the current node.
    If there is an alloc attribute, print the function call and line number. *)
let explain_leak tenv hpred prop alloc_att_opt bucket =
  let instro = State.get_instr () in
  let loc = State.get_loc () in
  let node = State.get_node () in
  let node_instrs = Cfg.Node.get_instrs node in
  let hpred_typ_opt = find_hpred_typ hpred in
  let value_str_from_pvars_vpath pvars vpath =
    if pvars <> [] then
      begin
        let pp = pp_seq (Pvar.pp_value pe_text) in
        let desc_string = pp_to_string pp pvars in
        Some desc_string
      end
    else match vpath with
      | Some de ->
          Some (Sil.dexp_to_string de)
      | None -> None in
  let res_action_opt, resource_opt, vpath = match alloc_att_opt with
    | Some (Sil.Aresource ({ Sil.ra_kind = Sil.Racquire } as ra)) ->
        Some ra, Some ra.Sil.ra_res, ra.Sil.ra_vpath
    | _ -> (None, None, None) in
  let is_file = match resource_opt with
    | Some Sil.Rfile -> true
    | _ -> false in
  let check_pvar pvar =
    (* check that pvar is local or global and has the same type as the leaked hpred *)
    (Pvar.is_local pvar || Pvar.is_global pvar) &&
    not (pvar_is_frontend_tmp pvar) &&
    match hpred_typ_opt, find_typ_without_ptr prop pvar with
    | Some (Sil.Sizeof (t1, _)), Some (Sil.Sizeof (Sil.Tptr (_t2, _), _)) ->
        (try
           let t2 = Tenv.expand_type tenv _t2 in
           Sil.typ_equal t1 t2
         with exn when exn_not_failure exn -> false)
    | Some (Sil.Sizeof (Sil.Tint _, _)), Some (Sil.Sizeof (Sil.Tint _, _))
      when is_file -> (* must be a file opened with "open" *)
        true
    | _ -> false in
  let value_str = match instro with
    | None ->
        if !verbose then (L.d_str "explain_leak: no current instruction"; L.d_ln ());
        value_str_from_pvars_vpath [] vpath
    | Some (Sil.Nullify (pvar, _, _)) when check_pvar pvar ->
        if !verbose
        then
          (L.d_str "explain_leak: current instruction is Nullify for pvar ";
           Pvar.d pvar; L.d_ln ());
        (match exp_lv_dexp (State.get_node ()) (Sil.Lvar pvar) with
         | None -> None
         | Some de -> Some (Sil.dexp_to_string de))
    | Some (Sil.Abstract _) ->
        if !verbose then (L.d_str "explain_leak: current instruction is Abstract"; L.d_ln ());
        let get_nullify = function
          | Sil.Nullify (pvar, _, _) when check_pvar pvar ->
              if !verbose
              then
                (L.d_str "explain_leak: found nullify before Abstract for pvar ";
                 Pvar.d pvar; L.d_ln ());
              [pvar]
          | _ -> [] in
        let nullify_pvars = IList.flatten (IList.map get_nullify node_instrs) in
        let nullify_pvars_notmp =
          IList.filter (fun pvar -> not (pvar_is_frontend_tmp pvar)) nullify_pvars in
        value_str_from_pvars_vpath nullify_pvars_notmp vpath
    | Some (Sil.Set (lexp, _, _, _)) when vpath = None ->
        if !verbose
        then
          (L.d_str "explain_leak: current instruction Set for ";
           Sil.d_exp lexp; L.d_ln ());
        (match exp_lv_dexp node lexp with
         | Some dexp -> Some (Sil.dexp_to_string dexp)
         | None -> None)
    | Some instr ->
        if !verbose
        then
          (L.d_str "explain_leak: case not matched in instr ";
           Sil.d_instr instr; L.d_ln());
        value_str_from_pvars_vpath [] vpath in
  let exn_cat, bucket = (* decide whether Exn_user or Exn_developer *)
    match resource_opt with
    | Some _ -> (* we know it has been allocated *)
        Exceptions.Exn_user, bucket
    | None ->
        if leak_from_list_abstraction hpred prop && value_str != None
        then
          (* we don't know it's been allocated,
             but it's coming from list abstraction and we have a name *)
          Exceptions.Exn_user, bucket
        else Exceptions.Exn_developer, Some (Mleak_buckets.ml_bucket_unknown_origin ()) in
  exn_cat, Localise.desc_leak hpred_typ_opt value_str resource_opt res_action_opt loc bucket

(** find the dexp, if any, where the given value is stored
    also return the type of the value if found *)
let vpath_find prop _exp : Sil.dexp option * Sil.typ option =
  if !verbose then (L.d_str "in vpath_find exp:"; Sil.d_exp _exp; L.d_ln ());
  let rec find sigma_acc sigma_todo exp =
    let do_fse res sigma_acc' sigma_todo' lexp texp (f, se) = match se with
      | Sil.Eexp (e, _) when Sil.exp_equal exp e ->
          let sigma' = (IList.rev_append sigma_acc' sigma_todo') in
          (match lexp with
           | Sil.Lvar pv ->
               let typo = match texp with
                 | Sil.Sizeof (Sil.Tstruct struct_typ, _) ->
                     (try
                        let _, t, _ =
                          IList.find (fun (f', _, _) ->
                              Ident.fieldname_equal f' f)
                            struct_typ.Sil.instance_fields in
                        Some t
                      with Not_found -> None)
                 | _ -> None in
               res := Some (Sil.Ddot (Sil.Dpvar pv, f)), typo
           | Sil.Var id ->
               (match find [] sigma' (Sil.Var id) with
                | None, _ -> ()
                | Some de, typo -> res := Some (Sil.Darrow (de, f)), typo)
           | lexp ->
               if !verbose
               then
                 (L.d_str "vpath_find do_fse: no match on Eexp ";
                  Sil.d_exp lexp; L.d_ln ()))
      | _ -> () in
    let do_sexp sigma_acc' sigma_todo' lexp sexp texp = match sexp with
      | Sil.Eexp (e, _) when Sil.exp_equal exp e ->
          let sigma' = (IList.rev_append sigma_acc' sigma_todo') in
          (match lexp with
           | Sil.Lvar pv when not (pvar_is_frontend_tmp pv) ->
               let typo = match texp with
                 | Sil.Sizeof (typ, _) -> Some typ
                 | _ -> None in
               Some (Sil.Dpvar pv), typo
           | Sil.Var id ->
               (match find [] sigma' (Sil.Var id) with
                | None, typo -> None, typo
                | Some de, typo -> Some (Sil.Dderef de), typo)
           | lexp ->
               if !verbose
               then
                 (L.d_str "vpath_find do_sexp: no match on Eexp ";
                  Sil.d_exp lexp; L.d_ln ());
               None, None)
      | Sil.Estruct (fsel, _) ->
          let res = ref (None, None) in
          IList.iter (do_fse res sigma_acc' sigma_todo' lexp texp) fsel;
          !res
      | _ ->
          None, None in
    let do_hpred sigma_acc' sigma_todo' =
      let substituted_from_normal id =
        let filter = function
          | (ni, Sil.Var id') -> Ident.is_normal ni && Ident.equal id' id
          | _ -> false in
        IList.exists filter (Sil.sub_to_list (Prop.get_sub prop)) in
      function
      | Sil.Hpointsto (Sil.Lvar pv, sexp, texp)
        when (Pvar.is_local pv || Pvar.is_global pv || Pvar.is_seed pv) ->
          do_sexp sigma_acc' sigma_todo' (Sil.Lvar pv) sexp texp
      | Sil.Hpointsto (Sil.Var id, sexp, texp)
        when Ident.is_normal id || (Ident.is_footprint id && substituted_from_normal id) ->
          do_sexp sigma_acc' sigma_todo' (Sil.Var id) sexp texp
      | _ ->
          None, None in
    match sigma_todo with
    | [] -> None, None
    | hpred:: sigma_todo' ->
        (match do_hpred sigma_acc sigma_todo' hpred with
         | Some de, typo -> Some de, typo
         | None, _ -> find (hpred:: sigma_acc) sigma_todo' exp) in
  let res = find [] (Prop.get_sigma prop) _exp in
  if !verbose then begin
    match res with
    | None, _ -> L.d_str "vpath_find: cannot find "; Sil.d_exp _exp; L.d_ln ()
    | Some de, typo -> L.d_str "vpath_find: found "; L.d_str (Sil.dexp_to_string de); L.d_str " : ";
        match typo with
        | None -> L.d_str " No type"
        | Some typ -> Sil.d_typ_full typ;
            L.d_ln ()
  end;
  res

(** produce a description of the access from the instrumentation at position [dexp] in [prop] *)
let explain_dexp_access prop dexp is_nullable =
  let sigma = Prop.get_sigma prop in
  let sexpo_to_inst = function
    | None -> None
    | Some (Sil.Eexp (_, inst)) -> Some inst
    | Some se ->
        if !verbose then (L.d_str "sexpo_to_inst: can't find inst "; Sil.d_sexp se; L.d_ln());
        None in
  let find_ptsto (e : Sil.exp) : Sil.strexp option =
    let res = ref None in
    let do_hpred = function
      | Sil.Hpointsto (e', se, _) when Sil.exp_equal e e' ->
          res := Some se
      | _ -> () in
    IList.iter do_hpred sigma;
    !res in
  let rec lookup_fld fsel f = match fsel with
    | [] ->
        if !verbose
        then
          (L.d_strln ("lookup_fld: can't find field " ^ Ident.fieldname_to_string f));
        None
    | (f1, se):: fsel' ->
        if Ident.fieldname_equal f1 f then Some se
        else lookup_fld fsel' f in
  let rec lookup_esel esel e = match esel with
    | [] ->
        if !verbose then (L.d_str "lookup_esel: can't find index "; Sil.d_exp e; L.d_ln ());
        None
    | (e1, se):: esel' ->
        if Sil.exp_equal e1 e then Some se
        else lookup_esel esel' e in
  let rec lookup : Sil.dexp -> Sil.strexp option = function
    | Sil.Dconst c ->
        Some (Sil.Eexp (Sil.Const c, Sil.inst_none))
    | Sil.Darray (de1, de2) ->
        (match lookup de1, lookup de2 with
         | None, _ | _, None -> None
         | Some Sil.Earray (_, esel, _), Some Sil.Eexp (e, _) ->
             lookup_esel esel e
         | Some se1, Some se2 ->
             if !verbose
             then
               (L.d_str "lookup: case not matched on Darray ";
                Sil.d_sexp se1; L.d_str " "; Sil.d_sexp se2; L.d_ln());
             None)
    | Sil.Darrow (de1, f) ->
        (match lookup (Sil.Dderef de1) with
         | None -> None
         | Some Sil.Estruct (fsel, _) ->
             lookup_fld fsel f
         | Some _ ->
             if !verbose then (L.d_str "lookup: case not matched on Darrow "; L.d_ln ());
             None)
    | Sil.Ddot (de1, f) ->
        (match lookup de1 with
         | None -> None
         | Some Sil.Estruct (fsel, _) ->
             lookup_fld fsel f
         | Some _ ->
             if !verbose then (L.d_str "lookup: case not matched on Ddot "; L.d_ln ());
             None)
    | Sil.Dpvar pvar ->
        if !verbose then (L.d_str "lookup: found Dpvar "; L.d_ln ());
        (find_ptsto (Sil.Lvar pvar))
    | Sil.Dderef de ->
        (match lookup de with
         | None -> None
         | Some (Sil.Eexp (e, _)) -> find_ptsto e
         | Some _ -> None)
    | (Sil.Dbinop(Sil.PlusPI, Sil.Dpvar _, Sil.Dconst _) as de) ->
        if !verbose then (L.d_strln ("lookup: case )pvar + constant) " ^ Sil.dexp_to_string de));
        None
    | Sil.Dfcall (Sil.Dconst c, _, loc, _) ->
        if !verbose then (L.d_strln "lookup: found Dfcall ");
        (match c with
         | Sil.Cfun _ -> (* Treat function as an update *)
             Some (Sil.Eexp (Sil.Const c, Sil.Ireturn_from_call loc.Location.line))
         | _ -> None)
    | de ->
        if !verbose then (L.d_strln ("lookup: unknown case not matched " ^ Sil.dexp_to_string de));
        None in
  let access_opt = match sexpo_to_inst (lookup dexp) with
    | None ->
        if !verbose
        then
          (L.d_strln ("explain_dexp_access: cannot find inst of " ^ Sil.dexp_to_string dexp));
        None
    | Some (Sil.Iupdate (_, ncf, n, _)) ->
        Some (Localise.Last_assigned (n, ncf))
    | Some (Sil.Irearrange (_, _, n, _)) ->
        Some (Localise.Last_accessed (n, is_nullable))
    | Some (Sil.Ireturn_from_call n) ->
        Some (Localise.Returned_from_call n)
    | Some Sil.Ialloc when !Config.curr_language = Config.Java ->
        Some Localise.Initialized_automatically
    | Some inst ->
        if !verbose
        then
          (L.d_strln
             ("explain_dexp_access: inst is not an update " ^
              Sil.inst_to_string inst));
        None in
  access_opt

let explain_dereference_access outermost_array is_nullable _de_opt prop =
  let de_opt =
    let rec remove_outermost_array_access = function (* remove outermost array access from [de] *)
      | Sil.Dbinop(Sil.PlusPI, de1, _) -> (* remove pointer arithmetic before array access *)
          remove_outermost_array_access de1
      | Sil.Darray(Sil.Dderef de1, _) -> (* array access is a deref already: remove both *)
          de1
      | Sil.Darray(de1, _) -> (* remove array access *)
          de1
      | Sil.Dderef de -> (* remove implicit array access *)
          de
      | Sil.Ddot (de, _) -> (* remove field access before array access *)
          remove_outermost_array_access de
      | de -> de in
    match _de_opt with
    | None -> None
    | Some de ->
        Some (if outermost_array then remove_outermost_array_access de else de) in
  let value_str = match de_opt with
    | Some de ->
        Sil.dexp_to_string de
    | None -> "" in
  let access_opt = match de_opt with
    | Some de -> explain_dexp_access prop de is_nullable
    | None -> None in
  (value_str, access_opt)

(** Create a description of a dereference operation *)
let create_dereference_desc
    ?use_buckets: (use_buckets = false)
    ?outermost_array: (outermost_array = false)
    ?is_nullable: (is_nullable = false)
    ?is_premature_nil: (is_premature_nil = false)
    de_opt deref_str prop loc =
  let value_str, access_opt =
    explain_dereference_access outermost_array is_nullable de_opt prop in
  let access_opt' = match access_opt with
    | Some (Localise.Last_accessed _)
      when outermost_array -> None (* don't report last accessed for arrays *)
    | _ -> access_opt in
  let desc = Localise.dereference_string deref_str value_str access_opt' loc in
  let desc =
    if !Config.curr_language = Config.C_CPP && not is_premature_nil then
      match de_opt with
      | Some (Sil.Dpvar pvar)
      | Some (Sil.Dpvaraddr pvar) ->
          (match Prop.get_objc_null_attribute prop (Sil.Lvar pvar) with
           | Some (Sil.Aobjc_null info) -> Localise.parameter_field_not_null_checked_desc desc info
           | _ -> desc)
      | _ -> desc
    else desc in
  if use_buckets then Buckets.classify_access desc access_opt' de_opt is_nullable
  else desc

(** explain memory access performed by the current instruction
    if outermost_array is true, the outermost array access is removed
    if outermost_dereference is true, stop at the outermost dereference
    (skipping e.g. outermost field access) *)
let _explain_access
    ?(use_buckets = false)
    ?(outermost_array = false)
    ?(outermost_dereference = false)
    ?(is_nullable = false)
    ?(is_premature_nil = false)
    deref_str prop loc =
  let rec find_outermost_dereference node e = match e with
    | Sil.Const _ ->
        if !verbose then (L.d_str "find_outermost_dereference: constant "; Sil.d_exp e; L.d_ln ());
        exp_lv_dexp node e
    | Sil.Var id when Ident.is_normal id -> (* look up the normal variable declaration *)
        if !verbose
        then
          (L.d_str "find_outermost_dereference: normal var ";
           Sil.d_exp e; L.d_ln ());
        find_normal_variable_letderef node id
    | Sil.Lfield (e', _, _) ->
        if !verbose then (L.d_str "find_outermost_dereference: Lfield "; Sil.d_exp e; L.d_ln ());
        find_outermost_dereference node e'
    | Sil.Lindex(e', _) ->
        if !verbose then (L.d_str "find_outermost_dereference: Lindex "; Sil.d_exp e; L.d_ln ());
        find_outermost_dereference node e'
    | Sil.Lvar _ ->
        if !verbose then (L.d_str "find_outermost_dereference: Lvar "; Sil.d_exp e; L.d_ln ());
        exp_lv_dexp node e
    | Sil.BinOp(Sil.PlusPI, Sil.Lvar _, _) ->
        if !verbose
        then
          (L.d_str "find_outermost_dereference: Lvar+index ";
           Sil.d_exp e; L.d_ln ());
        exp_lv_dexp node e
    | Sil.Cast (_, e') ->
        if !verbose then (L.d_str "find_outermost_dereference: cast "; Sil.d_exp e; L.d_ln ());
        find_outermost_dereference node e'
    | Sil.BinOp(Sil.PtrFld, _, e') ->
        if !verbose then (L.d_str "find_outermost_dereference: PtrFld "; Sil.d_exp e; L.d_ln ());
        find_outermost_dereference node e'
    | _ ->
        if !verbose
        then
          (L.d_str "find_outermost_dereference: no match for ";
           Sil.d_exp e; L.d_ln ());
        None in
  let find_exp_dereferenced () = match State.get_instr () with
    | Some Sil.Set (e, _, _, _) ->
        if !verbose then (L.d_str "explain_dereference Sil.Set "; Sil.d_exp e; L.d_ln ());
        Some e
    | Some Sil.Letderef (_, e, _, _) ->
        if !verbose then (L.d_str "explain_dereference Sil.Leteref "; Sil.d_exp e; L.d_ln ());
        Some e
    | Some Sil.Call (_, Sil.Const (Sil.Cfun fn), [(e, _)], _, _)
      when Procname.to_string fn = "free" ->
        if !verbose then (L.d_str "explain_dereference Sil.Call "; Sil.d_exp e; L.d_ln ());
        Some e
    | Some Sil.Call (_, (Sil.Var _ as e), _, _, _) ->
        if !verbose then (L.d_str "explain_dereference Sil.Call "; Sil.d_exp e; L.d_ln ());
        Some e
    | _ -> None in
  let node = State.get_node () in
  match find_exp_dereferenced () with
  | None ->
      if !verbose then L.d_strln "_explain_access: find_exp_dereferenced returned None";
      Localise.no_desc
  | Some e ->
      L.d_strln "Finding deref'd exp";
      let de_opt =
        if outermost_dereference then find_outermost_dereference node e
        else exp_lv_dexp node e in
      create_dereference_desc
        ~use_buckets ~outermost_array ~is_nullable ~is_premature_nil
        de_opt deref_str prop loc

(** Produce a description of which expression is dereferenced in the current instruction, if any.
    The subexpression to focus on is obtained by removing field and index accesses. *)
let explain_dereference
    ?(use_buckets = false)
    ?(is_nullable = false)
    ?(is_premature_nil = false)
    deref_str prop loc =
  _explain_access
    ~use_buckets ~outermost_array: false ~outermost_dereference: true ~is_nullable ~is_premature_nil
    deref_str prop loc

(** Produce a description of the array access performed in the current instruction, if any.
    The subexpression to focus on is obtained by removing the outermost array access. *)
let explain_array_access deref_str prop loc =
  _explain_access ~outermost_array: true deref_str prop loc

(** Produce a description of the memory access performed in the current instruction, if any. *)
let explain_memory_access deref_str prop loc =
  _explain_access deref_str prop loc

(* offset of an expression found following a program variable *)
type pvar_off =
  (* value of a pvar *)
  | Fpvar

  (* value obtained by dereferencing the pvar and following a sequence of fields *)
  | Fstruct of Ident.fieldname list

let dexp_apply_pvar_off dexp pvar_off =
  let rec add_ddot de = function
    | [] -> de
    | f:: fl ->
        add_ddot (Sil.Ddot (de, f)) fl in
  match pvar_off with
  | Fpvar -> dexp
  | Fstruct (f:: fl) -> add_ddot (Sil.Darrow (dexp, f)) fl
  | Fstruct [] -> dexp (* case should not happen *)

(** Produce a description of the nth parameter of the function call, if the current instruction
    is a function call with that parameter *)
let explain_nth_function_parameter use_buckets deref_str prop n pvar_off =
  let node = State.get_node () in
  let loc = State.get_loc () in
  match State.get_instr () with
  | Some Sil.Call (_, _, args, _, _) ->
      (try
         let arg = fst (IList.nth args (n - 1)) in
         let dexp_opt = exp_rv_dexp node arg in
         let dexp_opt' = match dexp_opt with
           | Some de ->
               Some (dexp_apply_pvar_off de pvar_off)
           | None -> None in
         create_dereference_desc ~use_buckets dexp_opt' deref_str prop loc
       with exn when exn_not_failure exn -> Localise.no_desc)
  | _ -> Localise.no_desc

(** Find a program variable whose value is [exp] or pointing to a struct containing [exp] *)
let find_with_exp prop exp =
  let res = ref None in
  let found_in_pvar pv =
    if not (Pvar.is_abducted pv) && not (Pvar.is_this pv) then
      res := Some (pv, Fpvar) in
  let found_in_struct pv fld_lst = (* found_in_pvar has priority *)
    if !res = None then res := Some (pv, Fstruct (IList.rev fld_lst)) in
  let rec search_struct pv fld_lst = function
    | Sil.Eexp (e, _) ->
        if Sil.exp_equal e exp then found_in_struct pv fld_lst
    | Sil.Estruct (fsel, _) ->
        IList.iter (fun (f, se) -> search_struct pv (f:: fld_lst) se) fsel
    | _ -> () in
  let do_hpred_pointed_by_pvar pv e = function
    | Sil.Hpointsto(e1, se, _) ->
        if Sil.exp_equal e e1 then search_struct pv [] se
    | _ -> () in
  let do_hpred = function
    | Sil.Hpointsto(Sil.Lvar pv, Sil.Eexp (e, _), _) ->
        if Sil.exp_equal e exp then found_in_pvar pv
        else IList.iter (do_hpred_pointed_by_pvar pv e) (Prop.get_sigma prop)
    | _ -> () in
  IList.iter do_hpred (Prop.get_sigma prop);
  !res

(** return a description explaining value [exp] in [prop] in terms of a source expression
    using the formal parameters of the call *)
let explain_dereference_as_caller_expression
    ?use_buckets: (use_buckets = false)
    deref_str actual_pre spec_pre exp node loc formal_params =
  let find_formal_param_number name =
    let rec find n = function
      | [] -> 0
      | v :: pars ->
          if Mangled.equal (Pvar.get_name v) name then n
          else find (n + 1) pars in
    find 1 formal_params in
  match find_with_exp spec_pre exp with
  | Some (pv, pvar_off) ->
      if !verbose then L.d_strln ("pvar: " ^ (Pvar.to_string pv));
      let pv_name = Pvar.get_name pv in
      if Pvar.is_global pv
      then
        let dexp = exp_lv_dexp node (Sil.Lvar pv) in
        create_dereference_desc ~use_buckets dexp deref_str actual_pre loc
      else if Pvar.is_callee pv then
        let position = find_formal_param_number pv_name in
        if !verbose then L.d_strln ("parameter number: " ^ string_of_int position);
        explain_nth_function_parameter use_buckets deref_str actual_pre position pvar_off
      else
      if Prop.has_dangling_uninit_attribute spec_pre exp then
        Localise.desc_uninitialized_dangling_pointer_deref deref_str (Pvar.to_string pv) loc
      else Localise.no_desc
  | None ->
      if !verbose
      then (L.d_str "explain_dereference_as_caller_expression ";
            Sil.d_exp exp; L.d_str ": cannot explain None "; L.d_ln ());
      Localise.no_desc

(** explain a class cast exception *)
let explain_class_cast_exception pname_opt typ1 typ2 exp node loc =
  let exp_str_opt = match exp_rv_dexp node exp with
    | Some dexp -> Some (Sil.dexp_to_string dexp)
    | None -> None in
  match exp_rv_dexp node typ1, exp_rv_dexp node typ2 with
  | Some de1, Some de2 ->
      let typ_str1 = Sil.dexp_to_string de1 in
      let typ_str2 = Sil.dexp_to_string de2 in
      Localise.desc_class_cast_exception pname_opt typ_str1 typ_str2 exp_str_opt loc
  | _ -> Localise.no_desc

(** explain a division by zero *)
let explain_divide_by_zero exp node loc =
  match exp_rv_dexp node exp with
  | Some de ->
      let exp_str = Sil.dexp_to_string de in
      Localise.desc_divide_by_zero exp_str loc
  | None -> Localise.no_desc

(** explain a return expression required *)
let explain_return_expression_required loc typ =
  let typ_str =
    let pp fmt () = Sil.pp_typ_full pe_text fmt typ in
    pp_to_string pp () in
  Localise.desc_return_expression_required typ_str loc

(** Explain retain cycle value error *)
let explain_retain_cycle prop cycle loc dotty_str =
  Localise.desc_retain_cycle prop cycle loc dotty_str

(** Explain a tainted value error *)
let explain_tainted_value_reaching_sensitive_function
    prop e { Sil.taint_source; taint_kind } sensitive_fun loc =
  let var_desc =
    match e with
    | Sil.Lvar pv -> Pvar.to_string pv
    | _ ->
        begin
          match find_with_exp prop e with
          | Some (pvar, pvar_off) ->
              let dexp = dexp_apply_pvar_off (Sil.Dpvar pvar) pvar_off in
              Sil.dexp_to_string dexp
          | None -> Sil.exp_to_string e
        end in
  Localise.desc_tainted_value_reaching_sensitive_function
    taint_kind
    var_desc
    taint_source
    sensitive_fun
    loc

(** explain a return statement missing *)
let explain_return_statement_missing loc =
  Localise.desc_return_statement_missing loc

(** explain a fronend warning *)
let explain_frontend_warning loc =
  Localise.desc_frontend_warning loc

(** explain a comparing floats for equality *)
let explain_comparing_floats_for_equality loc =
  Localise.desc_comparing_floats_for_equality loc

(** explain a condition is an assignment *)
let explain_condition_is_assignment loc =
  Localise.desc_condition_is_assignment loc

(** explain a condition which is always true or false *)
let explain_condition_always_true_false i cond node loc =
  let cond_str_opt = match exp_rv_dexp node cond with
    | Some de ->
        Some (Sil.dexp_to_string de)
    | None -> None in
  Localise.desc_condition_always_true_false i cond_str_opt loc

(** explain the escape of a stack variable address from its scope *)
let explain_stack_variable_address_escape loc pvar addr_dexp_opt =
  let addr_dexp_str = match addr_dexp_opt with
    | Some (Sil.Dpvar pv)
      when Pvar.is_local pv &&
           Mangled.equal (Pvar.get_name pv) Ident.name_return ->
        Some "the caller via a return"
    | Some dexp -> Some (Sil.dexp_to_string dexp)
    | None -> None in
  Localise.desc_stack_variable_address_escape (Pvar.to_string pvar) addr_dexp_str loc

(** explain unary minus applied to unsigned expression *)
let explain_unary_minus_applied_to_unsigned_expression exp typ node loc =
  let exp_str_opt = match exp_rv_dexp node exp with
    | Some de -> Some (Sil.dexp_to_string de)
    | None -> None in
  let typ_str =
    let pp fmt () = Sil.pp_typ_full pe_text fmt typ in
    pp_to_string pp () in
  Localise.desc_unary_minus_applied_to_unsigned_expression exp_str_opt typ_str loc

(** explain a test for NULL of a dereferenced pointer *)
let explain_null_test_after_dereference exp node line loc =
  match exp_rv_dexp node exp with
  | Some de ->
      let expr_str = Sil.dexp_to_string de in
      Localise.desc_null_test_after_dereference expr_str line loc
  | None -> Localise.no_desc

let _warning loc fmt fmt_string =
  if !Config.developer_mode then
    begin
      F.fprintf
        fmt "%s:%d: Warning: " (DB.source_file_to_string !DB.current_source) loc.Location.line;
      F.fprintf fmt fmt_string
    end
  else
    F.ifprintf fmt fmt_string

(** Print a warning to the err stream at the given location (note: only prints in developer mode) *)
let warning_err loc fmt_string =
  _warning loc (Logging.get_err_formatter ()) fmt_string
