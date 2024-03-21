(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Attribute manipulation in Propositions (i.e., Symbolic Heaps) *)

(** Check whether an atom is used to mark an attribute *)
let is_pred atom = match atom with Predicates.(Apred _ | Anpred _) -> true | _ -> false

(** Add an attribute associated to the argument expressions *)
let add tenv ?(footprint = false) ?(polarity = true) prop attr args =
  Prop.prop_atom_and tenv ~footprint prop
    (if polarity then Predicates.Apred (attr, args) else Predicates.Anpred (attr, args))


let attributes_in_same_category attr1 attr2 =
  let cat1 = PredSymb.to_category attr1 in
  let cat2 = PredSymb.to_category attr2 in
  PredSymb.equal_category cat1 cat2


(** Replace an attribute associated to the expression *)
let add_or_replace_check_changed tenv prop atom =
  match (atom : Predicates.atom) with
  | Apred (att0, (_ :: _ as exps0)) | Anpred (att0, (_ :: _ as exps0)) ->
      let pairs = List.map ~f:(fun e -> (e, Prop.exp_normalize_prop tenv prop e)) exps0 in
      let _, nexp = List.hd_exn pairs in
      (* len exps0 > 0 by match *)
      let atom_map = function
        | Predicates.(Apred (att, exp :: _) | Anpred (att, exp :: _))
          when Exp.equal nexp exp && attributes_in_same_category att att0 ->
            atom
        | atom' ->
            atom'
      in
      let pi = prop.Prop.pi in
      let pi' = IList.map_changed ~equal:Predicates.equal_atom ~f:atom_map pi in
      if phys_equal pi pi' then Prop.prop_atom_and tenv prop atom
      else Prop.normalize tenv (Prop.set prop ~pi:pi')
  | _ ->
      prop


let add_or_replace tenv prop atom =
  (* wrapper for the most common case: do nothing *)
  add_or_replace_check_changed tenv prop atom


(** Get all the attributes of the prop *)
let get_all (prop : 'a Prop.t) =
  let res = ref [] in
  let do_atom a = if is_pred a then res := a :: !res in
  List.iter ~f:do_atom prop.pi ;
  List.rev !res


(** Get the attribute associated to the expression, if any *)
let get_for_exp tenv (prop : 'a Prop.t) exp =
  let nexp = Prop.exp_normalize_prop tenv prop exp in
  let atom_get_attr attributes atom =
    match (atom : Predicates.atom) with
    | (Apred (_, es) | Anpred (_, es)) when List.mem ~equal:Exp.equal es nexp ->
        atom :: attributes
    | _ ->
        attributes
  in
  List.fold ~f:atom_get_attr ~init:[] prop.pi


let get tenv prop exp category =
  let atts = get_for_exp tenv prop exp in
  List.find
    ~f:(function
      | Predicates.(Apred (att, _) | Anpred (att, _)) ->
          PredSymb.equal_category (PredSymb.to_category att) category
      | _ ->
          false )
    atts


let get_undef tenv prop exp = get tenv prop exp ACundef

let get_resource tenv prop exp = get tenv prop exp ACresource

let get_objc_null tenv prop exp = get tenv prop exp ACobjc_null

let get_observer tenv prop exp = get tenv prop exp ACobserver

let get_wontleak tenv prop exp = get tenv prop exp ACwontleak

let has_dangling_uninit tenv prop exp =
  let la = get_for_exp tenv prop exp in
  List.exists
    ~f:(function Predicates.Apred (a, _) -> PredSymb.equal a (Adangling DAuninit) | _ -> false)
    la


let filter_atoms tenv ~f prop =
  let pi0 = prop.Prop.pi in
  let pi1 = IList.filter_changed ~f pi0 in
  if phys_equal pi1 pi0 then prop else Prop.normalize tenv (Prop.set prop ~pi:pi1)


let remove tenv prop atom =
  if is_pred atom then
    let natom = Prop.atom_normalize_prop tenv prop atom in
    let f a = not (Predicates.equal_atom natom a) in
    filter_atoms tenv ~f prop
  else prop


(** Remove an attribute from all the atoms in the heap *)
let remove_for_attr tenv prop att0 =
  let f = function
    | Predicates.(Apred (att, _) | Anpred (att, _)) ->
        not (PredSymb.equal att0 att)
    | _ ->
        true
  in
  filter_atoms tenv ~f prop


let remove_resource tenv ra_kind ra_res =
  let f = function
    | Predicates.Apred (Aresource res_action, _) ->
        PredSymb.compare_res_act_kind res_action.ra_kind ra_kind <> 0
        || PredSymb.compare_resource res_action.ra_res ra_res <> 0
    | _ ->
        true
  in
  filter_atoms tenv ~f


(** Apply f to every resource attribute in the prop *)
let map_resource tenv prop f =
  let attribute_map e = function
    | PredSymb.Aresource ra ->
        PredSymb.Aresource (f e ra)
    | att ->
        att
  in
  let atom_map = function
    | Predicates.Apred (att, ([e] as es)) ->
        Predicates.Apred (attribute_map e att, es)
    | Predicates.Anpred (att, ([e] as es)) ->
        Predicates.Anpred (attribute_map e att, es)
    | atom ->
        atom
  in
  let pi0 = prop.Prop.pi in
  let pi1 = IList.map_changed ~equal:Predicates.equal_atom ~f:atom_map pi0 in
  if phys_equal pi1 pi0 then prop else Prop.normalize tenv (Prop.set prop ~pi:pi1)


(* Replace an attribute OBJC_NULL($n1) with OBJC_NULL(var) when var = $n1, and also sets $n1 =
   0 *)
let replace_objc_null tenv prop lhs_exp rhs_exp =
  match (get_objc_null tenv prop rhs_exp, rhs_exp) with
  | Some atom, Exp.Var _ ->
      let prop = remove tenv prop atom in
      let prop = Prop.conjoin_eq tenv rhs_exp Exp.zero prop in
      let natom = Predicates.atom_replace_exp [(rhs_exp, lhs_exp)] atom in
      add_or_replace tenv prop natom
  | _ ->
      prop


let rec nullify_exp_with_objc_null tenv prop exp =
  match exp with
  | Exp.BinOp (_, exp1, exp2) ->
      let prop' = nullify_exp_with_objc_null tenv prop exp1 in
      nullify_exp_with_objc_null tenv prop' exp2
  | Exp.UnOp (_, exp, _) ->
      nullify_exp_with_objc_null tenv prop exp
  | Exp.Var _ -> (
    match get_objc_null tenv prop exp with
    | Some atom ->
        let prop' = remove tenv prop atom in
        Prop.conjoin_eq tenv exp Exp.zero prop'
    | _ ->
        prop )
  | _ ->
      prop


(** mark Exp.Var's or Exp.Lvar's as undefined The annotations of the return type of the method get
    propagated to the return id, with the exception of when the return type is a struct, and we
    translate it as passing a reference to the method. *)
let mark_vars_as_undefined tenv prop ~ret_exp ~undefined_actuals_by_ref callee_pname ret_annots loc
    path_pos =
  let mark_var_as_undefined ~annot exp prop =
    match exp with
    | Exp.(Var _ | Lvar _) ->
        let att_undef = PredSymb.Aundef (callee_pname, annot, loc, path_pos) in
        add_or_replace tenv prop (Apred (att_undef, [exp]))
    | _ ->
        prop
  in
  let prop_with_ret_attr = mark_var_as_undefined ~annot:ret_annots ret_exp prop in
  List.fold
    ~f:(fun prop id -> mark_var_as_undefined ~annot:[] id prop)
    ~init:prop_with_ret_attr undefined_actuals_by_ref


(** type for arithmetic problems *)
type arith_problem =
  (* division by zero *)
  | Div0 of Exp.t
  (* unary minus of unsigned type applied to the given expression *)
  | UminusUnsigned of Exp.t * Typ.t

(** Look for an arithmetic problem in [exp] *)
let find_arithmetic_problem tenv proc_node_session prop exp =
  let exps_divided = ref [] in
  let uminus_unsigned = ref [] in
  let res = ref prop in
  let check_zero e =
    match Prop.exp_normalize_prop tenv prop e with
    | Exp.Const c when Const.iszero_int_float c ->
        true
    | _ ->
        res := add_or_replace tenv !res (Apred (Adiv0 proc_node_session, [e])) ;
        false
  in
  let rec walk = function
    | Exp.Var _ ->
        ()
    | Exp.UnOp
        ( Unop.Neg
        , e
        , Some
            ( {Typ.desc= Tint (Typ.IUChar | Typ.IUInt | Typ.IUShort | Typ.IULong | Typ.IULongLong)}
              as typ ) ) ->
        uminus_unsigned := (e, typ) :: !uminus_unsigned
    | Exp.UnOp (_, e, _) ->
        walk e
    | Exp.BinOp (op, e1, e2) ->
        if Binop.equal op DivI || Binop.equal op DivF || Binop.equal op Mod then
          exps_divided := e2 :: !exps_divided ;
        walk e1 ;
        walk e2
    | Exp.Exn _ ->
        ()
    | Exp.Closure _ ->
        ()
    | Exp.Const _ ->
        ()
    | Exp.Cast (_, e) ->
        walk e
    | Exp.Lvar _ ->
        ()
    | Exp.Lfield (e, _, _) ->
        walk e
    | Exp.Lindex (e1, e2) ->
        walk e1 ;
        walk e2
    | Exp.Sizeof {dynamic_length= None} ->
        ()
    | Exp.Sizeof {dynamic_length= Some len} ->
        walk len
  in
  walk exp ;
  let problem_opt =
    match (List.find ~f:check_zero !exps_divided, !uminus_unsigned) with
    | Some e, _ ->
        Some (Div0 e)
    | None, (e, t) :: _ ->
        Some (UminusUnsigned (e, t))
    | None, [] ->
        None
  in
  (problem_opt, !res)


(** Deallocate the stack variables in [pvars], and replace them by normal variables. Return the list
    of stack variables whose address was still present after deallocation. *)
let deallocate_stack_vars tenv (p : 'a Prop.t) pvars =
  let filter = function
    | Predicates.Hpointsto (Exp.Lvar v, _, _) ->
        List.exists ~f:(Pvar.equal v) pvars
    | _ ->
        false
  in
  let sigma_stack, sigma_other = List.partition_tf ~f:filter p.sigma in
  let fresh_address_vars = ref [] in
  (* fresh vars substituted for the address of stack vars *)
  let stack_vars_address_in_post = ref [] in
  (* stack vars whose address is still present *)
  let exp_replace =
    List.map
      ~f:(function
        | Predicates.Hpointsto (Exp.Lvar v, _, _) ->
            let freshv = Ident.create_fresh Ident.kprimed in
            fresh_address_vars := (v, freshv) :: !fresh_address_vars ;
            (Exp.Lvar v, Exp.Var freshv)
        | _ ->
            assert false )
      sigma_stack
  in
  let pi1 =
    List.map ~f:(fun (id, e) -> Predicates.Aeq (Exp.Var id, e)) (Predicates.sub_to_list p.sub)
  in
  let pi = List.map ~f:(Predicates.atom_replace_exp exp_replace) (p.pi @ pi1) in
  let p' =
    Prop.normalize tenv
      (Prop.set p ~sub:Predicates.sub_empty
         ~sigma:(Prop.sigma_replace_exp tenv exp_replace sigma_other) )
  in
  let p'' =
    let res = ref p' in
    let p'_fav = Prop.free_vars p' |> Ident.set_of_sequence in
    let do_var (v, freshv) =
      (* static locals are not stack-allocated *)
      if not (Pvar.is_static_local v) then
        (* the address of a de-allocated stack var is in the post *)
        if Ident.Set.mem freshv p'_fav then (
          stack_vars_address_in_post := v :: !stack_vars_address_in_post ;
          let pred = Predicates.Apred (Adangling DAaddr_stack_var, [Exp.Var freshv]) in
          res := add_or_replace tenv !res pred )
    in
    List.iter ~f:do_var !fresh_address_vars ;
    !res
  in
  (* Filter out local addresses in p'' *)
  let filtered_pi, changed =
    List.fold_right p''.pi ~init:([], false) ~f:(fun a (filtered, changed) ->
        if Predicates.atom_has_local_addr a then (filtered, true) else (a :: filtered, changed) )
  in
  (* Avoid normalization when p'' does not change *)
  let p''' = if changed then Prop.normalize tenv (Prop.set p'' ~pi:filtered_pi) else p'' in
  (!stack_vars_address_in_post, List.fold ~f:(Prop.prop_atom_and tenv) ~init:p''' pi)


(** Input of this method is an exp in a prop. Output is a formal variable or path from a formal
    variable that is equal to the expression, or the OBJC_NULL attribute of the expression. *)
let find_equal_formal_path tenv e prop =
  let rec find_in_sigma e seen_hpreds =
    List.fold_right
      ~f:(fun hpred res ->
        if List.mem ~equal:Predicates.equal_hpred seen_hpreds hpred then None
        else
          let seen_hpreds = hpred :: seen_hpreds in
          match res with
          | Some _ ->
              res
          | None -> (
            match (hpred : Predicates.hpred) with
            | Hpointsto (Exp.Lvar pvar1, Eexp (exp2, Predicates.Iformal (_, _)), _)
              when Exp.equal exp2 e && (Pvar.is_local pvar1 || Pvar.is_seed pvar1) ->
                Some (Exp.Lvar pvar1)
            | Hpointsto (exp1, Estruct (fields, _), _) ->
                List.fold_right
                  ~f:(fun (field, strexp) res ->
                    match res with
                    | Some _ ->
                        res
                    | None -> (
                      match strexp with
                      | Predicates.Eexp (exp2, _) when Exp.equal exp2 e -> (
                        match find_in_sigma exp1 seen_hpreds with
                        | Some vfs ->
                            Some (Exp.Lfield (vfs, field, StdTyp.void))
                        | None ->
                            None )
                      | _ ->
                          None ) )
                  fields ~init:None
            | _ ->
                None ) )
      prop.Prop.sigma ~init:None
  in
  match find_in_sigma e [] with
  | Some vfs ->
      Some vfs
  | None -> (
    match get_objc_null tenv prop e with
    | Some (Apred (Aobjc_null, [_; vfs])) ->
        Some vfs
    | _ ->
        None )
