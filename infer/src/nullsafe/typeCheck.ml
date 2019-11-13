(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module DExp = DecompiledExp

(** Module for type checking. *)

(** Module to treat selected complex expressions as constants. *)
module ComplexExpressions = struct
  let procname_instanceof = Typ.Procname.equal BuiltinDecl.__instanceof

  let procname_is_false_on_null tenv pn =
    match PatternMatch.lookup_attributes tenv pn with
    | Some proc_attributes ->
        let annotated_signature = Models.get_modelled_annotated_signature tenv proc_attributes in
        let AnnotatedSignature.{ret_annotation_deprecated} = annotated_signature.ret in
        Annotations.ia_is_false_on_null ret_annotation_deprecated
    | None ->
        false


  let procname_is_true_on_null tenv pn =
    let annotated_true_on_null () =
      match PatternMatch.lookup_attributes tenv pn with
      | Some proc_attributes ->
          let annotated_signature = Models.get_modelled_annotated_signature tenv proc_attributes in
          let AnnotatedSignature.{ret_annotation_deprecated} = annotated_signature.ret in
          Annotations.ia_is_true_on_null ret_annotation_deprecated
      | None ->
          false
    in
    Models.is_true_on_null pn || annotated_true_on_null ()


  let procname_containsKey = Models.is_containsKey

  (** Recognize *all* the procedures treated specially in conditionals *)
  let procname_used_in_condition pn =
    procname_instanceof pn || procname_containsKey pn || BuiltinDecl.is_declared pn


  exception Not_handled

  (* Convert an expression to a unique string. *)
  (* This is used to turn complex expressions into pvar's.*)
  (* Arbitrary function parameters and field access are allowed *)
  (* when the relevant options are active. *)
  let exp_to_string_map_dexp tenv map_dexp node' exp =
    let rec dexp_to_string dexp =
      let case_not_handled () = raise Not_handled in
      match dexp with
      | DExp.Darray (de1, de2) ->
          dexp_to_string de1 ^ "[" ^ dexp_to_string de2 ^ "]"
      | DExp.Darrow (de, f) | DExp.Ddot (de, f) ->
          dexp_to_string de ^ "." ^ Typ.Fieldname.to_string f
      | DExp.Dbinop (op, de1, de2) ->
          "(" ^ dexp_to_string de1 ^ Binop.str Pp.text op ^ dexp_to_string de2 ^ ")"
      | DExp.Dconst (Const.Cfun pn) ->
          Typ.Procname.to_unique_id pn
      | DExp.Dconst c ->
          F.asprintf "%a" (Const.pp Pp.text) c
      | DExp.Dderef de ->
          dexp_to_string de
      | DExp.Dfcall (fun_dexp, args, _, {CallFlags.cf_virtual= isvirtual})
      | DExp.Dretcall (fun_dexp, args, _, {CallFlags.cf_virtual= isvirtual}) ->
          let pp_arg fmt de = F.pp_print_string fmt (dexp_to_string de) in
          let pp_args fmt des = Pp.comma_seq pp_arg fmt des in
          let pp fmt =
            let virt = if isvirtual then "V" else "" in
            F.fprintf fmt "%a(%a)%s" pp_arg fun_dexp pp_args args virt
          in
          F.asprintf "%t" pp
      | (DExp.Dpvar pv | DExp.Dpvaraddr pv) when not (Pvar.is_frontend_tmp pv) ->
          Pvar.to_string pv
      | DExp.Dpvar _ | DExp.Dpvaraddr _ (* front-end variable -- this should not happen) *) ->
          case_not_handled ()
      | DExp.Dunop (op, de) ->
          Unop.to_string op ^ dexp_to_string de
      | DExp.Dsizeof _ ->
          case_not_handled ()
      | DExp.Dunknown ->
          case_not_handled ()
    in
    match map_dexp (Errdesc.exp_rv_dexp tenv node' exp) with
    | Some de -> (
      try Some (dexp_to_string de) with Not_handled -> None )
    | None ->
        None


  let exp_to_string tenv node' exp =
    let map_dexp de_opt = de_opt in
    exp_to_string_map_dexp tenv map_dexp node' exp
end

(* ComplexExpressions *)

type check_return_type = Typ.Procname.t -> Procdesc.t -> Typ.t -> Typ.t option -> Location.t -> unit

type find_canonical_duplicate = Procdesc.Node.t -> Procdesc.Node.t

type checks = {eradicate: bool; check_ret_type: check_return_type list}

(** Typecheck an expression. *)
let rec typecheck_expr ~is_strict_mode find_canonical_duplicate visited checks tenv node instr_ref
    (curr_pdesc : Procdesc.t) typestate e tr_default loc : TypeState.range =
  match e with
  (* null literal or 0 *)
  | _ when Exp.is_null_literal e ->
      let typ, _ = tr_default in
      (* 0 is not the same thing as null. They are encoded as the same thing in SIL.
         We distinct them by type.
      *)
      if PatternMatch.type_is_class typ then
        (typ, InferredNullability.create (TypeOrigin.NullConst loc))
      else
        (* 0 const (this is not the same as null) *)
        (typ, InferredNullability.create (TypeOrigin.NonnullConst loc))
  | Exp.Const _ ->
      let typ, _ = tr_default in
      (* We already considered case of null literal above, so this is a non-null const. *)
      (typ, InferredNullability.create (TypeOrigin.NonnullConst loc))
  | Exp.Lvar pvar ->
      Option.value (TypeState.lookup_pvar pvar typestate) ~default:tr_default
  | Exp.Var id ->
      Option.value (TypeState.lookup_id id typestate) ~default:tr_default
  | Exp.Exn e1 ->
      typecheck_expr ~is_strict_mode find_canonical_duplicate visited checks tenv node instr_ref
        curr_pdesc typestate e1 tr_default loc
  | Exp.Lfield (exp, field_name, typ) ->
      let _, _ = tr_default in
      let _, inferred_nullability =
        typecheck_expr ~is_strict_mode find_canonical_duplicate visited checks tenv node instr_ref
          curr_pdesc typestate exp
          (* TODO(T54687014) optimistic default might be an unsoundness issue - investigate *)
          (typ, InferredNullability.create TypeOrigin.OptimisticFallback)
          loc
      in
      let object_origin = InferredNullability.get_origin inferred_nullability in
      let tr_new =
        match AnnotatedField.get tenv field_name typ with
        | Some AnnotatedField.{annotated_type= field_type} ->
            ( field_type.typ
            , InferredNullability.create
                (TypeOrigin.Field {object_origin; field_name; field_type; access_loc= loc}) )
        | None ->
            tr_default
      in
      if checks.eradicate then
        EradicateChecks.check_object_dereference ~is_strict_mode tenv find_canonical_duplicate
          curr_pdesc node instr_ref exp (DereferenceRule.AccessToField field_name)
          inferred_nullability loc ;
      tr_new
  | Exp.Lindex (array_exp, index_exp) ->
      let _, inferred_nullability =
        typecheck_expr ~is_strict_mode find_canonical_duplicate visited checks tenv node instr_ref
          curr_pdesc typestate array_exp tr_default loc
      in
      let index_desc =
        match EradicateChecks.explain_expr tenv node index_exp with Some s -> s | None -> "?"
      in
      if checks.eradicate then
        EradicateChecks.check_object_dereference ~is_strict_mode tenv find_canonical_duplicate
          curr_pdesc node instr_ref array_exp
          (DereferenceRule.AccessByIndex {index_desc})
          inferred_nullability loc ;
      tr_default
  | _ ->
      tr_default


(* Handle the case where a field access X.f happens via a temporary variable $Txxx.
     This has been observed in assignments this.f = exp when exp contains an ifthenelse.
     Reconstuct the original expression knowing: the origin of $Txxx is 'this'. *)
let handle_field_access_via_temporary idenv curr_pname typestate exp =
  let name_is_temporary name =
    let prefix = "$T" in
    String.is_prefix ~prefix name
  in
  let pvar_get_origin pvar =
    match TypeState.lookup_pvar pvar typestate with
    | Some (_, inferred_nullability) ->
        Some (InferredNullability.get_origin inferred_nullability)
    | None ->
        None
  in
  let handle_temporary e =
    match Idenv.expand_expr idenv e with
    | Exp.Lvar pvar when name_is_temporary (Pvar.to_string pvar) -> (
      match pvar_get_origin pvar with
      | Some TypeOrigin.This ->
          let pvar' = Pvar.mk Mangled.this curr_pname in
          Some (Exp.Lvar pvar')
      | _ ->
          None )
    | _ ->
        None
  in
  match exp with
  | Exp.Lfield (e, fn, typ) ->
      let exp' =
        match handle_temporary e with Some e' -> Exp.Lfield (e', fn, typ) | None -> exp
      in
      exp'
  | _ ->
      exp


(* Convert a function call to a pvar. *)
let handle_function_call tenv curr_pname typestate exp default ~is_assignment ~call_node ~node id =
  match Errdesc.find_normal_variable_funcall call_node id with
  | Some (Exp.Const (Const.Cfun pn), _, _, _)
    when not (ComplexExpressions.procname_used_in_condition pn) -> (
    match ComplexExpressions.exp_to_string tenv node exp with
    | None ->
        default
    | Some exp_str ->
        let pvar = Pvar.mk (Mangled.from_string exp_str) curr_pname in
        let already_defined_in_typestate =
          match TypeState.lookup_pvar pvar typestate with
          | Some (_, inferred_nullability) ->
              not
                (TypeOrigin.equal TypeOrigin.Undef
                   (InferredNullability.get_origin inferred_nullability))
          | None ->
              false
        in
        if is_assignment && already_defined_in_typestate then default
          (* Don't overwrite pvar representing result of function call. *)
        else (Exp.Lvar pvar, typestate) )
  | _ ->
      default


(* If this is an assignment, update the typestate for a field access pvar. *)
let update_typestate_fld ~is_assignment tenv access_loc typestate pvar object_origin field_name typ
    =
  match TypeState.lookup_pvar pvar typestate with
  | Some _ when not is_assignment ->
      typestate
  | _ -> (
    match AnnotatedField.get tenv field_name typ with
    | Some AnnotatedField.{annotated_type= field_type} ->
        let range =
          ( field_type.typ
          , InferredNullability.create
              (TypeOrigin.Field {object_origin; field_name; field_type; access_loc}) )
        in
        TypeState.add pvar range typestate
    | None ->
        typestate )


(* When [is_assignment] is true, update the relevant annotations for the pvar. *)
(* So what is the difference between ~node and ~original_node? I don't know. This is an artifact of refactoring of
   very old code. Sorry, dear future supporter, if names don't make sense. *)
let convert_complex_exp_to_pvar tenv idenv curr_pname
    (curr_annotated_signature : AnnotatedSignature.t) ~node ~(original_node : Procdesc.Node.t)
    ~is_assignment exp_ typestate loc =
  let exp =
    handle_field_access_via_temporary idenv curr_pname typestate (Idenv.expand_expr idenv exp_)
  in
  let default = (exp, typestate) in
  match exp with
  | Exp.Var id when Errdesc.find_normal_variable_funcall node id <> None ->
      handle_function_call tenv curr_pname typestate exp default ~is_assignment ~call_node:node
        ~node id
  | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
      let frontend_variable_assignment =
        Errdesc.find_program_variable_assignment original_node pvar
      in
      match frontend_variable_assignment with
      | Some (call_node, id) ->
          handle_function_call tenv curr_pname typestate exp default ~is_assignment ~call_node ~node
            id
      | _ ->
          default )
  | Exp.Lvar _ ->
      default
  | Exp.Lfield (exp_, fn, typ) ->
      let inner_origin =
        ( match exp_ with
        | Exp.Lvar pvar ->
            TypeState.lookup_pvar pvar typestate
        | Exp.Var id ->
            TypeState.lookup_id id typestate
        | _ ->
            None )
        |> Option.value_map
             ~f:(fun (_, nullability) -> InferredNullability.get_origin nullability)
             ~default:TypeOrigin.OptimisticFallback
      in
      let exp' = Idenv.expand_expr_temps idenv original_node exp_ in
      let is_parameter_field pvar =
        (* parameter.field *)
        let name = Pvar.get_name pvar in
        let filter AnnotatedSignature.{mangled} = Mangled.equal mangled name in
        List.exists ~f:filter curr_annotated_signature.params
      in
      let is_static_field pvar =
        (* static field *)
        Pvar.is_global pvar
      in
      let pvar_to_str pvar =
        if Exp.is_this (Exp.Lvar pvar) then "" else Pvar.to_string pvar ^ "_"
      in
      let res =
        match exp' with
        | Exp.Lvar pv when is_parameter_field pv || is_static_field pv ->
            let fld_name = pvar_to_str pv ^ Typ.Fieldname.to_string fn in
            let pvar = Pvar.mk (Mangled.from_string fld_name) curr_pname in
            let typestate' =
              update_typestate_fld ~is_assignment tenv loc typestate pvar inner_origin fn typ
            in
            (Exp.Lvar pvar, typestate')
        | Exp.Lfield (_exp', fn', _) when Typ.Fieldname.Java.is_outer_instance fn' ->
            (* handle double dereference when accessing a field from an outer class *)
            let fld_name = Typ.Fieldname.to_string fn' ^ "_" ^ Typ.Fieldname.to_string fn in
            let pvar = Pvar.mk (Mangled.from_string fld_name) curr_pname in
            let typestate' =
              update_typestate_fld ~is_assignment tenv loc typestate pvar inner_origin fn typ
            in
            (Exp.Lvar pvar, typestate')
        | Exp.Lvar _ | Exp.Lfield _ -> (
          (* treat var.field1. ... .fieldn as a constant *)
          match ComplexExpressions.exp_to_string tenv node exp with
          | Some exp_str ->
              let pvar = Pvar.mk (Mangled.from_string exp_str) curr_pname in
              let typestate' =
                update_typestate_fld ~is_assignment tenv loc typestate pvar inner_origin fn typ
              in
              (Exp.Lvar pvar, typestate')
          | None ->
              default )
        | _ ->
            default
      in
      res
  | _ ->
      default


let constructor_check_calls_this curr_pname calls_this pn =
  match (curr_pname, pn) with
  | Typ.Procname.Java curr_pname_java, Typ.Procname.Java pn_java ->
      if
        String.equal
          (Typ.Procname.Java.get_class_name curr_pname_java)
          (Typ.Procname.Java.get_class_name pn_java)
      then calls_this := true
  | _ ->
      ()


(* Drops hidden and synthetic parameters which we do not check in a call. *)
let drop_unchecked_params calls_this curr_pname proc_attributes params =
  let pname = proc_attributes.ProcAttributes.proc_name in
  if Typ.Procname.is_constructor pname then
    match PatternMatch.get_this_type_nonstatic_methods_only proc_attributes with
    | Some _ ->
        constructor_check_calls_this curr_pname calls_this pname ;
        (* Drop reference parameters to this and outer objects. *)
        let is_hidden_parameter (n, _) =
          Mangled.is_this n || Str.string_match (Str.regexp "$bcvar[0-9]+") (Mangled.to_string n) 0
        in
        let rec drop_n_args ntl =
          match ntl with fp :: tail when is_hidden_parameter fp -> 1 + drop_n_args tail | _ -> 0
        in
        let n = drop_n_args proc_attributes.ProcAttributes.formals in
        let visible_params = IList.drop params n in
        (* Drop the trailing hidden parameter if the constructor is synthetic. *)
        if proc_attributes.ProcAttributes.is_synthetic_method then
          List.take visible_params (List.length visible_params - 1)
        else visible_params
    | None ->
        params
  else params


(* Drop parameters from the signature which we do not check in a call. *)
let drop_unchecked_signature_params proc_attributes annotated_signature =
  if
    Typ.Procname.is_constructor proc_attributes.ProcAttributes.proc_name
    && proc_attributes.ProcAttributes.is_synthetic_method
  then
    List.take annotated_signature.AnnotatedSignature.params
      (List.length annotated_signature.AnnotatedSignature.params - 1)
  else annotated_signature.AnnotatedSignature.params


(* Apply a function to a pvar and its associated content if front-end generated. *)
let pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc handle_pvar typestate
    pvar node =
  let typestate' = handle_pvar typestate pvar in
  let curr_node = TypeErr.InstrRef.get_node instr_ref in
  let frontent_variable_assignment =
    if Pvar.is_frontend_tmp pvar then Errdesc.find_program_variable_assignment curr_node pvar
    else None
  in
  match frontent_variable_assignment with
  | None ->
      typestate'
  | Some (node', id) -> (
      (* handle the case where pvar is a frontend-generated program variable *)
      let exp = Idenv.expand_expr idenv (Exp.Var id) in
      match
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node:node'
          ~original_node:node ~is_assignment:false exp typestate' loc
      with
      | Exp.Lvar pvar', _ ->
          handle_pvar typestate' pvar'
      | _ ->
          typestate' )


(* typecheck_expr with fewer parameters, using a common template for typestate range  *)
let typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks tenv
    node instr_ref typestate1 exp1 typ1 origin1 loc1 =
  typecheck_expr ~is_strict_mode find_canonical_duplicate calls_this checks tenv node instr_ref
    curr_pdesc typestate1 exp1
    (typ1, InferredNullability.create origin1)
    loc1


(* check if there are errors in exp1 *)
let typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks
    tenv node instr_ref typestate1 exp1 loc1 : unit =
  ignore
    (typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks
       tenv node instr_ref typestate1 exp1 (Typ.mk Tvoid) TypeOrigin.Undef loc1)


(* Handle Preconditions.checkNotNull. *)
let do_preconditions_check_not_null instr_ref tenv find_canonical_duplicate node loc curr_pdesc
    curr_pname curr_annotated_signature checks call_params idenv parameter_num ~is_vararg typestate'
    =
  (* clear the nullable flag of the first parameter of the procedure *)
  let clear_nullable_flag typestate'' pvar =
    (* remove the nullable flag for the given pvar *)
    match TypeState.lookup_pvar pvar typestate'' with
    | Some (t, nullability) ->
        let should_report =
          Config.eradicate_condition_redundant
          (* TODO: This condition should be extracted into a dedicated rule *)
          && InferredNullability.is_nonnull_or_declared_nonnull nullability
          && not (InferredNullability.origin_is_fun_library nullability)
        in
        ( if checks.eradicate && should_report then
          let cond = Exp.BinOp (Binop.Ne, Exp.Lvar pvar, Exp.null) in
          EradicateChecks.report_error tenv find_canonical_duplicate
            (TypeErr.Condition_redundant (true, EradicateChecks.explain_expr tenv node cond))
            (Some instr_ref) loc curr_pdesc ) ;
        let previous_origin = InferredNullability.get_origin nullability in
        let new_origin = TypeOrigin.InferredNonnull {previous_origin} in
        TypeState.add pvar (t, InferredNullability.create new_origin) typestate''
    | None ->
        typestate'
  in
  let rec find_parameter n eetl1 =
    match (n, eetl1) with
    | n, _ :: eetl2 when n > 1 ->
        find_parameter (n - 1) eetl2
    | 1, ((_, Exp.Lvar pvar), typ) :: _ ->
        Some (pvar, typ)
    | _ ->
        None
  in
  match find_parameter parameter_num call_params with
  | Some (pvar, _) ->
      if is_vararg then
        let do_vararg_value e ts =
          match Idenv.expand_expr idenv e with
          | Exp.Lvar pvar1 ->
              pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc
                clear_nullable_flag ts pvar1 node
          | _ ->
              ts
        in
        let vararg_values = PatternMatch.java_get_vararg_values node pvar idenv in
        List.fold_right ~f:do_vararg_value vararg_values ~init:typestate'
      else
        pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc clear_nullable_flag
          typestate' pvar node
  | None ->
      typestate'


(* Handle Preconditions.checkState for &&-separated conditions x!=null. *)
let do_preconditions_check_state instr_ref idenv tenv curr_pname curr_annotated_signature
    call_params loc node typestate' =
  let set_nonnull_to_pvar typestate1 pvar =
    (* handle the annotation flag for pvar *)
    match TypeState.lookup_pvar pvar typestate1 with
    | Some (t, nullability) ->
        let previous_origin = InferredNullability.get_origin nullability in
        let new_origin = TypeOrigin.InferredNonnull {previous_origin} in
        TypeState.add pvar (t, InferredNullability.create new_origin) typestate1
    | None ->
        typestate1
  in
  let res_typestate = ref typestate' in
  let set_nonnull pvar =
    (* set nullability for pvar *)
    res_typestate :=
      pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc set_nonnull_to_pvar
        !res_typestate pvar node
  in
  let handle_negated_condition cond_node =
    let do_instr instr =
      let set_flag expression =
        let cond_e = Idenv.expand_expr_temps idenv cond_node expression in
        match
          convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node:cond_node
            ~original_node:node ~is_assignment:false cond_e typestate' loc
        with
        | Exp.Lvar pvar', _ ->
            set_nonnull pvar'
        | _ ->
            ()
      in
      match instr with
      | Sil.Prune (Exp.BinOp (Binop.Eq, Exp.Const (Const.Cint i), cond_e_), _, _, _)
        when IntLit.iszero i ->
          set_flag cond_e_
      | Sil.Prune (Exp.BinOp (Binop.Eq, cond_e_, Exp.Const (Const.Cint i)), _, _, _)
        when IntLit.iszero i ->
          set_flag cond_e_
      | _ ->
          ()
    in
    Instrs.iter ~f:do_instr (Procdesc.Node.get_instrs cond_node)
  in
  match call_params with
  | ((_, Exp.Lvar pvar), _) :: _ -> (
      (* temporary variable for the value of the boolean condition *)
      let curr_node = TypeErr.InstrRef.get_node instr_ref in
      let branch = false in
      match Errdesc.find_boolean_assignment curr_node pvar branch with
      (* In foo(cond1 && cond2), the node that sets the result to false
         has all the negated conditions as parents. *)
      | Some boolean_assignment_node ->
          List.iter ~f:handle_negated_condition (Procdesc.Node.get_preds boolean_assignment_node) ;
          !res_typestate
      | None ->
          !res_typestate )
  | _ ->
      typestate'


(* Handle m.put(k,v) as assignment pvar = v for the pvar associated to m.get(k) *)
let do_map_put call_params callee_pname tenv loc node curr_pname curr_pdesc calls_this checks
    instr_ref ~is_strict_mode find_canonical_duplicate typestate' =
  (* Get the proc name for map.get() from map.put() *)
  let pname_get_from_pname_put pname_put =
    let object_t = Typ.Name.Java.Split.java_lang_object in
    let parameters = [object_t] in
    pname_put
    |> Typ.Procname.Java.replace_method_name "get"
    |> Typ.Procname.Java.replace_return_type object_t
    |> Typ.Procname.Java.replace_parameters parameters
  in
  match call_params with
  | ((_, Exp.Lvar pv_map), _) :: ((_, exp_key), _) :: ((_, exp_value), typ_value) :: _ -> (
      (* Convert the dexp for k to the dexp for m.get(k) *)
      let convert_dexp_key_to_dexp_get dopt =
        match (dopt, callee_pname) with
        | Some dexp_key, Typ.Procname.Java callee_pname_java ->
            let pname_get = Typ.Procname.Java (pname_get_from_pname_put callee_pname_java) in
            let dexp_get = DExp.Dconst (Const.Cfun pname_get) in
            let dexp_map = DExp.Dpvar pv_map in
            let args = [dexp_map; dexp_key] in
            let call_flags = {CallFlags.default with CallFlags.cf_virtual= true} in
            Some (DExp.Dretcall (dexp_get, args, loc, call_flags))
        | _ ->
            None
      in
      match
        ComplexExpressions.exp_to_string_map_dexp tenv convert_dexp_key_to_dexp_get node exp_key
      with
      | Some map_get_str ->
          let pvar_map_get = Pvar.mk (Mangled.from_string map_get_str) curr_pname in
          TypeState.add pvar_map_get
            (typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
               checks tenv node instr_ref typestate' exp_value typ_value TypeOrigin.Undef loc)
            typestate'
      | None ->
          typestate' )
  | _ ->
      typestate'


(* Handle assignment fron a temp pvar in a condition.
   This recognizes the handling of temp variables in ((x = ...) != null) *)
let handle_assignment_in_condition_for_sil_prune idenv node pvar =
  match Procdesc.Node.get_preds node with
  | [prev_node] ->
      let found = ref None in
      let do_instr i =
        match i with
        | Sil.Store {e1= e; e2= e'} when Exp.equal (Exp.Lvar pvar) (Idenv.expand_expr idenv e') ->
            found := Some e
        | _ ->
            ()
      in
      Instrs.iter ~f:do_instr (Procdesc.Node.get_instrs prev_node) ;
      !found
  | _ ->
      None


let rec normalize_cond_for_sil_prune_rec idenv ~node ~original_node cond =
  match cond with
  | Exp.UnOp (Unop.LNot, c, top) ->
      let node', c' = normalize_cond_for_sil_prune_rec idenv ~node ~original_node c in
      (node', Exp.UnOp (Unop.LNot, c', top))
  | Exp.BinOp (bop, c1, c2) ->
      let node', c1' = normalize_cond_for_sil_prune_rec idenv ~node ~original_node c1 in
      let node'', c2' = normalize_cond_for_sil_prune_rec idenv ~node:node' ~original_node c2 in
      (node'', Exp.BinOp (bop, c1', c2'))
  | Exp.Var _ ->
      let c' = Idenv.expand_expr idenv cond in
      if not (Exp.equal c' cond) then normalize_cond_for_sil_prune_rec idenv ~node ~original_node c'
      else (node, c')
  | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
    match handle_assignment_in_condition_for_sil_prune idenv original_node pvar with
    | None -> (
      match Errdesc.find_program_variable_assignment node pvar with
      | Some (node', id) ->
          (node', Exp.Var id)
      | None ->
          (node, cond) )
    | Some e2 ->
        (node, e2) )
  | c ->
      (node, c)


(* Normalize the condition by resolving temp variables. *)
let normalize_cond_for_sil_prune idenv ~node cond =
  normalize_cond_for_sil_prune_rec idenv ~node ~original_node:node cond


let rec check_condition_for_sil_prune tenv idenv calls_this find_canonical_duplicate loc curr_pname
    curr_pdesc curr_annotated_signature linereader typestate checks true_branch instr_ref
    ~is_strict_mode ~original_node ~node c : TypeState.t =
  (* check if the expression is coming from a call, and return the argument *)
  let from_call filter_callee e : Exp.t option =
    match e with
    | Exp.Var id -> (
      match Errdesc.find_normal_variable_funcall node id with
      | Some (Exp.Const (Const.Cfun pn), e1 :: _, _, _) when filter_callee pn ->
          Some e1
      | _ ->
          None )
    | _ ->
        None
  in
  (* check if the expression is coming from instanceof *)
  let from_instanceof e : Exp.t option = from_call ComplexExpressions.procname_instanceof e in
  (* check if the expression is coming from a procedure returning false on null *)
  let from_is_false_on_null e : Exp.t option =
    from_call (ComplexExpressions.procname_is_false_on_null tenv) e
  in
  (* check if the expression is coming from a procedure returning true on null *)
  let from_is_true_on_null e : Exp.t option =
    from_call (ComplexExpressions.procname_is_true_on_null tenv) e
  in
  (* check if the expression is coming from Map.containsKey *)
  let from_containsKey e : Exp.t option = from_call ComplexExpressions.procname_containsKey e in
  (* Turn x.containsKey(e) into the pvar for x.get(e) *)
  (* which is then treated as a normal condition != null. *)
  let handle_containsKey e =
    let map_dexp = function
      | Some
          (DExp.Dretcall
            (DExp.Dconst (Const.Cfun (Typ.Procname.Java pname_java)), args, loc, call_flags)) ->
          let pname_java' =
            let object_t = Typ.Name.Java.Split.java_lang_object in
            pname_java
            |> Typ.Procname.Java.replace_method_name "get"
            |> Typ.Procname.Java.replace_return_type object_t
          in
          let fun_dexp = DExp.Dconst (Const.Cfun (Typ.Procname.Java pname_java')) in
          Some (DExp.Dretcall (fun_dexp, args, loc, call_flags))
      | _ ->
          None
    in
    match ComplexExpressions.exp_to_string_map_dexp tenv map_dexp node e with
    | Some e_str ->
        let pvar = Pvar.mk (Mangled.from_string e_str) curr_pname in
        let e1 = Exp.Lvar pvar in
        let typ, ta =
          typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
            checks tenv original_node instr_ref typestate e1 (Typ.mk Tvoid)
            TypeOrigin.OptimisticFallback loc
        in
        let range = (typ, ta) in
        let typestate1 = TypeState.add pvar range typestate in
        (typestate1, e1, EradicateChecks.From_containsKey)
    | None ->
        (typestate, e, EradicateChecks.From_condition)
  in
  let set_nonnull e' typestate2 =
    let handle_pvar typestate' pvar =
      match TypeState.lookup_pvar pvar typestate' with
      | Some (t, current_nullability) ->
          if not (InferredNullability.is_nonnull current_nullability) then
            let new_origin =
              TypeOrigin.InferredNonnull
                {previous_origin= InferredNullability.get_origin current_nullability}
            in
            let new_nullability = InferredNullability.create new_origin in
            TypeState.add pvar (t, new_nullability) typestate'
          else typestate'
      | None ->
          typestate'
    in
    match e' with
    | Exp.Lvar pvar ->
        pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc handle_pvar
          typestate2 pvar original_node
    | _ ->
        typestate2
  in
  match[@warning "-57"] c with
  | Exp.BinOp (Binop.Eq, Exp.Const (Const.Cint i), e)
  | Exp.BinOp (Binop.Eq, e, Exp.Const (Const.Cint i))
    when IntLit.iszero i -> (
      typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
        checks tenv original_node instr_ref typestate e loc ;
      let typestate1, e1, from_call =
        match from_is_true_on_null e with
        | Some e1 ->
            (typestate, e1, EradicateChecks.From_is_true_on_null)
        | None ->
            (typestate, e, EradicateChecks.From_condition)
      in
      let e', typestate2 =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node ~is_assignment:false e1 typestate1 loc
      in
      let typ, inferred_nullability =
        typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks
          tenv original_node instr_ref typestate2 e' (Typ.mk Tvoid) TypeOrigin.OptimisticFallback
          loc
      in
      if checks.eradicate then
        EradicateChecks.check_zero tenv find_canonical_duplicate curr_pdesc node e' typ
          inferred_nullability true_branch EradicateChecks.From_condition idenv linereader loc
          instr_ref ;
      match from_call with
      | EradicateChecks.From_is_true_on_null ->
          (* if f returns true on null, then false branch implies != null *)
          if not (InferredNullability.is_nonnull inferred_nullability) then
            set_nonnull e' typestate2
          else typestate2
      | _ ->
          typestate2 )
  | Exp.BinOp (Binop.Ne, Exp.Const (Const.Cint i), e)
  | Exp.BinOp (Binop.Ne, e, Exp.Const (Const.Cint i))
    when IntLit.iszero i -> (
      typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
        checks tenv original_node instr_ref typestate e loc ;
      let typestate1, e1, from_call =
        match from_instanceof e with
        | Some e1 ->
            (* (e1 instanceof C) implies (e1 != null) *)
            (typestate, e1, EradicateChecks.From_instanceof)
        | None -> (
          match from_is_false_on_null e with
          | Some e1 ->
              (typestate, e1, EradicateChecks.From_is_false_on_null)
          | None ->
              if Option.is_some (from_containsKey e) then handle_containsKey e
              else (typestate, e, EradicateChecks.From_condition) )
      in
      let e', typestate2 =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node ~is_assignment:false e1 typestate1 loc
      in
      let typ, inferred_nullability =
        typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks
          tenv original_node instr_ref typestate2 e' (Typ.mk Tvoid) TypeOrigin.OptimisticFallback
          loc
      in
      if checks.eradicate then
        EradicateChecks.check_nonzero tenv find_canonical_duplicate curr_pdesc original_node e' typ
          inferred_nullability true_branch from_call idenv linereader loc instr_ref ;
      match from_call with
      | EradicateChecks.From_is_true_on_null ->
          typestate2
      | EradicateChecks.From_condition
      | EradicateChecks.From_containsKey
      | EradicateChecks.From_instanceof
      | EradicateChecks.From_is_false_on_null ->
          if not (InferredNullability.is_nonnull inferred_nullability) then
            set_nonnull e' typestate2
          else typestate2 )
  | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, e1, e2), _) ->
      check_condition_for_sil_prune tenv idenv calls_this find_canonical_duplicate loc curr_pname
        curr_pdesc curr_annotated_signature linereader typestate checks true_branch instr_ref
        ~is_strict_mode ~original_node ~node
        (Exp.BinOp (Binop.Ne, e1, e2))
  | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Ne, e1, e2), _) ->
      check_condition_for_sil_prune tenv idenv calls_this find_canonical_duplicate loc curr_pname
        curr_pdesc curr_annotated_signature linereader typestate checks true_branch instr_ref
        ~is_strict_mode ~original_node ~node
        (Exp.BinOp (Binop.Eq, e1, e2))
  | _ ->
      (* TODO(T54687014): silenced warning may be an unsoundeness issue; investigate *)
      typestate


(* If the function has @PropagatesNullable params the nullability of result is determined by
  nullability of actual values of these params.
  *)
let clarify_ret_by_propagates_nullable ret (resolved_params : EradicateChecks.resolved_param list) =
  (* Nullability of actual values of params that are marked as propagating nullables *)
  let nullability_of_propagates_nullable_params =
    List.filter_map resolved_params
      ~f:(fun EradicateChecks.{is_formal_propagates_nullable; actual= _, inferred_nullability} ->
        Option.some_if is_formal_propagates_nullable inferred_nullability )
  in
  match nullability_of_propagates_nullable_params with
  | [] ->
      ret
  | head :: tail ->
      (* We got non-empty list of params that propagate null.
         It means that nullability of the return value will be determined by actual (inferred) nullability of them.
         Joining their nullability will give us the least upper bound of nullability of the result *)
      let upper_bound_nullability =
        List.fold tail ~init:head ~f:(fun acc nullability ->
            InferredNullability.join acc nullability )
      in
      let _, ret_typ = ret in
      (upper_bound_nullability, ret_typ)


let calc_typestate_after_call find_canonical_duplicate calls_this checks tenv idenv instr_ref
    signature_params cflags call_params ~is_anonymous_inner_class_constructor
    ~callee_annotated_signature ~callee_attributes ~callee_pname ~callee_pname_java ~curr_pname
    ~curr_pdesc ~curr_annotated_signature ~is_strict_mode ~typestate ~typestate1 loc node =
  let resolve_param i (formal_param, actual_param) =
    let (orig_e2, e2), t2 = actual_param in
    let _, inferred_nullability_actual =
      typecheck_expr ~is_strict_mode find_canonical_duplicate calls_this checks tenv node instr_ref
        curr_pdesc typestate e2
        (* TODO(T54687014) optimistic default might be an unsoundness issue - investigate *)
        (t2, InferredNullability.create TypeOrigin.OptimisticFallback)
        loc
    in
    let actual = (orig_e2, inferred_nullability_actual) in
    let num = i + 1 in
    let is_formal_propagates_nullable =
      Annotations.ia_is_propagates_nullable
        formal_param.AnnotatedSignature.param_annotation_deprecated
    in
    EradicateChecks.{num; formal= formal_param; actual; is_formal_propagates_nullable}
  in
  (* Infer nullability of function call result based on its signature *)
  let preliminary_resolved_ret =
    let ret = callee_annotated_signature.AnnotatedSignature.ret in
    let is_library = Summary.OnDisk.proc_is_library callee_attributes in
    let origin =
      TypeOrigin.MethodCall
        { TypeOrigin.pname= callee_pname
        ; call_loc= loc
        ; annotated_signature= callee_annotated_signature
        ; is_library }
    in
    (InferredNullability.create origin, ret.ret_annotated_type.typ)
  in
  let sig_len = List.length signature_params in
  let call_len = List.length call_params in
  let min_len = min sig_len call_len in
  let slice l =
    let len = List.length l in
    if len > min_len then List.slice l (len - min_len) 0 else l
  in
  let sig_slice = slice signature_params in
  let call_slice = slice call_params in
  let sig_call_params =
    List.filter
      ~f:(fun ((sparam : AnnotatedSignature.param_signature), _) ->
        let param_is_this =
          Mangled.is_this sparam.mangled
          || String.is_prefix ~prefix:"this$" (Mangled.to_string sparam.mangled)
        in
        not param_is_this )
      (List.zip_exn sig_slice call_slice)
  in
  let resolved_params = List.mapi ~f:resolve_param sig_call_params in
  (* Clarify function call result nullability based on params annotated with @PropagatesNullable
     and inferred nullability of those params *)
  let ret_respecting_propagates_nullable =
    clarify_ret_by_propagates_nullable preliminary_resolved_ret resolved_params
  in
  let typestate_after_call =
    if not is_anonymous_inner_class_constructor then (
      if cflags.CallFlags.cf_virtual && checks.eradicate then
        EradicateChecks.check_call_receiver ~is_strict_mode tenv find_canonical_duplicate curr_pdesc
          node typestate1 call_params callee_pname instr_ref loc
          (typecheck_expr ~is_strict_mode find_canonical_duplicate calls_this checks) ;
      if checks.eradicate then
        EradicateChecks.check_call_parameters ~is_strict_mode tenv find_canonical_duplicate
          curr_pdesc node callee_attributes resolved_params loc instr_ref ;
      if Models.is_check_not_null callee_pname then
        match Models.get_check_not_null_parameter callee_pname with
        | Some index ->
            do_preconditions_check_not_null instr_ref tenv find_canonical_duplicate node loc
              curr_pdesc curr_pname curr_annotated_signature checks call_params idenv index
              ~is_vararg:false typestate1
        | None when Typ.Procname.Java.is_vararg callee_pname_java ->
            let last_parameter = List.length call_params in
            do_preconditions_check_not_null instr_ref tenv find_canonical_duplicate node loc
              curr_pdesc curr_pname curr_annotated_signature checks call_params idenv last_parameter
              ~is_vararg:true typestate1
        | None ->
            (* assume the first parameter is checked for null *)
            do_preconditions_check_not_null instr_ref tenv find_canonical_duplicate node loc
              curr_pdesc curr_pname curr_annotated_signature checks call_params idenv 1
              ~is_vararg:false typestate1
      else if Models.is_check_state callee_pname || Models.is_check_argument callee_pname then
        do_preconditions_check_state instr_ref idenv tenv curr_pname curr_annotated_signature
          call_params loc node typestate1
      else if Models.is_mapPut callee_pname then
        do_map_put call_params callee_pname tenv loc node curr_pname curr_pdesc calls_this checks
          instr_ref ~is_strict_mode find_canonical_duplicate typestate1
      else typestate1 )
    else typestate1
  in
  (typestate_after_call, ret_respecting_propagates_nullable)


(* SIL instruction in form of [ret = fun(args);] where fun is a non-builtin Java function *)
let typecheck_sil_call_function find_canonical_duplicate checks tenv instr_ref typestate idenv
    ~callee_pname ~curr_pname curr_pdesc curr_annotated_signature calls_this ~is_strict_mode
    ret_id_typ etl_ loc callee_pname_java cflags node =
  let callee_attributes =
    match PatternMatch.lookup_attributes tenv callee_pname with
    | Some proc_attributes ->
        proc_attributes
    | None ->
        let formals =
          List.mapi
            ~f:(fun i (_, typ) ->
              let arg =
                if Int.equal i 0 && not (Typ.Procname.Java.is_static callee_pname_java) then
                  Mangled.this
                else Printf.sprintf "arg%d" i |> Mangled.from_string
              in
              (arg, typ) )
            etl_
        in
        let ret_type = Typ.Procname.Java.get_return_typ callee_pname_java in
        let proc_attributes =
          { (ProcAttributes.default (SourceFile.invalid __FILE__) callee_pname) with
            ProcAttributes.formals
          ; ret_type }
        in
        proc_attributes
  in
  let etl = drop_unchecked_params calls_this curr_pname callee_attributes etl_ in
  let call_params, typestate1 =
    let handle_et (e1, t1) (etl1, typestate1) =
      typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
        checks tenv node instr_ref typestate e1 loc ;
      let e2, typestate2 =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node:node ~is_assignment:false e1 typestate1 loc
      in
      (((e1, e2), t1) :: etl1, typestate2)
    in
    List.fold_right ~f:handle_et etl ~init:([], typestate)
  in
  let callee_annotated_signature = Models.get_modelled_annotated_signature tenv callee_attributes in
  let signature_params =
    drop_unchecked_signature_params callee_attributes callee_annotated_signature
  in
  let is_anonymous_inner_class_constructor =
    Typ.Procname.Java.is_anonymous_inner_class_constructor callee_pname_java
  in
  let do_return (ret_ta, ret_typ) typestate' =
    let mk_return_range () = (ret_typ, ret_ta) in
    let id = fst ret_id_typ in
    TypeState.add_id id (mk_return_range ()) typestate'
  in
  let typestate_after_call, finally_resolved_ret =
    calc_typestate_after_call find_canonical_duplicate calls_this checks tenv idenv instr_ref
      signature_params cflags call_params ~is_anonymous_inner_class_constructor
      ~callee_annotated_signature ~callee_attributes ~callee_pname ~callee_pname_java ~curr_pname
      ~curr_pdesc ~curr_annotated_signature ~is_strict_mode ~typestate ~typestate1 loc node
  in
  do_return finally_resolved_ret typestate_after_call


(** Typecheck an instruction. *)
let typecheck_instr tenv calls_this checks (node : Procdesc.Node.t) idenv curr_pname curr_pdesc
    find_canonical_duplicate (curr_annotated_signature : AnnotatedSignature.t) instr_ref linereader
    typestate instr =
  let is_return pvar =
    let ret_pvar = Procdesc.get_ret_var curr_pdesc in
    Pvar.equal pvar ret_pvar
  in
  let is_strict_mode = curr_annotated_signature.is_strict_mode in
  match instr with
  | Sil.Metadata (ExitScope (vars, _)) ->
      List.fold_right vars ~init:typestate ~f:(fun var astate ->
          match var with
          | Var.LogicalVar id ->
              TypeState.remove_id id astate
          | Var.ProgramVar _ ->
              astate )
  | Sil.Metadata (Abstract _ | Nullify _ | Skip | VariableLifetimeBegins _) ->
      typestate
  | Sil.Load {id; e; typ; loc} ->
      typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
        checks tenv node instr_ref typestate e loc ;
      let e', typestate' =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node:node ~is_assignment:false e typestate loc
      in
      TypeState.add_id id
        (typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks
           tenv node instr_ref typestate' e' typ TypeOrigin.Undef loc)
        typestate'
  | Sil.Store {e1= Exp.Lvar pvar; e2= Exp.Exn _} when is_return pvar ->
      (* skip assignment to return variable where it is an artifact of a throw instruction *)
      typestate
  | Sil.Store {e1; typ; e2; loc} ->
      typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
        checks tenv node instr_ref typestate e1 loc ;
      let e1', typestate1 =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node:node ~is_assignment:true e1 typestate loc
      in
      let check_field_assign () =
        match e1 with
        | Exp.Lfield (_, fn, f_typ) ->
            let field_type_opt = AnnotatedField.get tenv fn f_typ in
            if checks.eradicate then
              EradicateChecks.check_field_assignment ~is_strict_mode tenv find_canonical_duplicate
                curr_pdesc node instr_ref typestate1 e1' e2 typ loc fn field_type_opt
                (typecheck_expr ~is_strict_mode find_canonical_duplicate calls_this checks tenv)
        | _ ->
            ()
      in
      let typestate2 =
        match e1' with
        | Exp.Lvar pvar ->
            TypeState.add pvar
              (typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
                 checks tenv node instr_ref typestate1 e2 typ TypeOrigin.Undef loc)
              typestate1
        | Exp.Lfield _ ->
            typestate1
        | _ ->
            typestate1
      in
      check_field_assign () ; typestate2
  (* Java `new` operators *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), [(_, typ)], _, _)
    when Typ.Procname.equal pn BuiltinDecl.__new || Typ.Procname.equal pn BuiltinDecl.__new_array ->
      (* new never returns null *)
      TypeState.add_id id (typ, InferredNullability.create TypeOrigin.New) typestate
  (* Type cast *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), (e, typ) :: _, loc, _)
    when Typ.Procname.equal pn BuiltinDecl.__cast ->
      typecheck_expr_for_errors ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this
        checks tenv node instr_ref typestate e loc ;
      let e', typestate' =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node:node ~is_assignment:false e typestate loc
      in
      (* cast copies the type of the first argument *)
      TypeState.add_id id
        (typecheck_expr_simple ~is_strict_mode find_canonical_duplicate curr_pdesc calls_this checks
           tenv node instr_ref typestate' e' typ TypeOrigin.OptimisticFallback loc)
        typestate'
  (* myarray.length *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), [(array_exp, t)], loc, _)
    when Typ.Procname.equal pn BuiltinDecl.__get_array_length ->
      let _, ta =
        typecheck_expr ~is_strict_mode find_canonical_duplicate calls_this checks tenv node
          instr_ref curr_pdesc typestate array_exp
          (* TODO(T54687014) optimistic default might be an unsoundness issue - investigate *)
          (t, InferredNullability.create TypeOrigin.OptimisticFallback)
          loc
      in
      if checks.eradicate then
        EradicateChecks.check_object_dereference ~is_strict_mode tenv find_canonical_duplicate
          curr_pdesc node instr_ref array_exp DereferenceRule.ArrayLengthAccess ta loc ;
      TypeState.add_id id
        (Typ.mk (Tint Typ.IInt), InferredNullability.create TypeOrigin.ArrayLengthResult)
        typestate
  (* All other builtins that are not considered above *)
  | Sil.Call (_, Exp.Const (Const.Cfun pn), _, _, _) when BuiltinDecl.is_declared pn ->
      typestate (* skip other builtins *)
  (* Normal call of a function *)
  | Sil.Call
      ( ret_id_typ
      , Exp.Const (Const.Cfun (Typ.Procname.Java callee_pname_java as callee_pname))
      , etl_
      , loc
      , cflags ) ->
      typecheck_sil_call_function find_canonical_duplicate checks tenv instr_ref typestate idenv
        ~callee_pname ~curr_pname curr_pdesc curr_annotated_signature calls_this ~is_strict_mode
        ret_id_typ etl_ loc callee_pname_java cflags node
  (* Calls instruction that is not a function call *)
  | Sil.Call _ ->
      (* This is something weird, we don't normally expect this type of instruction
         This may be an unsoundness issue.
         TODO(T54687014) investigate if this happens in production and add assertion, if not, and handle if gracefully, if yes.
      *)
      typestate
  | Sil.Prune (cond, loc, true_branch, _) ->
      let node', normalized_cond = normalize_cond_for_sil_prune idenv ~node cond in
      check_condition_for_sil_prune tenv idenv calls_this find_canonical_duplicate loc curr_pname
        curr_pdesc curr_annotated_signature linereader typestate checks true_branch instr_ref
        ~is_strict_mode ~node:node' ~original_node:node normalized_cond


(** Typecheck the instructions in a cfg node. *)
let typecheck_node tenv calls_this checks idenv curr_pname curr_pdesc find_canonical_duplicate
    annotated_signature typestate node linereader =
  let instrs = Procdesc.Node.get_instrs node in
  let instr_ref_gen = TypeErr.InstrRef.create_generator node in
  let typestates_exn = ref [] in
  let noreturn = ref false in
  let handle_exceptions typestate instr =
    match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pname), _, _, _)
      when Models.is_noreturn callee_pname ->
        noreturn := true
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pname), _, _, _) ->
        let callee_attributes_opt = PatternMatch.lookup_attributes tenv callee_pname in
        (* check if the call might throw an exception *)
        let has_exceptions =
          match callee_attributes_opt with
          | Some callee_attributes ->
              callee_attributes.ProcAttributes.exceptions <> []
          | None ->
              false
        in
        if has_exceptions then typestates_exn := typestate :: !typestates_exn
    | Sil.Store {e1= Exp.Lvar pv}
      when Pvar.is_return pv
           && Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind node) Procdesc.Node.throw_kind ->
        (* throw instruction *)
        typestates_exn := typestate :: !typestates_exn
    | _ ->
        ()
  in
  let canonical_node = find_canonical_duplicate node in
  let do_instruction typestate instr =
    let instr_ref =
      (* keep unique instruction reference per-node *)
      TypeErr.InstrRef.gen instr_ref_gen
    in
    let instr' =
      typecheck_instr tenv calls_this checks node idenv curr_pname curr_pdesc
        find_canonical_duplicate annotated_signature instr_ref linereader typestate instr
    in
    handle_exceptions typestate instr ; instr'
  in
  (* Reset 'always' field for forall errors to false. *)
  (* This is used to track if it is set to true for all visit to the node. *)
  TypeErr.node_reset_forall canonical_node ;
  let typestate_succ = Instrs.fold ~f:do_instruction ~init:typestate instrs in
  let dont_propagate =
    Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind node) Procdesc.Node.exn_sink_kind
    (* don't propagate exceptions *)
    || !noreturn
  in
  if dont_propagate then ([], []) (* don't propagate to exit node *)
  else ([typestate_succ], !typestates_exn)
