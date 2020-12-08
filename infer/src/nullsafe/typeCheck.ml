(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module DExp = DecompiledExp

type typecheck_result =
  {normal_flow_typestate: TypeState.t option; exception_flow_typestates: TypeState.t list}

(** Module to treat selected complex expressions as constants. *)
module ComplexExpressions = struct
  let procname_instanceof pname = Procname.equal BuiltinDecl.__instanceof pname

  let is_annotated_with predicate tenv procname =
    match PatternMatch.lookup_attributes tenv procname with
    | Some proc_attributes ->
        let annotated_signature =
          (* TODO(T62825735): fully support trusted callees *)
          Models.get_modelled_annotated_signature ~is_callee_in_trust_list:false tenv
            proc_attributes
        in
        let AnnotatedSignature.{ret_annotation_deprecated} = annotated_signature.ret in
        predicate ret_annotation_deprecated
    | None ->
        false


  (* given a predicate that requries Java procname, return a filter that requires generic prodname
   *)
  let java_predicate_to_pname_predicate java_predicate pname =
    match pname with Procname.Java java_pname -> java_predicate java_pname | _ -> false


  let procname_is_false_on_null tenv procname =
    is_annotated_with Annotations.ia_is_false_on_null tenv procname
    || java_predicate_to_pname_predicate Models.is_false_on_null procname


  let procname_is_true_on_null tenv procname =
    is_annotated_with Annotations.ia_is_true_on_null tenv procname
    || java_predicate_to_pname_predicate Models.is_true_on_null procname


  let procname_containsKey = java_predicate_to_pname_predicate Models.is_containsKey

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
          dexp_to_string de ^ "." ^ Fieldname.to_string f
      | DExp.Dbinop (op, de1, de2) ->
          "(" ^ dexp_to_string de1 ^ Binop.str Pp.text op ^ dexp_to_string de2 ^ ")"
      | DExp.Dconst (Const.Cfun pn) ->
          Procname.to_unique_id pn
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

type check_return_type = Procdesc.t -> Typ.t -> Typ.t option -> Location.t -> unit

type find_canonical_duplicate = Procdesc.Node.t -> Procdesc.Node.t

type checks = {eradicate: bool; check_ret_type: check_return_type list}

(** Typecheck an expression. *)
let rec typecheck_expr ({IntraproceduralAnalysis.tenv; proc_desc= curr_proc_desc} as analysis_data)
    ~nullsafe_mode find_canonical_duplicate visited checks node instr_ref typestate e tr_default loc
    : TypeState.range =
  L.d_with_indent ~name:"typecheck_expr" ~pp_result:TypeState.pp_range (fun () ->
      L.d_printfln "Expr: %a" Exp.pp e ;
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
          TypeState.lookup_pvar pvar typestate
          |> IOption.if_none_eval ~f:(fun () ->
                 L.d_strln "WARNING: could not lookup Pvar in typestate: fallback to default" ;
                 tr_default )
      | Exp.Var id ->
          TypeState.lookup_id id typestate
          |> IOption.if_none_eval ~f:(fun () ->
                 L.d_strln "WARNING: could not lookup Id in typestate: fallback to default" ;
                 tr_default )
      | Exp.Exn e1 ->
          typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate visited checks node
            instr_ref typestate e1 tr_default loc
      | Exp.Lfield (exp, field_name, typ) ->
          let _, _ = tr_default in
          let _, inferred_nullability =
            typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate visited checks node
              instr_ref typestate exp
              (* TODO(T54687014) optimistic default might be an unsoundness issue - investigate *)
              (typ, InferredNullability.create TypeOrigin.OptimisticFallback)
              loc
          in
          let object_origin = InferredNullability.get_simple_origin inferred_nullability in
          let curr_procname =
            Procdesc.get_proc_name curr_proc_desc
            |> Procname.as_java_exn
                 ~explanation:"typecheck_expr: attempt to typecheck non-Java method"
          in
          let class_under_analysis = Procname.Java.get_class_type_name curr_procname in
          let tr_new =
            match AnnotatedField.get tenv field_name ~class_typ:typ ~class_under_analysis with
            | Some AnnotatedField.{annotated_type= field_type} ->
                ( field_type.typ
                , InferredNullability.create
                    (TypeOrigin.Field {object_origin; field_name; field_type; access_loc= loc}) )
            | None ->
                L.d_strln "WARNING: could not lookup field annotation: fallback to default" ;
                tr_default
          in
          if checks.eradicate then
            EradicateChecks.check_object_dereference analysis_data ~nullsafe_mode
              find_canonical_duplicate node instr_ref exp (AccessToField field_name)
              inferred_nullability loc ;
          tr_new
      | Exp.Lindex (array_exp, index_exp) ->
          let _, inferred_nullability =
            typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate visited checks node
              instr_ref typestate array_exp tr_default loc
          in
          let index_desc =
            match EradicateChecks.explain_expr tenv node index_exp with Some s -> s | None -> "?"
          in
          if checks.eradicate then
            EradicateChecks.check_object_dereference analysis_data ~nullsafe_mode
              find_canonical_duplicate node instr_ref array_exp
              (AccessByIndex {index_desc})
              inferred_nullability loc ;
          let typ, _ = tr_default in
          (typ, InferredNullability.create TypeOrigin.ArrayAccess)
      | _ ->
          L.d_strln "WARNING: unknown expression: fallback to default" ;
          tr_default )


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
        Some (InferredNullability.get_simple_origin inferred_nullability)
    | None ->
        None
  in
  let handle_temporary e =
    match IDEnv.expand_expr idenv e with
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


(* Try to convert a function call to a pvar that originated it; fallback to an original expression in case of failure *)
let funcall_exp_to_original_pvar_exp tenv curr_pname typestate exp ~is_assignment ~call_node ~node
    id =
  match Decompile.find_normal_variable_funcall call_node id with
  | Some (Exp.Const (Const.Cfun pn), _, _, _)
    when not (ComplexExpressions.procname_used_in_condition pn) -> (
    match ComplexExpressions.exp_to_string tenv node exp with
    | None ->
        exp
    | Some exp_str ->
        let pvar = Pvar.mk (Mangled.from_string exp_str) curr_pname in
        let already_defined_in_typestate = Option.is_some (TypeState.lookup_pvar pvar typestate) in
        if is_assignment && already_defined_in_typestate then exp
          (* Don't overwrite pvar representing result of function call. *)
        else Exp.Lvar pvar )
  | _ ->
      exp


let add_field_to_typestate_if_absent tenv access_loc typestate pvar object_origin field_name
    ~field_class_typ ~class_under_analysis =
  match TypeState.lookup_pvar pvar typestate with
  | Some _ ->
      typestate
  | None -> (
    match AnnotatedField.get tenv field_name ~class_typ:field_class_typ ~class_under_analysis with
    | Some AnnotatedField.{annotated_type= field_type} ->
        let range =
          ( field_type.typ
          , InferredNullability.create
              (TypeOrigin.Field {object_origin; field_name; field_type; access_loc}) )
        in
        TypeState.add ~descr:"add_field_to_typestate_if_absent" pvar range typestate
    | None ->
        typestate )


(* This does two things:
   1. The same as [convert_complex_exp_to_pvar]
   2. On top of this, if expr corresponds to a field access, stores this field in the typestate
      (if not stored yet).
   *)
let convert_complex_exp_to_pvar_and_register_field_in_typestate tenv idenv curr_pname
    (curr_annotated_signature : AnnotatedSignature.t) ~node ~(original_node : Procdesc.Node.t)
    ~is_assignment exp_ typestate loc =
  L.d_with_indent ~name:"convert_complex_exp_to_pvar_and_register_field_in_typestate"
    ~pp_result:(fun f (exp, typestate) ->
      F.fprintf f "Exp: %a;@\nTypestate: @\n%a" Exp.pp exp TypeState.pp typestate )
    (fun () ->
      let exp =
        handle_field_access_via_temporary idenv curr_pname typestate (IDEnv.expand_expr idenv exp_)
      in
      let default = (exp, typestate) in
      match exp with
      | Exp.Var id when Option.is_some (Decompile.find_normal_variable_funcall node id) ->
          ( funcall_exp_to_original_pvar_exp tenv curr_pname typestate exp ~is_assignment
              ~call_node:node ~node id
          , typestate )
      | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
          let frontend_variable_assignment =
            Decompile.find_program_variable_assignment original_node pvar
          in
          match frontend_variable_assignment with
          | Some (call_node, id) ->
              ( funcall_exp_to_original_pvar_exp tenv curr_pname typestate exp ~is_assignment
                  ~call_node ~node id
              , typestate )
          | _ ->
              default )
      | Exp.Lvar _ ->
          default
      | Exp.Lfield (exp_, fn, field_class_typ) ->
          let inner_origin =
            ( match exp_ with
            | Exp.Lvar pvar ->
                TypeState.lookup_pvar pvar typestate
            | Exp.Var id ->
                TypeState.lookup_id id typestate
            | _ ->
                None )
            |> Option.value_map
                 ~f:(fun (_, nullability) -> InferredNullability.get_simple_origin nullability)
                 ~default:TypeOrigin.OptimisticFallback
          in
          let exp' = IDEnv.expand_expr_temps idenv original_node exp_ in
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
          let class_under_analysis =
            Procname.Java.get_class_type_name
              (Procname.as_java_exn curr_pname
                 ~explanation:"Attempt to typecheck non-Java procname")
          in
          let res =
            match exp' with
            | Exp.Lvar pv when is_parameter_field pv || is_static_field pv ->
                let fld_name = pvar_to_str pv ^ Fieldname.to_string fn in
                let pvar = Pvar.mk (Mangled.from_string fld_name) curr_pname in
                let typestate' =
                  add_field_to_typestate_if_absent tenv loc typestate pvar inner_origin fn
                    ~field_class_typ ~class_under_analysis
                in
                (Exp.Lvar pvar, typestate')
            | Exp.Lfield (_exp', fn', _) when Fieldname.is_java_outer_instance fn' ->
                (* handle double dereference when accessing a field from an outer class *)
                let fld_name = Fieldname.to_string fn' ^ "_" ^ Fieldname.to_string fn in
                let pvar = Pvar.mk (Mangled.from_string fld_name) curr_pname in
                let typestate' =
                  add_field_to_typestate_if_absent tenv loc typestate pvar inner_origin fn
                    ~field_class_typ ~class_under_analysis
                in
                (Exp.Lvar pvar, typestate')
            | Exp.Lvar _ | Exp.Lfield _ -> (
              (* treat var.field1. ... .fieldn as a constant *)
              match ComplexExpressions.exp_to_string tenv node exp with
              | Some exp_str ->
                  let pvar = Pvar.mk (Mangled.from_string exp_str) curr_pname in
                  let typestate' =
                    add_field_to_typestate_if_absent tenv loc typestate pvar inner_origin fn
                      ~field_class_typ ~class_under_analysis
                  in
                  (Exp.Lvar pvar, typestate')
              | None ->
                  default )
            | _ ->
                default
          in
          res
      | _ ->
          default )


(* Tries to find (or create a synthetic) pvar variable name that originated the given expression.
   - This can be a "normal" pvar (e.g. a local variable or field parameter).
   - Additionally, this can be a "synthetic" pvar corresponding to lookups:
     - pvar representing "result of a function call"
     - pvar representing field access.
     Such synthetic pvars are needed to store once inferred nullability to make nullsafe remember
     it in future accesses (so that the next call of the same method or access to the same field
     does not require the programmer to write a check.
   What is the difference between ~node and ~original_node? I don't know. This is an artifact of refactoring of
   very old code. Sorry, dear future supporter, if names don't make sense.
 *)
let convert_complex_exp_to_pvar tenv idenv curr_pname ~is_assignment
    (curr_annotated_signature : AnnotatedSignature.t) ~node ~(original_node : Procdesc.Node.t) exp
    typestate loc =
  L.d_with_indent ~name:"convert_complex_exp_to_pvar" ~pp_result:Exp.pp (fun () ->
      (* For now, we implement the function via the generic version that modifies the typestate.
      *)
      let exp, _ =
        convert_complex_exp_to_pvar_and_register_field_in_typestate tenv idenv curr_pname
          curr_annotated_signature ~is_assignment ~node ~original_node exp typestate loc
      in
      L.d_printfln "Disregarding updated typestate" ;
      exp )


let constructor_check_calls_this curr_pname calls_this pn =
  match (curr_pname, pn) with
  | Procname.Java curr_pname_java, Procname.Java pn_java ->
      if
        String.equal
          (Procname.Java.get_class_name curr_pname_java)
          (Procname.Java.get_class_name pn_java)
      then calls_this := true
  | _ ->
      ()


(* Drops hidden and synthetic parameters which we do not check in a call. *)
let drop_unchecked_params calls_this curr_pname proc_attributes params =
  let pname = proc_attributes.ProcAttributes.proc_name in
  if Procname.is_constructor pname then
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
    Procname.is_constructor proc_attributes.ProcAttributes.proc_name
    && proc_attributes.ProcAttributes.is_synthetic_method
  then
    List.take annotated_signature.AnnotatedSignature.params
      (List.length annotated_signature.AnnotatedSignature.params - 1)
  else annotated_signature.AnnotatedSignature.params


(* Apply a function to a pvar and its associated content if front-end generated. *)
let pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc handle_pvar typestate
    pvar node =
  L.d_with_indent ~name:"pvar_apply" ~pp_result:TypeState.pp (fun () ->
      let typestate' = handle_pvar typestate pvar in
      let curr_node = TypeErr.InstrRef.get_node instr_ref in
      let frontent_variable_assignment =
        if Pvar.is_frontend_tmp pvar then Decompile.find_program_variable_assignment curr_node pvar
        else None
      in
      match frontent_variable_assignment with
      | None ->
          typestate'
      | Some (node', id) -> (
          (* handle the case where pvar is a frontend-generated program variable *)
          let exp = IDEnv.expand_expr idenv (Exp.Var id) in
          match
            convert_complex_exp_to_pvar ~is_assignment:false tenv idenv curr_pname
              curr_annotated_signature ~node:node' ~original_node:node exp typestate' loc
          with
          | Exp.Lvar pvar' ->
              handle_pvar typestate' pvar'
          | _ ->
              typestate' ) )


(* typecheck_expr with fewer parameters, using a common template for typestate range  *)
let typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate calls_this checks
    node instr_ref typestate1 exp1 typ1 origin1 loc1 =
  typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate calls_this checks node
    instr_ref typestate1 exp1
    (typ1, InferredNullability.create origin1)
    loc1


(* check if there are errors in exp1 *)
let typecheck_expr_for_errors analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
    checks node instr_ref typestate1 exp1 loc1 : unit =
  ignore
    (typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate calls_this checks
       node instr_ref typestate1 exp1 StdTyp.void TypeOrigin.OptimisticFallback loc1)


(** Get the values of a vararg parameter given the pvar used to assign the elements by looking for
    array assignments to the pvar. *)
let java_get_vararg_values node pvar idenv =
  let values_of_instr acc = function
    | Sil.Store {e1= Exp.Lindex (array_exp, _); e2= content_exp}
      when Exp.equal (Exp.Lvar pvar) (IDEnv.expand_expr idenv array_exp) ->
        (* Each vararg argument is an assignment to a pvar denoting an array of objects. *)
        content_exp :: acc
    | _ ->
        acc
  in
  let values_of_node acc n =
    Procdesc.Node.get_instrs n |> Instrs.fold ~f:values_of_instr ~init:acc
  in
  match Decompile.find_program_variable_assignment node pvar with
  | Some (node', _) ->
      Procdesc.fold_slope_range node' node ~f:values_of_node ~init:[]
  | None ->
      []


(* Handle Preconditions.checkNotNull. *)
let do_preconditions_check_not_null
    ({IntraproceduralAnalysis.proc_desc= curr_pdesc; tenv; _} as analysis_data) instr_ref
    find_canonical_duplicate node loc curr_annotated_signature checks call_params idenv
    parameter_num ~is_vararg typestate' =
  (* clear the nullable flag of the first parameter of the procedure *)
  let clear_nullable_flag ~nullsafe_mode typestate'' pvar =
    (* remove the nullable flag for the given pvar *)
    match TypeState.lookup_pvar pvar typestate'' with
    | Some (t, nullability) ->
        let should_report =
          Config.eradicate_condition_redundant
          (* TODO: This condition should be extracted into a dedicated rule *)
          && InferredNullability.is_nonnullish nullability
          && not (InferredNullability.origin_is_fun_defined nullability)
        in
        ( if checks.eradicate && should_report then
          let cond = Exp.BinOp (Binop.Ne, Exp.Lvar pvar, Exp.null) in
          TypeErr.register_error analysis_data find_canonical_duplicate
            (Condition_redundant
               { is_always_true= true
               ; loc
               ; condition_descr= EradicateChecks.explain_expr tenv node cond
               ; nonnull_origin= InferredNullability.get_simple_origin nullability })
            (Some instr_ref) ~nullsafe_mode ) ;
        let previous_origin = InferredNullability.get_simple_origin nullability in
        let new_origin = TypeOrigin.InferredNonnull {previous_origin} in
        TypeState.add pvar
          (t, InferredNullability.create new_origin)
          typestate'' ~descr:"check_not_null function argument"
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
      let curr_pname = Procdesc.get_proc_name curr_pdesc in
      if is_vararg then
        let do_vararg_value e ts =
          match IDEnv.expand_expr idenv e with
          | Exp.Lvar pvar1 ->
              pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc
                (clear_nullable_flag
                   ~nullsafe_mode:curr_annotated_signature.AnnotatedSignature.nullsafe_mode)
                ts pvar1 node
          | _ ->
              ts
        in
        let vararg_values = java_get_vararg_values node pvar idenv in
        List.fold_right ~f:do_vararg_value vararg_values ~init:typestate'
      else
        pvar_apply instr_ref idenv tenv curr_pname curr_annotated_signature loc
          (clear_nullable_flag
             ~nullsafe_mode:curr_annotated_signature.AnnotatedSignature.nullsafe_mode)
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
        let previous_origin = InferredNullability.get_simple_origin nullability in
        let new_origin = TypeOrigin.InferredNonnull {previous_origin} in
        TypeState.add pvar
          (t, InferredNullability.create new_origin)
          typestate1 ~descr:"check_state argument"
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
        let cond_e = IDEnv.expand_expr_temps idenv cond_node expression in
        match
          convert_complex_exp_to_pvar ~is_assignment:false tenv idenv curr_pname
            curr_annotated_signature ~node:cond_node ~original_node:node cond_e typestate' loc
        with
        | Exp.Lvar pvar' ->
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
      match Decompile.find_boolean_assignment curr_node pvar branch with
      (* In foo(cond1 && cond2), the node that sets the result to false
         has all the negated conditions as parents. *)
      | Some boolean_assignment_node ->
          List.iter ~f:handle_negated_condition (Procdesc.Node.get_preds boolean_assignment_node) ;
          !res_typestate
      | None ->
          !res_typestate )
  | _ ->
      typestate'


let object_typ = StdTyp.Java.pointer_to_java_lang_object

(* Handle m.put(k,v) as assignment pvar = v for the pvar associated to m.get(k) *)
let do_map_put ({IntraproceduralAnalysis.proc_desc= curr_pdesc; tenv; _} as analysis_data)
    call_params callee_pname loc node calls_this checks instr_ref ~nullsafe_mode
    find_canonical_duplicate typestate' =
  (* Get the proc name for map.get() from map.put() *)
  let pname_get_from_pname_put pname_put =
    let parameters = [object_typ] in
    pname_put
    |> Procname.Java.replace_method_name "get"
    |> Procname.Java.replace_return_type object_typ
    |> Procname.Java.replace_parameters parameters
  in
  match call_params with
  | ((_, Exp.Lvar pv_map), _) :: ((_, exp_key), _) :: ((_, exp_value), typ_value) :: _ -> (
      (* Convert the dexp for k to the dexp for m.get(k) *)
      let convert_dexp_key_to_dexp_get dopt =
        match dopt with
        | Some dexp_key ->
            let pname_get = Procname.Java (pname_get_from_pname_put callee_pname) in
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
          let curr_pname = Procdesc.get_proc_name curr_pdesc in
          let pvar_map_get = Pvar.mk (Mangled.from_string map_get_str) curr_pname in
          TypeState.add pvar_map_get
            (typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
               checks node instr_ref typestate' exp_value typ_value TypeOrigin.OptimisticFallback
               loc)
            typestate' ~descr:"do_map_put"
      | None ->
          typestate' )
  | _ ->
      typestate'


(* Handle assignment fron a temp pvar in a condition.
   This recognizes the handling of temp variables in ((x = ...) != null)

   The main idea is to take a quick look back in the CFG for any assignments
   from [pvar]. *)
let handle_assignment_in_condition_for_sil_prune idenv node pvar =
  L.d_with_indent ~pp_result:(Pp.option Exp.pp) ~name:"handle_assignment_in_condition_for_sil_prune"
    (fun () ->
      L.d_printfln "Pvar being pruned: %a" (Pvar.pp Pp.text) pvar ;
      (* We need to find the first *unique* immediate predecessor with non-empty
         list of instructions. Since some nodes like Join_node can have empty list of instrs,
          we need to traverse CFG a bit. *)
      let rec find_pred_node_with_instrs node =
        match Procdesc.Node.get_preds node with
        | [pred] ->
            if Instrs.is_empty (Procdesc.Node.get_instrs pred) then find_pred_node_with_instrs pred
            else Some pred
        | _ ->
            None
      in
      (* Inspect instructions within the node to find assignments from [pvar] *)
      let find_aliased_var node =
        let inspect_instr instr =
          match instr with
          | Sil.Store {e1= e; e2= e'} -> (
              let expanded_rhs = IDEnv.expand_expr idenv e' in
              L.d_printfln "Found store instr: %a; RHS expands to: %a"
                (Sil.pp_instr ~print_types:false Pp.text)
                instr Exp.pp expanded_rhs ;
              match expanded_rhs with Exp.Lvar v when Pvar.equal v pvar -> Some e | _ -> None )
          | _ ->
              None
        in
        (* Here we check last instructions first. IDK if it makes a difference, but
           it is at least compatible with the previous behaviour *)
        Instrs.find_map (Procdesc.Node.get_instrs node |> Instrs.reverse_order) ~f:inspect_instr
      in
      match find_pred_node_with_instrs node with
      | Some prev_node ->
          L.d_printfln "Found non-empty unique predecessor node: #%a" Procdesc.Node.pp prev_node ;
          find_aliased_var prev_node
      | _ ->
          None )


let pp_normalized_cond fmt (_, exp) = Exp.pp fmt exp

let rec normalize_cond_for_sil_prune_rec idenv ~node ~original_node cond =
  L.d_with_indent ~name:"normalize_cond_for_sil_prune_rec" ~pp_result:pp_normalized_cond (fun () ->
      L.d_printfln "cond=%a" Exp.pp cond ;
      match cond with
      | Exp.UnOp (Unop.LNot, c, top) ->
          L.d_printfln "UnOp" ;
          let node', c' = normalize_cond_for_sil_prune_rec idenv ~node ~original_node c in
          (node', Exp.UnOp (Unop.LNot, c', top))
      | Exp.BinOp (bop, c1, c2) ->
          L.d_printfln "BinOp" ;
          let node', c1' = normalize_cond_for_sil_prune_rec idenv ~node ~original_node c1 in
          let node'', c2' = normalize_cond_for_sil_prune_rec idenv ~node:node' ~original_node c2 in
          L.d_printfln "c1=%a@\nc2=%a" Exp.pp c1 Exp.pp c2 ;
          (node'', Exp.BinOp (bop, c1', c2'))
      | Exp.Var _ ->
          L.d_printfln "Var" ;
          let c' = IDEnv.expand_expr idenv cond in
          L.d_printfln "c'=%a" Exp.pp c' ;
          if not (Exp.equal c' cond) then
            normalize_cond_for_sil_prune_rec idenv ~node ~original_node c'
          else (node, c')
      | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
          L.d_printfln "Lvar" ;
          match handle_assignment_in_condition_for_sil_prune idenv original_node pvar with
          | None -> (
            match Decompile.find_program_variable_assignment node pvar with
            | Some (node', id) ->
                (node', Exp.Var id)
            | None ->
                (node, cond) )
          | Some e2 ->
              (node, e2) )
      | c ->
          L.d_printfln "other" ;
          (node, c) )


(* Normalize the condition by resolving temp variables. *)
let normalize_cond_for_sil_prune idenv ~node cond =
  normalize_cond_for_sil_prune_rec idenv ~node ~original_node:node cond


let rec check_condition_for_sil_prune
    ({IntraproceduralAnalysis.proc_desc= curr_pdesc; tenv; _} as analysis_data) idenv calls_this
    find_canonical_duplicate loc curr_annotated_signature linereader typestate checks true_branch
    instr_ref ~nullsafe_mode ~original_node ~node c : TypeState.t =
  let curr_pname = Procdesc.get_proc_name curr_pdesc in
  (* check if the expression is coming from a call, and return the arguments *)
  let extract_arguments_from_call filter_callee expr =
    match expr with
    | Exp.Var id -> (
      match Decompile.find_normal_variable_funcall node id with
      | Some (Exp.Const (Const.Cfun pn), arguments, _, _) when filter_callee pn ->
          Some arguments
      | _ ->
          None )
    | _ ->
        None
  in
  (* check if the expression is coming from (`a` instanceof `b`), and returns `a`, if it is the case *)
  let extract_first_argument_from_instanceof expr =
    match extract_arguments_from_call ComplexExpressions.procname_instanceof expr with
    | Some [argument; _] ->
        Some argument
    | Some _ ->
        Logging.die Logging.InternalError "expected exactly two arguments in instanceOf expression"
    | None ->
        None
  in
  (* check if the expression is coming from a procedure returning false on null *)
  let extract_arguments_from_call_to_false_on_null_func e =
    extract_arguments_from_call (ComplexExpressions.procname_is_false_on_null tenv) e
  in
  (* check if the expression is coming from a procedure returning true on null *)
  let extract_arguments_from_call_to_true_on_null_func e =
    extract_arguments_from_call (ComplexExpressions.procname_is_true_on_null tenv) e
  in
  (* check if the expression is coming from Map.containsKey *)
  let is_from_containsKey expr =
    extract_arguments_from_call ComplexExpressions.procname_containsKey expr |> Option.is_some
  in
  (* Call to x.containsKey(e) returned `true`.
     It means that subsequent calls to `x.get(e)` should be inferred as non-nullables.
     We achieve this behavior by adding the result of a call to `x.get(e)` (in form of corresponding pvar)
     to a typestate, with correspnding (non-null) type origin.
     Returns the updated typestate.
  *)
  let handle_containsKey_returned_true call_to_containsKey_exr typestate =
    let replace_contains_key_with_get_in_a_function_call_expression = function
      (* This will replace x.containsKey(e) to x.get(e) *)
      | Some
          (DExp.Dretcall
            (DExp.Dconst (Const.Cfun (Procname.Java pname_java)), args, loc, call_flags)) ->
          let pname_java' =
            pname_java
            |> Procname.Java.replace_method_name "get"
            |> Procname.Java.replace_return_type object_typ
          in
          let fun_dexp = DExp.Dconst (Const.Cfun (Procname.Java pname_java')) in
          Some (DExp.Dretcall (fun_dexp, args, loc, call_flags))
      | _ ->
          None
    in
    let string_representing_call_to_get =
      ComplexExpressions.exp_to_string_map_dexp tenv
        replace_contains_key_with_get_in_a_function_call_expression node call_to_containsKey_exr
    in
    match string_representing_call_to_get with
    | Some expr_str ->
        (* Add pvar representing call to `get` to a typestate, indicating that it is a non-nullable *)
        let pvar = Pvar.mk (Mangled.from_string expr_str) curr_pname in
        let range =
          (StdTyp.void, InferredNullability.create TypeOrigin.CallToGetKnownToContainsKey)
        in
        let typestate_with_new_pvar = TypeState.add pvar range typestate in
        typestate_with_new_pvar
          ~descr:"modelling result of Map.get() since containsKey() returned true"
    | None ->
        typestate
  in
  let set_nonnull e' typestate2 ~descr =
    let handle_pvar typestate' pvar =
      match TypeState.lookup_pvar pvar typestate' with
      | Some (t, current_nullability) ->
          let new_origin =
            TypeOrigin.InferredNonnull
              {previous_origin= InferredNullability.get_simple_origin current_nullability}
          in
          let new_nullability = InferredNullability.create new_origin in
          TypeState.add pvar (t, new_nullability) typestate' ~descr
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
  (* Trace [expr] back to the pvar that originated it, and set its nullability as inferred non-null.
     Optionally, if this was already non-nullable, emit a corresponding condition redudant issue.
     Returns the updated typestate.
  *)
  let set_original_pvar_to_nonnull_in_typestate ~with_cond_redundant_check expr typestate ~descr =
    (* Trace back to original to pvar *)
    let pvar_expr =
      convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature
        ~is_assignment:false ~node ~original_node expr typestate loc
    in
    ( if with_cond_redundant_check then
      (* We are about to set [pvar_expr] to nonnull. But what if it already is non-null?
         This means the corresponding condition (initiated this PRUNE branch) was redudant.
      *)
      let typ, inferred_nullability =
        typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
          checks original_node instr_ref typestate pvar_expr StdTyp.void
          TypeOrigin.OptimisticFallback loc
      in
      if checks.eradicate then
        EradicateChecks.check_condition_for_redundancy analysis_data ~is_always_true:true_branch
          find_canonical_duplicate original_node pvar_expr typ inferred_nullability ~nullsafe_mode
          idenv linereader loc instr_ref ) ;
    set_nonnull pvar_expr typestate ~descr
  in
  (* Assuming [expr] is a boolean, this is the branch where, according to PRUNE semantics,
     We've just ensured that [expr] == false.
     Update the typestate accordingly.
  *)
  let handle_boolean_equal_false expr typestate =
    match extract_arguments_from_call_to_true_on_null_func expr with
    | Some arguments ->
        (* [expr] is false hence, according to true-on-null contract, neither of [arguments] can be null.
           (otherwise the result would have been true)
           Hence we can infer their nullability as non-null.
        *)
        List.fold ~init:typestate
          ~f:(fun accumulated_typestate argument ->
            set_original_pvar_to_nonnull_in_typestate ~with_cond_redundant_check:false argument
              accumulated_typestate ~descr:"@TrueOnNull-proc argument in false branch" )
          arguments
    | None ->
        typestate
  in
  (* Assuming [expr] is a boolean, this is the branch where, according to PRUNE semantics,
     We've just ensured that [expr] == true.
     Update the typestate accordingly.
  *)
  let handle_boolean_equal_true expr typestate =
    match extract_arguments_from_call_to_false_on_null_func expr with
    | Some arguments ->
        (* [expr] is true hence, according to false-on-null contract, neither of [arguments] can be null.
           (otherwise the result would have been false).
           Hence we can infer their nullability as non-null.
        *)
        List.fold ~init:typestate
          ~f:(fun accumulated_typestate argument ->
            set_original_pvar_to_nonnull_in_typestate ~with_cond_redundant_check:false argument
              accumulated_typestate ~descr:"@FalseOnNull-proc argument in false branch" )
          arguments
    | None -> (
      match extract_first_argument_from_instanceof expr with
      | Some argument ->
          (* ([argument] instanceof <anything> == true) implies (argument != null) *)
          set_original_pvar_to_nonnull_in_typestate ~with_cond_redundant_check:false argument
            typestate ~descr:"instanceof argument in true branch"
      | None ->
          if is_from_containsKey expr then handle_containsKey_returned_true expr typestate
          else typestate )
  in
  (* Assuming [expr] is a non-primitive, this is the branch where, according to PRUNE semantics,
     We've just ensured that [expr] != null.
     Update the typestate accordingly.
  *)
  let handle_object_not_equal_null expr typestate =
    set_original_pvar_to_nonnull_in_typestate ~with_cond_redundant_check:true expr typestate
      ~descr:"`!= null` branch"
  in
  match[@warning "-57"] c with
  | Exp.BinOp (Binop.Eq, Exp.Const (Const.Cint i), expr)
  | Exp.BinOp (Binop.Eq, expr, Exp.Const (Const.Cint i))
    when IntLit.iszero i ->
      (* Zero literal has two different meanings important for Nullsafe:
         `null` and `false`, hence the expression means either "some_bool == false" or "some_object == null"
         We don't currently have a logic for the latter case, but we do support the former
      *)
      handle_boolean_equal_false expr typestate
  | Exp.BinOp (Binop.Ne, Exp.Const (Const.Cint i), expr)
  | Exp.BinOp (Binop.Ne, expr, Exp.Const (Const.Cint i))
    when IntLit.iszero i ->
      (* Zero literal has two different meanings important for Nullsafe:
         `null` and `false`, hence the expression means either "some_bool == true" or "some_object != null"
         Handle both cases sequentially
      *)
      typestate |> handle_boolean_equal_true expr |> handle_object_not_equal_null expr
  | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, e1, e2), _) ->
      check_condition_for_sil_prune analysis_data idenv calls_this find_canonical_duplicate loc
        curr_annotated_signature linereader typestate checks true_branch instr_ref ~nullsafe_mode
        ~original_node ~node
        (Exp.BinOp (Binop.Ne, e1, e2))
  | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Ne, e1, e2), _) ->
      check_condition_for_sil_prune analysis_data idenv calls_this find_canonical_duplicate loc
        curr_annotated_signature linereader typestate checks true_branch instr_ref ~nullsafe_mode
        ~original_node ~node
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


let calc_typestate_after_call
    ({IntraproceduralAnalysis.proc_desc= curr_pdesc; tenv; _} as analysis_data)
    find_canonical_duplicate calls_this checks idenv instr_ref signature_params cflags call_params
    ~is_anonymous_inner_class_constructor ~callee_annotated_signature ~callee_attributes
    ~callee_pname ~curr_annotated_signature ~nullsafe_mode ~typestate ~typestate1 loc node =
  let resolve_param param_index (formal_param, actual_param) =
    let (orig_e2, e2), t2 = actual_param in
    let _, inferred_nullability_actual =
      typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate calls_this checks node
        instr_ref typestate e2
        (* TODO(T54687014) optimistic default might be an unsoundness issue - investigate *)
        (t2, InferredNullability.create TypeOrigin.OptimisticFallback)
        loc
    in
    let actual = (orig_e2, inferred_nullability_actual) in
    let is_formal_propagates_nullable =
      Annotations.ia_is_propagates_nullable
        formal_param.AnnotatedSignature.param_annotation_deprecated
    in
    EradicateChecks.{param_index; formal= formal_param; actual; is_formal_propagates_nullable}
  in
  (* Infer nullability of function call result based on its signature *)
  let preliminary_resolved_ret =
    let ret = callee_annotated_signature.AnnotatedSignature.ret in
    let is_defined = not callee_attributes.ProcAttributes.is_defined in
    let origin =
      TypeOrigin.MethodCall
        { pname= callee_pname
        ; call_loc= loc
        ; annotated_signature= callee_annotated_signature
        ; is_defined }
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
        EradicateChecks.check_call_receiver analysis_data ~nullsafe_mode find_canonical_duplicate
          node typestate1 call_params callee_pname instr_ref loc
          (typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate calls_this checks) ;
      if checks.eradicate then
        EradicateChecks.check_call_parameters ~callee_pname analysis_data ~nullsafe_mode
          ~callee_annotated_signature find_canonical_duplicate node resolved_params loc instr_ref ;
      if Models.is_check_not_null callee_pname then
        match Models.get_check_not_null_parameter callee_pname with
        | Some index ->
            do_preconditions_check_not_null analysis_data instr_ref find_canonical_duplicate node
              loc curr_annotated_signature checks call_params idenv index ~is_vararg:false
              typestate1
        | None when Procname.Java.is_vararg callee_pname ->
            let last_parameter = List.length call_params in
            do_preconditions_check_not_null analysis_data instr_ref find_canonical_duplicate node
              loc curr_annotated_signature checks call_params idenv last_parameter ~is_vararg:true
              typestate1
        | None ->
            (* assume the first parameter is checked for null *)
            do_preconditions_check_not_null analysis_data instr_ref find_canonical_duplicate node
              loc curr_annotated_signature checks call_params idenv 1 ~is_vararg:false typestate1
      else if Models.is_check_state callee_pname || Models.is_check_argument callee_pname then
        let curr_pname = Procdesc.get_proc_name curr_pdesc in
        do_preconditions_check_state instr_ref idenv tenv curr_pname curr_annotated_signature
          call_params loc node typestate1
      else if Models.is_mapPut callee_pname then
        do_map_put analysis_data call_params callee_pname loc node calls_this checks instr_ref
          ~nullsafe_mode find_canonical_duplicate typestate1
      else typestate1 )
    else typestate1
  in
  (typestate_after_call, ret_respecting_propagates_nullable)


(* SIL instruction in form of [ret = fun(args);] where fun is a non-builtin Java function *)
let typecheck_sil_call_function
    ({IntraproceduralAnalysis.proc_desc= curr_pdesc; tenv; _} as analysis_data)
    find_canonical_duplicate checks instr_ref typestate idenv ~callee_pname curr_annotated_signature
    calls_this ~nullsafe_mode ret_id_typ etl_ loc callee_pname_java cflags node =
  L.d_with_indent ~name:"typecheck_sil_call_function" (fun () ->
      let callee_attributes =
        match PatternMatch.lookup_attributes tenv callee_pname with
        | Some proc_attributes ->
            proc_attributes
        | None ->
            let formals =
              List.mapi
                ~f:(fun i (_, typ) ->
                  let arg =
                    if Int.equal i 0 && not (Procname.Java.is_static callee_pname_java) then
                      Mangled.this
                    else Printf.sprintf "arg%d" i |> Mangled.from_string
                  in
                  (arg, typ) )
                etl_
            in
            let ret_type = Procname.Java.get_return_typ callee_pname_java in
            let proc_attributes =
              { (ProcAttributes.default (SourceFile.invalid __FILE__) callee_pname) with
                ProcAttributes.formals
              ; ret_type }
            in
            proc_attributes
      in
      let curr_pname = Procdesc.get_proc_name curr_pdesc in
      let etl = drop_unchecked_params calls_this curr_pname callee_attributes etl_ in
      let call_params, typestate1 =
        let handle_et (e1, t1) (etl1, typestate1) =
          typecheck_expr_for_errors analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
            checks node instr_ref typestate e1 loc ;
          let e2 =
            convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature
              ~is_assignment:false ~node ~original_node:node e1 typestate1 loc
          in
          (((e1, e2), t1) :: etl1, typestate1)
        in
        List.fold_right ~f:handle_et etl ~init:([], typestate)
      in
      let callee_annotated_signature =
        match
          (Procname.get_class_type_name callee_pname, Procname.get_class_type_name curr_pname)
        with
        | Some callee_class, Some class_under_analysis
          when Typ.Name.equal callee_class class_under_analysis ->
            (* The method call is to the method in the same class as we are currently analyzing. *)
            AnnotatedSignature.get_for_class_under_analysis tenv callee_attributes
        | _ ->
            (* The call is to external (relatively to the class under analysis) method. Lookup models, trust lists, etc. *)
            let is_callee_in_trust_list =
              let caller_nullsafe_mode = NullsafeMode.of_procname tenv curr_pname in
              let callee_class = Procname.get_class_type_name callee_pname in
              Option.value_map callee_class
                ~f:(fun class_name ->
                  Typ.Name.Java.get_java_class_name_exn class_name
                  |> NullsafeMode.is_in_trust_list caller_nullsafe_mode )
                ~default:false
            in
            Models.get_modelled_annotated_signature ~is_callee_in_trust_list tenv callee_attributes
      in
      if Config.write_html then
        L.d_printfln "Callee signature: %a"
          (AnnotatedSignature.pp callee_pname)
          callee_annotated_signature ;
      let signature_params =
        drop_unchecked_signature_params callee_attributes callee_annotated_signature
      in
      let is_anonymous_inner_class_constructor =
        Procname.Java.is_anonymous_inner_class_constructor_exn callee_pname_java
      in
      let do_return (ret_ta, ret_typ) typestate' =
        let mk_return_range () = (ret_typ, ret_ta) in
        let id = fst ret_id_typ in
        TypeState.add_id id (mk_return_range ()) typestate' ~descr:"typecheck_sil_call_function"
      in
      let typestate_after_call, finally_resolved_ret =
        calc_typestate_after_call analysis_data find_canonical_duplicate calls_this checks idenv
          instr_ref signature_params cflags call_params ~is_anonymous_inner_class_constructor
          ~callee_annotated_signature ~callee_attributes ~callee_pname:callee_pname_java
          ~curr_annotated_signature ~nullsafe_mode ~typestate ~typestate1 loc node
      in
      do_return finally_resolved_ret typestate_after_call )


(** Typecheck an instruction. *)
let typecheck_instr ({IntraproceduralAnalysis.proc_desc= curr_pdesc; tenv; _} as analysis_data)
    calls_this checks (node : Procdesc.Node.t) idenv find_canonical_duplicate
    (curr_annotated_signature : AnnotatedSignature.t) instr_ref linereader typestate instr =
  let curr_pname = Procdesc.get_proc_name curr_pdesc in
  let is_return pvar =
    let ret_pvar = Procdesc.get_ret_var curr_pdesc in
    Pvar.equal pvar ret_pvar
  in
  let nullsafe_mode = curr_annotated_signature.nullsafe_mode in
  match instr with
  | Sil.Metadata (ExitScope (vars, _)) ->
      List.fold_right vars ~init:typestate ~f:(fun var astate ->
          match var with
          | Var.LogicalVar id ->
              TypeState.remove_id id astate ~descr:"ExitScope"
          | Var.ProgramVar _ ->
              astate )
  | Sil.Metadata (Abstract _ | Nullify _ | Skip | VariableLifetimeBegins _) ->
      typestate
  | Sil.Load {id; e; typ; loc} ->
      typecheck_expr_for_errors analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
        checks node instr_ref typestate e loc ;
      let e', typestate' =
        convert_complex_exp_to_pvar_and_register_field_in_typestate tenv idenv curr_pname
          curr_annotated_signature ~node ~original_node:node ~is_assignment:false e typestate loc
      in
      TypeState.add_id id
        (typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
           checks node instr_ref typestate' e' typ TypeOrigin.OptimisticFallback loc)
        ~descr:"Sil.Load" typestate'
  | Sil.Store {e1= Exp.Lvar pvar; e2= Exp.Exn _} when is_return pvar ->
      (* skip assignment to return variable where it is an artifact of a throw instruction *)
      typestate
  | Sil.Store {e1; typ; e2; loc} ->
      typecheck_expr_for_errors analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
        checks node instr_ref typestate e1 loc ;
      let e1' =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature ~node
          ~original_node:node ~is_assignment:true e1 typestate loc
      in
      let check_field_assign () =
        match e1 with
        | Exp.Lfield (_, field_name, field_class_type) -> (
            let class_under_analysis =
              Procname.Java.get_class_type_name
                (Procname.as_java_exn curr_pname
                   ~explanation:"Attempt to typecheck non-Java method")
            in
            match
              AnnotatedField.get tenv field_name ~class_typ:field_class_type ~class_under_analysis
            with
            | Some annotated_field ->
                if checks.eradicate then
                  EradicateChecks.check_field_assignment analysis_data ~nullsafe_mode
                    find_canonical_duplicate node instr_ref typestate ~expr_rhs:e2 ~field_type:typ
                    loc field_name annotated_field
                    (typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
                       checks)
            | None ->
                L.d_strln "WARNING: could not fetch field declaration; skipping assignment check" )
        | _ ->
            ()
      in
      let typestate2 =
        match e1' with
        | Exp.Lvar pvar ->
            TypeState.add pvar
              (typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate
                 calls_this checks node instr_ref typestate e2 typ TypeOrigin.OptimisticFallback
                 loc)
              typestate ~descr:"Sil.Store: Exp.Lvar case"
        | Exp.Lfield _ ->
            typestate
        | _ ->
            typestate
      in
      check_field_assign () ;
      typestate2
  (* Java `new` operators *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), [(_, typ)], _, _)
    when Procname.equal pn BuiltinDecl.__new || Procname.equal pn BuiltinDecl.__new_array ->
      (* new never returns null *)
      TypeState.add_id id
        (typ, InferredNullability.create TypeOrigin.New)
        typestate ~descr:"new() operator"
  (* Type cast *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), (e, typ) :: _, loc, _)
    when Procname.equal pn BuiltinDecl.__cast ->
      typecheck_expr_for_errors analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
        checks node instr_ref typestate e loc ;
      let e' =
        convert_complex_exp_to_pvar tenv idenv curr_pname curr_annotated_signature
          ~is_assignment:false ~node ~original_node:node e typestate loc
      in
      (* cast copies the type of the first argument *)
      TypeState.add_id id
        (typecheck_expr_simple analysis_data ~nullsafe_mode find_canonical_duplicate calls_this
           checks node instr_ref typestate e' typ TypeOrigin.OptimisticFallback loc)
        typestate ~descr:"type cast"
  (* myarray.length *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), [(array_exp, t)], loc, _)
    when Procname.equal pn BuiltinDecl.__get_array_length ->
      let _, ta =
        typecheck_expr analysis_data ~nullsafe_mode find_canonical_duplicate calls_this checks node
          instr_ref typestate array_exp
          (* TODO(T54687014) optimistic default might be an unsoundness issue - investigate *)
          (t, InferredNullability.create TypeOrigin.OptimisticFallback)
          loc
      in
      if checks.eradicate then
        EradicateChecks.check_object_dereference analysis_data ~nullsafe_mode
          find_canonical_duplicate node instr_ref array_exp
          DereferenceRule.ReportableViolation.ArrayLengthAccess ta loc ;
      TypeState.add_id id
        (Typ.mk (Tint Typ.IInt), InferredNullability.create TypeOrigin.ArrayLengthResult)
        typestate ~descr:"array.length"
  (* All other builtins that are not considered above *)
  | Sil.Call (_, Exp.Const (Const.Cfun pn), _, _, _) when BuiltinDecl.is_declared pn ->
      typestate (* skip other builtins *)
  (* Normal call of a function *)
  | Sil.Call
      ( ret_id_typ
      , Exp.Const (Const.Cfun (Procname.Java callee_pname_java as callee_pname))
      , etl_
      , loc
      , cflags ) ->
      typecheck_sil_call_function analysis_data find_canonical_duplicate checks instr_ref typestate
        idenv ~callee_pname curr_annotated_signature calls_this ~nullsafe_mode ret_id_typ etl_ loc
        callee_pname_java cflags node
  (* Calls instruction that is not a function call *)
  | Sil.Call _ ->
      (* This is something weird, we don't normally expect this type of instruction
         This may be an unsoundness issue.
         TODO(T54687014) investigate if this happens in production and add assertion, if not, and handle if gracefully, if yes.
      *)
      typestate
  | Sil.Prune (cond, loc, true_branch, _) ->
      let node', normalized_cond = normalize_cond_for_sil_prune idenv ~node cond in
      check_condition_for_sil_prune analysis_data idenv calls_this find_canonical_duplicate loc
        curr_annotated_signature linereader typestate checks true_branch instr_ref ~nullsafe_mode
        ~node:node' ~original_node:node normalized_cond


let can_instrunction_throw tenv node instr =
  match instr with
  | Sil.Call (_, Exp.Const (Const.Cfun callee_pname), _, _, _) -> (
      let callee_attributes_opt = PatternMatch.lookup_attributes tenv callee_pname in
      (* We assume if the function is not annotated with throws(), it can not throw an exception.
         This is unsound.
         TODO(T63305137) nullsafe should assume all methods can throw.
      *)
      match callee_attributes_opt with
      | Some callee_attributes ->
          not (List.is_empty callee_attributes.ProcAttributes.exceptions)
      | None ->
          false )
  | Sil.Store {e1= Exp.Lvar pv}
    when Pvar.is_return pv
         && Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind node) Procdesc.Node.throw_kind ->
      (* Explicit throw instruction *)
      true
  | _ ->
      false


(* true if after this instruction the program interrupts *)
let is_noreturn_instruction = function
  | Sil.Call (_, Exp.Const (Const.Cfun (Procname.Java callee_pname)), _, _, _)
    when Models.is_noreturn callee_pname ->
      true
  | _ ->
      false


(** Typecheck the instructions in a cfg node. *)
let typecheck_node ({IntraproceduralAnalysis.tenv; _} as analysis_data) calls_this checks idenv
    find_canonical_duplicate annotated_signature typestate node linereader =
  if Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind node) Procdesc.Node.exn_sink_kind then
    {normal_flow_typestate= None; exception_flow_typestates= []}
  else
    let instrs = Procdesc.Node.get_instrs node in
    let instr_ref_gen = TypeErr.InstrRef.create_generator node in
    let canonical_node = find_canonical_duplicate node in
    (* typecheck the instruction and accumulate result *)
    let fold_instruction
        ( { normal_flow_typestate= normal_typestate_prev_opt
          ; exception_flow_typestates= exception_flow_typestates_prev } as prev_result ) instr =
      if Config.write_html then
        L.d_printfln "-----------------------------\nTypecking instr: %a@\n"
          (Sil.pp_instr ~print_types:true Pp.text)
          instr ;
      match normal_typestate_prev_opt with
      | None ->
          (* no input typestate - abort typechecking and propagate the current result *)
          prev_result
      | Some normal_flow_typestate_prev ->
          let instr_ref =
            (* keep unique instruction reference per-node *)
            TypeErr.InstrRef.gen instr_ref_gen
          in
          let normal_flow_typestate =
            typecheck_instr analysis_data calls_this checks node idenv find_canonical_duplicate
              annotated_signature instr_ref linereader normal_flow_typestate_prev instr
          in
          if Config.write_html then
            L.d_printfln "New state: @\n%a@\n" TypeState.pp normal_flow_typestate ;
          if is_noreturn_instruction instr then (
            L.d_strln "Found no return; aborting flow" ;
            {prev_result with normal_flow_typestate= None} )
          else
            let exception_flow_typestates =
              if can_instrunction_throw tenv node instr then (
                (* add the typestate after this instruction to the list of exception typestates *)
                L.d_strln "Throwable instruction: adding the typestate to exception list" ;
                normal_flow_typestate :: exception_flow_typestates_prev )
              else exception_flow_typestates_prev
            in
            {normal_flow_typestate= Some normal_flow_typestate; exception_flow_typestates}
    in
    (* Reset 'always' field for forall errors to false. *)
    (* This is used to track if it is set to true for all visit to the node. *)
    TypeErr.node_reset_forall canonical_node ;
    Instrs.fold instrs ~f:fold_instruction
      ~init:{normal_flow_typestate= Some typestate; exception_flow_typestates= []}
