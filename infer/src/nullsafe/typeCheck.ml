(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format
module DExp = DecompiledExp

(** Module for type checking. *)

(** Module to treat selected complex expressions as constants. *)
module ComplexExpressions = struct
  let procname_optional_isPresent = Models.is_optional_isPresent

  let procname_instanceof = Typ.Procname.equal BuiltinDecl.__instanceof

  let procname_is_false_on_null tenv pn =
    match PatternMatch.lookup_attributes tenv pn with
    | Some proc_attributes ->
        let annotated_signature = Models.get_modelled_annotated_signature proc_attributes in
        let ret_ann, _ = annotated_signature.AnnotatedSignature.ret in
        Annotations.ia_is_false_on_null ret_ann
    | None ->
        false


  let procname_is_true_on_null tenv pn =
    let annotated_true_on_null () =
      match PatternMatch.lookup_attributes tenv pn with
      | Some proc_attributes ->
          let annotated_signature = Models.get_modelled_annotated_signature proc_attributes in
          let ret_ann, _ = annotated_signature.AnnotatedSignature.ret in
          Annotations.ia_is_true_on_null ret_ann
      | None ->
          false
    in
    Models.is_true_on_null pn || annotated_true_on_null ()


  let procname_containsKey = Models.is_containsKey

  (** Recognize *all* the procedures treated specially in conditionals *)
  let procname_used_in_condition pn =
    procname_optional_isPresent pn || procname_instanceof pn || procname_containsKey pn
    || BuiltinDecl.is_declared pn


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

type check_return_type =
  Typ.Procname.t -> Procdesc.t -> Typ.t -> Typ.t option -> Location.t -> unit

type find_canonical_duplicate = Procdesc.Node.t -> Procdesc.Node.t

type checks = {eradicate: bool; check_ret_type: check_return_type list}

(** Typecheck an expression. *)
let rec typecheck_expr find_canonical_duplicate visited checks tenv node instr_ref
    (curr_pdesc : Procdesc.t) typestate e tr_default loc : TypeState.range =
  match e with
  | _ when Exp.is_null_literal e ->
      let typ, ta, locs = tr_default in
      if PatternMatch.type_is_class typ then
        (typ, TypeAnnotation.const AnnotatedSignature.Nullable true (TypeOrigin.Const loc), locs)
      else (typ, TypeAnnotation.with_origin ta (TypeOrigin.Const loc), locs)
  | Exp.Lvar pvar -> (
    match TypeState.lookup_pvar pvar typestate with
    | Some tr ->
        TypeState.range_add_locs tr [loc]
    | None ->
        tr_default )
  | Exp.Var id -> (
    match TypeState.lookup_id id typestate with
    | Some tr ->
        TypeState.range_add_locs tr [loc]
    | None ->
        tr_default )
  | Exp.Exn e1 ->
      typecheck_expr find_canonical_duplicate visited checks tenv node instr_ref curr_pdesc
        typestate e1 tr_default loc
  | Exp.Const _ ->
      let typ, _, locs = tr_default in
      (typ, TypeAnnotation.const AnnotatedSignature.Nullable false (TypeOrigin.Const loc), locs)
  | Exp.Lfield (exp, fn, typ) ->
      let _, _, locs = tr_default in
      let _, ta, locs' =
        typecheck_expr find_canonical_duplicate visited checks tenv node instr_ref curr_pdesc
          typestate exp
          (typ, TypeAnnotation.const AnnotatedSignature.Nullable false TypeOrigin.ONone, locs)
          loc
      in
      let exp_origin = TypeAnnotation.get_origin ta in
      let tr_new =
        match EradicateChecks.get_field_annotation tenv fn typ with
        | Some (t, ia) ->
            ( t
            , TypeAnnotation.from_item_annotation ia (TypeOrigin.Field (exp_origin, fn, loc))
            , locs' )
        | None ->
            tr_default
      in
      if checks.eradicate then
        EradicateChecks.check_field_access tenv find_canonical_duplicate curr_pdesc node instr_ref
          exp fn ta loc ;
      tr_new
  | Exp.Lindex (array_exp, index_exp) ->
      let _, ta, _ =
        typecheck_expr find_canonical_duplicate visited checks tenv node instr_ref curr_pdesc
          typestate array_exp tr_default loc
      in
      let index =
        match EradicateChecks.explain_expr tenv node index_exp with Some s -> s | None -> "?"
      in
      let fname = Typ.Fieldname.Java.from_string index in
      if checks.eradicate then
        EradicateChecks.check_array_access tenv find_canonical_duplicate curr_pdesc node instr_ref
          array_exp fname ta loc true ;
      tr_default
  | _ ->
      tr_default


(** Typecheck an instruction. *)
let typecheck_instr tenv calls_this checks (node : Procdesc.Node.t) idenv curr_pname curr_pdesc
    find_canonical_duplicate curr_annotated_signature instr_ref linereader typestate instr =
  (* Handle the case where a field access X.f happens via a temporary variable $Txxx.
     This has been observed in assignments this.f = exp when exp contains an ifthenelse.
     Reconstuct the original expression knowing: the origin of $Txxx is 'this'. *)
  let handle_field_access_via_temporary typestate exp =
    let name_is_temporary name =
      let prefix = "$T" in
      String.is_prefix ~prefix name
    in
    let pvar_get_origin pvar =
      match TypeState.lookup_pvar pvar typestate with
      | Some (_, ta, _) ->
          Some (TypeAnnotation.get_origin ta)
      | None ->
          None
    in
    let handle_temporary e =
      match Idenv.expand_expr idenv e with
      | Exp.Lvar pvar when name_is_temporary (Pvar.to_string pvar) -> (
        match pvar_get_origin pvar with
        | Some (TypeOrigin.Formal s) ->
            let pvar' = Pvar.mk s curr_pname in
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
  in
  (* Convert a complex expressions into a pvar.
     When [is_assignment] is true, update the relevant annotations for the pvar. *)
  let convert_complex_exp_to_pvar node' is_assignment exp_ typestate loc =
    let exp = handle_field_access_via_temporary typestate (Idenv.expand_expr idenv exp_) in
    let default = (exp, typestate) in
    (* If this is an assignment, update the typestate for a field access pvar. *)
    let update_typestate_fld pvar origin fn typ =
      match TypeState.lookup_pvar pvar typestate with
      | Some _ when not is_assignment ->
          typestate
      | _ -> (
        match EradicateChecks.get_field_annotation tenv fn typ with
        | Some (t, ia) ->
            let range =
              ( t
              , TypeAnnotation.from_item_annotation ia (TypeOrigin.Field (origin, fn, loc))
              , [loc] )
            in
            TypeState.add pvar range typestate
        | None ->
            typestate )
    in
    (* Convert a function call to a pvar. *)
    let handle_function_call call_node id =
      match Errdesc.find_normal_variable_funcall call_node id with
      | Some (Exp.Const (Const.Cfun pn), _, _, _)
        when not (ComplexExpressions.procname_used_in_condition pn) -> (
        match ComplexExpressions.exp_to_string tenv node' exp with
        | None ->
            default
        | Some exp_str ->
            let pvar = Pvar.mk (Mangled.from_string exp_str) curr_pname in
            let already_defined_in_typestate =
              match TypeState.lookup_pvar pvar typestate with
              | Some (_, ta, _) ->
                  not (TypeOrigin.equal TypeOrigin.Undef (TypeAnnotation.get_origin ta))
              | None ->
                  false
            in
            if is_assignment && already_defined_in_typestate then default
              (* Don't overwrite pvar representing result of function call. *)
            else (Exp.Lvar pvar, typestate) )
      | _ ->
          default
    in
    match exp with
    | Exp.Var id when Errdesc.find_normal_variable_funcall node' id <> None ->
        handle_function_call node' id
    | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
        let frontend_variable_assignment = Errdesc.find_program_variable_assignment node pvar in
        match frontend_variable_assignment with
        | Some (call_node, id) ->
            handle_function_call call_node id
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
               ~f:(fun (_, ta, _) -> TypeAnnotation.get_origin ta)
               ~default:TypeOrigin.ONone
        in
        let exp' = Idenv.expand_expr_temps idenv node exp_ in
        let is_parameter_field pvar =
          (* parameter.field *)
          let name = Pvar.get_name pvar in
          let filter (s, _, _) = Mangled.equal s name in
          List.exists ~f:filter curr_annotated_signature.AnnotatedSignature.params
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
              let typestate' = update_typestate_fld pvar inner_origin fn typ in
              (Exp.Lvar pvar, typestate')
          | Exp.Lfield (_exp', fn', _) when Typ.Fieldname.Java.is_outer_instance fn' ->
              (* handle double dereference when accessing a field from an outer class *)
              let fld_name = Typ.Fieldname.to_string fn' ^ "_" ^ Typ.Fieldname.to_string fn in
              let pvar = Pvar.mk (Mangled.from_string fld_name) curr_pname in
              let typestate' = update_typestate_fld pvar inner_origin fn typ in
              (Exp.Lvar pvar, typestate')
          | Exp.Lvar _ | Exp.Lfield _ -> (
            (* treat var.field1. ... .fieldn as a constant *)
            match ComplexExpressions.exp_to_string tenv node' exp with
            | Some exp_str ->
                let pvar = Pvar.mk (Mangled.from_string exp_str) curr_pname in
                let typestate' = update_typestate_fld pvar inner_origin fn typ in
                (Exp.Lvar pvar, typestate')
            | None ->
                default )
          | _ ->
              default
        in
        res
    | _ ->
        default
  in
  let constructor_check_calls_this calls_this pn =
    match (curr_pname, pn) with
    | Typ.Procname.Java curr_pname_java, Typ.Procname.Java pn_java ->
        if
          String.equal
            (Typ.Procname.Java.get_class_name curr_pname_java)
            (Typ.Procname.Java.get_class_name pn_java)
        then calls_this := true
    | _ ->
        ()
  in
  (* Drops hidden and synthetic parameters which we do not check in a call. *)
  let drop_unchecked_params calls_this proc_attributes params =
    let pname = proc_attributes.ProcAttributes.proc_name in
    if Typ.Procname.is_constructor pname then
      match PatternMatch.get_this_type proc_attributes with
      | Some _ ->
          constructor_check_calls_this calls_this pname ;
          (* Drop reference parameters to this and outer objects. *)
          let is_hidden_parameter (n, _) =
            Mangled.is_this n
            || Str.string_match (Str.regexp "$bcvar[0-9]+") (Mangled.to_string n) 0
          in
          let rec drop_n_args ntl =
            match ntl with
            | fp :: tail when is_hidden_parameter fp ->
                1 + drop_n_args tail
            | _ ->
                0
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
  in
  (* Drop parameters from the signature which we do not check in a call. *)
  let drop_unchecked_signature_params proc_attributes annotated_signature =
    if
      Typ.Procname.is_constructor proc_attributes.ProcAttributes.proc_name
      && proc_attributes.ProcAttributes.is_synthetic_method
    then
      List.take annotated_signature.AnnotatedSignature.params
        (List.length annotated_signature.AnnotatedSignature.params - 1)
    else annotated_signature.AnnotatedSignature.params
  in
  let is_return pvar =
    let ret_pvar = Procdesc.get_ret_var curr_pdesc in
    Pvar.equal pvar ret_pvar
  in
  (* Apply a function to a pvar and its associated content if front-end generated. *)
  let pvar_apply loc handle_pvar typestate pvar =
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
        match convert_complex_exp_to_pvar node' false exp typestate' loc with
        | Exp.Lvar pvar', _ ->
            handle_pvar typestate' pvar'
        | _ ->
            typestate' )
  in
  (* typecheck_expr with fewer parameters, using a common template for typestate range  *)
  let typecheck_expr_simple typestate1 exp1 typ1 origin1 loc1 =
    typecheck_expr find_canonical_duplicate calls_this checks tenv node instr_ref curr_pdesc
      typestate1 exp1
      (typ1, TypeAnnotation.const AnnotatedSignature.Nullable false origin1, [loc1])
      loc1
  in
  (* check if there are errors in exp1 *)
  let typecheck_expr_for_errors typestate1 exp1 loc1 : unit =
    ignore (typecheck_expr_simple typestate1 exp1 (Typ.mk Tvoid) TypeOrigin.Undef loc1)
  in
  match instr with
  | Sil.ExitScope (vars, _) ->
      List.fold_right vars ~init:typestate ~f:(fun var astate ->
          match var with
          | Var.LogicalVar id ->
              TypeState.remove_id id astate
          | Var.ProgramVar _ ->
              astate )
  | Sil.Abstract _ | Sil.Nullify _ ->
      typestate
  | Sil.Load (id, e, typ, loc) ->
      typecheck_expr_for_errors typestate e loc ;
      let e', typestate' = convert_complex_exp_to_pvar node false e typestate loc in
      TypeState.add_id id (typecheck_expr_simple typestate' e' typ TypeOrigin.Undef loc) typestate'
  | Sil.Store (Exp.Lvar pvar, _, Exp.Exn _, _) when is_return pvar ->
      (* skip assignment to return variable where it is an artifact of a throw instruction *)
      typestate
  | Sil.Store (e1, typ, e2, loc) ->
      typecheck_expr_for_errors typestate e1 loc ;
      let e1', typestate1 = convert_complex_exp_to_pvar node true e1 typestate loc in
      let check_field_assign () =
        match e1 with
        | Exp.Lfield (_, fn, f_typ) ->
            let t_ia_opt = EradicateChecks.get_field_annotation tenv fn f_typ in
            if checks.eradicate then
              EradicateChecks.check_field_assignment tenv find_canonical_duplicate curr_pdesc node
                instr_ref typestate1 e1' e2 typ loc fn t_ia_opt
                (typecheck_expr find_canonical_duplicate calls_this checks tenv)
        | _ ->
            ()
      in
      let typestate2 =
        match e1' with
        | Exp.Lvar pvar ->
            TypeState.add pvar
              (typecheck_expr_simple typestate1 e2 typ TypeOrigin.Undef loc)
              typestate1
        | Exp.Lfield _ ->
            typestate1
        | _ ->
            typestate1
      in
      check_field_assign () ; typestate2
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), [(_, typ)], loc, _)
    when Typ.Procname.equal pn BuiltinDecl.__new || Typ.Procname.equal pn BuiltinDecl.__new_array
    ->
      TypeState.add_id id
        (typ, TypeAnnotation.const AnnotatedSignature.Nullable false TypeOrigin.New, [loc])
        typestate
      (* new never returns null *)
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), (e, typ) :: _, loc, _)
    when Typ.Procname.equal pn BuiltinDecl.__cast ->
      typecheck_expr_for_errors typestate e loc ;
      let e', typestate' = convert_complex_exp_to_pvar node false e typestate loc in
      (* cast copies the type of the first argument *)
      TypeState.add_id id (typecheck_expr_simple typestate' e' typ TypeOrigin.ONone loc) typestate'
  | Sil.Call ((id, _), Exp.Const (Const.Cfun pn), [(array_exp, t)], loc, _)
    when Typ.Procname.equal pn BuiltinDecl.__get_array_length ->
      let _, ta, _ =
        typecheck_expr find_canonical_duplicate calls_this checks tenv node instr_ref curr_pdesc
          typestate array_exp
          (t, TypeAnnotation.const AnnotatedSignature.Nullable false TypeOrigin.ONone, [loc])
          loc
      in
      if checks.eradicate then
        EradicateChecks.check_array_access tenv find_canonical_duplicate curr_pdesc node instr_ref
          array_exp
          (Typ.Fieldname.Java.from_string "length")
          ta loc false ;
      TypeState.add_id id
        ( Typ.mk (Tint Typ.IInt)
        , TypeAnnotation.const AnnotatedSignature.Nullable false TypeOrigin.New
        , [loc] )
        typestate
  | Sil.Call (_, Exp.Const (Const.Cfun pn), _, _, _) when BuiltinDecl.is_declared pn ->
      typestate (* skip othe builtins *)
  | Sil.Call
      ( ret_id_typ
      , Exp.Const (Const.Cfun (Typ.Procname.Java callee_pname_java as callee_pname))
      , etl_
      , loc
      , cflags ) ->
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
                ProcAttributes.formals; ret_type }
            in
            proc_attributes
      in
      let etl = drop_unchecked_params calls_this callee_attributes etl_ in
      let call_params, typestate1 =
        let handle_et (e1, t1) (etl1, typestate1) =
          typecheck_expr_for_errors typestate e1 loc ;
          let e2, typestate2 = convert_complex_exp_to_pvar node false e1 typestate1 loc in
          (((e1, e2), t1) :: etl1, typestate2)
        in
        List.fold_right ~f:handle_et etl ~init:([], typestate)
      in
      let callee_annotated_signature = Models.get_modelled_annotated_signature callee_attributes in
      let signature_params =
        drop_unchecked_signature_params callee_attributes callee_annotated_signature
      in
      let is_anonymous_inner_class_constructor =
        Typ.Procname.Java.is_anonymous_inner_class_constructor callee_pname_java
      in
      let do_return (ret_ta, ret_typ) loc' typestate' =
        let mk_return_range () = (ret_typ, ret_ta, [loc']) in
        let id = fst ret_id_typ in
        TypeState.add_id id (mk_return_range ()) typestate'
      in
      (* Handle Preconditions.checkNotNull. *)
      let do_preconditions_check_not_null parameter_num ~is_vararg typestate' =
        (* clear the nullable flag of the first parameter of the procedure *)
        let clear_nullable_flag typestate'' pvar =
          (* remove the nullable flag for the given pvar *)
          match TypeState.lookup_pvar pvar typestate'' with
          | Some (t, ta, _) ->
              let should_report =
                Config.eradicate_condition_redundant
                && (not (TypeAnnotation.get_value AnnotatedSignature.Nullable ta))
                && not (TypeAnnotation.origin_is_fun_library ta)
              in
              ( if checks.eradicate && should_report then
                let cond = Exp.BinOp (Binop.Ne, Exp.Lvar pvar, Exp.null) in
                EradicateChecks.report_error tenv find_canonical_duplicate
                  (TypeErr.Condition_redundant
                     (true, EradicateChecks.explain_expr tenv node cond, false))
                  (Some instr_ref) loc curr_pdesc ) ;
              TypeState.add pvar
                (t, TypeAnnotation.const AnnotatedSignature.Nullable false TypeOrigin.ONone, [loc])
                typestate''
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
                    pvar_apply loc clear_nullable_flag ts pvar1
                | _ ->
                    ts
              in
              let vararg_values = PatternMatch.java_get_vararg_values node pvar idenv in
              List.fold_right ~f:do_vararg_value vararg_values ~init:typestate'
            else pvar_apply loc clear_nullable_flag typestate' pvar
        | None ->
            typestate'
      in
      (* Handle Preconditions.checkState for &&-separated conditions x!=null. *)
      let do_preconditions_check_state typestate' =
        let handle_pvar ann b typestate1 pvar =
          (* handle the annotation flag for pvar *)
          match TypeState.lookup_pvar pvar typestate1 with
          | Some (t, _, _) ->
              TypeState.add pvar (t, TypeAnnotation.const ann b TypeOrigin.ONone, [loc]) typestate1
          | None ->
              typestate1
        in
        let res_typestate = ref typestate' in
        let set_flag pvar ann b =
          (* set the annotation flag for pvar *)
          res_typestate := pvar_apply loc (handle_pvar ann b) !res_typestate pvar
        in
        let handle_negated_condition cond_node =
          let do_instr instr =
            let set_flag expression =
              let cond_e = Idenv.expand_expr_temps idenv cond_node expression in
              match convert_complex_exp_to_pvar cond_node false cond_e typestate' loc with
              | Exp.Lvar pvar', _ ->
                  set_flag pvar' AnnotatedSignature.Nullable false
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
        let handle_optional_isPresent node' e =
          match convert_complex_exp_to_pvar node' false e typestate' loc with
          | Exp.Lvar pvar', _ ->
              set_flag pvar' AnnotatedSignature.Present true
          | _ ->
              ()
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
                List.iter ~f:handle_negated_condition
                  (Procdesc.Node.get_preds boolean_assignment_node) ;
                !res_typestate
            | None ->
                ( match Errdesc.find_program_variable_assignment curr_node pvar with
                | None ->
                    ()
                | Some (node', id) ->
                    let () =
                      match Errdesc.find_normal_variable_funcall node' id with
                      | Some (Exp.Const (Const.Cfun pn), [e], _, _)
                        when ComplexExpressions.procname_optional_isPresent pn ->
                          handle_optional_isPresent node' e
                      | _ ->
                          ()
                    in
                    () ) ;
                !res_typestate )
        | _ ->
            typestate'
      in
      (* Handle m.put(k,v) as assignment pvar = v for the pvar associated to m.get(k) *)
      let do_map_put typestate' =
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
              ComplexExpressions.exp_to_string_map_dexp tenv convert_dexp_key_to_dexp_get node
                exp_key
            with
            | Some map_get_str ->
                let pvar_map_get = Pvar.mk (Mangled.from_string map_get_str) curr_pname in
                TypeState.add pvar_map_get
                  (typecheck_expr_simple typestate' exp_value typ_value TypeOrigin.Undef loc)
                  typestate'
            | None ->
                typestate' )
        | _ ->
            typestate'
      in
      let typestate_after_call, resolved_ret =
        let resolve_param i (sparam, cparam) =
          let s1, ia1, t1 = sparam in
          let (orig_e2, e2), t2 = cparam in
          let ta1 = TypeAnnotation.from_item_annotation ia1 (TypeOrigin.Formal s1) in
          let _, ta2, _ =
            typecheck_expr find_canonical_duplicate calls_this checks tenv node instr_ref
              curr_pdesc typestate e2
              (t2, TypeAnnotation.const AnnotatedSignature.Nullable false TypeOrigin.ONone, [])
              loc
          in
          let formal = (s1, ta1, t1) in
          let actual = (orig_e2, ta2) in
          let num = i + 1 in
          let formal_is_propagates_nullable = Annotations.ia_is_propagates_nullable ia1 in
          let actual_is_nullable = TypeAnnotation.get_value AnnotatedSignature.Nullable ta2 in
          let propagates_nullable = formal_is_propagates_nullable && actual_is_nullable in
          EradicateChecks.{num; formal; actual; propagates_nullable}
        in
        (* Apply a function that operates on annotations *)
        let apply_annotation_transformer resolved_ret
            (resolved_params : EradicateChecks.resolved_param list) =
          let rec handle_params resolved_ret params =
            match (params : EradicateChecks.resolved_param list) with
            | param :: params' when param.propagates_nullable ->
                let _, actual_ta = param.actual in
                let resolved_ret' =
                  let ret_ta, ret_typ = resolved_ret in
                  let ret_ta' =
                    let actual_nullable =
                      TypeAnnotation.get_value AnnotatedSignature.Nullable actual_ta
                    in
                    let old_nullable =
                      TypeAnnotation.get_value AnnotatedSignature.Nullable ret_ta
                    in
                    let new_nullable = old_nullable || actual_nullable in
                    TypeAnnotation.set_value AnnotatedSignature.Nullable new_nullable ret_ta
                  in
                  (ret_ta', ret_typ)
                in
                handle_params resolved_ret' params'
            | _ :: params' ->
                handle_params resolved_ret params'
            | [] ->
                resolved_ret
          in
          handle_params resolved_ret resolved_params
        in
        let resolved_ret_ =
          let ret_ia, ret_typ = callee_annotated_signature.AnnotatedSignature.ret in
          let is_library = Summary.proc_is_library callee_attributes in
          let origin =
            TypeOrigin.Proc
              { TypeOrigin.pname= callee_pname
              ; loc
              ; annotated_signature= callee_annotated_signature
              ; is_library }
          in
          let ret_ta = TypeAnnotation.from_item_annotation ret_ia origin in
          (ret_ta, ret_typ)
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
            ~f:(fun (sparam, _) ->
              let s, _, _ = sparam in
              let param_is_this =
                Mangled.is_this s || String.is_prefix ~prefix:"this$" (Mangled.to_string s)
              in
              not param_is_this )
            (List.zip_exn sig_slice call_slice)
        in
        let resolved_params = List.mapi ~f:resolve_param sig_call_params in
        let resolved_ret = apply_annotation_transformer resolved_ret_ resolved_params in
        let typestate_after_call =
          if not is_anonymous_inner_class_constructor then (
            ( if Config.eradicate_debug then
              let unique_id = Typ.Procname.to_unique_id callee_pname in
              let classification = EradicateChecks.classify_procedure callee_attributes in
              L.result "  %s unique id: %s@." classification unique_id ) ;
            if cflags.CallFlags.cf_virtual && checks.eradicate then
              EradicateChecks.check_call_receiver tenv find_canonical_duplicate curr_pdesc node
                typestate1 call_params callee_pname instr_ref loc
                (typecheck_expr find_canonical_duplicate calls_this checks) ;
            if checks.eradicate then
              EradicateChecks.check_call_parameters tenv find_canonical_duplicate curr_pdesc node
                callee_attributes resolved_params loc instr_ref ;
            if Models.is_check_not_null callee_pname then
              if Typ.Procname.Java.is_vararg callee_pname_java then
                let last_parameter = List.length call_params in
                do_preconditions_check_not_null last_parameter ~is_vararg:true typestate1
              else
                do_preconditions_check_not_null
                  (Models.get_check_not_null_parameter callee_pname)
                  ~is_vararg:false typestate1
            else if Models.is_check_state callee_pname || Models.is_check_argument callee_pname
            then do_preconditions_check_state typestate1
            else if Models.is_mapPut callee_pname then do_map_put typestate1
            else typestate1 )
          else typestate1
        in
        (typestate_after_call, resolved_ret)
      in
      do_return resolved_ret loc typestate_after_call
  | Sil.Call _ ->
      typestate
  | Sil.Prune (cond, loc, true_branch, _) ->
      let rec check_condition node' c : TypeState.t =
        (* check if the expression is coming from a call, and return the argument *)
        let from_call filter_callee e : Exp.t option =
          match e with
          | Exp.Var id -> (
            match Errdesc.find_normal_variable_funcall node' id with
            | Some (Exp.Const (Const.Cfun pn), e1 :: _, _, _) when filter_callee pn ->
                Some e1
            | _ ->
                None )
          | _ ->
              None
        in
        (* check if the expression is coming from instanceof *)
        let from_instanceof e : Exp.t option =
          from_call ComplexExpressions.procname_instanceof e
        in
        (* check if the expression is coming from Optional.isPresent *)
        let from_optional_isPresent e : Exp.t option =
          from_call ComplexExpressions.procname_optional_isPresent e
        in
        (* check if the expression is coming from a procedure returning false on null *)
        let from_is_false_on_null e : Exp.t option =
          from_call (ComplexExpressions.procname_is_false_on_null tenv) e
        in
        (* check if the expression is coming from a procedure returning true on null *)
        let from_is_true_on_null e : Exp.t option =
          from_call (ComplexExpressions.procname_is_true_on_null tenv) e
        in
        (* check if the expression is coming from Map.containsKey *)
        let from_containsKey e : Exp.t option =
          from_call ComplexExpressions.procname_containsKey e
        in
        (* Turn x.containsKey(e) into the pvar for x.get(e) *)
        (* which is then treated as a normal condition != null. *)
        let handle_containsKey e =
          let map_dexp = function
            | Some
                (DExp.Dretcall
                  (DExp.Dconst (Const.Cfun (Typ.Procname.Java pname_java)), args, loc, call_flags))
              ->
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
          match ComplexExpressions.exp_to_string_map_dexp tenv map_dexp node' e with
          | Some e_str ->
              let pvar = Pvar.mk (Mangled.from_string e_str) curr_pname in
              let e1 = Exp.Lvar pvar in
              let typ, ta, _ =
                typecheck_expr_simple typestate e1 (Typ.mk Tvoid) TypeOrigin.ONone loc
              in
              let range = (typ, ta, [loc]) in
              let typestate1 = TypeState.add pvar range typestate in
              (typestate1, e1, EradicateChecks.From_containsKey)
          | None ->
              (typestate, e, EradicateChecks.From_condition)
        in
        let set_flag e' ann b typestate2 =
          (* add constraint on e' for annotation ann *)
          let handle_pvar typestate' pvar =
            match TypeState.lookup_pvar pvar typestate' with
            | Some (t, ta1, locs) ->
                if TypeAnnotation.get_value ann ta1 <> b then
                  let ta2 = TypeAnnotation.set_value ann b ta1 in
                  TypeState.add pvar (t, ta2, locs) typestate'
                else typestate'
            | None ->
                typestate'
          in
          match e' with
          | Exp.Lvar pvar ->
              pvar_apply loc handle_pvar typestate2 pvar
          | _ ->
              typestate2
        in
        match[@warning "-57"] c with
        | Exp.BinOp (Binop.Eq, Exp.Const (Const.Cint i), e)
        | Exp.BinOp (Binop.Eq, e, Exp.Const (Const.Cint i))
          when IntLit.iszero i -> (
            typecheck_expr_for_errors typestate e loc ;
            let typestate1, e1, from_call =
              match from_is_true_on_null e with
              | Some e1 ->
                  (typestate, e1, EradicateChecks.From_is_true_on_null)
              | None ->
                  (typestate, e, EradicateChecks.From_condition)
            in
            let e', typestate2 = convert_complex_exp_to_pvar node' false e1 typestate1 loc in
            let typ, ta, _ =
              typecheck_expr_simple typestate2 e' (Typ.mk Tvoid) TypeOrigin.ONone loc
            in
            if checks.eradicate then
              EradicateChecks.check_zero tenv find_canonical_duplicate curr_pdesc node' e' typ ta
                true_branch EradicateChecks.From_condition idenv linereader loc instr_ref ;
            match from_call with
            | EradicateChecks.From_is_true_on_null ->
                (* if f returns true on null, then false branch implies != null *)
                if TypeAnnotation.get_value AnnotatedSignature.Nullable ta then
                  set_flag e' AnnotatedSignature.Nullable false typestate2
                else typestate2
            | _ ->
                typestate2 )
        | Exp.BinOp (Binop.Ne, Exp.Const (Const.Cint i), e)
        | Exp.BinOp (Binop.Ne, e, Exp.Const (Const.Cint i))
          when IntLit.iszero i -> (
            typecheck_expr_for_errors typestate e loc ;
            let typestate1, e1, from_call =
              match from_instanceof e with
              | Some e1 ->
                  (* (e1 instanceof C) implies (e1 != null) *)
                  (typestate, e1, EradicateChecks.From_instanceof)
              | None -> (
                match from_optional_isPresent e with
                | Some e1 ->
                    (typestate, e1, EradicateChecks.From_optional_isPresent)
                | None -> (
                  match from_is_false_on_null e with
                  | Some e1 ->
                      (typestate, e1, EradicateChecks.From_is_false_on_null)
                  | None ->
                      if Option.is_some (from_containsKey e) then handle_containsKey e
                      else (typestate, e, EradicateChecks.From_condition) ) )
            in
            let e', typestate2 = convert_complex_exp_to_pvar node' false e1 typestate1 loc in
            let typ, ta, _ =
              typecheck_expr_simple typestate2 e' (Typ.mk Tvoid) TypeOrigin.ONone loc
            in
            if checks.eradicate then
              EradicateChecks.check_nonzero tenv find_canonical_duplicate curr_pdesc node e' typ ta
                true_branch from_call idenv linereader loc instr_ref ;
            match from_call with
            | EradicateChecks.From_optional_isPresent ->
                if not (TypeAnnotation.get_value AnnotatedSignature.Present ta) then
                  set_flag e' AnnotatedSignature.Present true typestate2
                else typestate2
            | EradicateChecks.From_is_true_on_null ->
                typestate2
            | EradicateChecks.From_condition
            | EradicateChecks.From_containsKey
            | EradicateChecks.From_instanceof
            | EradicateChecks.From_is_false_on_null ->
                if TypeAnnotation.get_value AnnotatedSignature.Nullable ta then
                  set_flag e' AnnotatedSignature.Nullable false typestate2
                else typestate2 )
        | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, e1, e2), _) ->
            check_condition node' (Exp.BinOp (Binop.Ne, e1, e2))
        | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Ne, e1, e2), _) ->
            check_condition node' (Exp.BinOp (Binop.Eq, e1, e2))
        | _ ->
            typestate
        (* FIXME: silenced warning may be legit *)
      in
      (* Handle assignment fron a temp pvar in a condition.
         This recognizes the handling of temp variables in ((x = ...) != null) *)
      let handle_assignment_in_condition pvar =
        match Procdesc.Node.get_preds node with
        | [prev_node] ->
            let found = ref None in
            let do_instr i =
              match i with
              | Sil.Store (e, _, e', _) when Exp.equal (Exp.Lvar pvar) (Idenv.expand_expr idenv e')
                ->
                  found := Some e
              | _ ->
                  ()
            in
            Instrs.iter ~f:do_instr (Procdesc.Node.get_instrs prev_node) ;
            !found
        | _ ->
            None
      in
      (* Normalize the condition by resolving temp variables. *)
      let rec normalize_cond node_ cond_ =
        match cond_ with
        | Exp.UnOp (Unop.LNot, c, top) ->
            let node', c' = normalize_cond node_ c in
            (node', Exp.UnOp (Unop.LNot, c', top))
        | Exp.BinOp (bop, c1, c2) ->
            let node', c1' = normalize_cond node_ c1 in
            let node'', c2' = normalize_cond node' c2 in
            (node'', Exp.BinOp (bop, c1', c2'))
        | Exp.Var _ ->
            let c' = Idenv.expand_expr idenv cond_ in
            if not (Exp.equal c' cond_) then normalize_cond node_ c' else (node_, c')
        | Exp.Lvar pvar when Pvar.is_frontend_tmp pvar -> (
          match handle_assignment_in_condition pvar with
          | None -> (
            match Errdesc.find_program_variable_assignment node_ pvar with
            | Some (node', id) ->
                (node', Exp.Var id)
            | None ->
                (node_, cond_) )
          | Some e2 ->
              (node_, e2) )
        | c ->
            (node_, c)
      in
      let node', ncond = normalize_cond node cond in
      check_condition node' ncond


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
    | Sil.Store (Exp.Lvar pv, _, _, _)
      when Pvar.is_return pv
           && Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind node) Procdesc.Node.throw_kind
      ->
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
