(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module IRAttributes = Attributes
open PulseDomainInterface
open PulseBasicInterface

let matcher_of_config ~default_taint_target ~option_name matchers =
  let open TaintConfig.Unit in
  List.map matchers ~f:(fun (matcher : Pulse_config_j.matcher) ->
      let procedure_matcher =
        match matcher with
        | { procedure= Some name
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= None
          ; method_names= None
          ; method_return_type_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to= None
          ; allocation= None } ->
            ProcedureName {name}
        | { procedure= None
          ; procedure_regex= Some name_regex
          ; class_name_regex= None
          ; class_names= None
          ; method_names= None
          ; method_return_type_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to= None
          ; allocation= None } ->
            ProcedureNameRegex {name_regex= Str.regexp name_regex}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= Some name_regex
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to= None
          ; allocation= None } ->
            ClassNameRegex {name_regex= Str.regexp name_regex}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= Some class_names
          ; method_names= Some method_names
          ; method_return_type_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to= None
          ; allocation= None } ->
            ClassAndMethodNames {class_names; method_names}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= Some class_names
          ; method_names= None
          ; method_return_type_names= Some method_return_type_names
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; allocation= None } ->
            ClassAndMethodReturnTypeNames {class_names; method_return_type_names}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= None
          ; method_names= None
          ; method_return_type_names= None
          ; overrides_of_class_with_annotation= Some annotation
          ; method_with_annotation= None
          ; allocation= None } ->
            OverridesOfClassWithAnnotation {annotation}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= None
          ; method_names= None
          ; method_return_type_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= Some annotation
          ; block_passed_to= None
          ; allocation= None } ->
            MethodWithAnnotation {annotation}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= None
          ; method_names= None
          ; method_return_type_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to= None
          ; allocation= Some class_name } ->
            Allocation {class_name}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to= Some s
          ; allocation= None } ->
            Block {name= s}
        | { procedure= None
          ; procedure_regex= None
          ; class_name_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; block_passed_to_regex= Some name_regex
          ; allocation= None } ->
            BlockNameRegex {name_regex= Str.regexp name_regex}
        | _ ->
            L.die UserError
              "When parsing option %s: Unexpected JSON format: Exactly one of \n\
              \ \"procedure\", \n\
              \ \"procedure_regex\", \n\
              \ \"class_name_regex\", \n\
              \ \"block_passed_to\", \n\
              \ \"block_passed_to_regex\", \n\
              \ \"allocation\" or \n\
              \ \"overrides_of_class_with_annotation\" must be provided, \n\
               or else \"class_names\" and \"method_names\" must be provided, \n\
               or else \"class_names\" and \"method_return_type_names\" must be provided, \n\
               but got \n\
              \ \"procedure\": %a, \n\
              \ \"procedure_regex\": %a, \n\
              \ \"class_name_regex\": %a, \n\
              \ \"class_names\": %a, \n\
              \ \"method_names\": %a, \n\
              \ \"method_return_type_names\": %a, \n\
              \ \"overrides_of_class_with_annotation\": %a,\n\
              \ \"method_with_annotation\": %a, \n\
              \ \"block_passed_to\": %a, \n\
              \ \"block_passed_to_regex\": %a, \n\
              \ \"allocation\": %a" option_name (Pp.option F.pp_print_string) matcher.procedure
              (Pp.option F.pp_print_string) matcher.procedure_regex (Pp.option F.pp_print_string)
              matcher.class_name_regex
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.class_names
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.method_names
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.method_return_type_names (Pp.option F.pp_print_string)
              matcher.overrides_of_class_with_annotation (Pp.option F.pp_print_string)
              matcher.method_with_annotation (Pp.option F.pp_print_string) matcher.block_passed_to
              (Pp.option F.pp_print_string) matcher.block_passed_to_regex
              (Pp.option F.pp_print_string) matcher.allocation
      in
      { procedure_matcher
      ; arguments= matcher.argument_constraints
      ; kinds= TaintConfig.Kind.kinds_of_strings_opt matcher.kinds
      ; target=
          TaintConfig.Target.target_of_gen_target
            (Option.value ~default:default_taint_target matcher.taint_target) } )


let type_matches tenv actual_typ types =
  (* TODO: [Typ.Name.name] may not be the most intuitive representation of types here, also
     could be slow to generate. Maybe have more fine-grained matching for primitive types vs
     class names (with separate package names to avoid string building) *)
  match actual_typ with
  | {Typ.desc= Tptr ({desc= Tstruct actual_name}, _)} ->
      PatternMatch.supertype_exists tenv
        (fun type_name _struct ->
          let type_name_str = Typ.Name.name type_name in
          List.exists types ~f:(fun typ -> String.is_substring ~substring:typ type_name_str) )
        actual_name
  | _ ->
      false


let taint_target_matches tenv taint_target actual_index actual_typ =
  let open TaintConfig.Target in
  match taint_target with
  | AllArguments ->
      true
  | ArgumentPositions indices ->
      List.mem ~equal:Int.equal indices actual_index
  | AllArgumentsButPositions indices ->
      not (List.mem ~equal:Int.equal indices actual_index)
  | ArgumentsMatchingTypes types ->
      type_matches tenv actual_typ types
  | Fields _ | ReturnValue ->
      false


let procedure_matches tenv matchers ?block_passed_to proc_name actuals =
  let open TaintConfig.Unit in
  List.filter_map matchers ~f:(fun matcher ->
      let class_names_match class_names =
        Option.exists (Procname.get_class_type_name proc_name) ~f:(fun class_name ->
            let class_name =
              if Language.curr_language_is Hack && Typ.Name.Hack.is_static class_name then
                (* in Hack, instance and static method are dispatched in the class and in its static
                   companion, but they can not appear in both *)
                Typ.Name.Hack.static_companion_origin class_name
              else class_name
            in
            PatternMatch.supertype_exists tenv
              (fun class_name _ ->
                List.mem ~equal:String.equal class_names (Typ.Name.name class_name) )
              class_name )
      in
      let procedure_name_matches =
        match matcher.procedure_matcher with
        | ProcedureName {name} ->
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose proc_name in
            String.is_substring ~substring:name proc_name_s
        | ProcedureNameRegex {name_regex} -> (
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose proc_name in
            L.d_printfln "Matching regex wrt %s" proc_name_s ;
            match Str.search_forward name_regex proc_name_s 0 with
            | _ ->
                true
            | exception Caml.Not_found ->
                false )
        | ClassNameRegex {name_regex} ->
            Option.exists (Procname.get_class_type_name proc_name) ~f:(fun class_name ->
                PatternMatch.supertype_exists tenv
                  (fun class_name _ ->
                    let class_name_s = Typ.Name.name class_name in
                    L.d_printfln "Matching regex wrt %s" class_name_s ;
                    match Str.search_forward name_regex class_name_s 0 with
                    | _ ->
                        true
                    | exception Caml.Not_found ->
                        false )
                  class_name )
        | ClassAndMethodNames {class_names; method_names} ->
            class_names_match class_names
            && List.mem ~equal:String.equal method_names (Procname.get_method proc_name)
        | ClassAndMethodReturnTypeNames {class_names; method_return_type_names} ->
            let procedure_return_type_match method_return_type_names =
              Option.exists (IRAttributes.load proc_name) ~f:(fun attrs ->
                  type_matches tenv attrs.ProcAttributes.ret_type method_return_type_names )
            in
            class_names_match class_names && procedure_return_type_match method_return_type_names
        | OverridesOfClassWithAnnotation {annotation} ->
            Option.exists (Procname.get_class_type_name proc_name) ~f:(fun procedure_class_name ->
                let method_name = Procname.get_method proc_name in
                PatternMatch.supertype_exists tenv
                  (fun class_name _ ->
                    Option.exists (Tenv.lookup tenv class_name) ~f:(fun procedure_superclass_type ->
                        Annotations.struct_typ_has_annot procedure_superclass_type
                          (fun annot_item -> Annotations.ia_ends_with annot_item annotation)
                        && PatternMatch.override_exists ~check_current_type:false
                             (fun superclass_pname ->
                               String.equal (Procname.get_method superclass_pname) method_name )
                             tenv proc_name ) )
                  procedure_class_name )
        | MethodWithAnnotation {annotation} ->
            Annotations.pname_has_return_annot proc_name (fun annot_item ->
                Annotations.ia_ends_with annot_item annotation )
        | Allocation _ | Block _ | BlockNameRegex _ ->
            false
      in
      let block_passed_to_matches =
        match (matcher.procedure_matcher, block_passed_to) with
        | Block {name}, Some block_passed_to_proc_name ->
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose block_passed_to_proc_name in
            String.is_substring ~substring:name proc_name_s
        | BlockNameRegex {name_regex}, Some block_passed_to_proc_name -> (
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose block_passed_to_proc_name in
            L.d_printfln "Matching regex wrt %s" proc_name_s ;
            match Str.search_forward name_regex proc_name_s 0 with
            | _ ->
                true
            | exception Caml.Not_found ->
                false )
        | _ ->
            false
      in
      if
        (procedure_name_matches && not (Procname.is_objc_block proc_name))
        || block_passed_to_matches
      then
        let actuals_match =
          List.for_all matcher.arguments ~f:(fun {Pulse_config_t.index; type_matches= types} ->
              List.nth actuals index
              |> Option.exists ~f:(fun {ProcnameDispatcher.Call.FuncArg.typ} ->
                     type_matches tenv typ types ) )
        in
        Option.some_if actuals_match matcher
      else None )


let get_tainted tenv path location matchers return_opt ~has_added_return_param ?block_passed_to
    proc_name actuals astate =
  let open TaintConfig.Target in
  let open TaintConfig.Unit in
  let matches = procedure_matches tenv matchers ?block_passed_to proc_name actuals in
  if not (List.is_empty matches) then L.d_printfln "taint matches" ;
  let actuals =
    List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload; typ; exp} ->
        (arg_payload, typ, Some exp) )
  in
  List.fold matches ~init:(astate, []) ~f:(fun acc matcher ->
      let {kinds} = matcher in
      let rec match_target acc = function
        | ReturnValue -> (
            L.d_printf "matching return value... " ;
            match return_opt with
            | None ->
                L.d_printfln "no match" ;
                acc
            | Some (return, return_typ) -> (
                L.d_printfln "match! tainting return value" ;
                let return_as_actual = if has_added_return_param then List.last actuals else None in
                match return_as_actual with
                | Some actual ->
                    let astate, acc = acc in
                    let taint =
                      {TaintItem.proc_name; origin= ReturnValue; kinds; block_passed_to}
                    in
                    (astate, (taint, actual) :: acc)
                | None ->
                    let return = Var.of_id return in
                    let astate =
                      if Stack.mem return astate then astate
                      else
                        let ret_val = AbstractValue.mk_fresh () in
                        Stack.add return (ret_val, ValueHistory.epoch) astate
                    in
                    Stack.find_opt return astate
                    |> Option.fold ~init:acc ~f:(fun (_, tainted) return_value ->
                           let taint =
                             {TaintItem.proc_name; origin= ReturnValue; kinds; block_passed_to}
                           in
                           (astate, (taint, (return_value, return_typ, None)) :: tainted) ) ) )
        | ( AllArguments
          | ArgumentPositions _
          | AllArgumentsButPositions _
          | ArgumentsMatchingTypes _ ) as taint_target ->
            L.d_printf "matching actuals... " ;
            List.foldi actuals ~init:acc
              ~f:(fun i ((astate, tainted) as acc) ((_, actual_typ, exp) as actual_hist_and_typ) ->
                let is_const_exp = match exp with Some exp -> Exp.is_const exp | None -> false in
                if taint_target_matches tenv taint_target i actual_typ && not is_const_exp then (
                  L.d_printfln_escaped "match! tainting actual #%d with type %a" i
                    (Typ.pp_full Pp.text) actual_typ ;
                  let taint =
                    {TaintItem.proc_name; origin= Argument {index= i}; kinds; block_passed_to}
                  in
                  (astate, (taint, actual_hist_and_typ) :: tainted) )
                else (
                  L.d_printfln_escaped "no match for #%d with type %a" i (Typ.pp_full Pp.text)
                    actual_typ ;
                  acc ) )
        | Fields fields ->
            let type_check astate value typ fieldname =
              (* Dereference the value as much as possible and verify the result
                 is of structure type holding the expected fieldname. *)
              let open IOption.Let_syntax in
              let rec get_val_and_typ_name astate value typ =
                match typ.Typ.desc with
                | Tstruct typ_name | Tptr ({desc= Tstruct typ_name}, _) ->
                    Some (astate, value, typ_name)
                | Tptr (typ', _) ->
                    let* astate, value =
                      PulseOperations.eval_access path Read location value Dereference astate
                      |> PulseResult.ok
                    in
                    get_val_and_typ_name astate value typ'
                | _ ->
                    None
              in
              let* astate, value, typ_name = get_val_and_typ_name astate value typ in
              let+ field_typ =
                let* {Struct.fields} = Tenv.lookup tenv typ_name in
                List.find_map fields ~f:(fun (field, typ, _) ->
                    if String.equal fieldname (Fieldname.get_field_name field) then Some typ
                    else None )
              in
              (astate, value, typ_name, field_typ)
            in
            let move_taint_to_field ((astate, tainted) as acc) (taint, (value, typ, _)) fieldname =
              (* Move the taint from [value] to [value]'s field [fieldname] *)
              match type_check astate value typ fieldname with
              | None ->
                  (* The value cannot hold the expected field.
                     This is a type mismatch and we need to inform the user. *)
                  L.die UserError
                    "Type error in taint configuration: Model for `%a`:Type `%a` does not have a \
                     field `%s`"
                    Procname.pp proc_name (Typ.pp_full Pp.text) typ fieldname
              | Some (astate, value, typ_name, field_typ) ->
                  Option.value ~default:acc
                    (PulseResult.ok
                       (let open PulseResult.Let_syntax in
                        let* astate, ret_value =
                          PulseOperations.eval_access path Read location value
                            (FieldAccess (Fieldname.make typ_name fieldname))
                            astate
                        in
                        let+ astate, ret_value =
                          PulseOperations.eval_access path Read location ret_value Dereference
                            astate
                        in
                        L.d_printfln "match! tainting field %s with type %a" fieldname
                          (Typ.pp_full Pp.text) field_typ ;
                        ( astate
                        , ( TaintItem.
                              {taint with origin= Field {name= fieldname; origin= taint.origin}}
                          , (ret_value, field_typ, None) )
                          :: tainted ) ) )
            in
            List.fold fields ~init:acc ~f:(fun ((astate, tainted) as acc) (fieldname, origin) ->
                let astate', tainted' = match_target acc origin in
                let new_taints, _ =
                  List.split_n tainted' (List.length tainted' - List.length tainted)
                in
                let acc = if phys_equal astate' astate then acc else (astate', tainted) in
                List.fold new_taints ~init:acc ~f:(fun acc taint ->
                    move_taint_to_field acc taint fieldname ) )
      in
      match_target acc matcher.target )
