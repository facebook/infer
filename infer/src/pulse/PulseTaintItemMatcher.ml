(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface

let pp_procedure_matcher_error_message f (matcher : Pulse_config_t.matcher) =
  let pp_procedure_matcher f (matcher : Pulse_config_t.matcher) =
    F.fprintf f
      "\"procedure\": %a, \n\
      \ \"procedure_regex\": %a, \n\
      \ \"class_name_regex\": %a, \n\
      \ \"class_names\": %a, \n\
      \ \"method_names\": %a, \n\
      \ \"method_return_type_names\": %a, \n\
      \ \"overrides_of_class_with_annotation\": %a,\n\
      \ \"method_with_annotation\": %a, \n\
      \ \"block_passed_to\": %a, \n\
      \ \"block_passed_to_regex\": %a, \n\
      \ \"allocation\": %a" (Pp.option F.pp_print_string) matcher.procedure
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
      (Pp.option F.pp_print_string) matcher.block_passed_to_regex (Pp.option F.pp_print_string)
      matcher.allocation
  in
  F.fprintf f
    "To build a procedure matcher, exactly one of \n\
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
    \ %a." pp_procedure_matcher matcher


let pp_field_matcher_error_message f (matcher : Pulse_config_t.matcher) =
  let pp_field_matcher f (matcher : Pulse_config_t.matcher) =
    F.fprintf f "\"field_regex\": %a, \n \"class_names\": %a, \n \"field_names\": %a"
      (Pp.option F.pp_print_string) matcher.field_regex
      (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
      matcher.class_names
      (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
      matcher.field_names
  in
  F.fprintf f
    "To build a field matcher, exactly one of \n\
    \ \"field_regex\", \n\
     or else \"class_names\" and \"field_names\" must be provided, \n\
     but got \n\
    \ %a" pp_field_matcher matcher


let procedure_matcher_of_config ~default_taint_target ~option_name (matcher : Pulse_config_t.matcher)
    =
  let open TaintConfig.Unit in
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
        ProcedureNameRegex
          {name_regex= Str.regexp name_regex; exclude_in= matcher.exclude_from_regex_in}
    | { procedure= None
      ; procedure_regex= None
      ; class_name_regex= Some name_regex
      ; class_names= None
      ; method_names= None
      ; overrides_of_class_with_annotation= None
      ; method_with_annotation= None
      ; block_passed_to= None
      ; allocation= None } ->
        ClassNameRegex {name_regex= Str.regexp name_regex; exclude_in= matcher.exclude_from_regex_in}
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
        BlockNameRegex {name_regex= Str.regexp name_regex; exclude_in= matcher.exclude_from_regex_in}
    | _ ->
        L.die UserError "When parsing option %s: Unexpected JSON format: %a \n %a" option_name
          pp_procedure_matcher_error_message matcher pp_field_matcher_error_message matcher
  in
  let taint_target =
    TaintConfig.Target.target_of_gen_target
      (Option.value ~default:default_taint_target matcher.taint_target)
  in
  match taint_target with
  | ProcedureTarget procedure_target ->
      { procedure_matcher
      ; arguments= matcher.argument_constraints
      ; kinds= TaintConfig.Kind.kinds_of_strings_opt matcher.kinds
      ; procedure_target }
  | FieldTarget _ ->
      L.die UserError
        "Target %a found but one of the following targets must be provided:\n\
        \         ReturnValue, AllArguments, ArgumentPositions, AllArgumentsButPositions, \
         ArgumentsMatchingTypes, InstanceReference, FieldsOfValue"
        TaintConfig.Target.pp taint_target


let field_matcher_of_config ~default_taint_target ~option_name (matcher : Pulse_config_t.matcher) =
  let open TaintConfig.Unit in
  let field_matcher =
    match matcher with
    | {field_regex= Some name_regex; class_names= None; field_names= None} ->
        FieldRegex {name_regex= Str.regexp name_regex; exclude_in= matcher.exclude_from_regex_in}
    | {field_regex= None; class_names= Some class_names; field_names= Some field_names} ->
        ClassAndFieldNames {class_names; field_names}
    | _ ->
        L.die UserError "When parsing option %s: Unexpected JSON format: %a %a" option_name
          pp_field_matcher_error_message matcher pp_procedure_matcher_error_message matcher
  in
  let taint_target =
    TaintConfig.Target.target_of_gen_target
      (Option.value ~default:default_taint_target matcher.taint_target)
  in
  match taint_target with
  | FieldTarget field_target ->
      {field_matcher; kinds= TaintConfig.Kind.kinds_of_strings_opt matcher.kinds; field_target}
  | ProcedureTarget _ ->
      L.die UserError
        "Target %a found but one of the following targets must be provided: GetField or SetField"
        TaintConfig.Target.pp taint_target


let matcher_of_config ~default_taint_target ~option_name (matchers : Pulse_config_t.matcher list) =
  List.map matchers ~f:(fun (matcher : Pulse_config_t.matcher) ->
      if Option.is_some matcher.field_regex || Option.is_some matcher.field_names then
        TaintConfig.Unit.FieldUnit
          (field_matcher_of_config ~default_taint_target ~option_name matcher)
      else
        TaintConfig.Unit.ProcedureUnit
          (procedure_matcher_of_config ~default_taint_target ~option_name matcher) )


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


let class_names_match tenv class_names class_name =
  Option.exists class_name ~f:(fun class_name ->
      let class_name =
        if Language.curr_language_is Hack && Typ.Name.Hack.is_static class_name then
          (* in Hack, instance and static method are dispatched in the class and in its static
             companion, but they can not appear in both *)
          Typ.Name.Hack.static_companion_origin class_name
        else class_name
      in
      PatternMatch.supertype_exists tenv
        (fun class_name _ -> List.mem ~equal:String.equal class_names (Typ.Name.name class_name))
        class_name )


let taint_procedure_target_matches tenv taint_target actual_index actual_typ =
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
  | FieldsOfValue _ | ReturnValue | InstanceReference ->
      false


let check_regex name_regex elem ?source_file exclude_in =
  L.d_printfln "Matching regex wrt %s" elem ;
  let should_exclude_location =
    match (source_file, exclude_in) with
    | Some source_file, Some exclude_in ->
        L.d_printfln "Checking exclude_in list with location of elem = %a and exclude_in = %a"
          SourceFile.pp source_file (Pp.semicolon_seq String.pp) exclude_in ;
        List.exists exclude_in ~f:(fun exclude ->
            (* This is needed because in clang languages the location of the elem can be either in the header
               or in the source file, depending whether the source file is available in the analysis or not.
               So we assume exclude_in always ends in .h for clang languages, we drop the suffix and then we
               can compare the remaining name with the location. *)
            let exclude_header = String.chop_suffix_if_exists exclude ~suffix:".h" in
            String.is_substring (SourceFile.to_string source_file) ~substring:exclude_header )
    | _ ->
        false
  in
  if should_exclude_location then (
    L.d_printfln "No match because of exclude_in" ;
    false )
  else
    match Str.search_forward name_regex elem 0 with
    | _ ->
        L.d_printfln "Found match" ;
        true
    | exception Caml.Not_found ->
        L.d_printfln "No match" ;
        false


let procedure_matches tenv matchers ?block_passed_to ?proc_attributes proc_name actuals =
  let open TaintConfig.Unit in
  List.filter_map matchers ~f:(fun matcher ->
      let class_name = Procname.get_class_type_name proc_name in
      let procedure_name_matches =
        match matcher.procedure_matcher with
        | ProcedureName {name} ->
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose proc_name in
            String.is_substring ~substring:name proc_name_s
        | ProcedureNameRegex {name_regex; exclude_in} ->
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose proc_name in
            let source_file =
              Option.map ~f:(fun attr -> attr.ProcAttributes.loc.Location.file) proc_attributes
            in
            check_regex name_regex proc_name_s ?source_file exclude_in
        | ClassNameRegex {name_regex; exclude_in} -> (
            let check_regex_class class_name class_struct_opt =
              let class_name_s = Typ.Name.name class_name in
              let source_file =
                Option.value_map class_struct_opt ~default:None ~f:(fun class_struct ->
                    class_struct.Struct.source_file )
              in
              (* Sometimes the classes don't get added to the tenv, so we use the proc_attributes loc as a backup *)
              let source_file =
                if Option.is_some source_file then source_file
                else
                  Option.map ~f:(fun attr -> attr.ProcAttributes.loc.Location.file) proc_attributes
              in
              check_regex ?source_file name_regex class_name_s exclude_in
            in
            match class_name with
            | Some class_name ->
                Tenv.mem_supers tenv ~f:check_regex_class class_name
            | None ->
                false )
        | ClassAndMethodNames {class_names; method_names} ->
            class_names_match tenv class_names class_name
            && List.mem ~equal:String.equal method_names (Procname.get_method proc_name)
        | ClassAndMethodReturnTypeNames {class_names; method_return_type_names} ->
            let procedure_return_type_match method_return_type_names =
              Option.exists proc_attributes ~f:(fun attrs ->
                  type_matches tenv attrs.ProcAttributes.ret_type method_return_type_names )
            in
            class_names_match tenv class_names class_name
            && procedure_return_type_match method_return_type_names
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
        | BlockNameRegex {name_regex; exclude_in}, Some block_passed_to_proc_name ->
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose block_passed_to_proc_name in
            let source_file =
              Option.map ~f:(fun attr -> attr.ProcAttributes.loc.Location.file) proc_attributes
            in
            check_regex name_regex ?source_file proc_name_s exclude_in
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


let match_field_target matches actual potential_taint_value =
  let open TaintConfig.Target in
  let actual =
    match actual with
    | {ProcnameDispatcher.Call.FuncArg.arg_payload; typ; exp} ->
        (arg_payload, typ, Some exp)
  in
  let match_target acc (matcher : TaintConfig.Unit.field_unit) =
    let origin : TaintItem.origin =
      match matcher.field_target with GetField -> GetField | SetField -> SetField
    in
    let taint = {TaintItem.value= potential_taint_value; origin; kinds= matcher.kinds} in
    (taint, actual) :: acc
  in
  List.fold matches ~init:[] ~f:(fun acc (matcher : TaintConfig.Unit.field_unit) ->
      match_target acc matcher )


let field_matches tenv matchers field_name =
  let open TaintConfig.Unit in
  List.filter_map matchers ~f:(fun matcher ->
      match matcher.field_matcher with
      | FieldRegex {name_regex; exclude_in} ->
          let field_name_s = Fieldname.get_field_name field_name in
          let class_name = Fieldname.get_class_name field_name in
          let source_file =
            if Option.is_some exclude_in then
              let class_struct_opt = Tenv.lookup tenv class_name in
              Option.value_map ~default:None class_struct_opt ~f:(fun class_struct ->
                  class_struct.Struct.source_file )
            else None
          in
          if check_regex ?source_file name_regex field_name_s exclude_in then Some matcher else None
      | ClassAndFieldNames {class_names; field_names} ->
          let class_name = Fieldname.get_class_name field_name in
          if
            class_names_match tenv class_names (Some class_name)
            && List.mem ~equal:String.equal field_names (Fieldname.get_field_name field_name)
          then Some matcher
          else None )


let match_procedure_target tenv astate matches path location return_opt ~has_added_return_param
    actuals ~instance_reference potential_taint_value =
  let open TaintConfig.Target in
  let actuals =
    List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload; typ; exp} ->
        (arg_payload, typ, Some exp) )
  in
  let rec match_target kinds acc procedure_target =
    match procedure_target with
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
                let taint = {TaintItem.value= potential_taint_value; origin= ReturnValue; kinds} in
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
                         {TaintItem.value= potential_taint_value; origin= ReturnValue; kinds}
                       in
                       (astate, (taint, (return_value, return_typ, None)) :: tainted) ) ) )
    | (AllArguments | ArgumentPositions _ | AllArgumentsButPositions _ | ArgumentsMatchingTypes _)
      as taint_target ->
        L.d_printf "matching actuals... " ;
        List.foldi actuals ~init:acc
          ~f:(fun i ((astate, tainted) as acc) ((_, actual_typ, exp) as actual_hist_and_typ) ->
            let is_const_exp = match exp with Some exp -> Exp.is_const exp | None -> false in
            if taint_procedure_target_matches tenv taint_target i actual_typ && not is_const_exp
            then (
              L.d_printfln_escaped "match! tainting actual #%d with type %a" i (Typ.pp_full Pp.text)
                actual_typ ;
              let taint =
                {TaintItem.value= potential_taint_value; origin= Argument {index= i}; kinds}
              in
              (astate, (taint, actual_hist_and_typ) :: tainted) )
            else (
              L.d_printfln_escaped "no match for #%d with type %a" i (Typ.pp_full Pp.text)
                actual_typ ;
              acc ) )
    | InstanceReference -> (
        L.d_printf "matching this/self... " ;
        match instance_reference with
        | Some instance_reference ->
            let instance_reference =
              match instance_reference with
              | {ProcnameDispatcher.Call.FuncArg.arg_payload; typ; exp} ->
                  (arg_payload, typ, Some exp)
            in
            let taint =
              {TaintItem.value= potential_taint_value; origin= InstanceReference; kinds}
            in
            let astate, tainted = acc in
            (astate, (taint, instance_reference) :: tainted)
        | None ->
            L.die UserError "Error in taint configuration: `%a` is not an instance method"
              TaintItem.pp_value potential_taint_value )
    | FieldsOfValue fields ->
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
                if String.equal fieldname (Fieldname.get_field_name field) then Some typ else None )
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
                "Type error in taint configuration: Model for `%a`:Type `%a` does not have a field \
                 `%s`"
                TaintItem.pp_value potential_taint_value (Typ.pp_full Pp.text) typ fieldname
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
                      PulseOperations.eval_access path Read location ret_value Dereference astate
                    in
                    L.d_printfln "match! tainting field %s with type %a" fieldname
                      (Typ.pp_full Pp.text) field_typ ;
                    ( astate
                    , ( TaintItem.
                          {taint with origin= FieldOfValue {name= fieldname; origin= taint.origin}}
                      , (ret_value, field_typ, None) )
                      :: tainted ) ) )
        in
        List.fold fields ~init:acc ~f:(fun ((astate, tainted) as acc) (fieldname, origin) ->
            let astate', tainted' = match_target kinds acc origin in
            let new_taints, _ =
              List.split_n tainted' (List.length tainted' - List.length tainted)
            in
            let acc = if phys_equal astate' astate then acc else (astate', tainted) in
            List.fold new_taints ~init:acc ~f:(fun acc taint ->
                move_taint_to_field acc taint fieldname ) )
  in
  List.fold matches ~init:(astate, []) ~f:(fun acc (matcher : TaintConfig.Unit.procedure_unit) ->
      match_target matcher.kinds acc matcher.procedure_target )


let get_tainted tenv path location ~procedure_matchers ~block_matchers ~field_matchers return_opt
    ~has_added_return_param ?proc_attributes potential_taint_value actuals astate =
  let block_passed_to, matchers =
    match potential_taint_value with
    | TaintItem.TaintBlockPassedTo proc_name ->
        (Some proc_name, block_matchers)
    | _ ->
        (None, procedure_matchers)
  in
  let match_procedure proc_name actuals ~instance_reference =
    let matches =
      procedure_matches tenv matchers ?block_passed_to ?proc_attributes proc_name actuals
    in
    if not (List.is_empty matches) then L.d_printfln "taint matches" ;
    match_procedure_target tenv astate matches path location return_opt ~has_added_return_param
      actuals ~instance_reference potential_taint_value
  in
  match potential_taint_value with
  | TaintItem.TaintProcedure proc_name ->
      let get_this_from_actuals = function
        (* Instance method a guaranteed to have this/self as a first formal *)
        | instance_reference :: actuals ->
            (Some instance_reference, actuals)
        | [] ->
            L.die InternalError "Procedure %a is supposed to have this/self as a first parameter"
              Procname.pp proc_name
      in
      (* Drop implicit this/self from instance method formals *)
      let instance_reference, actuals =
        match Procname.is_static proc_name with
        | Some is_static ->
            (* NSObject.self and NSObject.class are special methods that don't have implicit self param *)
            if is_static || Procname.is_objc_nsobject_self_or_class_method proc_name then
              (None, actuals)
            else get_this_from_actuals actuals
        | None ->
            (* Hack is special with each method having a reference to this/self except some special cases *)
            if
              Procname.is_hack proc_name
              && Procname.has_hack_classname proc_name
              && not (Procname.is_hack_builtins proc_name)
            then get_this_from_actuals actuals
            else (None, actuals)
      in
      match_procedure proc_name actuals ~instance_reference
  | TaintItem.TaintBlockPassedTo proc_name ->
      match_procedure proc_name actuals ~instance_reference:None
  | TaintItem.TaintField field_name -> (
      let matches = field_matches tenv field_matchers field_name in
      if not (List.is_empty matches) then L.d_printfln "taint matches" ;
      match actuals with
      | [actual] ->
          (astate, match_field_target matches actual potential_taint_value)
      | _ ->
          (astate, []) )
