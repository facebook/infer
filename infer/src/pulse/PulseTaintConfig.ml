(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module Kind = struct
  type t = string [@@deriving compare, equal, hash]

  type kind_info = {name: string; is_data_flow_only: bool}

  (** Taint "kinds" are user-configurable and thus represented as strings. This hash table ensures
      we only store one copy of each kind. It also identifies which kinds are designated for data
      flow reporting only. *)
  let all_kinds = Hashtbl.create (module String)

  let of_string name =
    (* use [all_kinds] to do a weak hashconsing and try to keep only one version of each string
       around. This does not ensure we always get the same representative for each string because
       kinds get marshalled in and out of summaries, which does not maintain physical equality
       between equal kinds *)
    (Hashtbl.find_or_add all_kinds name ~default:(fun () -> {name; is_data_flow_only= false})).name


  let hash kind = String.hash kind

  let sexp_of_t kind = String.sexp_of_t kind

  let mark_data_flow_only name =
    Hashtbl.update all_kinds name ~f:(fun _ -> {name; is_data_flow_only= true})


  let is_data_flow_only name =
    Hashtbl.find all_kinds name |> Option.exists ~f:(fun {is_data_flow_only} -> is_data_flow_only)


  let pp fmt kind =
    F.fprintf fmt "`%s`%s" kind (if is_data_flow_only kind then " (data flow only)" else "")


  let simple_kind = of_string "Simple"

  let kinds_of_strings_opt = function
    | None ->
        [simple_kind]
    | Some kinds ->
        List.map kinds ~f:of_string
end

module Target = struct
  type procedure_target =
    | ReturnValue
    | AllArguments
    | ArgumentPositions of int list
    | AllArgumentsButPositions of int list
    | ArgumentsMatchingTypes of string list
    | InstanceReference
    | FieldsOfValue of (string * procedure_target) list

  type field_target = GetField | SetField

  type t = ProcedureTarget of procedure_target | FieldTarget of field_target

  let rec pp_procedure_target f procedure_target =
    match procedure_target with
    | ReturnValue ->
        F.pp_print_string f "ReturnValue"
    | AllArguments ->
        F.pp_print_string f "AllArguments"
    | ArgumentPositions positions ->
        F.fprintf f "ArgumentPositions %a" (Pp.comma_seq Int.pp) positions
    | AllArgumentsButPositions positions ->
        F.fprintf f "AllArgumentsButPositions %a" (Pp.comma_seq Int.pp) positions
    | ArgumentsMatchingTypes types ->
        F.fprintf f "ArgumentsMatchingTypes %a" (Pp.comma_seq String.pp) types
    | InstanceReference ->
        F.pp_print_string f "InstanceReference"
    | FieldsOfValue targets ->
        F.fprintf f "Fields %a"
          (Pp.comma_seq (Pp.pair ~fst:String.pp ~snd:pp_procedure_target))
          targets


  let pp_field_target f field_target =
    match field_target with
    | GetField ->
        F.pp_print_string f "GetField"
    | SetField ->
        F.pp_print_string f "SetField"


  let pp f target =
    match target with
    | ProcedureTarget target ->
        pp_procedure_target f target
    | FieldTarget target ->
        pp_field_target f target


  let rec target_of_gen_target (gen_target : Pulse_config_t.taint_target) =
    match gen_target with
    | `ReturnValue ->
        ProcedureTarget ReturnValue
    | `AllArguments ->
        ProcedureTarget AllArguments
    | `ArgumentPositions l ->
        ProcedureTarget (ArgumentPositions l)
    | `AllArgumentsButPositions l ->
        ProcedureTarget (AllArgumentsButPositions l)
    | `ArgumentsMatchingTypes l ->
        ProcedureTarget (ArgumentsMatchingTypes l)
    | `InstanceReference ->
        ProcedureTarget InstanceReference
    | `FieldsOfValue l ->
        let fields_targets =
          List.map l ~f:(fun (field, target) ->
              match target_of_gen_target target with
              | ProcedureTarget procedure_target ->
                  (field, procedure_target)
              | FieldTarget field_target ->
                  L.die UserError
                    "Only procedure targets are allowed within FieldsOfValue target, but found %a"
                    pp_field_target field_target )
        in
        ProcedureTarget (FieldsOfValue fields_targets)
    | `GetField ->
        FieldTarget GetField
    | `SetField ->
        FieldTarget SetField
end

module Unit = struct
  type procedure_matcher =
    | ProcedureName of {name: string}
    | ProcedureNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | ClassNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | ClassAndMethodNames of {class_names: string list; method_names: string list}
    | ClassAndMethodReturnTypeNames of
        {class_names: string list; method_return_type_names: string list}
    | OverridesOfClassWithAnnotation of {annotation: string}
    | MethodWithAnnotation of {annotation: string}
    | Block of {name: string}
    | BlockNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | Allocation of {class_name: string}

  let pp_procedure_matcher f procedure_matcher =
    match procedure_matcher with
    | ProcedureName {name} ->
        F.fprintf f "Procedure %s" name
    | ProcedureNameRegex _ ->
        F.pp_print_string f "Procedure name regex"
    | ClassNameRegex _ ->
        F.pp_print_string f "Class Name regex"
    | ClassAndMethodNames {class_names; method_names} ->
        F.fprintf f "class_names=%a, method_names=%a" (Pp.comma_seq String.pp) class_names
          (Pp.comma_seq String.pp) method_names
    | ClassAndMethodReturnTypeNames {class_names; method_return_type_names} ->
        F.fprintf f "class_names=%a, method_return_type_names=%a" (Pp.comma_seq String.pp)
          class_names (Pp.comma_seq String.pp) method_return_type_names
    | OverridesOfClassWithAnnotation {annotation} ->
        F.fprintf f "overrides of class with annotation=%s" annotation
    | MethodWithAnnotation {annotation} ->
        F.fprintf f "method with annotation=%s" annotation
    | Block {name} ->
        F.fprintf f "Block %s" name
    | BlockNameRegex _ ->
        F.fprintf f "Block regex"
    | Allocation {class_name} ->
        F.fprintf f "allocation %s" class_name


  type field_matcher =
    | FieldRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | ClassAndFieldNames of {class_names: string list; field_names: string list}

  let pp_field_matcher f field_matcher =
    match field_matcher with
    | FieldRegex _ ->
        F.pp_print_string f "Field name regex"
    | ClassAndFieldNames {class_names; field_names} ->
        F.fprintf f "class_names=%a, field_names=%a" (Pp.comma_seq String.pp) class_names
          (Pp.comma_seq String.pp) field_names


  type procedure_unit =
    { procedure_matcher: procedure_matcher
    ; arguments: Pulse_config_t.argument_constraint list
    ; kinds: Kind.t list
    ; procedure_target: Target.procedure_target }

  let pp_arguments f arguments =
    F.pp_print_string f (Pulse_config_j.string_of_argument_constraint arguments)


  let pp_procedure_unit f unit =
    F.fprintf f "procedure_matcher=%a, arguments=%a, kinds=%a, target=%a" pp_procedure_matcher
      unit.procedure_matcher (Pp.comma_seq pp_arguments) unit.arguments (Pp.comma_seq Kind.pp)
      unit.kinds Target.pp_procedure_target unit.procedure_target


  type field_unit =
    {field_matcher: field_matcher; kinds: Kind.t list; field_target: Target.field_target}

  let pp_field_unit f unit =
    F.fprintf f "field_matcher=%a, kinds=%a, field_target=%a" pp_field_matcher unit.field_matcher
      (Pp.comma_seq Kind.pp) unit.kinds Target.pp_field_target unit.field_target


  type t = ProcedureUnit of procedure_unit | FieldUnit of field_unit
end

module SinkPolicy = struct
  type t =
    { source_kinds: Kind.t list [@ignore]
    ; sanitizer_kinds: Kind.t list [@ignore]
    ; description: string [@ignore]
    ; policy_id: int
    ; privacy_effect: string option [@ignore] }
  [@@deriving equal]

  let pp f policy =
    F.fprintf f
      "source_kinds=%a, description=%a, sanitizer_kinds=%a, policy_id=%d, privacy_effect=%a"
      (Pp.comma_seq Kind.pp) policy.source_kinds F.pp_print_string policy.description
      (Pp.comma_seq Kind.pp) policy.sanitizer_kinds policy.policy_id (Pp.option String.pp)
      policy.privacy_effect


  let next_policy_id =
    let policy_id_counter = ref 0 in
    fun () ->
      incr policy_id_counter ;
      !policy_id_counter


  let sink_policies : (Kind.t, t list) Base.Hashtbl.t = Hashtbl.create (module Kind)

  let pp_sink_policies f sink_policies =
    Hashtbl.iteri sink_policies ~f:(fun ~key:kind ~data:policies_by_kind ->
        F.fprintf f "%a -> %a@\n" Kind.pp kind (Pp.comma_seq pp) policies_by_kind )
end
