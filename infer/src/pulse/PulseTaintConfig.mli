(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Kind : sig
  type t = private string [@@deriving compare, equal]

  module Set : PrettyPrintable.PPSet with type elt = t

  val pp : F.formatter -> t -> unit

  val is_data_flow_only : t -> bool
end

module Target : sig
  type procedure_target =
    | ReturnValue
    | AllArguments
    | ArgumentPositions of int list
    | AllArgumentsButPositions of int list
    | ArgumentsMatchingTypes of string list
    | InstanceReference
    | FieldsOfValue of (string * procedure_target) list

  type field_target = GetField | SetField
end

module Unit : sig
  type procedure_matcher =
    | ProcedureName of {name: string}
    | ProcedureNameRegex of
        {name_regex: Str.regexp; exclude_in: string list option; exclude_names: string list option}
    | ClassNameRegex of
        {name_regex: Str.regexp; exclude_in: string list option; exclude_names: string list option}
    | ClassAndMethodNames of {class_names: string list; method_names: string list}
    | ClassNameAndMethodRegex of
        { class_names: string list
        ; method_name_regex: Str.regexp
        ; exclude_in: string list option
        ; exclude_names: string list option }
    | ClassRegexAndMethodRegex of
        { class_name_regex: Str.regexp
        ; method_name_regex: Str.regexp
        ; exclude_in: string list option
        ; exclude_names: string list option }
    | ClassAndMethodReturnTypeNames of
        {class_names: string list; method_return_type_names: string list}
    | ClassWithAnnotation of {annotation: string; annotation_values: string list option}
    | ClassWithAnnotationAndRegexAndMethodRegex of
        { annotation: string
        ; annotation_values: string list option
        ; class_name_regex: Str.regexp
        ; method_name_regex: Str.regexp
        ; exclude_in: string list option
        ; exclude_names: string list option }
    | OverridesOfClassWithAnnotation of {annotation: string}
    | MethodWithAnnotation of {annotation: string; annotation_values: string list option}
    | Block of {name: string}
    | BlockNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | Allocation of {class_name: string}

  type field_matcher =
    | FieldRegex of
        {name_regex: Str.regexp; exclude_in: string list option; exclude_names: string list option}
    | ClassAndFieldNames of {class_names: string list; field_names: string list}
    | FieldWithAnnotation of {annotation: string; annotation_values: string list option}

  type procedure_unit =
    { procedure_matcher: procedure_matcher
    ; arguments: Pulse_config_t.argument_constraint list
    ; kinds: Kind.t list
    ; procedure_target: Target.procedure_target }

  type field_unit =
    { field_matcher: field_matcher
    ; kinds: Kind.t list
    ; field_target: Target.field_target
    ; sanitized_in: string list option }

  type t = ProcedureUnit of procedure_unit | FieldUnit of field_unit
  [@@warning "-unused-type-declaration"]
end

module SinkPolicy : sig
  type t =
    { source_kinds: Kind.t list [@ignore]
    ; sanitizer_kinds: Kind.t list [@ignore]
    ; description: string [@ignore]
    ; policy_id: int
    ; privacy_effect: string option [@ignore]
    ; exclude_in: string list option [@ignore]
    ; exclude_matching: Str.regexp list option [@ignore]
    ; report_as_issue_type: string option [@ignore]
    ; report_as_category: string option [@ignore] }
  [@@deriving equal]

  val sink_policies : (Kind.t, t list) Base.Hashtbl.t
end

val allocation_sources : (string * Kind.t list) list

val source_procedure_matchers : Unit.procedure_unit list

val source_block_matchers : Unit.procedure_unit list

val source_field_getters_matchers : Unit.field_unit list

val source_field_setters_matchers : Unit.field_unit list [@@warning "-unused-value-declaration"]

val sink_procedure_matchers : Unit.procedure_unit list

val sink_field_getters_matchers : Unit.field_unit list

val sink_field_setters_matchers : Unit.field_unit list

val sanitizer_matchers : Unit.procedure_unit list

val propagator_matchers : Unit.procedure_unit list

val log_taint_config : unit -> unit [@@warning "-unused-value-declaration"]
