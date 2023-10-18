(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Kind : sig
  type t [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val of_string : string -> t

  val mark_data_flow_only : t -> unit

  val is_data_flow_only : t -> bool

  val simple_kind : t
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
    | ProcedureNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | ClassNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | ClassAndMethodNames of {class_names: string list; method_names: string list}
    | ClassNameAndMethodRegex of
        {class_names: string list; method_name_regex: Str.regexp; exclude_in: string list option}
    | ClassAndMethodReturnTypeNames of
        {class_names: string list; method_return_type_names: string list}
    | ClassWithAnnotation of {annotation: string; annotation_values: string list option}
    | OverridesOfClassWithAnnotation of {annotation: string}
    | MethodWithAnnotation of {annotation: string; annotation_values: string list option}
    | Block of {name: string}
    | BlockNameRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | Allocation of {class_name: string}

  type field_matcher =
    | FieldRegex of {name_regex: Str.regexp; exclude_in: string list option}
    | ClassAndFieldNames of {class_names: string list; field_names: string list}
    | FieldWithAnnotation of {annotation: string; annotation_values: string list option}

  type procedure_unit =
    { procedure_matcher: procedure_matcher
    ; arguments: Pulse_config_t.argument_constraint list
    ; kinds: Kind.t list
    ; procedure_target: Target.procedure_target }

  val pp_procedure_unit : F.formatter -> procedure_unit -> unit

  type field_unit =
    { field_matcher: field_matcher
    ; kinds: Kind.t list
    ; field_target: Target.field_target
    ; sanitized_in: string list option }

  val pp_field_unit : F.formatter -> field_unit -> unit

  type t = ProcedureUnit of procedure_unit | FieldUnit of field_unit

  val of_config :
       default_taint_target:Pulse_config_t.taint_target
    -> option_name:string
    -> Pulse_config_t.matcher list
    -> t list
end

module SinkPolicy : sig
  type t =
    { source_kinds: Kind.t list [@ignore]
    ; sanitizer_kinds: Kind.t list [@ignore]
    ; description: string [@ignore]
    ; policy_id: int
    ; privacy_effect: string option [@ignore]
    ; exclude_in: string list option [@ignore] }
  [@@deriving equal]

  val next_policy_id : unit -> int

  val sink_policies : (Kind.t, t list) Base.Hashtbl.t

  val pp_sink_policies : F.formatter -> (Kind.t, t list) Base.Hashtbl.t -> unit
end
