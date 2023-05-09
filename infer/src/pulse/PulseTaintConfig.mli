(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Kind : sig
  type t = string [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  val of_string : string -> t

  val mark_data_flow_only : t -> unit

  val is_data_flow_only : t -> bool

  val simple_kind : t

  val kinds_of_strings_opt : t list option -> t list
end

module Target : sig
  type t =
    | ReturnValue
    | AllArguments
    | ArgumentPositions of int list
    | AllArgumentsButPositions of int list
    | ArgumentsMatchingTypes of string list
    | Fields of (string * t) list

  val target_of_gen_target : Pulse_config_t.taint_target -> t
end

module Unit : sig
  type procedure_matcher =
    | ProcedureName of {name: string}
    | ProcedureNameRegex of {name_regex: Str.regexp}
    | ClassNameRegex of {name_regex: Str.regexp}
    | ClassAndMethodNames of {class_names: string list; method_names: string list}
    | ClassAndMethodReturnTypeNames of
        {class_names: string list; method_return_type_names: string list}
    | OverridesOfClassWithAnnotation of {annotation: string}
    | MethodWithAnnotation of {annotation: string}
    | Block of {name: string}
    | BlockNameRegex of {name_regex: Str.regexp}
    | Allocation of {class_name: string}

  type t =
    { procedure_matcher: procedure_matcher
    ; arguments: Pulse_config_t.argument_constraint list
    ; kinds: Kind.t list
    ; target: Target.t }

  val pp : F.formatter -> t -> unit
end

module SinkPolicy : sig
  type t =
    { source_kinds: Kind.t list [@ignore]
    ; sanitizer_kinds: Kind.t list [@ignore]
    ; description: string [@ignore]
    ; policy_id: int
    ; privacy_effect: string option [@ignore] }
  [@@deriving equal]

  val next_policy_id : unit -> int

  val sink_policies : (Kind.t, t list) Base.Hashtbl.t

  val pp_sink_policies : F.formatter -> (Kind.t, t list) Base.Hashtbl.t -> unit
end
