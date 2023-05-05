(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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
  type t =
    | ReturnValue
    | AllArguments
    | ArgumentPositions of int list
    | AllArgumentsButPositions of int list
    | ArgumentsMatchingTypes of string list
    | Fields of (string * t) list

  let rec target_of_gen_target (gen_target : Pulse_config_t.taint_target) =
    match gen_target with
    | `ReturnValue ->
        ReturnValue
    | `AllArguments ->
        AllArguments
    | `ArgumentPositions l ->
        ArgumentPositions l
    | `AllArgumentsButPositions l ->
        AllArgumentsButPositions l
    | `ArgumentsMatchingTypes l ->
        ArgumentsMatchingTypes l
    | `Fields l ->
        Fields (List.map ~f:(fun (field, target) -> (field, target_of_gen_target target)) l)
end

module Unit = struct
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
    ; target: Target.t
    ; block_passed_to: procedure_matcher }
end

module SinkPolicy = struct
  type t =
    { source_kinds: Kind.t list [@ignore]
    ; sanitizer_kinds: Kind.t list [@ignore]
    ; description: string [@ignore]
    ; policy_id: int
    ; privacy_effect: string option [@ignore] }
  [@@deriving equal]

  let next_policy_id =
    let policy_id_counter = ref 0 in
    fun () ->
      incr policy_id_counter ;
      !policy_id_counter


  let sink_policies : (Kind.t, t list) Base.Hashtbl.t = Hashtbl.create (module Kind)
end
