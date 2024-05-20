(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module AbstractValue = PulseAbstractValue
module Access = PulseAccess
module CallEvent = PulseCallEvent
module DecompilerExpr = PulseDecompilerExpr
module ValueHistory = PulseValueHistory

type key = AbstractValue.t

module Map : sig
  type t

  val empty : t

  val add : AbstractValue.t -> DecompilerExpr.source_expr -> t -> t

  val find : AbstractValue.t -> t -> DecompilerExpr.t

  val pp : F.formatter -> t -> unit
end = struct
  type t = DecompilerExpr.source_expr AbstractValue.Map.t

  let empty = AbstractValue.Map.empty

  let add = AbstractValue.Map.add

  let find v m =
    match AbstractValue.Map.find_opt v m with
    | None ->
        DecompilerExpr.Unknown (Some v)
    | Some source_expr ->
        DecompilerExpr.SourceExpr (source_expr, Some v)


  let pp fmt m = AbstractValue.Map.pp ~pp_value:DecompilerExpr.pp_source_expr fmt m
end

type t = Invalid  (** to store in summaries *) | Map of Map.t [@@deriving show {with_path= false}]

let empty = Map Map.empty

let invalid = Invalid

let assert_valid = function
  | Invalid ->
      L.die InternalError "Invalid decompiler found!"
  | Map m ->
      m


let map_valid decompiler f = Map (f (assert_valid decompiler))

let ( let+ ) = map_valid

let find v decompiler =
  let decompiler = assert_valid decompiler in
  Map.find v decompiler


let add_var_source v var decompiler =
  let+ decompiler in
  if Var.appears_in_source_code var then
    let[@warning "-partial-match"] (Var.ProgramVar pvar) = var in
    Map.add v (DecompilerExpr.PVar pvar, []) decompiler
  else decompiler


let add_block_source v block decompiler =
  let+ decompiler in
  Map.add v (DecompilerExpr.Block block, [Dereference]) decompiler


let access_of_field_access field =
  if Fieldname.is_capture_field_in_closure field then
    DecompilerExpr.CaptureFieldAccess (Fieldname.get_field_name field)
  else DecompilerExpr.FieldAccess field


let access_of_memory_access decompiler (access : Access.t) : DecompilerExpr.access =
  match access with
  | ArrayAccess (_, index) ->
      let index_expr =
        match Map.find index decompiler with Unknown _ -> None | SourceExpr (expr, _) -> Some expr
      in
      ArrayAccess index_expr
  | FieldAccess field ->
      access_of_field_access field
  | Dereference ->
      Dereference


let add_access_source v (access : Access.t) ~src decompiler =
  let+ decompiler in
  match Map.find src decompiler with
  | Unknown _ -> (
    match (Map.find v decompiler, access) with
    | SourceExpr ((base, [Dereference]), _), Dereference ->
        Map.add src (base, []) decompiler
    | _ ->
        decompiler )
  | SourceExpr ((base, accesses), _) ->
      Map.add v (base, access_of_memory_access decompiler access :: accesses) decompiler


let replace_getter_call_with_property_access procname v call actuals decompiler =
  if List.is_empty actuals then Map.add v (ReturnValue call, []) decompiler
  else
    let (fst, _), typ = List.hd_exn actuals in
    let typ_struct = match typ.Typ.desc with Typ.Tptr (typ, _) -> typ | _ -> typ in
    let typ_name = Option.value (Typ.name typ_struct) ~default:StdTyp.Name.Objc.ns_object in
    let procname_str = Procname.to_simplified_string procname in
    match Map.find fst decompiler with
    | Unknown _ ->
        Map.add v (ReturnValue call, []) decompiler
    | SourceExpr ((base, accesses), _) ->
        let accesses =
          match accesses with
          | Dereference :: accesses ->
              (* HACK: obj-c uses [.] for field accesses even though objects are pointers, so eg
                 [obj.f] really means [obj->f] in C but obj-c writes it [obj.f]. To get the same
                 behaviour, let's ignore the latest [Dereference] before a field access so it looks
                 like a (C) [.f] access to the decompiler instead of a [->f] one *)
              accesses
          | _ ->
              accesses
        in
        Map.add v
          ( base
          , access_of_memory_access decompiler (FieldAccess (Fieldname.make typ_name procname_str))
            :: accesses )
          decompiler


let add_call_source v (call : CallEvent.t) actuals decompiler =
  let+ decompiler in
  match call with
  | CallEvent.Call procname | SkippedKnownCall procname -> (
    match Attributes.load procname with
    | Some {objc_accessor= Some (Objc_getter _)} ->
        replace_getter_call_with_property_access procname v call actuals decompiler
    | _ ->
        Map.add v (ReturnValue call, []) decompiler )
  | Model _ | SkippedUnknownCall _ ->
      Map.add v (ReturnValue call, []) decompiler
