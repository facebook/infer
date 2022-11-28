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
module BaseMemory = PulseBaseMemory
module BaseAddressAttributes = PulseBaseAddressAttributes
module CallEvent = PulseCallEvent
module DecompilerExpr = PulseDecompilerExpr
module ValueHistory = PulseValueHistory

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


let access_of_field_access src attrs field =
  let capture_field_access =
    let open IOption.Let_syntax in
    let+ captured_var =
      let* pos = Fieldname.get_capture_field_position field in
      let* attributes =
        let* procname = BaseAddressAttributes.get_closure_proc_name src attrs in
        Attributes.load procname
      in
      List.nth attributes.ProcAttributes.captured pos
    in
    DecompilerExpr.CaptureFieldAccess captured_var
  in
  IOption.if_none_eval capture_field_access ~f:(fun () -> DecompilerExpr.FieldAccess field)


let access_of_memory_access src attrs decompiler (access : BaseMemory.Access.t) :
    DecompilerExpr.access =
  match access with
  | ArrayAccess (_, index) ->
      let index_expr =
        match Map.find index decompiler with Unknown _ -> None | SourceExpr (expr, _) -> Some expr
      in
      ArrayAccess index_expr
  | FieldAccess field ->
      access_of_field_access src attrs field
  | TakeAddress ->
      TakeAddress
  | Dereference ->
      Dereference


let add_access_source ?(allow_cycle = false) v (access : BaseMemory.Access.t) ~src attrs decompiler
    =
  let contains_access v src =
    match (v, src) with
    | ( DecompilerExpr.SourceExpr ((PVar pvar_v, access_v), _)
      , DecompilerExpr.SourceExpr ((PVar pvar_src, access_src), _) )
      when Pvar.equal pvar_v pvar_src ->
        let rec is_sub_access sub access =
          match (sub, access) with
          | [], _ ->
              true
          | _, [] ->
              false
          | s :: sub, a :: access ->
              DecompilerExpr.equal_access s a && is_sub_access sub access
        in
        is_sub_access (List.rev access_v) (List.rev access_src)
    | _ ->
        false
  in
  let+ decompiler in
  match Map.find src decompiler with
  | Unknown _ ->
      decompiler
  | src when (not allow_cycle) && contains_access (Map.find v decompiler) src ->
      decompiler
  | SourceExpr ((base, accesses), _) ->
      Map.add v (base, access_of_memory_access src attrs decompiler access :: accesses) decompiler


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
        Map.add v
          ( base
          , access_of_memory_access (AbstractValue.mk_fresh ()) BaseAddressAttributes.empty
              decompiler
              (FieldAccess (Fieldname.make typ_name procname_str))
            :: TakeAddress :: Dereference :: accesses )
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
