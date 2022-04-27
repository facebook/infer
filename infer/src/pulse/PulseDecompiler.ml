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
module ValueHistory = PulseValueHistory

type base = PVar of Pvar.t | ReturnValue of CallEvent.t [@@deriving compare, equal]

type access =
  | CaptureFieldAccess of CapturedVar.t
  | FieldAccess of Fieldname.t
  | ArrayAccess of source_expr option
  | TakeAddress
  | Dereference

(* TODO: could add more kinds of operations too to show "x + y" or "x + 4", or even "x +
   sizeof(struct x)". At the moment the memory model doesn't do anything useful with those though so
   there's no need to be that fancy. *)
and source_expr = base * access list [@@deriving compare, equal]

(** intermediate representation of [source_expr] used for pretty-printing only *)
type access_expr =
  | ProgramVar of Pvar.t
  | Call of CallEvent.t
  | Capture of access_expr * CapturedVar.t
  | Deref of access_expr
  | ArrowField of access_expr * Fieldname.t
  | DotField of access_expr * Fieldname.t
  | Array of access_expr * access_expr option
  | AddressOf of access_expr
  | Parens of access_expr

let rec pp_access_expr fmt = function
  | ProgramVar pvar ->
      Pvar.pp_value fmt pvar
  | Call call ->
      let java_or_objc_getter =
        match call with
        | Call procname | SkippedKnownCall procname -> (
            Procname.is_java procname
            ||
            match Attributes.load procname with
            | Some {objc_accessor= Some (Objc_getter _)} ->
                true
            | _ ->
                false )
        | Model _ | SkippedUnknownCall _ ->
            false
      in
      if java_or_objc_getter then CallEvent.pp_name_only fmt call
      else F.fprintf fmt "%a()" CallEvent.pp_name_only call
  | Capture (access_expr, captured_var) ->
      F.fprintf fmt "%a capturing %a" pp_access_expr access_expr Pvar.pp_value
        captured_var.CapturedVar.pvar
  | ArrowField (access_expr, field) ->
      F.fprintf fmt "%a->%a" pp_access_expr access_expr Fieldname.pp field
  | DotField (access_expr, field) ->
      F.fprintf fmt "%a.%a" pp_access_expr access_expr Fieldname.pp field
  | Array (access_expr, index) ->
      let pp_index fmt index =
        match index with
        | None ->
            F.pp_print_char fmt '_'
        | Some index_expr ->
            pp_access_expr fmt index_expr
      in
      F.fprintf fmt "%a[%a]" pp_access_expr access_expr pp_index index
  | AddressOf access_expr ->
      F.fprintf fmt "&%a" pp_access_expr access_expr
  | Deref access_expr ->
      F.fprintf fmt "*%a" pp_access_expr access_expr
  | Parens access_expr ->
      F.fprintf fmt "(%a)" pp_access_expr access_expr


let rec access_expr_of_source_expr (base, rev_accesses) =
  let accesses = List.rev rev_accesses in
  let base_expr, prev_is_deref, accesses =
    match (base, accesses) with
    | PVar pvar, [] ->
        (AddressOf (ProgramVar pvar), false, [])
    | PVar pvar, Dereference :: accesses' ->
        (ProgramVar pvar, true, accesses')
    | PVar pvar, _ ->
        (ProgramVar pvar, false, accesses)
    | ReturnValue call, _ ->
        (Call call, false, accesses)
  in
  let deref_if b access_expr =
    if b then
      match access_expr with
      | ProgramVar _ | Deref _ | ArrowField _ | DotField _ ->
          Deref access_expr
      | _ ->
          Deref (Parens access_expr)
    else access_expr
  in
  let rec aux ~prev_is_deref ~prev_is_capture access_expr accesses =
    match (accesses, base) with
    | [], _ ->
        access_expr
    | Dereference :: TakeAddress :: accesses', _ ->
        aux ~prev_is_deref:false ~prev_is_capture:false access_expr accesses'
    | Dereference :: accesses', _ ->
        aux ~prev_is_deref:true ~prev_is_capture:false
          (deref_if prev_is_deref access_expr)
          accesses'
    | TakeAddress :: accesses', _ ->
        aux ~prev_is_deref:false ~prev_is_capture:false (AddressOf access_expr) accesses'
    | CaptureFieldAccess captured_var :: accesses', _ ->
        aux ~prev_is_deref:false ~prev_is_capture:true
          (Capture (access_expr, captured_var))
          accesses'
    | FieldAccess field :: accesses', _ ->
        let access_expr' =
          let needs_parens = match access_expr with Deref _ | AddressOf _ -> true | _ -> false in
          let access_expr = if needs_parens then Parens access_expr else access_expr in
          if prev_is_deref || prev_is_capture then ArrowField (access_expr, field)
          else DotField (access_expr, field)
        in
        aux ~prev_is_deref:false ~prev_is_capture:false access_expr' accesses'
    | ArrayAccess index :: accesses', _ ->
        aux ~prev_is_deref:false ~prev_is_capture:false
          (Array (access_expr, Option.map index ~f:access_expr_of_source_expr))
          accesses'
  in
  aux ~prev_is_deref ~prev_is_capture:false base_expr accesses


let pp_source_expr fmt source_expr = pp_access_expr fmt (access_expr_of_source_expr source_expr)

type decompiled = SourceExpr of source_expr * AbstractValue.t | Unknown of AbstractValue.t
[@@deriving compare, equal]

let pp_decompiled_aux fmt = function
  | Unknown _ ->
      F.fprintf fmt "UNKNOWN"
  | SourceExpr (source_expr, _) ->
      pp_source_expr fmt source_expr


module Map : sig
  type t

  val empty : t

  val add : AbstractValue.t -> source_expr -> t -> t

  val find : AbstractValue.t -> t -> decompiled

  val pp : F.formatter -> t -> unit
end = struct
  type t = source_expr AbstractValue.Map.t

  let empty = AbstractValue.Map.empty

  let add = AbstractValue.Map.add

  let find v m =
    match AbstractValue.Map.find_opt v m with
    | None ->
        Unknown v
    | Some source_expr ->
        SourceExpr (source_expr, v)


  let pp fmt m = AbstractValue.Map.pp ~pp_value:pp_source_expr fmt m
end

type t = Invalid  (** to store in summaries *) | Map of Map.t

let pp fmt = function Invalid -> F.fprintf fmt "Invalid" | Map m -> Map.pp fmt m

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
  let+ decompiler = decompiler in
  if Var.appears_in_source_code var then
    let[@warning "-8"] (Var.ProgramVar pvar) = var in
    Map.add v (PVar pvar, []) decompiler
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
    CaptureFieldAccess captured_var
  in
  IOption.if_none_eval capture_field_access ~f:(fun () -> FieldAccess field)


let access_of_memory_access src attrs decompiler (access : BaseMemory.Access.t) : access =
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


let add_access_source v (access : BaseMemory.Access.t) ~src attrs decompiler =
  let+ decompiler in
  match Map.find src decompiler with
  | Unknown _ ->
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
  let+ decompiler = decompiler in
  match call with
  | CallEvent.Call procname | SkippedKnownCall procname -> (
    match Attributes.load procname with
    | Some {objc_accessor= Some (Objc_getter _)} ->
        replace_getter_call_with_property_access procname v call actuals decompiler
    | _ ->
        Map.add v (ReturnValue call, []) decompiler )
  | Model _ | SkippedUnknownCall _ ->
      Map.add v (ReturnValue call, []) decompiler


type expr = decompiled [@@deriving compare, equal]

let pp_expr fmt decompiled = pp_decompiled_aux fmt decompiled

let yojson_of_expr expr = `String (F.asprintf "%a" pp_expr expr)

let abstract_value_of_expr = function Unknown v | SourceExpr (_, v) -> v

let is_unknown = function Unknown _ -> true | SourceExpr _ -> false
