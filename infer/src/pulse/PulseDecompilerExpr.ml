(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module CallEvent = PulseCallEvent

type base = PVar of Pvar.t | Block of string | ReturnValue of CallEvent.t
[@@deriving compare, equal]

type access =
  | CaptureFieldAccess of string
  | FieldAccess of Fieldname.t
  | ArrayAccess of source_expr option
  | Dereference

(* TODO: could add more kinds of operations too to show "x + y" or "x + 4", or even "x +
   sizeof(struct x)". At the moment the memory model doesn't do anything useful with those though so
   there's no need to be that fancy. *)
and source_expr = base * access list [@@deriving compare, equal]

(** intermediate representation of [source_expr] used for pretty-printing only *)
type access_expr =
  | ProgramVar of Pvar.t
  | ProgramVarAddress of Pvar.t
  | ProgramBlock of string
  | Call of CallEvent.t
  | Capture of access_expr * string
  | Deref of access_expr
  | ArrowField of access_expr * Fieldname.t
  | DotField of access_expr * Fieldname.t
  | Array of access_expr * access_expr option
  | Parens of access_expr

let rec pp_access_expr fmt access_expr =
  let pp_field_acces_expr fmt access_expr sep field =
    let pp_access_expr fmt access_expr =
      match access_expr with
      | Capture (_, captured_var) ->
          F.fprintf fmt "%s" captured_var
      | _ ->
          pp_access_expr fmt access_expr
    in
    F.fprintf fmt "%a%s%a" pp_access_expr access_expr sep Fieldname.pp field
  in
  match access_expr with
  | ProgramVar pvar when Language.curr_language_is Language.Clang ->
      Pvar.pp_value_non_verbose fmt pvar
  | ProgramVar pvar ->
      Pvar.pp_value fmt pvar
  | ProgramVarAddress pvar ->
      Pvar.pp Pp.text fmt pvar
  | ProgramBlock block ->
      let block =
        String.chop_prefix ~prefix:Config.anonymous_block_prefix block
        |> Option.value ~default:block
      in
      F.fprintf fmt "block defined in %s" block
  | Call call ->
      let java_or_objc =
        match call with
        | Call procname | SkippedKnownCall procname -> (
            Procname.is_java procname || Procname.is_objc_method procname
            ||
            match Attributes.load procname with
            | Some {objc_accessor= Some (Objc_getter _)} ->
                true
            | _ ->
                false )
        | Model _ | SkippedUnknownCall _ ->
            false
      in
      if java_or_objc then CallEvent.pp_name_only fmt call
      else F.fprintf fmt "%a()" CallEvent.pp_name_only call
  | Capture (access_expr, captured_var) ->
      F.fprintf fmt "%s captured by %a" captured_var pp_access_expr access_expr
  | ArrowField (access_expr, field) ->
      pp_field_acces_expr fmt access_expr "->" field
  | DotField (access_expr, field) ->
      pp_field_acces_expr fmt access_expr "." field
  | Array (access_expr, index) ->
      let pp_index fmt index =
        match index with
        | None ->
            F.pp_print_char fmt '_'
        | Some index_expr ->
            pp_access_expr fmt index_expr
      in
      F.fprintf fmt "%a[%a]" pp_access_expr access_expr pp_index index
  | Deref access_expr ->
      F.fprintf fmt "*%a" pp_access_expr access_expr
  | Parens access_expr ->
      F.fprintf fmt "(%a)" pp_access_expr access_expr


let rec access_expr_of_source_expr (base, rev_accesses) =
  let accesses = List.rev rev_accesses in
  let base_expr, prev_is_deref, accesses =
    match (base, accesses) with
    | PVar pvar, [] ->
        (ProgramVarAddress pvar, false, [])
    | PVar pvar, Dereference :: accesses' ->
        (ProgramVar pvar, true, accesses')
    | PVar pvar, _ ->
        (ProgramVar pvar, false, accesses)
    | Block block, accesses ->
        (ProgramBlock block, false, accesses)
    | ReturnValue call, _ ->
        (Call call, false, accesses)
  in
  let deref_if b access_expr =
    if b then
      match access_expr with
      | ProgramVar _ | Deref _ | ArrowField _ | DotField _ ->
          (Deref access_expr, true)
      | ProgramVarAddress pvar ->
          (ProgramVar pvar, true)
      | ((ProgramBlock _ | Call _ | Capture _ | Array _) as access_expr) | Parens access_expr ->
          (Deref (Parens access_expr), true)
    else (access_expr, true)
  in
  let rec aux ~prev_is_deref ~prev_is_capture access_expr accesses =
    match (accesses, base) with
    | [], _ ->
        access_expr
    | Dereference :: accesses', _ ->
        let access_expr, prev_is_deref = deref_if prev_is_deref access_expr in
        aux ~prev_is_deref ~prev_is_capture:false access_expr accesses'
    | CaptureFieldAccess captured_var :: accesses', _ ->
        aux ~prev_is_deref:false ~prev_is_capture:true
          (Capture (access_expr, captured_var))
          accesses'
    | FieldAccess field :: accesses', _ ->
        let access_expr' =
          let needs_parens = match access_expr with Deref _ -> true | _ -> false in
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

type t = SourceExpr of source_expr * AbstractValue.t option | Unknown of AbstractValue.t option
[@@deriving compare, equal]

let abstract_value_of_expr = function Unknown v | SourceExpr (_, v) -> v

let pp fmt = function
  | Unknown _ ->
      F.fprintf fmt "UNKNOWN"
  | SourceExpr (source_expr, _) ->
      pp_source_expr fmt source_expr


let access_expr_exists ~f = function
  | Unknown _ ->
      false
  | SourceExpr (source_expr, _) ->
      f (access_expr_of_source_expr source_expr)


let includes_captured_variable = access_expr_exists ~f:(function Capture _ -> true | _ -> false)

let includes_block = access_expr_exists ~f:(function ProgramBlock _ -> true | _ -> false)

let pp_with_abstract_value fmt decompiled =
  F.fprintf fmt "%a:%a" (Pp.option AbstractValue.pp)
    (abstract_value_of_expr decompiled)
    pp decompiled


let yojson_of_t expr = `String (F.asprintf "%a" pp expr)

let is_unknown = function Unknown _ -> true | SourceExpr _ -> false

let reset_abstract_value expr =
  if Option.is_none (abstract_value_of_expr expr) then expr
  else
    match expr with
    | Unknown _ ->
        Unknown None
    | SourceExpr (source_expr, _) ->
        SourceExpr (source_expr, None)
