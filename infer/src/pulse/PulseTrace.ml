(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type breadcrumb =
  | VariableDeclaration of Location.t
  | CppTemporaryCreated of Location.t
  | Assignment of {lhs: HilExp.AccessExpression.t; location: Location.t}
  | Capture of
      { captured_as: AccessPath.base
      ; captured: HilExp.AccessExpression.t
      ; location: Location.t }
  | Call of
      { f: [`HilCall of HilInstr.call | `Model of string]
      ; actuals: HilExp.t list
      ; location: Location.t }
[@@deriving compare]

let pp_breadcrumb_no_location fmt = function
  | VariableDeclaration _ ->
      F.pp_print_string fmt "variable declared"
  | CppTemporaryCreated _ ->
      F.pp_print_string fmt "C++ temporary created"
  | Capture {captured_as; captured; location= _} ->
      F.fprintf fmt "`%a` captured as `%a`" HilExp.AccessExpression.pp captured AccessPath.pp_base
        captured_as
  | Assignment {lhs; location= _} ->
      F.fprintf fmt "assigned to `%a`" HilExp.AccessExpression.pp lhs
  | Call {f; actuals; location= _} ->
      let pp_f fmt = function
        | `HilCall call ->
            F.fprintf fmt "%a" HilInstr.pp_call call
        | `Model model ->
            F.pp_print_string fmt model
      in
      F.fprintf fmt "returned from call to `%a(%a)`" pp_f f (Pp.seq ~sep:"," HilExp.pp) actuals


let location_of_breadcrumb = function
  | VariableDeclaration location
  | CppTemporaryCreated location
  | Assignment {location}
  | Capture {location}
  | Call {location} ->
      location


let pp_breadcrumb fmt crumb =
  F.fprintf fmt "%a at %a" pp_breadcrumb_no_location crumb Location.pp_line
    (location_of_breadcrumb crumb)


let errlog_trace_elem_of_breadcrumb ~nesting crumb =
  let location = location_of_breadcrumb crumb in
  let description = F.asprintf "%a" pp_breadcrumb_no_location crumb in
  let tags = [] in
  Errlog.make_trace_element nesting location description tags


type breadcrumbs = breadcrumb list [@@deriving compare]

let pp_breadcrumbs f breadcrumbs = Pp.seq ~print_env:Pp.text_break pp_breadcrumb f breadcrumbs

let add_errlog_of_breadcrumbs ~nesting breadcrumbs errlog =
  List.rev_map_append ~f:(errlog_trace_elem_of_breadcrumb ~nesting) breadcrumbs errlog


let start_location_of_breadcrumbs = function
  | [] ->
      None
  | crumb :: _ ->
      Some (location_of_breadcrumb crumb)


type 'a action =
  | Immediate of {imm: 'a; location: Location.t}
  | ViaCall of {action: 'a action; proc_name: Typ.Procname.t; location: Location.t}
[@@deriving compare]

let rec immediate_of_action = function
  | Immediate {imm; _} ->
      imm
  | ViaCall {action; _} ->
      immediate_of_action action


let pp_action pp_immediate fmt = function
  | Immediate {imm; _} ->
      pp_immediate fmt imm
  | ViaCall {proc_name; action; _} ->
      F.fprintf fmt "%a in call to `%a`" pp_immediate (immediate_of_action action)
        Typ.Procname.describe proc_name


let add_errlog_of_action ~nesting pp_immediate action errlog =
  let rec aux ~nesting rev_errlog action =
    match action with
    | Immediate {imm; location} ->
        let rev_errlog =
          Errlog.make_trace_element nesting location (F.asprintf "%a here" pp_immediate imm) []
          :: rev_errlog
        in
        List.rev_append rev_errlog errlog
    | ViaCall {action; proc_name; location} ->
        aux ~nesting:(nesting + 1)
          ( Errlog.make_trace_element nesting location
              (F.asprintf "when calling `%a` here" Typ.Procname.describe proc_name)
              []
          :: rev_errlog )
          action
  in
  aux ~nesting [] action


let outer_location_of_action = function Immediate {location} | ViaCall {location} -> location

type 'a t = {action: 'a action; breadcrumbs: breadcrumbs} [@@deriving compare]

let pp pp_immediate f {action; _} = pp_action pp_immediate f action

let add_errlog_header ~title location errlog =
  let depth = 0 in
  let tags = [] in
  Errlog.make_trace_element depth location title tags :: errlog


let add_to_errlog ~header pp_immediate trace errlog =
  let start_location =
    match start_location_of_breadcrumbs trace.breadcrumbs with
    | Some location ->
        location
    | None ->
        outer_location_of_action trace.action
  in
  add_errlog_header ~title:header start_location
  @@ add_errlog_of_breadcrumbs ~nesting:1 trace.breadcrumbs
  @@ add_errlog_of_action ~nesting:1 pp_immediate trace.action
  @@ errlog
