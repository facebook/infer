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
      F.fprintf fmt "variable declared"
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
  | VariableDeclaration location | Assignment {location} | Capture {location} | Call {location} ->
      location


let pp_breadcrumb fmt crumb =
  F.fprintf fmt "%a at %a" pp_breadcrumb_no_location crumb Location.pp_line
    (location_of_breadcrumb crumb)


let errlog_trace_elem_of_breadcrumb ~depth crumb =
  let location = location_of_breadcrumb crumb in
  let description = F.asprintf "%a" pp_breadcrumb_no_location crumb in
  let tags = [] in
  Errlog.make_trace_element depth location description tags


type t = breadcrumb list [@@deriving compare]

let pp f trace = Pp.seq ~print_env:Pp.text_break pp_breadcrumb f trace

let make_errlog_trace ~depth trace = List.rev_map ~f:(errlog_trace_elem_of_breadcrumb ~depth) trace

let pp_last_event f = function
  | [] ->
      ()
  | crumb :: _ ->
      pp_breadcrumb f crumb ;
      (* HACK: needed by the call site *) F.pp_print_string f " "


let pp_interesting_events f trace = pp_last_event f trace
