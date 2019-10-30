(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent

type event =
  | Assignment of Location.t
  | Call of {f: CallEvent.t; location: Location.t; in_call: t}
  | Capture of {captured_as: Pvar.t; location: Location.t}
  | Conditional of {is_then_branch: bool; if_kind: Sil.if_kind; location: Location.t}
  | CppTemporaryCreated of Location.t
  | FormalDeclared of Pvar.t * Location.t
  | VariableAccessed of Pvar.t * Location.t
  | VariableDeclared of Pvar.t * Location.t

and t = event list [@@deriving compare]

let pp_event_no_location fmt event =
  let pp_pvar fmt pvar =
    if Pvar.is_global pvar then F.fprintf fmt "global variable `%a`" Pvar.pp_value_non_verbose pvar
    else F.fprintf fmt "variable `%a`" Pvar.pp_value_non_verbose pvar
  in
  match event with
  | Assignment _ ->
      F.pp_print_string fmt "assigned"
  | Call {f; location= _} ->
      F.fprintf fmt "passed as argument to %a" CallEvent.pp f
  | Capture {captured_as; location= _} ->
      F.fprintf fmt "value captured as `%a`" Pvar.pp_value_non_verbose captured_as
  | Conditional {is_then_branch; if_kind; location= _} ->
      F.fprintf fmt "expression in %s condition is %b" (Sil.if_kind_to_string if_kind)
        is_then_branch
  | CppTemporaryCreated _ ->
      F.pp_print_string fmt "C++ temporary created"
  | FormalDeclared (pvar, _) ->
      let pp_proc fmt pvar =
        Pvar.get_declaring_function pvar
        |> Option.iter ~f:(fun proc_name -> F.fprintf fmt " of %a" Typ.Procname.pp proc_name)
      in
      F.fprintf fmt "parameter `%a`%a" Pvar.pp_value_non_verbose pvar pp_proc pvar
  | VariableAccessed (pvar, _) ->
      F.fprintf fmt "%a accessed here" pp_pvar pvar
  | VariableDeclared (pvar, _) ->
      F.fprintf fmt "%a declared here" pp_pvar pvar


let location_of_event = function
  | Assignment location
  | Call {location}
  | Capture {location}
  | Conditional {location}
  | CppTemporaryCreated location
  | FormalDeclared (_, location)
  | VariableAccessed (_, location)
  | VariableDeclared (_, location) ->
      location


let pp_event fmt event =
  F.fprintf fmt "%a at %a" pp_event_no_location event Location.pp_line (location_of_event event)


let pp fmt history =
  let rec pp_aux fmt = function
    | [] ->
        ()
    | (Call {f; in_call} as event) :: tail ->
        F.fprintf fmt "%a@;" pp_event event ;
        F.fprintf fmt "[%a]@;" pp_aux (List.rev in_call) ;
        if not (List.is_empty tail) then F.fprintf fmt "return from call to %a@;" CallEvent.pp f ;
        pp_aux fmt tail
    | event :: tail ->
        F.fprintf fmt "%a@;" pp_event event ;
        pp_aux fmt tail
  in
  F.fprintf fmt "@[%a@]" pp_aux (List.rev history)


let add_event_to_errlog ~nesting event errlog =
  let location = location_of_event event in
  let description = F.asprintf "%a" pp_event_no_location event in
  let tags = [] in
  Errlog.make_trace_element nesting location description tags :: errlog


let add_returned_from_call_to_errlog ~nesting f location errlog =
  let description = F.asprintf "return from call to %a" CallEvent.pp f in
  let tags = [] in
  Errlog.make_trace_element nesting location description tags :: errlog


let add_to_errlog ~nesting history errlog =
  let rec add_to_errlog_aux ~nesting history errlog =
    match history with
    | [] ->
        errlog
    | (Call {f; location; in_call} as event) :: tail ->
        add_to_errlog_aux ~nesting tail
        @@ add_event_to_errlog ~nesting event
        @@ add_to_errlog_aux ~nesting:(nesting + 1) in_call
        @@ add_returned_from_call_to_errlog ~nesting f location
        @@ errlog
    | event :: tail ->
        add_to_errlog_aux ~nesting tail @@ add_event_to_errlog ~nesting event @@ errlog
  in
  add_to_errlog_aux ~nesting history errlog
