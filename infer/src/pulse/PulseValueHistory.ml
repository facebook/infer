(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CallEvent = PulseCallEvent
module Invalidation = PulseInvalidation
module Timestamp = PulseTimestamp

type event =
  | Allocation of {f: CallEvent.t; location: Location.t; timestamp: Timestamp.t}
  | Assignment of Location.t * Timestamp.t
  | Call of {f: CallEvent.t; location: Location.t; in_call: t; timestamp: Timestamp.t}
  | Capture of
      { captured_as: Pvar.t
      ; mode: CapturedVar.capture_mode
      ; location: Location.t
      ; timestamp: Timestamp.t }
  | Conditional of
      {is_then_branch: bool; if_kind: Sil.if_kind; location: Location.t; timestamp: Timestamp.t}
  | CppTemporaryCreated of Location.t * Timestamp.t
  | FormalDeclared of Pvar.t * Location.t * Timestamp.t
  | Invalidated of PulseInvalidation.t * Location.t * Timestamp.t
  | NilMessaging of Location.t * Timestamp.t
  | Returned of Location.t * Timestamp.t
  | StructFieldAddressCreated of Fieldname.t RevList.t * Location.t * Timestamp.t
  | VariableAccessed of Pvar.t * Location.t * Timestamp.t
  | VariableDeclared of Pvar.t * Location.t * Timestamp.t

and t = event list [@@deriving compare, equal]

let location_of_event = function
  | Allocation {location}
  | Assignment (location, _)
  | Call {location}
  | Capture {location}
  | Conditional {location}
  | CppTemporaryCreated (location, _)
  | FormalDeclared (_, location, _)
  | Invalidated (_, location, _)
  | NilMessaging (location, _)
  | Returned (location, _)
  | StructFieldAddressCreated (_, location, _)
  | VariableAccessed (_, location, _)
  | VariableDeclared (_, location, _) ->
      location


let rec iter_event event ~f =
  f event ;
  match event with
  | Call {in_call} ->
      iter in_call ~f
  | Allocation _
  | Assignment _
  | Capture _
  | Conditional _
  | CppTemporaryCreated _
  | FormalDeclared _
  | Invalidated _
  | NilMessaging _
  | Returned _
  | StructFieldAddressCreated _
  | VariableAccessed _
  | VariableDeclared _ ->
      ()


and iter history ~f = List.iter history ~f:(fun event -> iter_event ~f event)

let yojson_of_event = [%yojson_of: _]

let yojson_of_t = [%yojson_of: _]

let pp_fields =
  let pp_sep fmt () = Format.pp_print_char fmt '.' in
  fun fmt fields -> Format.pp_print_list ~pp_sep Fieldname.pp fmt (RevList.to_list fields)


let pp_event_no_location fmt event =
  let pp_pvar fmt pvar =
    if Pvar.is_global pvar then F.fprintf fmt "global variable `%a`" Pvar.pp_value_non_verbose pvar
    else F.fprintf fmt "variable `%a`" Pvar.pp_value_non_verbose pvar
  in
  match event with
  | Allocation {f} ->
      F.fprintf fmt "allocated by call to %a" CallEvent.pp f
  | Assignment _ ->
      F.pp_print_string fmt "assigned"
  | Call {f} ->
      F.fprintf fmt "in call to %a" CallEvent.pp f
  | Capture {captured_as; mode; location= _} ->
      F.fprintf fmt "value captured %s as `%a`"
        (CapturedVar.string_of_capture_mode mode)
        Pvar.pp_value_non_verbose captured_as
  | Conditional {is_then_branch; if_kind; location= _} ->
      F.fprintf fmt "expression in %a condition is %b" Sil.pp_if_kind if_kind is_then_branch
  | CppTemporaryCreated _ ->
      F.pp_print_string fmt "C++ temporary created"
  | FormalDeclared (pvar, _, _) ->
      let pp_proc fmt pvar =
        Pvar.get_declaring_function pvar
        |> Option.iter ~f:(fun proc_name -> F.fprintf fmt " of %a" Procname.pp proc_name)
      in
      F.fprintf fmt "parameter `%a`%a" Pvar.pp_value_non_verbose pvar pp_proc pvar
  | Invalidated (invalidation, _, _) ->
      Invalidation.describe fmt invalidation
  | NilMessaging _ ->
      F.pp_print_string fmt "a message sent to nil returns nil"
  | Returned _ ->
      F.pp_print_string fmt "returned"
  | StructFieldAddressCreated (field_names, _, _) ->
      F.fprintf fmt "struct field address `%a` created" pp_fields field_names
  | VariableAccessed (pvar, _, _) ->
      F.fprintf fmt "%a accessed here" pp_pvar pvar
  | VariableDeclared (pvar, _, _) ->
      F.fprintf fmt "%a declared here" pp_pvar pvar


let pp_event fmt event =
  F.fprintf fmt "%a at %a" pp_event_no_location event Location.pp_line (location_of_event event)


let pp fmt history =
  let rec pp_aux fmt = function
    | [] ->
        ()
    | (Call {f; in_call} as event) :: tail ->
        F.fprintf fmt "%a@;" pp_event event ;
        F.fprintf fmt "[%a]@;" pp_aux (List.rev in_call) ;
        if not (List.is_empty in_call) then F.fprintf fmt "return from call to %a@;" CallEvent.pp f ;
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
        @@ ( if List.is_empty in_call then Fn.id
           else add_returned_from_call_to_errlog ~nesting f location )
        @@ errlog
    | event :: tail ->
        add_to_errlog_aux ~nesting tail @@ add_event_to_errlog ~nesting event @@ errlog
  in
  add_to_errlog_aux ~nesting history errlog
