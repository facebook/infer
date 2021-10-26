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
  | ConditionPassed of
      {if_kind: Sil.if_kind; is_then_branch: bool; location: Location.t; timestamp: Timestamp.t}
  | CppTemporaryCreated of Location.t * Timestamp.t
  | FormalDeclared of Pvar.t * Location.t * Timestamp.t
  | Invalidated of PulseInvalidation.t * Location.t * Timestamp.t
  | NilMessaging of Location.t * Timestamp.t
  | Returned of Location.t * Timestamp.t
  | StructFieldAddressCreated of Fieldname.t RevList.t * Location.t * Timestamp.t
  | VariableAccessed of Pvar.t * Location.t * Timestamp.t
  | VariableDeclared of Pvar.t * Location.t * Timestamp.t

and t = Epoch | Sequence of event * t [@@deriving compare, equal]

let singleton event = Sequence (event, Epoch)

let location_of_event = function
  | Allocation {location}
  | Assignment (location, _)
  | Call {location}
  | Capture {location}
  | ConditionPassed {location}
  | CppTemporaryCreated (location, _)
  | FormalDeclared (_, location, _)
  | Invalidated (_, location, _)
  | NilMessaging (location, _)
  | Returned (location, _)
  | StructFieldAddressCreated (_, location, _)
  | VariableAccessed (_, location, _)
  | VariableDeclared (_, location, _) ->
      location


type iter_event =
  | EnterCall of CallEvent.t * Location.t
  | ReturnFromCall of CallEvent.t * Location.t
  | Event of event

let rec iter_event event ~f =
  ( match event with
  | Call {f= callee; location; in_call= Sequence _ as in_call} ->
      f (ReturnFromCall (callee, location)) ;
      iter in_call ~f ;
      f (EnterCall (callee, location)) ;
      ()
  | _ ->
      () ) ;
  f (Event event)


and iter (history : t) ~f =
  match history with
  | Epoch ->
      ()
  | Sequence (event, rest) ->
      iter_event event ~f ;
      iter rest ~f


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
  | Capture {captured_as; mode} ->
      F.fprintf fmt "value captured %s as `%a`"
        (CapturedVar.string_of_capture_mode mode)
        Pvar.pp_value_non_verbose captured_as
  | ConditionPassed {if_kind; is_then_branch} ->
      ( match (is_then_branch, if_kind) with
      | true, Ik_if _ ->
          "taking \"then\" branch"
      | false, Ik_if _ ->
          "taking \"else\" branch"
      | true, (Ik_for | Ik_while | Ik_dowhile) ->
          "loop condition is true; entering loop body"
      | false, (Ik_for | Ik_while | Ik_dowhile) ->
          "loop condition is false; leaving loop"
      | true, Ik_switch ->
          "switch condition is true, entering switch case"
      | false, Ik_switch ->
          "switch condition is false, skipping switch case"
      | true, Ik_compexch ->
          "pointer contains expected value; writing desired to pointer"
      | false, Ik_compexch ->
          "pointer does not contain expected value; writing to expected"
      | true, (Ik_bexp _ | Ik_land_lor) ->
          "condition is true"
      | false, (Ik_bexp _ | Ik_land_lor) ->
          "condition is false" )
      |> F.pp_print_string fmt
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
    | Epoch ->
        ()
    | Sequence ((Call {in_call} as event), tail) ->
        F.fprintf fmt "%a@;" pp_event event ;
        F.fprintf fmt "[%a]@;" pp_aux in_call ;
        pp_aux fmt tail
    | Sequence (event, tail) ->
        F.fprintf fmt "%a@;" pp_event event ;
        pp_aux fmt tail
  in
  F.fprintf fmt "@[%a@]" pp_aux history


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
  let nesting = ref nesting in
  let errlog = ref errlog in
  let one_iter_event = function
    | Event event ->
        errlog := add_event_to_errlog ~nesting:!nesting event !errlog
    | EnterCall _ ->
        decr nesting
    | ReturnFromCall (call, location) ->
        errlog := add_returned_from_call_to_errlog ~nesting:!nesting call location !errlog ;
        incr nesting
  in
  iter history ~f:one_iter_event ;
  !errlog


let get_first_event hist =
  Iter.head (Iter.rev (fun f -> iter hist ~f))
  |> Option.bind ~f:(function Event event -> Some event | _ -> None)
