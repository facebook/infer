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
module TaintItem = PulseTaintItem
module Timestamp = PulseTimestamp

module CellId = struct
  type t = int [@@deriving compare, equal, yojson_of]

  let pp = Int.pp

  let next_id = ref 0

  let () = AnalysisGlobalState.register_ref ~init:(fun () -> 0) next_id

  let next () =
    let id = !next_id in
    incr next_id ;
    id


  module Map = IntMap
  module Set = IntSet
end

type event =
  | Allocation of {f: CallEvent.t; location: Location.t; timestamp: Timestamp.t}
  | Assignment of Location.t * Timestamp.t
  | Call of {f: CallEvent.t; location: Location.t; in_call: t [@ignore]; timestamp: Timestamp.t}
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
  | TaintSource of TaintItem.t * Location.t * Timestamp.t
  | TaintPropagated of Location.t * Timestamp.t
  | VariableAccessed of Pvar.t * Location.t * Timestamp.t
  | VariableDeclared of Pvar.t * Location.t * Timestamp.t

(** INVARIANT: [InContext] always comes first in a given history, if any, then [FromCellIds], if
    any. This is for efficient querying of context and cell ids. *)
and t =
  | Epoch
  | Sequence of event * t
  | InContext of {main: t; context: t list}
  | BinaryOp of Binop.t * t * t
  | FromCellIds of CellId.Set.t * t
  | Multiplex of t list
  | UnknownCall of {f: CallEvent.t; actuals: t list; location: Location.t; timestamp: Timestamp.t}
[@@deriving compare, equal]

module EventSet = Caml.Set.Make (struct
  type t = event [@@deriving compare]
end)

let get_cell_ids = function
  (* thanks to INVARIANT these are the only cases we need to check *)
  | FromCellIds (ids, _) | InContext {main= FromCellIds (ids, _)} ->
      Some ids
  | _ ->
      None


let get_cell_id_exn hist =
  let open Option.Monad_infix in
  get_cell_ids hist >>| CellId.Set.min_elt


let pop_context_and_cell_ids hist =
  let context, hist =
    match hist with InContext {main; context} -> (context, main) | hist -> ([], hist)
  in
  let ids, hist =
    match hist with FromCellIds (ids, hist) -> (ids, hist) | hist -> (CellId.Set.empty, hist)
  in
  (context, ids, hist)


let context_quick_rev_append x ~into =
  List.fold x ~init:into ~f:(fun acc context ->
      if List.mem acc context ~equal:phys_equal then acc else context :: acc )


let pop_all_context_and_cell_ids hists =
  List.fold_map hists ~init:([], CellId.Set.empty) ~f:(fun (context, ids) hist ->
      let context', ids', hist' = pop_context_and_cell_ids hist in
      ((context_quick_rev_append context' ~into:context, CellId.Set.union ids ids'), hist') )


let epoch = Epoch

let in_context new_context main =
  if List.is_empty new_context then main
  else
    match main with
    | InContext {context} when phys_equal context new_context ->
        main
    | InContext {main; context} ->
        InContext {main; context= context_quick_rev_append new_context ~into:context}
    | Epoch | Sequence _ | BinaryOp _ | FromCellIds _ | Multiplex _ | UnknownCall _ ->
        InContext {main; context= new_context}


(* CAUTION: The [hist] argument must not include [InContext] or [FromCellIds]. *)
let construct context ids hist =
  let hist = if CellId.Set.is_empty ids then hist else FromCellIds (ids, hist) in
  if List.is_empty context then hist else InContext {main= hist; context}


let sequence ?(context = []) event hist =
  let context', ids, hist = pop_context_and_cell_ids hist in
  let context = context_quick_rev_append context ~into:context' in
  construct context ids (Sequence (event, hist))


let binary_op bop hist1 hist2 =
  let context1, ids1, hist1 = pop_context_and_cell_ids hist1 in
  let context2, ids2, hist2 = pop_context_and_cell_ids hist2 in
  let context = context_quick_rev_append context2 ~into:context1 in
  let ids = CellId.Set.union ids1 ids2 in
  construct context ids (BinaryOp (bop, hist1, hist2))


let from_cell_id id hist =
  let context, ids, hist = pop_context_and_cell_ids hist in
  construct context (CellId.Set.add id ids) hist


let multiplex hists =
  let (context, ids), hists = pop_all_context_and_cell_ids hists in
  construct context ids (Multiplex hists)


let unknown_call f actuals location timestamp =
  let (context, ids), actuals = pop_all_context_and_cell_ids actuals in
  construct context ids (UnknownCall {f; actuals; location; timestamp})


let singleton event = Sequence (event, Epoch)

let of_cell_ids_in_map hist_map ids =
  let hists =
    CellId.Set.fold
      (fun id hists -> (CellId.Map.find_opt id hist_map |> Option.to_list) @ hists)
      ids []
  in
  match hists with [] -> None | [hist] -> Some hist | _ :: _ :: _ -> Some (multiplex hists)


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
  | TaintSource (_, location, _)
  | TaintPropagated (location, _)
  | VariableAccessed (_, location, _)
  | VariableDeclared (_, location, _) ->
      location


let timestamp_of_event = function
  | Allocation {timestamp}
  | Assignment (_, timestamp)
  | Call {timestamp}
  | Capture {timestamp}
  | ConditionPassed {timestamp}
  | CppTemporaryCreated (_, timestamp)
  | FormalDeclared (_, _, timestamp)
  | Invalidated (_, _, timestamp)
  | NilMessaging (_, timestamp)
  | Returned (_, timestamp)
  | StructFieldAddressCreated (_, _, timestamp)
  | TaintSource (_, _, timestamp)
  | TaintPropagated (_, timestamp)
  | VariableAccessed (_, _, timestamp)
  | VariableDeclared (_, _, timestamp) ->
      timestamp


let pop_least_timestamp ~main_only hists0 =
  let rec aux orig_hists_prefix curr_ts_hists_prefix latest_events (highest_t : Timestamp.t option)
      hists =
    match (highest_t, hists) with
    | _, [] ->
        (latest_events, curr_ts_hists_prefix)
    | _, Epoch :: hists ->
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t hists
    | _, FromCellIds (_, hist) :: hists ->
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t (hist :: hists)
    | _, InContext {main} :: hists when main_only ->
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t (main :: hists)
    | _, InContext {main; context} :: hists ->
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t
          (main :: List.rev_append context hists)
    | _, BinaryOp (_, hist1, hist2) :: hists ->
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t (hist1 :: hist2 :: hists)
    | _, Multiplex hists' :: hists ->
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t (hists' @ hists)
    | _, UnknownCall {f; location; timestamp} :: hists ->
        (* cheat a bit: transform an [UnknownCall] history into a singleton [Call] history *)
        aux orig_hists_prefix curr_ts_hists_prefix latest_events highest_t
          (Sequence (Call {f; location; timestamp; in_call= Epoch}, Epoch) :: hists)
    | None, (Sequence (event, hist') as hist) :: hists ->
        aux (hist :: orig_hists_prefix) (hist' :: orig_hists_prefix) [event]
          (Some (timestamp_of_event event))
          hists
    | Some highest_ts, (Sequence (event, hist') as hist) :: hists
      when (timestamp_of_event event :> int) > (highest_ts :> int) ->
        aux (hist :: orig_hists_prefix) (hist' :: orig_hists_prefix) [event]
          (Some (timestamp_of_event event))
          hists
    | Some highest_ts, (Sequence (event, _) as hist) :: hists
      when (timestamp_of_event event :> int) < (highest_ts :> int) ->
        aux (hist :: orig_hists_prefix) (hist :: curr_ts_hists_prefix) latest_events highest_t hists
    | Some _, (Sequence (event, hist') as hist) :: hists
    (* when  timestamp_of_event _event = highest_ts *) ->
        aux (hist :: orig_hists_prefix) (hist' :: curr_ts_hists_prefix) (event :: latest_events)
          highest_t hists
  in
  aux [] [] [] None hists0


type iter_event =
  | EnterCall of CallEvent.t * Location.t
  | ReturnFromCall of CallEvent.t * Location.t
  | Event of event

let rec rev_iter_branches ~main_only hists ~f =
  if List.is_empty hists then ()
  else
    let latest_events, hists = pop_least_timestamp ~main_only hists in
    rev_iter_simultaneous_events ~main_only latest_events ~f ;
    rev_iter_branches ~main_only hists ~f


and rev_iter_simultaneous_events ~main_only events ~f =
  let is_nonempty = function
    | Epoch | FromCellIds (_, Epoch) ->
        false
    | Sequence _ | InContext _ | BinaryOp _ | FromCellIds _ | Multiplex _ | UnknownCall _ ->
        true
  in
  let in_calls =
    let in_call = function Call {in_call} when is_nonempty in_call -> Some in_call | _ -> None in
    List.filter_map events ~f:in_call
  in
  (* is there any call event in the list? *)
  ( match
      List.find_map events ~f:(function
        | Call {f= callee; location; in_call} when is_nonempty in_call ->
            Some (callee, location)
        | _ ->
            None )
    with
  | Some (callee, location) ->
      (* if there are call events in the list, iterate on their inner histories first; we stored
         those in [in_calls] just above and it will be non-empty in this match branch by
         construction *)
      f (ReturnFromCall (callee, location)) ;
      rev_iter_branches ~main_only in_calls ~f ;
      f (EnterCall (callee, location)) ;
      ()
  | _ ->
      () ) ;
  (* iterate over each unique event; events have been "reversed" (i.e. put in the correct order,
     which is the wrong one here since we do a [rev_iter_...]) so reverse them once more using
     [fold_left] *)
  List.fold_left events ~init:EventSet.empty ~f:(fun seen event ->
      if EventSet.mem event seen then seen
      else (
        f (Event event) ;
        EventSet.add event seen ) )
  |> ignore


and rev_iter ~main_only (history : t) ~f =
  match history with
  | Epoch ->
      ()
  | Sequence (event, rest) ->
      rev_iter_simultaneous_events ~main_only [event] ~f ;
      rev_iter ~main_only rest ~f
  | FromCellIds (_, hist) ->
      rev_iter ~main_only hist ~f
  | InContext {main} when main_only ->
      rev_iter ~main_only main ~f
  | InContext {main; context} ->
      (* [not main_only] *)
      rev_iter_branches ~main_only (main :: context) ~f
  | BinaryOp (_, hist1, hist2) ->
      rev_iter_branches ~main_only [hist1; hist2] ~f
  | Multiplex hists ->
      rev_iter_branches ~main_only hists ~f
  | UnknownCall {f= f_; location; timestamp} ->
      rev_iter_simultaneous_events ~main_only [Call {f= f_; location; timestamp; in_call= Epoch}] ~f


let rev_iter_main = rev_iter ~main_only:true

let iter ~main_only history ~f = Iter.rev (Iter.from_labelled_iter (rev_iter ~main_only history)) f

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
  | TaintSource (taint_source, _, _) ->
      F.fprintf fmt "source of the taint here: %a" TaintItem.pp taint_source
  | TaintPropagated _ ->
      F.fprintf fmt "taint propagated"
  | VariableAccessed (pvar, _, _) ->
      F.fprintf fmt "%a accessed here" pp_pvar pvar
  | VariableDeclared (pvar, _, _) ->
      F.fprintf fmt "%a declared here" pp_pvar pvar


let pp_event fmt event =
  F.fprintf fmt "%a at %a :t%a" pp_event_no_location event Location.pp_line
    (location_of_event event) Timestamp.pp (timestamp_of_event event)


let pp fmt history =
  let rec pp_aux ~is_first fmt hist =
    (match hist with Epoch -> () | _ when not is_first -> F.fprintf fmt "@,::" | _ -> ()) ;
    match hist with
    | Epoch ->
        ()
    | FromCellIds (ids, hist) ->
        F.fprintf fmt "@[<h>from_cell_ids%a@]" CellId.Set.pp ids ;
        pp_aux ~is_first:false fmt hist
    | Sequence ((Call {in_call= sub_hist} as event), tail) ->
        F.pp_open_box fmt 0 ;
        pp_event fmt event ;
        ( match sub_hist with
        | Epoch ->
            F.pp_print_string fmt "[]"
        | _ ->
            F.fprintf fmt "[@\n  @[%a@]@\n]" (pp_aux ~is_first:true) sub_hist ) ;
        F.pp_close_box fmt () ;
        pp_aux ~is_first:false fmt tail
    | Sequence (event, tail) ->
        pp_event fmt event ;
        pp_aux ~is_first:false fmt tail
    | InContext {main; context} ->
        F.fprintf fmt "(@[%a@]){@[%a@]}"
          (Pp.seq ~sep:"; " (pp_aux ~is_first:true))
          context (pp_aux ~is_first:true) main
    | BinaryOp (bop, hist1, hist2) ->
        F.fprintf fmt "[@[%a@]] %a [@[%a@]]" (pp_aux ~is_first:true) hist1 Binop.pp bop
          (pp_aux ~is_first:true) hist2
    | Multiplex hists ->
        F.fprintf fmt "{@[<v>%a@]}" (Pp.seq ~sep:"||" (pp_aux ~is_first:true)) hists
    | UnknownCall {f} ->
        F.fprintf fmt "in unknown call to %a" CallEvent.pp f
  in
  F.pp_open_box fmt 0 ;
  pp_aux ~is_first:true fmt history ;
  F.pp_close_box fmt ()


let add_event_to_errlog ~nesting event errlog =
  let location = location_of_event event in
  let description = F.asprintf "%a" pp_event_no_location event in
  let tags = [] in
  Errlog.make_trace_element nesting location description tags :: errlog


let add_returned_from_call_to_errlog ~nesting f location errlog =
  let description = F.asprintf "return from call to %a" CallEvent.pp f in
  let tags = [] in
  Errlog.make_trace_element nesting location description tags :: errlog


let is_taint_event = function
  | Allocation _
  | Assignment _
  | Call _
  | Capture _
  | ConditionPassed _
  | CppTemporaryCreated _
  | FormalDeclared _
  | Invalidated _
  | NilMessaging _
  | Returned _
  | StructFieldAddressCreated _
  | VariableAccessed _
  | VariableDeclared _ ->
      false
  | TaintSource _ | TaintPropagated _ ->
      true


let add_to_errlog ?(include_taint_events = false) ~nesting history errlog =
  let nesting = ref nesting in
  let errlog = ref errlog in
  let one_iter_event = function
    | Event event ->
        if include_taint_events || not (is_taint_event event) then
          errlog := add_event_to_errlog ~nesting:!nesting event !errlog
    | EnterCall _ ->
        decr nesting
    | ReturnFromCall (call, location) ->
        errlog := add_returned_from_call_to_errlog ~nesting:!nesting call location !errlog ;
        incr nesting
  in
  rev_iter ~main_only:false history ~f:one_iter_event ;
  !errlog


let get_first_main_event hist =
  Iter.head (Iter.from_labelled_iter (iter ~main_only:true hist))
  |> Option.bind ~f:(function Event event -> Some event | _ -> None)


let exists_main t ~f =
  Container.exists ~iter:rev_iter_main t ~f:(function Event event -> f event | _ -> false)
