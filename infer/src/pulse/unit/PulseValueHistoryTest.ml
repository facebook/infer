(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CallEvent = PulseCallEvent
module Timestamp = PulseTimestamp
module ValueHistory = PulseValueHistory

let timestamp_of_int n =
  (* we could expose a magic function in PulseTimestamp to upcast [int -> Timestamp.t] but let's be
     really lazy and not do it and instead count in unary to the given int *)
  let rec aux n ts = if n <= 0 then ts else aux (n - 1) (Timestamp.incr ts) in
  aux n Timestamp.t0


let ev t =
  (* [Assignment] is just an event that is easy to construct *)
  ValueHistory.Assignment (Location.dummy, timestamp_of_int t)


let call t f in_call =
  ValueHistory.Call {f; in_call; timestamp= timestamp_of_int t; location= Location.dummy}


let ( ^:: ) event history = ValueHistory.sequence event history

let ( ^::^ ) event1 event2 = event1 ^:: ValueHistory.singleton event2

let ( ++ ) history1 history2 = ValueHistory.binary_op (PlusA None) history1 history2

let iter_print_history history =
  let call_stack = ref [] in
  let is_first = ref true in
  let print_time event =
    match (event : ValueHistory.iter_event) with
    | EnterCall (call, _) ->
        call_stack := call :: !call_stack ;
        F.printf "Enter %a()@,[@[<hv>" (CallEvent.pp_name_only ~with_class:true) call ;
        is_first := true ;
        ()
    | ReturnFromCall (call, _) ->
        let[@warning "-partial-match"] (prev_call :: prev_stack) = !call_stack in
        assert (CallEvent.equal call prev_call) ;
        call_stack := prev_stack ;
        F.printf "@]]@;Return from f"
    | Event event ->
        F.printf "ev%a" Timestamp.pp (ValueHistory.timestamp_of_event event)
  in
  F.printf "@[<hv>" ;
  (ValueHistory.iter history) ~f:(fun event ->
      if not !is_first then F.printf ";@," ;
      is_first := false ;
      print_time event ) ;
  F.printf "@]"


let%test_module "iteration order" =
  ( module struct
    let f = CallEvent.Model "f"

    let%expect_test "two events" =
      ev 1 ^::^ ev 0 |> iter_print_history ;
      [%expect {|ev0;ev1|}]


    let%expect_test "function call" =
      ev 2 ^:: call 1 f (ev 1 ^::^ ev 0) ^::^ ev 0 |> iter_print_history ;
      [%expect {|ev0;ev1;Enter f()[ev0;ev1;] Return from f;ev2|}]


    let%expect_test "two branches" =
      (ev 4 ^::^ ev 2) ++ (ev 3 ^::^ ev 1) |> iter_print_history ;
      [%expect {|ev1;ev2;ev3;ev4|}]


    let%expect_test "four branches" =
      (ev 10 ^:: ev 1 ^::^ ev 0)
      ++ (ev 8 ^:: ev 6 ^::^ ev 4)
      ++ (ev 9 ^:: ev 7 ^::^ ev 3)
      ++ (ev 8 ^:: ev 5 ^:: ev 2 ^::^ ev 0)
      |> iter_print_history ;
      [%expect {|ev0;ev1;ev2;ev3;ev4;ev5;ev6;ev7;ev8;ev9;ev10|}]
  end )


(* Coverage for [ValueHistory.redact_compiler_generated_locations] and
   [PulseTrace.redact_compiler_generated_locations]: replace every [Location.t] whose [file] is
   a [SourceFile.is_compiler_generated] sentinel with the closest enclosing non-sentinel location,
   with the report's [fallback] used only at the outermost level. User-code locations are
   preserved verbatim. *)
let%test_module "redact_compiler_generated_locations" =
  ( module struct
    module Trace = PulseTrace

    let synth_file = SourceFile.compiler_generated ~bitcode_id:"bc"

    let user_file = SourceFile.invalid "u.swift"

    let mk_loc file line = {Location.dummy with file; line}

    let synth_loc line = mk_loc synth_file line

    let user_loc line = mk_loc user_file line

    let fallback = user_loc 100

    let ev_at loc t = ValueHistory.Assignment (loc, timestamp_of_int t)

    let history_loc h =
      (* helper: peek the head event's location (only used in tests below where there's one event) *)
      match (h : ValueHistory.t) with
      | Sequence (event, _) ->
          ValueHistory.location_of_event event
      | _ ->
          Location.dummy


    let f = CallEvent.Model "f"

    let%test "history: synthetic top-level event redacts to fallback" =
      let h = ValueHistory.singleton (ev_at (synth_loc 1) 0) in
      let h' = ValueHistory.redact_compiler_generated_locations ~fallback h in
      Location.equal (history_loc h') fallback


    let%test "history: user top-level event is preserved verbatim" =
      let loc = user_loc 5 in
      let h = ValueHistory.singleton (ev_at loc 0) in
      let h' = ValueHistory.redact_compiler_generated_locations ~fallback h in
      Location.equal (history_loc h') loc


    let%test "history: synthetic event inside Call.in_call inherits the user-code Call site" =
      let inner_synth = ValueHistory.singleton (ev_at (synth_loc 1) 0) in
      let call_loc = user_loc 7 in
      let call_ev =
        ValueHistory.Call
          {f; location= call_loc; in_call= inner_synth; timestamp= timestamp_of_int 1}
      in
      let h = ValueHistory.singleton call_ev in
      let h' = ValueHistory.redact_compiler_generated_locations ~fallback h in
      match (h' : ValueHistory.t) with
      | Sequence (Call {in_call; location}, _) ->
          (* the Call's own location stays user, the inner synthetic event inherits it as parent *)
          Location.equal location call_loc && Location.equal (history_loc in_call) call_loc
      | _ ->
          false


    let%test "history: synthetic Call with synthetic body inherits fallback at the outermost level"
        =
      let inner_synth = ValueHistory.singleton (ev_at (synth_loc 1) 0) in
      let call_ev =
        ValueHistory.Call
          {f; location= synth_loc 2; in_call= inner_synth; timestamp= timestamp_of_int 1}
      in
      let h = ValueHistory.singleton call_ev in
      let h' = ValueHistory.redact_compiler_generated_locations ~fallback h in
      match (h' : ValueHistory.t) with
      | Sequence (Call {in_call; location}, _) ->
          Location.equal location fallback && Location.equal (history_loc in_call) fallback
      | _ ->
          false


    let%test "trace: ViaCall with user location + synthetic Immediate inside, inner uses parent" =
      let inner_loc = synth_loc 1 in
      let outer_loc = user_loc 8 in
      let inner = Trace.Immediate {location= inner_loc; history= ValueHistory.epoch} in
      let trace =
        Trace.ViaCall {f; location= outer_loc; history= ValueHistory.epoch; in_call= inner}
      in
      let trace' = Trace.redact_compiler_generated_locations ~fallback trace in
      match trace' with
      | ViaCall {location; in_call= Immediate {location= inner_loc'; _}; _} ->
          Location.equal location outer_loc && Location.equal inner_loc' outer_loc
      | _ ->
          false


    let%test "trace: synthetic ViaCall location falls back, propagating to inner Immediate too" =
      let inner = Trace.Immediate {location= synth_loc 1; history= ValueHistory.epoch} in
      let trace =
        Trace.ViaCall {f; location= synth_loc 2; history= ValueHistory.epoch; in_call= inner}
      in
      let trace' = Trace.redact_compiler_generated_locations ~fallback trace in
      match trace' with
      | ViaCall {location; in_call= Immediate {location= inner_loc'; _}; _} ->
          Location.equal location fallback && Location.equal inner_loc' fallback
      | _ ->
          false


    let%test "trace: Immediate user location preserved verbatim" =
      let loc = user_loc 9 in
      let trace = Trace.Immediate {location= loc; history= ValueHistory.epoch} in
      let trace' = Trace.redact_compiler_generated_locations ~fallback trace in
      match trace' with Immediate {location; _} -> Location.equal location loc | _ -> false
  end )
