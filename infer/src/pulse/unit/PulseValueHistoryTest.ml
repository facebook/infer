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
        F.printf "Enter %a()@,[@[<hv>" CallEvent.pp_name_only call ;
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
  (ValueHistory.iter ~main_only:false history) ~f:(fun event ->
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
