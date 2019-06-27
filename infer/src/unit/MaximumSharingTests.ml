(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let inputs =
  let a = `A 'a' in
  let b = Array.create ~len:10_000 a in
  let c = Array.create ~len:100 b in
  [ ("unit", Obj.repr ())
  ; ("same representation", Obj.repr ([42], [|42; 0|]))
  ; ("10K times the same element", Obj.repr b)
  ; ("100 times 10K times the same element", Obj.repr c) ]


let tests =
  let normalize input =
    let sharer = MaximumSharing.Sharer.create () in
    MaximumSharing.Sharer.normalize_value sharer input
  in
  let test_one input _ =
    (* Save this now, in case `MaximumSharing` mutates the [input], even though it shouldn't *)
    let serialized_input_with_sharing = Marshal.to_string input [] in
    let serialized_input_no_sharing = Marshal.to_string input [Marshal.No_sharing] in
    let reachable_words_input = Obj.reachable_words input in
    let normalized = normalize input in
    (*
      We can't really check [input] hasn't been mutated but its marshalling with sharing
      shouldn't have changed
    *)
    let serialized_input_with_sharing' = Marshal.to_string input [] in
    assert_equal serialized_input_with_sharing serialized_input_with_sharing' ;
    (*
      The whole goal of [MaximumSharing] is to reduce the memory footprint.
      Let's make sure this contract is fulfilled.
      There is no guarantee the serialized version will be smaller, e.g.
      [let x = Some 0 in (x, Array.init 254 (fun i -> Some i), x)]
      is smaller with no sharing.
    *)
    let reachable_words_normalized = Obj.reachable_words normalized in
    assert_bool "less reachable words" (reachable_words_normalized <= reachable_words_input) ;
    let eq =
      (* Cannot use [assert_equal] because it doesn't shortcut physical equalities *)
      Polymorphic_compare.equal input normalized
    in
    assert_bool "equal" eq ;
    (*
      In case structural equality and marshalling have slightly different semantics,
      let's also make sure the serialized versions are indistinguishable
    *)
    let serialized_normalized_no_sharing = Marshal.to_string normalized [Marshal.No_sharing] in
    assert_equal serialized_input_no_sharing serialized_normalized_no_sharing
  in
  let tests_ = List.map inputs ~f:(fun (name, input) -> name >:: test_one input) in
  "MaximumSharing_tests" >::: tests_
