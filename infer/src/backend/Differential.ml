(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = {introduced: Jsonbug_t.report; fixed: Jsonbug_t.report; preexisting: Jsonbug_t.report}

(** Set operations should keep duplicated issues with identical hashes *)
let of_reports ~(current_report: Jsonbug_t.report) ~(previous_report: Jsonbug_t.report) : t =
  let to_map report =
    List.fold_left
      ~f:(fun map issue -> Map.add_multi map ~key:issue.Jsonbug_t.hash ~data:issue)
      ~init:Int.Map.empty report
  in
  let fold_aux ~key:_ ~data (left, both, right) =
    match data with
    | `Left left'
     -> (List.rev_append left' left, both, right)
    | `Both (both', _)
     -> (left, List.rev_append both' both, right)
    | `Right right'
     -> (left, both, List.rev_append right' right)
  in
  let introduced, preexisting, fixed =
    Map.fold2 (to_map current_report) (to_map previous_report) ~f:fold_aux ~init:([], [], [])
  in
  {introduced; fixed; preexisting}

let to_files {introduced; fixed; preexisting} destdir =
  Out_channel.write_all (destdir ^/ "introduced.json")
    ~data:(Jsonbug_j.string_of_report introduced) ;
  Out_channel.write_all (destdir ^/ "fixed.json") ~data:(Jsonbug_j.string_of_report fixed) ;
  Out_channel.write_all (destdir ^/ "preexisting.json")
    ~data:(Jsonbug_j.string_of_report preexisting)
