(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

type t =
  { sum: float
  ; avg: float
  ; min: float
  ; p10: float
  ; median: float
  ; p75: float
  ; max: float
  ; count: int }

let to_json s =
  `Assoc
    [ ("sum", `Float s.sum)
    ; ("avg", `Float s.avg)
    ; ("min", `Float s.min)
    ; ("p10", `Float s.p10)
    ; ("median", `Float s.median)
    ; ("p75", `Float s.p75)
    ; ("max", `Float s.max)
    ; ("count", `Int s.count) ]

let from_json json =
  let open! Yojson.Basic.Util in
  { sum= json |> member "sum" |> to_float
  ; avg= json |> member "avg" |> to_float
  ; min= json |> member "min" |> to_float
  ; p10= json |> member "p10" |> to_float
  ; median= json |> member "median" |> to_float
  ; p75= json |> member "p75" |> to_float
  ; max= json |> member "max" |> to_float
  ; count= json |> member "count" |> to_int }

let compute_statistics values =
  let num_elements = List.length values in
  let sum = List.fold ~f:(fun acc v -> acc +. v) ~init:0.0 values in
  let average = sum /. float_of_int num_elements in
  let values_arr = Array.of_list values in
  Array.sort
    ~cmp:(fun a b -> if Float.equal a b then 0 else if a -. b < 0.0 then -1 else 1)
    values_arr ;
  let percentile pct =
    assert (pct >= 0.0 && pct <= 1.0) ;
    assert (num_elements > 0) ;
    let max_index = num_elements - 1 in
    let pct_index = float_of_int max_index *. pct in
    let low_index = int_of_float (Pervasives.floor pct_index) in
    let high_index = int_of_float (Pervasives.ceil pct_index) in
    let low = values_arr.(low_index) in
    let high = values_arr.(high_index) in
    (low +. high) /. 2.0
  in
  { sum
  ; avg= average
  ; min= percentile 0.0
  ; p10= percentile 0.10
  ; median= percentile 0.50
  ; p75= percentile 0.75
  ; max= percentile 1.0
  ; count= num_elements }
