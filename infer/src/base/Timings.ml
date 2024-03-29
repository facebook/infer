(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = Tdigest.t Timeable.Map.t

let init = Timeable.mk_map_of_all ~init:(Tdigest.create ())

let merge timings1 timings2 =
  Timeable.Map.mapi
    (fun timeable t1 ->
      let t2 = Timeable.Map.find timeable timings2 in
      Tdigest.merge [t1; t2] )
    timings1


let percentiles =
  [0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 0.95; 0.96; 0.97; 0.98; 0.99; 0.999; 0.9999]


let add timeable time timings =
  let previous = Timeable.Map.find timeable timings in
  let updated = Tdigest.add ~data:time previous in
  Timeable.Map.add timeable updated timings


type serialized = string Timeable.Map.t

let serialize timings = Timeable.Map.map (fun t -> Tdigest.to_string t |> snd) timings

let deserialize timings = Timeable.Map.map Tdigest.of_string timings

let to_scuba timings =
  Timeable.Map.bindings timings
  |> List.concat_map ~f:(fun (timeable, digest) ->
         let _updated_digest, percentile_values = Tdigest.percentiles digest percentiles in
         List.map2_exn percentiles percentile_values ~f:(fun percentile pvalue_opt ->
             (* log [0s] if we don't have any measurement, i.e. the timeable didn't run *)
             let pvalue = Option.value ~default:0.0 pvalue_opt in
             let label =
               F.sprintf "backend_stats.analysis_%s_p%g" (Timeable.to_string timeable)
                 (100. *. percentile)
             in
             let duration_us = 1000_000. *. pvalue |> Float.to_int in
             LogEntry.mk_time ~label ~duration_us ) )
