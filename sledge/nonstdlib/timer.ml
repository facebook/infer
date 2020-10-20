(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Timers for runtime statistics *)

open! NS0

type t =
  { mutable ustart: float
  ; mutable sstart: float
  ; mutable uaggregate: float
  ; mutable saggregate: float
  ; mutable count: int
  ; mutable max: float
  ; mutable threshold: float
  ; name: string }

let enabled = ref false

let start t =
  if !enabled then (
    let {Unix.tms_utime; tms_stime} = Unix.times () in
    t.ustart <- tms_utime ;
    t.sstart <- tms_stime )

let stop_ t =
  let {Unix.tms_utime; tms_stime} = Unix.times () in
  let ud = tms_utime -. t.ustart in
  let sd = tms_stime -. t.sstart in
  t.uaggregate <- t.uaggregate +. ud ;
  t.saggregate <- t.saggregate +. sd ;
  let usd = ud +. sd in
  if Float.(t.max < usd) then t.max <- usd ;
  t.count <- t.count + 1 ;
  (tms_utime, tms_stime)

let stop t = if !enabled then stop_ t |> (ignore : float * float -> unit)

let stop_report t report =
  if !enabled then
    let tms_utime, tms_stime = stop_ t in
    let elapsed = tms_utime +. tms_stime -. (t.ustart +. t.sstart) in
    if Float.(elapsed > t.threshold) then (
      t.threshold <- elapsed ;
      report ~name:t.name ~elapsed:(elapsed *. 1000.)
        ~aggregate:((t.uaggregate +. t.saggregate) *. 1000.)
        ~count:t.count )

let create ?at_exit:printf name =
  let t =
    { ustart= 0.
    ; uaggregate= 0.
    ; sstart= 0.
    ; saggregate= 0.
    ; count= 0
    ; max= 0.
    ; threshold= 0.
    ; name }
  in
  Option.iter printf ~f:(fun report ->
      at_exit (fun () ->
          if !enabled then
            report ~name:t.name ~elapsed:(t.max *. 1000.)
              ~aggregate:((t.uaggregate +. t.saggregate) *. 1000.)
              ~count:t.count ) ) ;
  t
