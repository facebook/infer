(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Timers for runtime statistics *)

type t = private
  { mutable ustart: float
  ; mutable sstart: float
  ; mutable uaggregate: float
  ; mutable saggregate: float
  ; mutable count: int
  ; mutable max: float
  ; mutable threshold: float
  ; name: string }

val create :
     ?at_exit:
       (   name:string
        -> elapsed:float
        -> aggregate:float
        -> count:int
        -> unit)
  -> string
  -> t
(** Construct a timer with the given name and register the given function to
    run at exit. The [at_exit] function receives [name]: the name of the
    timer passed to [create], [elapsed]: the number of milliseconds between
    the longest single [start]-[stop] pair, [aggregate]: the sum of the time
    that elapsed while the named timer was running, [count]: the number of
    times [stop] was called on the timer. *)

val start : t -> unit
(** Start a timer. *)

val stop : t -> unit
(** Stop a timer. *)

val stop_report :
     t
  -> (name:string -> elapsed:float -> aggregate:float -> count:int -> unit)
  -> unit
(** Stop a timer and report using the given function, which receives [name]:
    the name of the timer passed to [create], [elapsed]: the number of
    milliseconds since [start] was called, [aggregate]: the sum of the time
    that has elapsed while the timer was running, [count]: the number of
    times [stop] has been called on the timer. *)

val enabled : bool ref
(** Timers do nothing unless [enabled] is set. *)
