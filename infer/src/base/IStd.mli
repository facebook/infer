(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core.Std

(* Core's Gc and Signal modules wrap handler functions in catch-all exception handlers that
   exit. This defeats the timeout mechanisms. So deprecate the offending functions to cause a
   compilation failure if they are used. *)

module Gc : sig
  module Expert : sig
    module Alarm : sig
      include module type of Gc.Expert.Alarm
      val create : (unit -> unit) -> t
      [@@deprecated "Adds a catch-all exception handler that exits, use [Caml.Gc.create_alarm]"]
    end
    include module type of Gc.Expert with module Alarm := Alarm
    val add_finalizer : 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit
    [@@deprecated "Adds a catch-all exception handler that exits, use [Caml.Gc.finalise]"]
    val add_finalizer_exn : 'a -> ('a -> unit) -> unit
    [@@deprecated "Adds a catch-all exception handler that exits, use [Caml.Gc.finalise]"]
  end
  include (module type of Gc with module Expert := Expert)
end

module Signal : sig
  module Expert : sig
    include module type of Signal.Expert
    val signal : Signal.t -> behavior -> behavior
    [@@deprecated "Adds a catch-all exception handler that exits, use [Caml.Sys.signal]"]
    val set : Signal.t -> behavior -> unit
    [@@deprecated "Adds a catch-all exception handler that exits, use [Caml.Sys.set_signal]"]
    val handle : Signal.t -> (Signal.t -> unit) -> unit
    [@@deprecated "Adds a catch-all exception handler that exits, use [Caml.Sys.set_signal]"]
  end
  include (module type of Signal with module Expert := Expert)
end

include
  (module type of Core.Std
    with module Gc := Gc
    and module Signal := Signal)

module IntSet : Caml.Set.S with type elt = Int.t

(* Compare police: generic compare mostly disabled. *)
val compare : No_polymorphic_compare.compare
val equal : No_polymorphic_compare.compare

val failwithf : ('a, Format.formatter, unit, 'b) format4 -> 'a

val invalid_argf : ('a, Format.formatter, unit, 'b) format4 -> 'a
