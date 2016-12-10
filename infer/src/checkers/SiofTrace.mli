(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module Global :
sig
  type t = Pvar.t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

include SinkTrace.S with module Sink.Kind = Global

val make_access : Global.t -> Location.t -> Sink.t

val is_intraprocedural_access : Sink.t -> bool
