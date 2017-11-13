(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** Sanitizers for filtering traces in taint analysis *)
module type S = sig
  type t [@@deriving compare]

  val get : Typ.Procname.t -> t option
  (** Get the sanitizer that should be applied to the return value of given procedure, if any *)

  val pp : F.formatter -> t -> unit
end

module Dummy : S
