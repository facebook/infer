(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Sanitizers for filtering traces in taint analysis *)
module type S = sig
  type t [@@deriving compare]

  val get : Typ.Procname.t -> Tenv.t -> t option
  (** Get the sanitizer that should be applied to the return value of given procedure, if any *)

  val pp : F.formatter -> t -> unit
end

module Dummy : S
