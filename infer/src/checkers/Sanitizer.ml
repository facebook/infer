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

module type S = sig
  type t [@@deriving compare]

  val get : Typ.Procname.t -> t option

  val pp : F.formatter -> t -> unit
end

module Dummy = struct
  type t = unit [@@deriving compare]

  let get _ = None

  let pp _ _ = ()
end
