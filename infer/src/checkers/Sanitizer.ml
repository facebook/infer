(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module type S = sig
  type t [@@deriving compare]

  val get : Typ.Procname.t -> Tenv.t -> t option

  val pp : F.formatter -> t -> unit
end

module Dummy = struct
  type t = unit [@@deriving compare]

  let get _ _ = None

  let pp _ _ = ()
end
