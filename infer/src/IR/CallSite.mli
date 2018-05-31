(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t [@@deriving compare]

val equal : t -> t -> bool

val pname : t -> Typ.Procname.t

val loc : t -> Location.t

val make : Typ.Procname.t -> Location.t -> t

val dummy : t

val pp : F.formatter -> t -> unit

module Set : PrettyPrintable.PPSet with type elt = t
