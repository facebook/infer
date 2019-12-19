(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t [@@deriving compare]

val equal : t -> t -> bool

val pname : t -> Procname.t

val loc : t -> Location.t

val make : Procname.t -> Location.t -> t

val dummy : t

val pp : F.formatter -> t -> unit

module Set : PrettyPrintable.PPSet with type elt = t
