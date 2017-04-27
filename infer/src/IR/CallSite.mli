(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
