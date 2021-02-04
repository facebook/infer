(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = {name: Mangled.t; typ: Typ.t; capture_mode: Pvar.capture_mode} [@@deriving compare]

val pp : Format.formatter -> t -> unit

val make : name:Mangled.t -> typ:Typ.t -> capture_mode:Pvar.capture_mode -> t
