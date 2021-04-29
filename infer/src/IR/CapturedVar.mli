(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type capture_mode = ByReference | ByValue [@@deriving compare, equal]

val string_of_capture_mode : capture_mode -> string

type t = {name: Mangled.t; typ: Typ.t; capture_mode: capture_mode} [@@deriving compare]

val pp : Format.formatter -> t -> unit

val make : name:Mangled.t -> typ:Typ.t -> capture_mode:capture_mode -> t
