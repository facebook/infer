(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type source = ReturnValue of Procname.t [@@deriving compare, equal]

val pp_source : F.formatter -> source -> unit

type sink = PassedAsArgumentTo of Procname.t [@@deriving compare, equal]

val pp_sink : F.formatter -> sink -> unit

type sanitizer = SanitizedBy of Procname.t [@@deriving compare, equal]

val pp_sanitizer : F.formatter -> sanitizer -> unit
