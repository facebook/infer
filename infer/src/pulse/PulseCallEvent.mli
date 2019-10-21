(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | Call of Typ.Procname.t  (** known function with summary *)
  | Model of string  (** hardcoded model *)
  | SkippedKnownCall of Typ.Procname.t  (** known function without summary *)
  | SkippedUnknownCall of Exp.t  (** couldn't link the expression to a proc name *)
[@@deriving compare]

val pp : F.formatter -> t -> unit

val describe : F.formatter -> t -> unit
