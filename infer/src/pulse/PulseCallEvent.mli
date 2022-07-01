(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | Call of Procname.t  (** known function with summary *)
  | Model of string  (** hardcoded model *)
  | SkippedKnownCall of Procname.t  (** known function without summary *)
  | SkippedUnknownCall of Exp.t  (** couldn't link the expression to a proc name *)
[@@deriving compare, equal]

val pp : F.formatter -> t -> unit

val describe : F.formatter -> t -> unit

val pp_name_only : F.formatter -> t -> unit
