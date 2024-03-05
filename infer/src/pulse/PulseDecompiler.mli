(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module Access = PulseAccess
module CallEvent = PulseCallEvent
module DecompilerExpr = PulseDecompilerExpr
module ValueHistory = PulseValueHistory

(** {1 Describe abstract values in terms of source code elements} *)

type key = AbstractValue.t

type t

val pp : F.formatter -> t -> unit

val empty : t

val invalid : t

val add_var_source : key -> Var.t -> t -> t

val add_block_source : key -> string -> t -> t

val add_call_source :
  key -> CallEvent.t -> ((AbstractValue.t * ValueHistory.t) * Typ.t) list -> t -> t

val add_access_source : key -> Access.t -> src:key -> t -> t

val find : AbstractValue.t -> t -> DecompilerExpr.t
