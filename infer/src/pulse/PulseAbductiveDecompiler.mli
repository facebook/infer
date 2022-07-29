(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module AbstractValue = PulseAbstractValue
module CallEvent = PulseCallEvent
module Decompiler = PulseDecompiler
module ValueHistory = PulseValueHistory

(** {1 Wrapper to access the {!PulseDecompiler.t} inside {!AbductiveDomain.t}; all of the interface
    of {!PulseDecompiler} is duplicated for ease of use} *)

val add_call_source :
     AbstractValue.t
  -> CallEvent.t
  -> ((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> AbductiveDomain.t
  -> AbductiveDomain.t

type expr = Decompiler.expr [@@deriving compare, equal, yojson_of]

val pp_expr : Format.formatter -> expr -> unit

val pp_expr_with_abstract_value : Format.formatter -> expr -> unit

val find : AbstractValue.t -> AbductiveDomain.t -> expr

val abstract_value_of_expr : expr -> AbstractValue.t option

val reset_abstract_value : expr -> expr
