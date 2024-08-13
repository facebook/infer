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
module ValueHistory = PulseValueHistory

(** Describes a (value, history) pair with path/origin when available.

    Useful when one needs to overwrite a history of a particular value in the abstract state. *)
type 'value t_ =
  | InMemory of
      {src: 'value * ValueHistory.t; access: 'value Access.access; dest: 'value * ValueHistory.t}
  | OnStack of {var: Var.t; addr_hist: 'value * ValueHistory.t}
  | Unknown of 'value * ValueHistory.t
      (** Values without a known origin such as those containing constant values. *)

type t = AbstractValue.t t_ [@@deriving compare, equal, yojson_of]

val pp : F.formatter -> t -> unit

val unknown : 'value * ValueHistory.t -> 'value t_

val with_value : AbstractValue.t -> t -> t

val with_hist : ValueHistory.t -> t -> t

val map_value : 'value t_ -> f:('value -> 'value') -> 'value' t_

val addr_hist : 'value t_ -> 'value * ValueHistory.t

val addr_hist_args :
     t ProcnameDispatcher.Call.FuncArg.t list
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list

val value : 'value t_ -> 'value

val hist : _ t_ -> ValueHistory.t
