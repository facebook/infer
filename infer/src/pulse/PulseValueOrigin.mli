(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractValue = PulseAbstractValue
module ValueHistory = PulseValueHistory
module Access = PulseAccess

(** Describes a (value, history) pair with path/origin when available.

    Useful when one needs to overwrite a history of a particular value in the abstract state. *)
type t =
  | InMemory of
      { src: AbstractValue.t * ValueHistory.t
      ; access: Access.t
      ; dest: AbstractValue.t * ValueHistory.t }
  | OnStack of {var: Var.t; addr_hist: AbstractValue.t * ValueHistory.t}
  | Unknown of (AbstractValue.t * ValueHistory.t)
      (** Values without a known origin such as those containing constant values. *)

val unknown : AbstractValue.t * ValueHistory.t -> t

val addr_hist : t -> AbstractValue.t * ValueHistory.t

val addr_hist_args :
     t ProcnameDispatcher.Call.FuncArg.t list
  -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list

val value : t -> AbstractValue.t
