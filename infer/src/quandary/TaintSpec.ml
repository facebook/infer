(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** combination of a trace with functions for handling unknown code and converting to and from
    summaries *)

type action =
  | Propagate_to_actual of int
      (** Propagate taint from all actuals to the actual with the given index *)
  | Propagate_to_receiver
      (** Propagate taint from all non-receiver actuals to the receiver actual *)
  | Propagate_to_return  (** Propagate taint from all actuals to the return value *)

module type S = sig
  module Trace : Trace.S

  module AccessTree : module type of AccessTree.Make (Trace) (AccessTree.DefaultConfig)

  val handle_unknown_call : Typ.Procname.t -> Typ.t -> HilExp.t list -> Tenv.t -> action list
  (** return a summary for handling an unknown call at the given site with the given return type
      and actuals *)

  val get_model :
    Typ.Procname.t -> Typ.t -> HilExp.t list -> Tenv.t -> AccessTree.t -> action list option
  (** returns a model that should be used for the given (procname, return type, actuals, summary)
      instead of using the summary for the procname *)

  val is_taintable_type : Typ.t -> bool
  (** return true if the given typ can be tainted *)

  val to_summary_access_tree : AccessTree.t -> QuandarySummary.AccessTree.t

  val of_summary_access_tree : QuandarySummary.AccessTree.t -> AccessTree.t

  val name : string
end
