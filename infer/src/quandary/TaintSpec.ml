(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** combination of a trace with functions for handling unknown code and converting to and from
    summaries *)

type handle_unknown =
  | Propagate_to_actual of int
  (** Propagate taint from all actuals to the actual with the given index *)
  | Propagate_to_receiver
  (** Propagate taint from all non-receiver actuals to the receiver actual *)
  | Propagate_to_return
  (** Propagate taint from all actuals to the return value *)


module type S = sig
  module Trace : Trace.S
  module AccessTree : module type of AccessTree.Make(Trace)

  (** return a summary for handling an unknown call at the given site with the given return type
      and actuals *)
  val handle_unknown_call :
    Typ.Procname.t -> Typ.t option -> HilExp.t list -> Tenv.t -> handle_unknown list

  (** return true if the given typ can be tainted *)
  val is_taintable_type : Typ.t -> bool

  val to_summary_access_tree : AccessTree.t -> QuandarySummary.AccessTree.t

  val of_summary_access_tree : QuandarySummary.AccessTree.t -> AccessTree.t
end
