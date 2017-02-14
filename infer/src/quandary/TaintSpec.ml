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
  | Propagate_to_return
  | Propagate_to_receiver

module type S = sig
  module Trace : Trace.S
  module AccessTree : module type of AccessTree.Make(Trace)

  (** return a summary for handling an unknown call at the given site with the given return type
      and actuals *)
  val handle_unknown_call :
    Procname.t -> Typ.t option -> (Exp.t * Typ.t) list -> Tenv.t -> handle_unknown list

  val to_summary_access_tree : AccessTree.t -> QuandarySummary.AccessTree.t

  val of_summary_access_tree : QuandarySummary.AccessTree.t -> AccessTree.t
end
