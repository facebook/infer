(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The analysis' semantics of control flow. *)

module Make (_ : Domain_intf.Opts) (Dom : Domain_intf.Dom) : sig
  val exec_pgm : Llair.program -> unit

  val compute_summaries :
    Llair.program -> Dom.summary list Llair.Function.Map.t
end
