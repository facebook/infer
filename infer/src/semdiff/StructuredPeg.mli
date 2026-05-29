(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Equations : sig
  type t

  val pp : CongruenceClosureSolver.t -> Format.formatter -> t -> unit
end

val convert_proc :
     ?theta_counter:int ref
  -> CongruenceClosureSolver.t
  -> Textual.ProcDesc.t
  -> (CongruenceClosureSolver.Atom.t * Equations.t * int, string) result
(** [convert_proc cc proc] converts a Textual procedure to a PEG via StructuredIR. Returns the root
    atom, equation trace, and number of loops. *)
