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

  val pp_thetas : CongruenceClosureSolver.t -> Format.formatter -> t -> unit
end

val convert_proc :
     ?theta_counter:int ref
  -> CongruenceClosureSolver.t
  -> Textual.ProcDesc.t
  -> (CongruenceClosureSolver.Atom.t * Equations.t * int, string) result
(** [convert_proc cc proc] converts a Textual procedure to a PEG via StructuredIR. Returns the root
    atom, equation trace, and number of loops. *)

val extract_defaults : Textual.Module.t -> Textual.ProcDesc.t -> Textual.Exp.t IString.Map.t
(** [extract_defaults module_ proc] recovers the default value expression of each parameter of
    [proc] that has one, by analysing the [py_make_function] call in the enclosing
    [__module_body__]. Positional defaults bind to the last parameters. Used for B006 migration
    checking. *)

val pp_tree :
     ?depth:int
  -> CongruenceClosureSolver.t
  -> Format.formatter
  -> CongruenceClosureSolver.Atom.t
  -> unit
[@@warning "-unused-value-declaration"]
(** [pp_tree cc fmt atom] prints the PEG rooted at [atom] as an ASCII tree. Used only by unit tests.
*)
