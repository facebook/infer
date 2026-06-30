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
  -> ?defaults:Textual.Exp.t IString.Map.t
  -> CongruenceClosureSolver.t
  -> Textual.ProcDesc.t
  -> (CongruenceClosureSolver.Atom.t * Equations.t * int, string) result
(** [convert_proc cc proc] converts a Textual procedure to a PEG via StructuredIR. Returns the root
    atom, equation trace, and number of loops. [defaults] maps parameter names to their default
    expression; such parameters are modelled as [@phi(@is_default(p), default, @arg(p))] to make the
    calling convention explicit (used for B006 migration checking). *)

val extract_defaults : Textual.Module.t -> Textual.ProcDesc.t -> Textual.Exp.t IString.Map.t
(** [extract_defaults module_ proc] recovers the default value expression of each parameter of
    [proc] that has one, by analysing the [py_make_function] call in the enclosing scope (the
    [__module_body__] for a top-level function, or the class body for a method). Positional defaults
    bind to the last parameters. Used for B006 migration checking. *)

val find_class_body_procs : Textual.Module.t -> Textual.QualifiedProcName.t list
(** [find_class_body_procs module_] returns the qualified names of the class body procedures, i.e.
    the [py_make_function] closure targets passed as the first argument of [py_build_class]. Like
    [__module_body__], these definition-site scopes are skipped during B006/B007 migration checking
    because a codemod legitimately perturbs the method defaults/annotations they bake in. *)

val pp_tree :
     ?depth:int
  -> CongruenceClosureSolver.t
  -> Format.formatter
  -> CongruenceClosureSolver.Atom.t
  -> unit
[@@warning "-unused-value-declaration"]
(** [pp_tree cc fmt atom] prints the PEG rooted at [atom] as an ASCII tree. Used only by unit tests.
*)
