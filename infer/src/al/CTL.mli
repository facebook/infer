(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** This module defines a language to define checkers. These checkers are interpreted over the AST
    of the program. A checker is defined by a CTL formula which expresses a condition saying when
    the checker should report a problem. *)

(** "set" clauses are used for defining mandatory variables that will be used by when reporting
    issues: eg for defining the condition.

    "desc" clauses are used for defining the error message, the suggestion, the severity.

    "let" clauses are used to define temporary formulas which are then used to abbreviate the
    another formula. For example

    {v
    let f = a And B

    set formula = f OR f

    set message = "bla"
    v} *)
type clause =
  | CLet of ALVar.formula_id * ALVar.t list * CTLTypes.t  (** Let clause: let id = definifion; *)
  | CSet of ALVar.keyword * CTLTypes.t  (** Set clause: set id = definition *)
  | CDesc of ALVar.keyword * string  (** Description clause eg: set message = "..." *)
  | CPath of [`WhitelistPath | `BlacklistPath] * ALVar.t list

type ctl_checker =
  {id: string  (** Checker's id *); definitions: clause list  (** A list of let/set definitions *)}

type al_file =
  { import_files: string list
  ; global_macros: clause list
  ; global_paths: (string * ALVar.alexp list) list
  ; checkers: ctl_checker list }

val print_checker : ctl_checker -> unit

val eval_formula :
     ?keep_witness:bool
  -> CTLTypes.t
  -> Ctl_parser_types.ast_node
  -> CLintersContext.context
  -> Ctl_parser_types.ast_node option
(** return the evaluation of the formula and a witness *)

val save_dotty_when_in_debug_mode : SourceFile.t -> unit

val next_state_via_transition :
  Ctl_parser_types.ast_node -> CTLTypes.transitions -> Ctl_parser_types.ast_node list

val create_ctl_evaluation_tracker : SourceFile.t -> unit
