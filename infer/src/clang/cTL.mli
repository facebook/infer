(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Ctl_parser_types

(* This module defines a language to define checkers. These checkers
   are intepreted over the AST of the program. A checker is defined by a
   CTL formula which express a condition saying when the checker should
    report a problem *)

(* Transition labels used for example to switch from decl to stmt *)
type transitions =
  | Body (* decl to stmt *)
  | InitExpr (* decl to stmt *)
  | Super (* decl to decl *)
  | Cond
  | PointerToDecl (* stmt to decl *)

(* In formulas below prefix
   "E" means "exists a path"
   "A" means "for all path" *)

(** A ctl formula *)
type t =
  | True
  | False
  | Atomic of CPredicates.t (** Atomic formula *)
  | Not of t
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | InNode of ALVar.alexp list * t
  | AX of t (** AX phi <=> for all children of the current node phi holds *)
  | EX of transitions option * t (** EX phi <=> exist a child of the current node such that phi holds *)
  | AF of t (** AF phi <=> for all path from the current node there is a descendant where phi holds *)
  | EF of transitions option * t (** EF phi <=> there exits a a path from the current node with a descendant where phi hold *)
  | AG of t (** AG phi <=> for all discendant of the current node phi hold *)
  | EG of transitions option * t (** EG phi <=>
                                     there exists a path (of descendants) from the current node where phi hold at each node of the path *)
  | AU of t * t (** AU(phi1, phi2) <=>
                    for all paths from the current node phi1 holds in every node until ph2 holds *)
  | EU of transitions option * t * t (** EU(phi1, phi2) <=>
                                         there exists a path from the current node such that phi1 holds until phi2 holds *)
  | EH of ALVar.alexp list * t (** EH[classes]phi <=>
                                   there exists a node defining a super class in the hierarchy of the class
                                   defined by the current node (if any) where  phi holds *)
  | ET of ALVar.alexp list * transitions option * t (** ET[T][l] phi <=>
                                                               there exists a descentant an of the current node such that an is of type in set T
                                                               making a transition to a node an' via label l, such that in an phi holds. *)
  | ETX of ALVar.alexp list * transitions option * t (** ET[T][l] phi <=>
                                                               there exists a descentant an of the current node such that an is of type in set T
                                                               making a transition to a node an' via label l, such that in an phi holds. *)

(* "set" clauses are used for defining mandatory variables that will be used
   by when reporting issues: eg for defining the condition.

   "desc" clauses are used for defining the error message,
   the suggestion, the severity.

   "let" clauses are used to define temporary formulas which are then
   used to abbreviate the another formula. For example

   let f = a And B

   set formula  = f OR f

   set message = "bla"

*)

type clause =
  | CLet  of ALVar.formula_id * ALVar.t list * t (* Let clause: let id = definifion;  *)
  | CSet of ALVar.keyword * t (* Set clause: set id = definition *)
  | CDesc of ALVar.keyword * string (* Description clause eg: set message = "..." *)

type ctl_checker = {
  name : string; (* Checker's name *)
  definitions : clause list (* A list of let/set definitions *)
}

type al_file = {
  import_files : string list;
  global_macros : clause list;
  checkers : ctl_checker list
}

val print_checker : ctl_checker -> unit

val eval_formula : t -> ast_node -> CLintersContext.context -> bool

val save_dotty_when_in_debug_mode : SourceFile.t -> unit

val next_state_via_transition : ast_node -> transitions option -> ast_node option

val create_ctl_evaluation_tracker : SourceFile.t -> unit

module Debug : sig
  val pp_formula : Format.formatter -> t -> unit
end
