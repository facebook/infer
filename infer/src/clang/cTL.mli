(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ctl_parser_types
(* This module defines a language to define checkers. These checkers
   are intepreted over the AST of the program. A checker is defined by a
   CTL formula which express a condition saying when the checker should
    report a problem *)

(** Transition labels used for example to switch from decl to stmt *)
type transitions =
  | AccessorForProperty of ALVar.alexp  (** decl to decl *)
  | Body  (** decl to stmt *)
  | FieldName of ALVar.alexp  (** stmt to stmt, decl to decl *)
  | Fields  (** stmt to stmt, decl to decl *)
  | InitExpr  (** decl to stmt *)
  | Super  (** decl to decl *)
  | ParameterName of ALVar.alexp  (** stmt to stmt, decl to decl *)
  | ParameterPos of ALVar.alexp  (** stmt to stmt, decl to decl *)
  | Parameters  (** stmt to stmt, decl to decl *)
  | Cond
  | PointerToDecl  (** stmt to decl *)
  | Protocol  (** decl to decl *)
[@@deriving compare]
(* In formulas below prefix
   "E" means "exists a path"
   "A" means "for all path" *)

(** A ctl formula *)
type t =
  | True
  | False
  | Atomic of CPredicates.t  (** Atomic formula *)
  | Not of t
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | InNode of ALVar.alexp list * t
  | AX of transitions option * t  (** AX phi <=> for all children of the current node phi holds *)
  | EX of transitions option * t
      (** EX phi <=> exist a child of the current node such that phi holds *)
  | AF of transitions option * t
      (** AF phi <=> for all path from the current node there is a descendant where phi holds *)
  | EF of transitions option * t
      (** EF phi <=> there exits a a path from the current node with a descendant where phi hold *)
  | AG of transitions option * t  (** AG phi <=> for all discendant of the current node phi hold *)
  | EG of transitions option * t
      (** EG phi <=>
                                     there exists a path (of descendants) from the current node where phi hold at each node of the path *)
  | AU of transitions option * t * t
      (** AU(phi1, phi2) <=>
                                         for all paths from the current node phi1 holds in every node until ph2 holds *)
  | EU of transitions option * t * t
      (** EU(phi1, phi2) <=>
                                         there exists a path from the current node such that phi1 holds until phi2 holds *)
  | EH of ALVar.alexp list * t
      (** EH[classes]phi <=>
                                   there exists a node defining a super class in the hierarchy of the class
                                   defined by the current node (if any) where  phi holds *)
  | ET of ALVar.alexp list * transitions option * t
      (** ET [T] [l] phi <=> there exists a descentant an of the current node such that an is of type in set T
          making a transition to a node an' via label l, such that in an phi holds. *)
  | InObjCClass of t * t
[@@deriving compare]

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

val equal : t -> t -> bool

type clause =
  | CLet of ALVar.formula_id * ALVar.t list * t
  (* Let clause: let id = definifion;  *)
  | CSet of ALVar.keyword * t
  (* Set clause: set id = definition *)
  | CDesc of ALVar.keyword * string
  (* Description clause eg: set message = "..." *)
  | CPath of [`WhitelistPath | `BlacklistPath] * ALVar.t list

type ctl_checker =
  {id: string; (* Checker's id *) definitions: clause list (* A list of let/set definitions *)}

type al_file =
  { import_files: string list
  ; global_macros: clause list
  ; global_paths: (string * ALVar.alexp list) list
  ; checkers: ctl_checker list }

val print_checker : ctl_checker -> unit

val eval_formula : t -> ast_node -> CLintersContext.context -> ast_node option
(** return the evaluation of the formula and a witness *)

val save_dotty_when_in_debug_mode : SourceFile.t -> unit

val next_state_via_transition : ast_node -> transitions -> ast_node list

val create_ctl_evaluation_tracker : SourceFile.t -> unit

module Debug : sig
  val pp_formula : Format.formatter -> t -> unit

  val pp_transition : Format.formatter -> transitions option -> unit
end
