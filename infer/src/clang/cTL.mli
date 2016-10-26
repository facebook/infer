(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* This module defines a language to define checkers. These checkers
   are intepreted over the AST of the program. A checker is defined by a
   CTL formula which express a condition saying when the checker should
    report a problem *)

type transition_decl_to_stmt =
  | Body
  | InitExpr


(* In formulas below prefix
   "E" means "exists a path"
   "A" means "for all path" *)

(** A ctl formula *)
type t =
  | True
  | False
  | Atomic of Predicates.t (** Atomic formula *)
  | Not of t
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | AX of t (** AX phi <=> for all children of the current node phi holds *)
  | EX of t (** EX phi <=> exist a child of the current node such that phi holds *)
  | AF of t (** AF phi <=> for all path from the current node there is a descendant where phi holds *)
  | EF of t (** EF phi <=> there exits a a path from the current node with a descendant where phi hold *)
  | AG of t (** AG phi <=> for all discendant of the current node phi hold *)
  | EG of t (** EG phi <=>
                there exists a path (of descendants) from the current node where phi hold at each node of the path *)
  | AU of t * t (** AU(phi1, phi2) <=>
                    for all paths from the current node phi1 holds in every node until ph2 holds *)
  | EU of t * t (** EU(phi1, phi2) <=>
                    there exists a path from the current node such that phi1 holds until phi2 holds *)
  | EH of string list * t (** EH[classes]phi <=>
                              there exists a node defining a super class in the hierarchy of the class
                              defined by the current node (if any) where  phi holds *)
  | ET of string list * transition_decl_to_stmt option * t (** ET[T][l] phi <=>
                                                               there exists a descentant an of the current node such that an is of type in set T
                                                               making a transition to a node an' via label l, such that in an phi holds. *)

(** the kind of AST nodes where formulas are evaluated *)
type ast_node =
  | Stmt of Clang_ast_t.stmt
  | Decl of Clang_ast_t.decl

val eval_formula : t -> ast_node -> CLintersContext.context -> bool

val save_dotty_when_in_debug_mode : DB.source_file -> unit


module Debug : sig
  val pp_formula : Format.formatter -> t -> unit
end
