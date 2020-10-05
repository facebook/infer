(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language *)

open! IStd
module F = Format

(** {2 Programs and Types} *)

(** Kind of prune instruction *)
type if_kind =
  | Ik_bexp  (** boolean expressions, and exp ? exp : exp *)
  | Ik_dowhile
  | Ik_for
  | Ik_if
  | Ik_land_lor  (** obtained from translation of && or || *)
  | Ik_while
  | Ik_switch
[@@deriving compare, equal]

type instr_metadata =
  | Abstract of Location.t
      (** a good place to apply abstraction, mostly used in the biabduction analysis *)
  | ExitScope of Var.t list * Location.t  (** remove temporaries and dead program variables *)
  | Nullify of Pvar.t * Location.t  (** nullify stack variable *)
  | Skip  (** no-op *)
  | VariableLifetimeBegins of Pvar.t * Typ.t * Location.t  (** stack variable declared *)
[@@deriving compare]

(** An instruction. *)
type instr =
  (* Note for frontend writers:
     [x] must be used in a subsequent instruction, otherwise the entire
     `Load` instruction may be eliminated by copy-propagation. *)
  | Load of {id: Ident.t; e: Exp.t; root_typ: Typ.t; typ: Typ.t; loc: Location.t}
      (** Load a value from the heap into an identifier.

          [id = *exp:typ(root_typ)] where

          - [exp] is an expression denoting a heap address
          - [typ] is typ of [exp] and [id]
          - [root_typ] is the root type of [exp]

          The [root_typ] is deprecated: it is broken in C/C++. We are removing [root_typ] in the
          future, so please use [typ] instead. *)
  | Store of {e1: Exp.t; root_typ: Typ.t; typ: Typ.t; e2: Exp.t; loc: Location.t}
      (** Store the value of an expression into the heap.

          [*exp1:typ(root_typ) = exp2] where

          - [exp1] is an expression denoting a heap address
          - [typ] is typ of [*exp1] and [exp2]
          - [root_typ] is the root type of [exp1]
          - [exp2] is the expression whose value is stored.

          The [root_typ] is deprecated: it is broken in C/C++. We are removing [root_typ] in the
          future, so please use [typ] instead. *)
  | Prune of Exp.t * Location.t * bool * if_kind
      (** prune the state based on [exp=1], the boolean indicates whether true branch *)
  | Call of (Ident.t * Typ.t) * Exp.t * (Exp.t * Typ.t) list * Location.t * CallFlags.t
      (** [Call ((ret_id, ret_typ), e_fun, arg_ts, loc, call_flags)] represents an instruction
          [ret_id = e_fun(arg_ts);] *)
  | Metadata of instr_metadata
      (** hints about the program that are not strictly needed to understand its semantics, for
          instance information about its original syntactic structure *)
[@@deriving compare]

val equal_instr : instr -> instr -> bool

val skip_instr : instr

val instr_is_auxiliary : instr -> bool
(** Check if an instruction is auxiliary, or if it comes from source instructions. *)

val location_of_instr : instr -> Location.t
(** Get the location of the instruction *)

val exps_of_instr : instr -> Exp.t list
(** get the expressions occurring in the instruction *)

val if_kind_to_string : if_kind -> string
(** Pretty print an if_kind *)

val pp_instr_metadata : Pp.env -> F.formatter -> instr_metadata -> unit

val pp_instr : print_types:bool -> Pp.env -> F.formatter -> instr -> unit
(** Pretty print an instruction. *)

val d_instr : instr -> unit
(** Dump an instruction. *)
