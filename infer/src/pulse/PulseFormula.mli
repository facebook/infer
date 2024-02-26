(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module SatUnsat = PulseSatUnsat
module ValueHistory = PulseValueHistory

(* NOTE: using [Var] for [AbstractValue] here since this is how "abstract values" are interpreted,
   in particular as far as arithmetic is concerned *)
module Var :
  module type of PulseAbstractValue
    with type t = PulseAbstractValue.t
     and module Set = PulseAbstractValue.Set
     and module Map = PulseAbstractValue.Map

(* Pulse-infinite added *)
(** Linear Arithmetic *)
module LinArith : sig
  (** linear combination of variables, eg [2·x + 3/4·y + 12] *)
  type t [@@deriving compare, yojson_of, equal]
end
  
module Term : sig 
  type t =
    | Const of Q.t
    | String of string
    | Var of Var.t
    | Procname of Procname.t
    | FunctionApplication of {f: t; actuals: t list}
    | Linear of LinArith.t
    | Add of t * t
    | Minus of t
    | LessThan of t * t
    | LessEqual of t * t
    | Equal of t * t
    | NotEqual of t * t
    | Mult of t * t
    | DivI of t * t
    | DivF of t * t
    | And of t * t
    | Or of t * t
    | Not of t
    | Mod of t * t
    | BitAnd of t * t
    | BitOr of t * t
    | BitNot of t
    | BitShiftLeft of t * t
    | BitShiftRight of t * t
    | BitXor of t * t
    | IsInstanceOf of Var.t * Typ.t
    | IsInt of t
  [@@deriving compare, equal, yojson_of]
end
     
module Atom : sig
  type t =
    | LessEqual of Term.t * Term.t
    | LessThan of Term.t * Term.t
    | Equal of Term.t * Term.t
    | NotEqual of Term.t * Term.t
   [@@deriving compare, equal, yojson_of]

  val equal : Term.t -> Term.t -> t
                
  module Set : Caml.Set.S [@@deriving compare]
end

module Formula : sig
  type t [@@deriving compare, equal, yojson_of]
end

type t =
  { conditions: Atom.Set.t
        (** collection of conditions that have been assumed (via [PRUNE] CFG nodes) along the path.
            Note that these conditions are *not* normalized w.r.t. [phi]: [phi] already contains
            them so normalization w.r.t. [phi] would make them trivially true most of the time. *)
  ; phi: Formula.t
        (** the arithmetic constraints of the current symbolic state; true in both the pre and post
            since abstract values [Var.t] have immutable semantics *) }
[@@deriving compare, equal, yojson_of]

val extract_cond : t -> Atom.Set.t

val set_is_empty : Atom.Set.t -> bool  
(* End pulse-infinite *)

               
(** {2 Arithmetic solver}

    Build formulas from SIL and tries to decide if they are (mostly un-)satisfiable. *)

(* type t [@@deriving compare, equal, yojson_of] *)

val pp : F.formatter -> t -> unit

val pp_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit
  [@@warning "-unused-value-declaration"]
(** only used for unit tests *)

type function_symbol = Unknown of Var.t | Procname of Procname.t [@@deriving compare, equal]

type operand =
  | AbstractValueOperand of Var.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: function_symbol; actuals: Var.t list}
[@@deriving compare, equal]

val pp_operand : F.formatter -> operand -> unit

(** {3 Build formulas} *)

(** some operations will return a set of new facts discovered that are relevant to communicate to
    the memory domain *)
type new_eq =
  | EqZero of Var.t  (** [v=0] *)
  | Equal of Var.t * Var.t  (** [v_old = v_new]: [v_old] is to be replaced with [v_new] *)

val pp_new_eq : F.formatter -> new_eq -> unit

type new_eqs = new_eq RevList.t

val ttrue : t

val and_equal : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_vars : Var.t -> Var.t -> t -> (t * new_eqs) SatUnsat.t
(** [and_equal_vars v1 v2 phi] is
    [and_equal (AbstractValueOperand v1) (AbstractValueOperand v2) phi] *)

val and_not_equal : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_instanceof : Var.t -> Var.t -> Typ.t -> t -> (t * new_eqs) SatUnsat.t

val and_less_equal : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_less_than : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_unop : Var.t -> Unop.t -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_binop : Var.t -> Binop.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_is_int : Var.t -> t -> (t * new_eqs) SatUnsat.t

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

(** {3 Operations} *)

val normalize : Tenv.t -> get_dynamic_type:(Var.t -> Typ.t option) -> t -> (t * new_eqs) SatUnsat.t
(** think a bit harder about the formula *)

val simplify :
     Tenv.t
  -> get_dynamic_type:(Var.t -> Typ.t option)
  -> precondition_vocabulary:Var.Set.t
  -> keep:Var.Set.t
  -> t
  -> (t * Var.Set.t * new_eqs) SatUnsat.t

val is_known_zero : t -> Var.t -> bool

val get_known_constant_opt : t -> Var.t -> Q.t option

val is_known_non_pointer : t -> Var.t -> bool

val is_manifest : is_allocated:(Var.t -> bool) -> t -> bool
(** Some types or errors detected by Pulse require that the state be *manifest*, which corresponds
    to the fact that the error can happen in *any reasonable* calling context (see below). If not,
    the error is flagged as *latent* and not reported until it becomes manifest.

    A state is *manifest* when its path condition (here meaning the conjunction of conditions
    encountered in [if] statements, loop conditions, etc., i.e. anything in a {!IR.Sil.Prune} node)
    is either a) empty or b) comprised only of facts of the form [p>0] or [p≠0] where [p] is known
    to be allocated. The latter condition captures the idea that addresses being valid pointers in
    memory should not deter us from reporting any error that we find on that program path as it is
    somewhat the happy/expected case (also, the fact [p] is allocated already implies [p≠0]). The
    unhappy/unexpected case here would be to report errors that require a pointer to be invalid or
    null in the precondition; we do not want to report such errors until we see that there exists a
    calling context in which the pointer is indeed invalid or null! But, to reiterate, we do want to
    report errors that only have valid pointers in their precondition. Similarly, ignore conditions
    of the form [x≠y] where [x] and [y] are already different according to the heap (because they
    are separate memory allocations).

    Some equalities might be represented implicitly in the precondition, see the documentation of
    {!PulseArithmetic.is_manifest}. *)

val get_var_repr : t -> Var.t -> Var.t
(** get the canonical representative for the variable according to the equality relation *)

val and_callee_formula :
     (Var.t * ValueHistory.t) Var.Map.t
  -> t
  -> callee:t
  -> ((Var.t * ValueHistory.t) Var.Map.t * t * new_eqs) SatUnsat.t

val fold_variables : (t, Var.t, 'acc) Container.fold
(** note: each variable mentioned in the formula is visited at least once, possibly more *)

val absval_of_int : t -> IntLit.t -> t * Var.t
(** Get or create an abstract value ([Var.t] is [AbstractValue.t]) associated with a constant
    {!IR.IntLit.t}. The idea is that clients will record in the abstract state that the returned [t]
    is equal to the given integer. If the same integer is queried later on then this module will
    return the same abstract variable. *)
