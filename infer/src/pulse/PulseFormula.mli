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

(** {2 Arithmetic solver}

    Build formulas from SIL and tries to decide if they are (mostly un-)satisfiable. *)

type t [@@deriving compare, equal, yojson_of]

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

val and_equal_instanceof : Var.t -> Var.t -> Typ.t -> nullable:bool -> t -> (t * new_eqs) SatUnsat.t

type dynamic_type_data = {typ: Typ.t; source_file: SourceFile.t option}

val get_dynamic_type : Var.t -> t -> dynamic_type_data option

val and_dynamic_type : Var.t -> Typ.t -> ?source_file:SourceFile.t -> t -> (t * new_eqs) SatUnsat.t

val add_dynamic_type_unsafe : Var.t -> Typ.t -> ?source_file:SourceFile.t -> Location.t -> t -> t

val copy_type_constraints : Var.t -> Var.t -> t -> t

val and_less_equal : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_less_than : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_unop : Var.t -> Unop.t -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_binop : Var.t -> Binop.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_string_concat : Var.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_is_int : Var.t -> t -> (t * new_eqs) SatUnsat.t

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

(** {3 Operations} *)

val simplify :
  precondition_vocabulary:Var.Set.t -> keep:Var.Set.t -> t -> (t * Var.Set.t * new_eqs) SatUnsat.t

val is_known_zero : t -> Var.t -> bool

val as_constant_q : t -> Var.t -> Q.t option

val as_constant_string : t -> Var.t -> string option

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

val absval_of_string : t -> string -> t * Var.t

type term

val explain_as_term : t -> Var.t -> term option

val pp_term : (F.formatter -> Var.t -> unit) -> F.formatter -> term -> unit

val pp_conditions_explained : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

val pp_formula_explained : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit
