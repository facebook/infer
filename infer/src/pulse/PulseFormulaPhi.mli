(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CItv = PulseCItv
module SatUnsat = PulseSatUnsat
module Var = PulseFormulaVar
module LinArith = PulseFormulaLinArit
module Tableau = PulseFormulaTableau
module Term = PulseFormulaTerm
module Atom = PulseFormulaAtom
module InstanceOf = PulseFormulaInstanceOf

module VarUF : module type of UnionFind.Make (Var) (Var.Set) (Var.Map)

type new_eq = EqZero of Var.t | Equal of Var.t * Var.t

val pp_new_eq : F.formatter -> new_eq -> unit

type new_eqs = new_eq RevList.t

val pp_new_eqs : F.formatter -> new_eqs -> unit

type var_eqs = VarUF.t [@@deriving compare, equal, yojson_of]

type linear_eqs = LinArith.t Var.Map.t [@@deriving compare, equal, yojson_of]

type intervals = CItv.t Var.Map.t [@@deriving compare, equal, yojson_of]

module VarMapOccurrences : sig
  type t = Var.Set.t Var.Map.t
end

module TermDomainOrRange : sig
  module Set : sig
    type t
  end
end

module TermMapOccurrences : sig
  type t = TermDomainOrRange.Set.t Var.Map.t
end

module AtomMapOccurrences : sig
  type t = Atom.Set.t Var.Map.t
end

type term_eqs

type t = private
  { var_eqs: var_eqs
        (** Equality relation between variables. We want to only use canonical representatives from
            this equality relation in the rest of the formula and more generally in all of the
            abstract state. See also {!AbductiveDomain}. *)
  ; const_eqs: Term.t Var.Map.t
  ; type_constraints: InstanceOf.t
  ; linear_eqs: linear_eqs
        (** Equalities of the form [x = l] where [l] is from linear arithmetic. These are
            interpreted over the *rationals*, not integers (or floats), so this will be incomplete.
            There are mitigations to recover a little of that lost completeness:

            - [atoms] have [is_int()] terms that will detect when we get (constant) rationals where
              we expected integers eg [is_int(1.5)] becomes [false].

            - [intervals] are over integers

            INVARIANT:

            1. the domain and the range of [phi.linear_eqs] mention distinct variables:
            [domain(linear_eqs) ∩ range(linear_eqs) = ∅], when seeing [linear_eqs] as a map [x->l]

            2. for all [x=l ∊ linear_eqs], [x < min({x'|x'∊l})] according to [is_simpler_than] (in
            other words: [x] is the simplest variable in [x=l]). *)
  ; term_eqs: term_eqs
        (** Equalities of the form [t = x], used to detect when two abstract values are equal to the
            same term (hence equal). Together with [var_eqs] and [linear_eqs] this gives a
            congruence closure capability to the domain over uninterpreted parts of the domain
            (meaning the atoms and term equalities that are not just linear arithmetic and handled
            in a complete-ish fashion by [linear_eqs], [var_eqs], [tableau]). Even on interpreted
            domains like linear arithmetic [term_eqs] is needed to provide on-the-fly normalisation
            by detecting when two variables are equal to the same term, which [linear_eqs] will do
            nothing about (eg [x=0∧y=0] doesn't trigger [x=y] in [linear_eqs] but [term_eqs] is in
            charge of detecting it).

            Under the hood this is a map [t -> x] that may contain un-normalized [x]s in its
            co-domain; these are normalized on the fly when reading from the map

            INVARIANT: each term in [term_eqs] is *shallow*, meaning it is either a constant or a
            term of the form [f(x1, ..., xN)] with [x1], ..., [xN] either variables or constants
            themselves. *)
  ; tableau: Tableau.t
        (** linear equalities similar to [linear_eqs] but involving only "restricted" (aka "slack")
            variables; this is used for reasoning about inequalities, see \[2\]

            INVARIANT: see {!Tableau} *)
  ; intervals: intervals
        (** A simple, non-relational domain of concrete integer intervals of the form [x∈[i,j]] or
            [x∉[i,j]].

            This is used to recover a little bit of completeness on integer reasoning at no great
            cost. *)
  ; atoms: Atom.Set.t
        (** "everything else": atoms that cannot be expressed in a form suitable for one of the
            other domains, in particular disequalities.

            INVARIANT: Contrarily to [term_eqs] atoms are *maximally expanded* meaning if a variable
            [x] is equal to a term [t] then [x] shouldn't appear in [atoms] and should be
            substituted by [t] instead. This is looser than other invariants since a given variable
            can be equal to several syntactically-distinct terms so "maximally expanded" doesn't
            really make sense in general and there is incompleteness there. *)
  ; linear_eqs_occurrences: VarMapOccurrences.t
        (** occurrences of variables in [linear_eqs]: a binding [x -> y] means that [x] appears in
            [linear_eqs(y)] and will be used to propagate new (linear) equalities about [x] to
            maintain the [linear_eqs] invariant without having to do a linear scan of [linear_eqs]
        *)
  ; tableau_occurrences: VarMapOccurrences.t  (** likewise for [tableau] *)
  ; term_eqs_occurrences: TermMapOccurrences.t
        (** like [linear_eqs_occurrences] but for [term_eqs] so bindings are from variables to sets
            of terms *)
  ; atoms_occurrences: AtomMapOccurrences.t  (** likewise for [atoms] *)
  ; term_conditions: Atom.Set.t  (** Termination conditions for pulse-inf *)
  ; term_conditions2: Term.Set.t  (** Termination conditions for pulse-inf *) }
[@@deriving compare, equal, yojson_of]

val pp_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

val get_repr : t -> Var.t -> VarUF.repr
(** the canonical representative of a given variable *)

val ttrue : t

(* {2 [term_eqs] interface due to the totally opaque type} *)

val get_term_eq : t -> Term.t -> Var.t option

val term_eqs_fold : (Term.t -> Var.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val term_eqs_iter : (Term.t -> Var.t -> unit) -> t -> unit

val term_eqs_exists : (Term.t -> Var.t -> bool) -> t -> bool

val term_eqs_filter : (Term.t -> Var.t -> bool) -> t -> Term.VarMap.t_

val fold_term_eqs_vars : t -> init:'acc -> f:('acc -> Var.t -> 'acc) -> 'acc

val subst_term_eqs : Var.t Var.Map.t -> t -> Term.VarMap.t_

val pp_term_eqs_with_pp_var :
  ?filter:(Term.t -> Var.t -> bool) -> (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

(* {2 mutations} *)

val add_const_eq : Var.t -> Term.t -> t -> t SatUnsat.t
(** [add_const_eq v t phi] adds [v=t] to [const_eqs]; [Unsat] if [v] was already bound to a
    different constant *)

val remove_const_eq : Var.t -> t -> t

val add_dynamic_type : Var.t -> Typ.t -> ?source_file:SourceFile.t -> t -> t * bool
(** We're now tracking nullability more precisely. If the language includes nulls and we have
    apparently contradictory type information then, instead of immediately returning [Unsat], we
    want to add an assertion that the value must be null (which inhabits all (nullable) types). But
    at this low level in the solver stack (the [Unsafe] module), we can't yet decide if the
    resulting state is [Unsat]. So we return an extra boolean that indicates whether or not a
    higher-level wrapper should add, and compute the consequences of, that assertion. See
    [and_dynamic_type] further down (in the [Normalizer] module) for such a wrapper around
    [add_dynamic_type] *)

val add_below : Var.t -> Typ.t -> t -> t * bool

val add_notbelow : Var.t -> Typ.t -> t -> t * bool

val copy_type_constraints : Var.t -> Var.t -> t -> t

val add_linear_eq : Var.t -> LinArith.t -> t -> t * Var.t option
(** [add_linear_eq v l phi] adds [v=l] to [linear_eqs] and updates the occurrences maps and
    [term_eqs] appropriately; don't forget to call [propagate_linear_eq] after this *)

val remove_linear_eq : Var.t -> LinArith.t -> t -> t
(** [remove_linear_eq v l phi] removes [v] from [linear_eqs] and updates the occurrences maps and
    [term_eqs] appropriately; [l] is the existing binding for [v] *)

val remove_tableau_eq : Var.t -> LinArith.t -> t -> t

val add_term_eq : Term.t -> Var.t -> t -> t * Var.t option
(** [add_term_eq t v phi] adds [t=v] to [term_eqs] and updates the occurrences maps appropriately;
    don't forget to call [propagate_term_eq] after this *)

val remove_term_eq : Term.t -> Var.t -> t -> t
(** [remove_term_eq t v phi] removes [t] from the [term_eqs] map and updates the occurrences maps
    appropriately; [v] is the existing (representative for the) binding for [t] *)

val add_atom : Atom.t -> t -> t

val and_termcond_atoms : t -> Atom.t list -> t

val and_termcond_binop : t -> Term.t -> t

val remove_atom : Atom.t -> t -> t

val add_tableau_eq : Var.t -> LinArith.t -> t -> t

val add_interval : Var.t -> CItv.t -> t -> t SatUnsat.t

val add_interval_ : Var.t -> CItv.t -> intervals -> intervals SatUnsat.t
(** same as [add_interval] but operates on the inner [intervals] datatype *)

val add_occurrence_to_range_of_term_eq : Term.t -> VarUF.repr -> t -> t
(** assumes that the variable doesn't appear in the term itself *)

val remove_from_linear_eqs_occurrences : Var.t -> t -> t

val remove_from_tableau_occurrences : Var.t -> t -> t

val remove_from_term_eqs_occurrences : Var.t -> t -> t

val remove_from_atoms_occurrences : Var.t -> t -> t

val set_var_eqs : var_eqs -> t -> t

val set_tableau : Tableau.t -> t -> t

val set_intervals : intervals -> t -> t

val get_terminal_conds : t -> Atom.Set.t

val get_terminal_terms : t -> Term.Set.t

val join : t -> t -> t

val remove_conditions_for_join : Atom.t list -> t -> t -> t

val unsafe_mk :
     var_eqs:var_eqs
  -> const_eqs:Term.t Var.Map.t
  -> type_constraints:InstanceOf.t
  -> linear_eqs:linear_eqs
  -> term_eqs:Term.VarMap.t_
  -> tableau:Tableau.t
  -> intervals:intervals
  -> atoms:Atom.Set.t
  -> linear_eqs_occurrences:VarMapOccurrences.t
  -> tableau_occurrences:VarMapOccurrences.t
  -> term_eqs_occurrences:TermMapOccurrences.t
  -> atoms_occurrences:AtomMapOccurrences.t
  -> t
(** escape hatch *)

val fold_variables : t -> init:'a -> f:('a -> Var.t -> 'a) -> 'a

val is_non_pointer : t -> Var.t -> bool

(** module that breaks invariants more often that the rest, with an interface that is safer to use
*)
module Normalizer : sig
  val and_var_linarith : Var.t -> LinArith.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_var_term : Var.t -> Term.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_var_var : Var.t -> Var.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val normalize_atom : t -> Atom.t -> Atom.t list SatUnsat.t

  val and_normalized_atoms :
    t * new_eqs -> Atom.t list -> orig_atom:Atom.t -> add_term:bool -> (t * new_eqs) SatUnsat.t
  (** use with the result of {!normalize_atom} in place of {!and_atom} *)

  val and_atom : Atom.t -> t * new_eqs -> add_term:bool -> (t * new_eqs) SatUnsat.t
  (** [and_atom atom (phi, new_eqs)] is
      [SatUnsat.(normalize_atom phi atom >>= and_normalized_atoms (phi, new_eqs))] *)

  val and_dynamic_type :
    Var.t -> Typ.t -> ?source_file:SourceFile.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_below : Var.t -> Typ.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t

  val and_notbelow : Var.t -> Typ.t -> t * new_eqs -> (t * new_eqs) SatUnsat.t
end

val is_neq_zero : t -> Term.t -> bool
