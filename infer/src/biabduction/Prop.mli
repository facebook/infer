(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Predicates

(** Functions for Propositions (i.e., Symbolic Heaps) *)

(** kind for normal props, i.e. normalized *)
type normal

(** kind for exposed props *)
type exposed

(** kind for sorted props *)
type sorted

(** Proposition. *)

type pi = atom list

type sigma = hpred list

(** the kind 'a should range over [normal] and [exposed] *)
type 'a t = private
  { sigma: sigma  (** spatial part *)
  ; sub: subst  (** substitution *)
  ; pi: pi  (** pure part *)
  ; sigma_fp: sigma  (** abduced spatial part *)
  ; pi_fp: pi  (** abduced pure part *) }
[@@deriving compare]

(** type to describe different strategies for initializing fields of a structure. [No_init] does not
    initialize any fields of the struct. [Fld_init] initializes the fields of the struct with fresh
    variables (C) or default values (Java). *)
type struct_init_mode = No_init | Fld_init

(** {2 Basic Functions for propositions} *)

val has_footprint : 'a t -> bool
(** sigma_fp is nonempty or pi_fp is nonempty *)

val compare_prop : 'a t -> 'a t -> int
(** Compare propositions *)

val equal_sigma : sigma -> sigma -> bool
(** Check the equality of two sigma's *)

val d_sub : subst -> unit
(** Dump a substitution. *)

val pp_pi : Pp.env -> Format.formatter -> pi -> unit
(** Pretty print a pi. *)

val d_pi : pi -> unit
(** Dump a pi. *)

val d_sigma : sigma -> unit
(** Dump a sigma. *)

val d_pi_sigma : pi -> sigma -> unit
(** Dump a pi and a sigma *)

val sigma_get_stack_nonstack : bool -> sigma -> sigma * sigma
(** Split sigma into stack and nonstack parts. The boolean indicates whether the stack should only
    include local variales. *)

val prop_update_obj_sub : Pp.env -> 'a t -> Pp.env
(** Update the object substitution given the stack variables in the prop *)

val pp_prop : Pp.env -> Format.formatter -> 'a t -> unit
(** Pretty print a proposition. *)

val prop_pred_env : 'a t -> Predicates.Env.t
(** Create a predicate environment for a prop *)

val d_prop : 'a t -> unit
(** Dump a proposition. *)

val d_proplist_with_typ : 'a t list -> unit

val max_stamp : ?f:(Ident.t -> bool) -> normal t -> int

val pi_free_vars : pi -> Ident.t Sequence.t

val sigma_free_vars : sigma -> Ident.t Sequence.t

val free_vars : normal t -> Ident.t Sequence.t

val gen_free_vars : normal t -> (unit, Ident.t) Sequence.Generator.t

val sorted_gen_free_vars : sorted t -> (unit, Ident.t) Sequence.Generator.t

val non_pure_free_vars : normal t -> Ident.t Sequence.t

val dfs_sort : Tenv.t -> normal t -> sorted t

val pi_sub : subst -> atom list -> atom list
(** Apply substitution for pi *)

val sigma_sub : subst -> hpred list -> hpred list
(** Apply subsitution for sigma *)

val prop_sub : subst -> 'a t -> exposed t
(** Apply subsitution to prop. Result is not normalized. *)

val prop_expmap : (Exp.t -> Exp.t) -> 'a t -> exposed t
(** Apply the substitution to all the expressions in the prop. *)

val sigma_replace_exp : Tenv.t -> (Exp.t * Exp.t) list -> hpred list -> hpred list
(** Relaces all expressions in the [hpred list] using the first argument. Assume that the first
    parameter defines a partial function. No expressions inside hpara are replaced. *)

(** {2 Normalization} *)

val mk_inequality : Tenv.t -> Exp.t -> atom
(** Turn an inequality expression into an atom *)

val atom_is_inequality : atom -> bool
(** Return [true] if the atom is an inequality *)

val atom_exp_le_const : atom -> (Exp.t * IntLit.t) option
(** If the atom is [e<=n] return [e,n] *)

val atom_const_lt_exp : atom -> (IntLit.t * Exp.t) option
(** If the atom is [n<e] return [n,e] *)

val exp_normalize_prop : ?destructive:bool -> Tenv.t -> 'a t -> Exp.t -> Exp.t
(** Normalize [exp] using the pure part of [prop]. Later, we should change this such that the
    normalization exposes offsets of [exp] as much as possible.

    If [destructive] is true then normalize more aggressively, which may lose some useful structure
    or types. *)

val exp_normalize_noabs : Tenv.t -> subst -> Exp.t -> Exp.t
(** Normalize the expression without abstracting complex subexpressions *)

val exp_collapse_consecutive_indices_prop : Typ.t -> Exp.t -> Exp.t
(** Collapse consecutive indices that should be added. For instance, this function reduces [x[1][1]]
    to [x[2]]. The [typ] argument is used to ensure the soundness of this collapsing. *)

val lexp_normalize_prop : Tenv.t -> 'a t -> Exp.t -> Exp.t
(** Normalize [exp] used for the address of a heap cell. This normalization does not combine two
    offsets inside [exp]. *)

val atom_normalize_prop : Tenv.t -> 'a t -> atom -> atom

val sigma_normalize_prop : Tenv.t -> 'a t -> hpred list -> hpred list

val normalize : Tenv.t -> exposed t -> normal t
(** normalize a prop *)

val expose : _ t -> exposed t
(** expose a prop, no-op used to instantiate the sub-type relation *)

(** {2 Compaction} *)

val prop_compact : sharing_env -> normal t -> normal t
(** Return a compact representation of the prop *)

(** {2 Queries about propositions} *)

val prop_is_emp : 'a t -> bool
(** Check if the sigma part of the proposition is emp *)

(** {2 Functions for changing and generating propositions} *)

val mk_neq : Tenv.t -> Exp.t -> Exp.t -> atom
(** Construct a disequality. *)

val mk_eq : Tenv.t -> Exp.t -> Exp.t -> atom
(** Construct an equality. *)

val mk_pred : Tenv.t -> PredSymb.t -> Exp.t list -> atom
(** Construct a positive pred. *)

val mk_npred : Tenv.t -> PredSymb.t -> Exp.t list -> atom
(** Construct a negative pred. *)

val create_strexp_of_type : Tenv.t -> struct_init_mode -> Typ.t -> Exp.t option -> inst -> strexp
(** create a strexp of the given type, populating the structures if [expand_structs] is true *)

val mk_ptsto : Tenv.t -> Exp.t -> strexp -> Exp.t -> hpred
(** Construct a pointsto. *)

val mk_ptsto_exp : Tenv.t -> struct_init_mode -> Exp.t * Exp.t * Exp.t option -> inst -> hpred
(** Construct a points-to predicate for an expression using either the provided expression [name] as
    base for fresh identifiers. *)

val mk_ptsto_lvar : Tenv.t -> struct_init_mode -> inst -> Pvar.t * Exp.t * Exp.t option -> hpred
(** Construct a points-to predicate for a single program variable. If [expand_structs] is true,
    initialize the fields of structs with fresh variables. *)

val mk_lseg : Tenv.t -> lseg_kind -> hpara -> Exp.t -> Exp.t -> Exp.t list -> hpred
(** Construct a lseg predicate *)

val mk_dllseg :
  Tenv.t -> lseg_kind -> hpara_dll -> Exp.t -> Exp.t -> Exp.t -> Exp.t -> Exp.t list -> hpred
(** Construct a dllseg predicate *)

val prop_emp : normal t
(** Proposition [true /\ emp]. *)

val prop_reset_inst : (inst -> inst) -> 'a t -> exposed t
(** Reset every inst in the prop using the given map *)

val prop_hpred_star : 'a t -> hpred -> exposed t
(** Conjoin a heap predicate by separating conjunction. *)

val prop_sigma_star : 'a t -> hpred list -> exposed t
(** Conjoin a list of heap predicates by separating conjunction *)

val prop_atom_and : Tenv.t -> ?footprint:bool -> normal t -> atom -> normal t
(** Conjoin a pure atomic predicate by normal conjunction. *)

val conjoin_eq : Tenv.t -> ?footprint:bool -> Exp.t -> Exp.t -> normal t -> normal t
(** Conjoin [exp1]=[exp2] with a symbolic heap [prop]. *)

val conjoin_neq : Tenv.t -> ?footprint:bool -> Exp.t -> Exp.t -> normal t -> normal t
(** Conjoin [exp1]!=[exp2] with a symbolic heap [prop]. *)

val get_pure : 'a t -> atom list
(** Return the pure part of [prop]. *)

val prop_rename_primed_footprint_vars : Tenv.t -> normal t -> normal t
(** Canonicalize the names of primed variables. *)

val extract_footprint : 'a t -> exposed t
(** Extract the footprint and return it as a prop *)

val extract_spec : normal t -> normal t * normal t
(** Extract the (footprint,current) pair *)

val prop_expand : Tenv.t -> normal t -> normal t list
(** Expand PE listsegs if the flag is on. *)

(** {2 Functions for existentially quantifying and unquantifying variables} *)

val exist_quantify :
  Tenv.t -> ?ids_queue:unit Ident.HashQueue.t -> Ident.t list -> normal t -> normal t
(** Existentially quantify the [ids] in [prop]. *)

val prop_normal_vars_to_primed_vars : Tenv.t -> normal t -> normal t
(** convert the footprint vars to primed vars. *)

val prop_primed_vars_to_normal_vars : Tenv.t -> normal t -> normal t
(** convert the primed vars to normal vars. *)

val from_pi : pi -> exposed t
(** Build an exposed prop from pi *)

val from_sigma : sigma -> exposed t
(** Build an exposed prop from sigma *)

val set : ?sub:subst -> ?pi:pi -> ?sigma:sigma -> ?pi_fp:pi -> ?sigma_fp:sigma -> 'a t -> exposed t
(** Set individual fields of the prop. *)

(** {2 Prop iterators} *)

(** Iterator over the sigma part. Each iterator has a current [hpred]. *)
type 'a prop_iter

val prop_iter_create : normal t -> unit prop_iter option
(** Create an iterator, return None if sigma part is empty. *)

val prop_iter_to_prop : Tenv.t -> 'a prop_iter -> normal t
(** Return the prop associated to the iterator. *)

val prop_iter_add_atom : bool -> 'a prop_iter -> atom -> 'a prop_iter
(** Add an atom to the pi part of prop iter. The first parameter records whether it is done during
    footprint or during re - execution. *)

val prop_iter_remove_curr_then_to_prop : Tenv.t -> 'a prop_iter -> normal t
(** Remove the current element from the iterator, and return the prop associated to the resulting
    iterator. *)

val prop_iter_current : Tenv.t -> 'a prop_iter -> hpred * 'a
(** Return the current hpred and state. *)

val prop_iter_next : 'a prop_iter -> unit prop_iter option
(** Return the next iterator. *)

val prop_iter_update_current : 'a prop_iter -> hpred -> 'a prop_iter
(** Update the current element of the iterator. *)

val prop_iter_prev_then_insert : 'a prop_iter -> hpred -> 'a prop_iter
(** Insert before the current element of the iterator. *)

val prop_iter_max_stamp : ?f:(Ident.t -> bool) -> 'a prop_iter -> int
(** Find the maximum stamp of a free variable of a certain kind. *)

val prop_iter_get_footprint_sigma : 'a prop_iter -> hpred list
(** Extract the sigma part of the footprint *)

val prop_iter_replace_footprint_sigma : 'a prop_iter -> hpred list -> 'a prop_iter
(** Replace the sigma part of the footprint *)

val prop_iter_find : unit prop_iter -> (hpred -> 'a option) -> 'a prop_iter option
(** Scan sigma to find an [hpred] satisfying the filter function. *)

val prop_iter_update_current_by_list : 'a prop_iter -> hpred list -> unit prop_iter
(** Update the current element of the iterator by a nonempty list of elements. *)

val prop_iter_set_state : 'a prop_iter -> 'b -> 'b prop_iter
(** Set the state of an iterator *)

val prop_iter_make_id_primed : Tenv.t -> Ident.t -> 'a prop_iter -> 'a prop_iter
(** Rename [ident] in [iter] by a fresh primed identifier *)

val prop_iter_gc_fields : unit prop_iter -> unit prop_iter
(** Collect garbage fields. *)

(** {2 Internal modules} *)

module Metrics : sig
  val prop_size : 'a t -> int
  (** Compute a size value for the prop, which indicates its complexity *)
end

module CategorizePreconditions : sig
  type pre_category =
    (* no preconditions *)
    | NoPres
    (* the preconditions impose no restrictions *)
    | Empty
    (* the preconditions only demand that some pointers are allocated *)
    | OnlyAllocation
    (* the preconditions impose constraints on the values of variables and/or memory *)
    | DataConstraints

  val categorize : 'a t list -> pre_category
  (** categorize a list of preconditions *)
end
