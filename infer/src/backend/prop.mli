(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Functions for Propositions (i.e., Symbolic Heaps) *)

open Sil

type normal (** kind for normal props, i.e. normalized *)
type exposed (** kind for exposed props *)

(** Proposition. *)

type pi = Sil.atom list
type sigma = Sil.hpred list

type 'a t (** the kind 'a should range over [normal] and [exposed] *)

(** type to describe different strategies for initializing fields of a structure. [No_init] does not
    initialize any fields of the struct. [Fld_init] initializes the fields of the struct with fresh
    variables (C) or default values (Java). *)
type struct_init_mode =
  | No_init
  | Fld_init

exception Cannot_star of Logging.ml_loc

(** {2 Basic Functions for propositions} *)

(** Compare propositions *)
val prop_compare : 'a t -> 'a t -> int

(** Check the equality of two sigma's *)
val sigma_equal : sigma -> sigma -> bool

(** Check the equality of two propositions *)
val prop_equal : 'a t -> 'a t -> bool

(** Pretty print a substitution. *)
val pp_sub : printenv -> Format.formatter -> subst -> unit

(** Dump a substitution. *)
val d_sub : subst -> unit

(** Pretty print a pi. *)
val pp_pi : printenv -> Format.formatter -> pi -> unit

(** Dump a pi. *)
val d_pi : pi -> unit

(** Pretty print a sigma. *)
val pp_sigma : printenv -> Format.formatter -> sigma -> unit

(** Dump a sigma. *)
val d_sigma : sigma -> unit

(** Dump a pi and a sigma *)
val d_pi_sigma: pi -> sigma -> unit

(** Split sigma into stack and nonstack parts.
    The boolean indicates whether the stack should only include local variales. *)
val sigma_get_stack_nonstack : bool -> sigma -> sigma * sigma

(** Update the object substitution given the stack variables in the prop *)
val prop_update_obj_sub : printenv -> 'a t -> printenv

(** Pretty print a proposition. *)
val pp_prop : printenv -> Format.formatter -> 'a t -> unit

(** Pretty print a proposition with type information *)
val pp_prop_with_typ : printenv -> Format.formatter -> normal t -> unit

(** Create a predicate environment for a prop *)
val prop_pred_env : 'a t -> Sil.Predicates.env

(** Dump a proposition. *)
val d_prop : 'a t -> unit

(** Dump a proposition with type information *)
val d_prop_with_typ : 'a t -> unit

(** Pretty print a list propositions with type information *)
val pp_proplist_with_typ : printenv -> Format.formatter -> normal t list -> unit

val d_proplist_with_typ : 'a t list -> unit

(** Compute free non-program variables of pi *)
val pi_fav : atom list -> fav

val pi_fav_add : fav -> atom list -> unit

(** Compute free non-program variables of sigma *)
val sigma_fav_add : fav -> hpred list -> unit

val sigma_fav : hpred list -> fav

(** returns free non-program variables that are used to express
    the contents of stack variables *)
val sigma_fav_in_pvars_add : fav -> hpred list -> unit

(** Compute free non-program variables of prop *)
val prop_fav_add : fav -> 'a t -> unit

(** Compute free non-program variables of prop, visited in depth first order *)
val prop_fav_add_dfs : fav -> 'a t -> unit

val prop_fav: normal t -> fav

(** free vars, except pi and sub, of current and footprint parts *)
val prop_fav_nonpure : normal t -> fav

(** Find fav of the footprint part of the prop *)
val prop_footprint_fav : 'a t -> fav

(** Compute all the free program variables in the prop *)
val prop_fpv: 'a t -> Pvar.t list

(** Apply substitution for pi *)
val pi_sub : subst -> atom list -> atom list

(** Apply subsitution for sigma *)
val sigma_sub : subst -> hpred list -> hpred list

(** Apply subsitution to prop. Result is not normalized. *)
val prop_sub : subst -> 'a t -> exposed t

(** Apply the substitution to all the expressions in the prop. *)
val prop_expmap : (Sil.exp -> Sil.exp) -> 'a t -> exposed t

(** Relaces all expressions in the [hpred list] using the first argument.
    Assume that the first parameter defines a partial function.
    No expressions inside hpara are replaced. *)
val sigma_replace_exp : (exp * exp) list -> hpred list -> hpred list

val sigma_map : 'a t -> (hpred -> hpred) -> 'a t

(** {2 Normalization} *)

(** Turn an inequality expression into an atom *)
val mk_inequality : Sil.exp -> Sil.atom

(** Return [true] if the atom is an inequality *)
val atom_is_inequality : Sil.atom -> bool

(** If the atom is [e<=n] return [e,n] *)
val atom_exp_le_const : Sil.atom -> (Sil.exp * Sil.Int.t) option

(** If the atom is [n<e] return [n,e] *)
val atom_const_lt_exp : Sil.atom -> (Sil.Int.t * Sil.exp) option

(** Negate an atom *)
val atom_negate : Sil.atom -> Sil.atom

(** type for arithmetic problems *)
type arith_problem =
  (* division by zero *)
  | Div0 of Sil.exp

  (* unary minus of unsigned type applied to the given expression *)
  | UminusUnsigned of Sil.exp * Sil.typ

(** Look for an arithmetic problem in [exp] *)
val find_arithmetic_problem : path_pos -> normal t -> Sil.exp -> arith_problem option * normal t

(** Normalize [exp] using the pure part of [prop].  Later, we should
    change this such that the normalization exposes offsets of [exp]
    as much as possible. *)
val exp_normalize_prop : 'a t -> Sil.exp -> Sil.exp

(** Normalize the expression without abstracting complex subexpressions *)
val exp_normalize_noabs : Sil.subst -> Sil.exp -> Sil.exp

(** Collapse consecutive indices that should be added. For instance,
    this function reduces x[1][1] to x[2]. The [typ] argument is used
    to ensure the soundness of this collapsing. *)
val exp_collapse_consecutive_indices_prop : Sil.typ -> Sil.exp -> Sil.exp

(** Normalize [exp] used for the address of a heap cell.
    This normalization does not combine two offsets inside [exp]. *)
val lexp_normalize_prop : 'a t -> exp -> exp

val atom_normalize_prop : 'a t -> atom -> atom

val strexp_normalize_prop : 'a t -> strexp -> strexp

val hpred_normalize_prop : 'a t -> hpred -> hpred

val sigma_normalize_prop : 'a t -> hpred list -> hpred list

val pi_normalize_prop : 'a t -> atom list -> atom list

(** normalize a prop *)
val normalize : exposed t -> normal t

(** expose a prop, no-op used to instantiate the sub-type relation *)
val expose : normal t -> exposed t

(** {2 Compaction} *)
(** Return a compact representation of the prop *)
val prop_compact : sharing_env -> normal t -> normal t

(** {2 Queries about propositions} *)

(** Check if the sigma part of the proposition is emp *)
val prop_is_emp : 'a t -> bool

(** {2 Functions for changing and generating propositions} *)

(** Construct a disequality. *)
val mk_neq : exp -> exp -> atom

(** Construct an equality. *)
val mk_eq : exp -> exp -> atom

(** create a strexp of the given type, populating the structures if [expand_structs] is true *)
val create_strexp_of_type: Tenv.t option -> struct_init_mode -> Sil.typ -> Sil.inst -> Sil.strexp

(** Construct a pointsto. *)
val mk_ptsto : exp -> strexp -> exp -> hpred

(** Construct a points-to predicate for an expression using either the provided expression [name] as
    base for fresh identifiers. *)
val mk_ptsto_exp : Tenv.t option -> struct_init_mode -> exp * exp * exp option -> Sil.inst -> hpred

(** Construct a points-to predicate for a single program variable.
    If [expand_structs] is true, initialize the fields of structs with fresh variables. *)
val mk_ptsto_lvar :
  Tenv.t option -> struct_init_mode -> Sil.inst -> Pvar.t * exp * exp option -> hpred

(** Construct a lseg predicate *)
val mk_lseg : lseg_kind -> hpara -> exp -> exp -> exp list -> hpred

(** Construct a dllseg predicate *)
val mk_dllseg : lseg_kind -> hpara_dll -> exp -> exp -> exp -> exp -> exp list -> hpred

(** Construct a hpara *)
val mk_hpara : Ident.t -> Ident.t -> Ident.t list -> Ident.t list -> hpred list -> hpara

(** Construct a dll_hpara *)
val mk_dll_hpara :
  Ident.t -> Ident.t -> Ident.t -> Ident.t list -> Ident.t list -> hpred list -> hpara_dll

(** Proposition [true /\ emp]. *)
val prop_emp : normal t

(** Reset every inst in the prop using the given map *)
val prop_reset_inst : (Sil.inst -> Sil.inst) -> 'a t -> exposed t

(** Conjoin a heap predicate by separating conjunction. *)
val prop_hpred_star : 'a t -> hpred -> exposed t

(** Conjoin a list of heap predicates by separating conjunction *)
val prop_sigma_star : 'a t -> hpred list -> exposed t

(** Conjoin a pure atomic predicate by normal conjunction. *)
val prop_atom_and : ?footprint: bool -> normal t -> atom -> normal t

(** Conjoin [exp1]=[exp2] with a symbolic heap [prop]. *)
val conjoin_eq : ?footprint: bool -> exp -> exp -> normal t -> normal t

(** Conjoin [exp1]!=[exp2] with a symbolic heap [prop]. *)
val conjoin_neq : ?footprint: bool -> exp -> exp -> normal t -> normal t

(** Check whether an atom is used to mark an attribute *)
val atom_is_attribute : atom -> bool

(** Apply f to every resource attribute in the prop *)
val attribute_map_resource : normal t -> (Sil.exp -> Sil.res_action -> Sil.res_action) -> normal t

(** Return the exp and attribute marked in the atom if any, and return None otherwise *)
val atom_get_exp_attribute : atom -> (Sil.exp * Sil.attribute) option

(** Get the attributes associated to the expression, if any *)
val get_exp_attributes : 'a t -> exp -> attribute list

(** Get the undef attribute associated to the expression, if any *)
val get_undef_attribute : 'a t -> exp -> attribute option

(** Get the resource attribute associated to the expression, if any *)
val get_resource_attribute : 'a t -> exp -> attribute option

(** Get the taint attribute associated to the expression, if any *)
val get_taint_attribute : 'a t -> exp -> attribute option

(** Get the autorelease attribute associated to the expression, if any *)
val get_autorelease_attribute : 'a t -> exp -> attribute option

(** Get the div0 attribute associated to the expression, if any *)
val get_div0_attribute : 'a t -> exp -> attribute option

(** Get the observer attribute associated to the expression, if any *)
val get_observer_attribute : 'a t -> exp -> attribute option

(** Get the objc null attribute associated to the expression, if any *)
val get_objc_null_attribute : 'a t -> exp -> attribute option

(** Get all the attributes of the prop *)
val get_all_attributes : 'a t -> (exp * attribute) list

val has_dangling_uninit_attribute : 'a t -> exp -> bool

val set_exp_attribute : normal t -> exp -> attribute -> normal t

val add_or_replace_exp_attribute_check_changed : (Sil.attribute -> Sil.attribute -> unit) ->
  normal t -> exp -> attribute -> normal t

(** Replace an attribute associated to the expression *)
val add_or_replace_exp_attribute : normal t -> exp -> attribute -> normal t

(** mark Sil.Var's or Sil.Lvar's as undefined *)
val mark_vars_as_undefined : normal t -> Sil.exp list -> Procname.t -> Location.t ->
  Sil.path_pos -> normal t

(** Remove an attribute from all the atoms in the heap *)
val remove_attribute : Sil.attribute -> 'a t -> normal t

(** [replace_objc_null lhs rhs].
    If rhs has the objc_null attribute, replace the attribute and set the lhs = 0 *)
val replace_objc_null : normal t -> exp -> exp -> normal t

val nullify_exp_with_objc_null : normal t -> exp -> normal t

(** Remove an attribute from an exp in the heap *)
val remove_attribute_from_exp : Sil.attribute -> 'a t -> exp -> normal t

(** Retireve all the atoms in the heap that contain a specific attribute *)
val get_atoms_with_attribute : Sil.attribute -> 'a t -> Sil.exp list

(** Return the sub part of [prop]. *)
val get_sub : 'a t -> subst

(** Return the pi part of [prop]. *)
val get_pi : 'a t -> atom list

(** Return the pure part of [prop]. *)
val get_pure : 'a t -> atom list

(** Return the sigma part of [prop] *)
val get_sigma : 'a t -> hpred list

(** Return the pi part of the footprint of [prop] *)
val get_pi_footprint : 'a t -> atom list

(** Return the sigma part of the footprint of [prop] *)
val get_sigma_footprint : 'a t -> hpred list

(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)
val deallocate_stack_vars : normal t -> Pvar.t list -> Pvar.t list * normal t

(** Canonicalize the names of primed variables. *)
val prop_rename_primed_footprint_vars : normal t -> normal t

(** Extract the footprint and return it as a prop *)
val extract_footprint : 'a t -> exposed t

(** Extract the (footprint,current) pair *)
val extract_spec : normal t -> (normal t * normal t)

(** [prop_set_fooprint p p_foot] sets proposition [p_foot] as footprint of [p]. *)
val prop_set_footprint : 'a t -> 'b t -> exposed t

(** Expand PE listsegs if the flag is on. *)
val prop_expand : normal t -> normal t list

(** translate a logical and/or operation
    taking care of the non-strict semantics for side effects *)
val trans_land_lor :
  Sil.binop -> (Ident.t list * Sil.instr list) * Sil.exp ->
  (Ident.t list * Sil.instr list) * Sil.exp -> Location.t ->
  (Ident.t list * Sil.instr list) * Sil.exp

(** translate an if-then-else expression *)
val trans_if_then_else :
  (Ident.t list * Sil.instr list) * Sil.exp -> (Ident.t list * Sil.instr list) * Sil.exp ->
  (Ident.t list * Sil.instr list) * Sil.exp -> Location.t ->
  (Ident.t list * Sil.instr list) * Sil.exp

(** {2 Functions for existentially quantifying and unquantifying variables} *)

(** Existentially quantify the [ids] in [prop]. *)
val exist_quantify : fav -> normal t -> normal t

(** convert the footprint vars to primed vars. *)
val prop_normal_vars_to_primed_vars : normal t -> normal t

(** convert the primed vars to normal vars. *)
val prop_primed_vars_to_normal_vars : normal t -> normal t

(** Rename all primed variables. *)
val prop_rename_primed_fresh : normal t -> normal t

(** Build an exposed prop from pi *)
val from_pi : pi -> exposed t

(** Build an exposed prop from sigma *)
val from_sigma : sigma -> exposed t

(** Replace the substitution part of a prop *)
val replace_sub : Sil.subst -> 'a t -> exposed t

(** Replace the pi part of a prop *)
val replace_pi : pi -> 'a t -> exposed t

(** Replace the sigma part of a prop *)
val replace_sigma : sigma -> 'a t -> exposed t

(** Replace the sigma part of the footprint of a prop *)
val replace_sigma_footprint : sigma -> 'a t -> exposed t

(** Replace the pi part of the footprint of a prop *)
val replace_pi_footprint : pi -> 'a t -> exposed t

(** Rename free variables in a prop replacing them with existentially quantified vars *)
val prop_rename_fav_with_existentials : normal t -> normal t

(** {2 Prop iterators} *)

(** Iterator over the sigma part. Each iterator has a current [hpred]. *)
type 'a prop_iter

(** Create an iterator, return None if sigma part is empty. *)
val prop_iter_create : normal t -> unit prop_iter option

(** Return the prop associated to the iterator. *)
val prop_iter_to_prop : 'a prop_iter -> normal t

(** Add an atom to the pi part of prop iter. The
    first parameter records whether it is done
    during footprint or during re - execution. *)
val prop_iter_add_atom : bool -> 'a prop_iter -> atom -> 'a prop_iter

(** Remove the current element from the iterator, and return the prop
    associated to the resulting iterator. *)
val prop_iter_remove_curr_then_to_prop : 'a prop_iter -> normal t

(** Return the current hpred and state. *)
val prop_iter_current : 'a prop_iter -> (hpred * 'a)

(** Return the next iterator. *)
val prop_iter_next : 'a prop_iter -> unit prop_iter option

(** Remove the current hpred and return the next iterator. *)
val prop_iter_remove_curr_then_next : 'a prop_iter -> unit prop_iter option

(** Update the current element of the iterator. *)
val prop_iter_update_current : 'a prop_iter -> hpred -> 'a prop_iter

(** Insert before the current element of the iterator. *)
val prop_iter_prev_then_insert : 'a prop_iter -> hpred -> 'a prop_iter

(** Find fav of the footprint part of the iterator *)
val prop_iter_footprint_fav : 'a prop_iter -> fav

(** Find fav of the iterator *)
val prop_iter_fav : 'a prop_iter -> fav

(** Extract the sigma part of the footprint *)
val prop_iter_get_footprint_sigma : 'a prop_iter -> hpred list

(** Replace the sigma part of the footprint *)
val prop_iter_replace_footprint_sigma : 'a prop_iter -> hpred list -> 'a prop_iter

(** Scan sigma to find an [hpred] satisfying the filter function. *)
val prop_iter_find : unit prop_iter -> (hpred -> 'a option) -> 'a prop_iter option

(** Update the current element of the iterator by a nonempty list of elements. *)
val prop_iter_update_current_by_list : 'a prop_iter -> hpred list -> unit prop_iter

(** Set the state of an iterator *)
val prop_iter_set_state : 'a prop_iter -> 'b -> 'b prop_iter

(** Rename [ident] in [iter] by a fresh primed identifier *)
val prop_iter_make_id_primed : Ident.t -> 'a prop_iter -> 'a prop_iter

(** Collect garbage fields. *)
val prop_iter_gc_fields : unit prop_iter -> unit prop_iter

val find_equal_formal_path : exp -> 'a t -> Sil.exp option

(** return the set of subexpressions of [strexp] *)
val strexp_get_exps : Sil.strexp -> Sil.ExpSet.t

(** get the set of expressions on the righthand side of [hpred] *)
val hpred_get_targets : Sil.hpred -> Sil.ExpSet.t

(** return the set of hpred's and exp's in [sigma] that are reachable from an expression in
    [exps] *)
val compute_reachable_hpreds : hpred list -> Sil.ExpSet.t -> Sil.HpredSet.t * Sil.ExpSet.t


(** if possible, produce a (fieldname, typ) path from one of the [src_exps] to [snk_exp] using
    [reachable_hpreds]. *)
val get_fld_typ_path_opt : Sil.ExpSet.t -> Sil.exp -> Sil.HpredSet.t ->
  (Ident.fieldname option * Sil.typ) list option

(** filter [pi] by removing the pure atoms that do not contain an expression in [exps] *)
val compute_reachable_atoms : pi -> Sil.ExpSet.t -> pi

(** {2 Internal modules} *)

module Metrics : sig
  (** Compute a size value for the prop, which indicates its complexity *)
  val prop_size : 'a t -> int

  (** Approximate the size of the longest chain by counting the max
      number of |-> with the same type and whose lhs is primed or
      footprint *)
  val prop_chain_size : 'a t -> int
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

  (** categorize a list of preconditions *)
  val categorize : 'a t list -> pre_category
end
