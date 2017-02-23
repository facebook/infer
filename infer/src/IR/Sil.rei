/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** The Smallfoot Intermediate Language */
let module F = Format;


/** {2 Programs and Types} */

/** Convert expression lists to expression sets. */
let elist_to_eset: list Exp.t => Exp.Set.t;


/** Kind of prune instruction */
type if_kind =
  | Ik_bexp /* boolean expressions, and exp ? exp : exp */
  | Ik_dowhile
  | Ik_for
  | Ik_if
  | Ik_land_lor /* obtained from translation of && or || */
  | Ik_while
  | Ik_switch
[@@deriving compare];


/** An instruction. */
type instr =
  /** Load a value from the heap into an identifier.
      [x = *lexp:typ] where
        [lexp] is an expression denoting a heap address
        [typ] is the root type of [lexp]. */
  /* Note for frontend writers:
     [x] must be used in a subsequent instruction, otherwise the entire
     `Load` instruction may be eliminated by copy-propagation. */
  | Load Ident.t Exp.t Typ.t Location.t
  /** Store the value of an expression into the heap.
      [*lexp1:typ = exp2] where
        [lexp1] is an expression denoting a heap address
        [typ] is the root type of [lexp1]
        [exp2] is the expression whose value is store. */
  | Store Exp.t Typ.t Exp.t Location.t
  /** prune the state based on [exp=1], the boolean indicates whether true branch */
  | Prune Exp.t Location.t bool if_kind
  /** [Call (ret_id, e_fun, arg_ts, loc, call_flags)] represents an instruction
      [ret_id = e_fun(arg_ts);]. The return value is ignored when [ret_id = None]. */
  | Call (option (Ident.t, Typ.t)) Exp.t (list (Exp.t, Typ.t)) Location.t CallFlags.t
  /** nullify stack variable */
  | Nullify Pvar.t Location.t
  | Abstract Location.t /** apply abstraction */
  | Remove_temps (list Ident.t) Location.t /** remove temporaries */
  | Declare_locals (list (Pvar.t, Typ.t)) Location.t /** declare local variables */
[@@deriving compare];

let equal_instr: instr => instr => bool;


/** compare instructions from different procedures without considering loc's, ident's, and pvar's.
    the [exp_map] param gives a mapping of names used in the procedure of [instr1] to identifiers
    used in the procedure of [instr2] */
let compare_structural_instr: instr => instr => Exp.Map.t Exp.t => (int, Exp.Map.t Exp.t);

let skip_instr: instr;


/** Check if an instruction is auxiliary, or if it comes from source instructions. */
let instr_is_auxiliary: instr => bool;


/** Offset for an lvalue. */
type offset =
  | Off_fld Ident.fieldname Typ.t
  | Off_index Exp.t;


/** {2 Components of Propositions} */

/** an atom is a pure atomic formula */
type atom =
  | Aeq Exp.t Exp.t /** equality */
  | Aneq Exp.t Exp.t /** disequality */
  | Apred PredSymb.t (list Exp.t) /** predicate symbol applied to exps */
  | Anpred PredSymb.t (list Exp.t) /** negated predicate symbol applied to exps */
[@@deriving compare];

let equal_atom: atom => atom => bool;


/** kind of lseg or dllseg predicates */
type lseg_kind =
  | Lseg_NE /** nonempty (possibly circular) listseg */
  | Lseg_PE /** possibly empty (possibly circular) listseg */
[@@deriving compare];

let equal_lseg_kind: lseg_kind => lseg_kind => bool;


/** The boolean is true when the pointer was dereferenced without testing for zero. */
type zero_flag = option bool;


/** True when the value was obtained by doing case analysis on null in a procedure call. */
type null_case_flag = bool;


/** instrumentation of heap values */
type inst =
  | Iabstraction
  | Iactual_precondition
  | Ialloc
  | Iformal zero_flag null_case_flag
  | Iinitial
  | Ilookup
  | Inone
  | Inullify
  | Irearrange zero_flag null_case_flag int PredSymb.path_pos
  | Itaint
  | Iupdate zero_flag null_case_flag int PredSymb.path_pos
  | Ireturn_from_call int
[@@deriving compare];

let equal_inst: inst => inst => bool;

let inst_abstraction: inst;

let inst_actual_precondition: inst;

let inst_alloc: inst;

let inst_formal: inst; /** for formal parameters and heap values at the beginning of the function */

let inst_initial: inst; /** for initial values */

let inst_lookup: inst;

let inst_none: inst;

let inst_nullify: inst;


/** the boolean indicates whether the pointer is known nonzero */
let inst_rearrange: bool => Location.t => PredSymb.path_pos => inst;

let inst_taint: inst;

let inst_update: Location.t => PredSymb.path_pos => inst;


/** Get the null case flag of the inst. */
let inst_get_null_case_flag: inst => option bool;


/** Set the null case flag of the inst. */
let inst_set_null_case_flag: inst => inst;


/** update the location of the instrumentation */
let inst_new_loc: Location.t => inst => inst;


/** Update [inst_old] to [inst_new] preserving the zero flag */
let update_inst: inst => inst => inst;

exception JoinFail;


/** join of instrumentations, can raise JoinFail */
let inst_partial_join: inst => inst => inst;


/** meet of instrumentations */
let inst_partial_meet: inst => inst => inst;


/** structured expressions represent a value of structured type, such as an array or a struct. */
type strexp0 'inst =
  | Eexp Exp.t 'inst /** Base case: expression with instrumentation */
  | Estruct (list (Ident.fieldname, strexp0 'inst)) 'inst /** C structure */
  /** Array of given length
      There are two conditions imposed / used in the array case.
      First, if some index and value pair appears inside an array
      in a strexp, then the index is less than the length of the array.
      For instance, x |->[10 | e1: v1] implies that e1 <= 9.
      Second, if two indices appear in an array, they should be different.
      For instance, x |->[10 | e1: v1, e2: v2] implies that e1 != e2. */
  | Earray Exp.t (list (Exp.t, strexp0 'inst)) 'inst
[@@deriving compare];

type strexp = strexp0 inst;


/** Comparison function for strexp.
    The inst:: parameter specifies whether instumentations should also
    be considered (false by default).  */
let compare_strexp: inst::bool? => strexp => strexp => int;


/** Equality function for strexp.
    The inst:: parameter specifies whether instumentations should also
    be considered (false by default).  */
let equal_strexp: inst::bool? => strexp => strexp => bool;


/** an atomic heap predicate */
type hpred0 'inst =
  | Hpointsto Exp.t (strexp0 'inst) Exp.t
  /** represents [exp|->strexp:typexp] where [typexp]
      is an expression representing a type, e.h. [sizeof(t)]. */
  | Hlseg lseg_kind (hpara0 'inst) Exp.t Exp.t (list Exp.t)
  /** higher - order predicate for singly - linked lists.
      Should ensure that exp1!= exp2 implies that exp1 is allocated.
      This assumption is used in the rearrangement. The last [exp list] parameter
      is used to denote the shared links by all the nodes in the list. */
  | Hdllseg lseg_kind (hpara_dll0 'inst) Exp.t Exp.t Exp.t Exp.t (list Exp.t)
  /** higher-order predicate for doubly-linked lists.
      Parameter for the higher-order singly-linked list predicate.
      Means "lambda (root,next,svars). Exists evars. body".
      Assume that root, next, svars, evars are disjoint sets of
      primed identifiers, and include all the free primed identifiers in body.
      body should not contain any non - primed identifiers or program
      variables (i.e. pvars). */
[@@deriving compare]
and hpara0 'inst = {
  root: Ident.t,
  next: Ident.t,
  svars: list Ident.t,
  evars: list Ident.t,
  body: list (hpred0 'inst)
}
[@@deriving compare]
/** parameter for the higher-order doubly-linked list predicates.
    Assume that all the free identifiers in body_dll should belong to
    cell, blink, flink, svars_dll, evars_dll. */
and hpara_dll0 'inst = {
  cell: Ident.t, /** address cell */
  blink: Ident.t, /** backward link */
  flink: Ident.t, /** forward link */
  svars_dll: list Ident.t,
  evars_dll: list Ident.t,
  body_dll: list (hpred0 'inst)
}
[@@deriving compare];

type hpred = hpred0 inst;

type hpara = hpara0 inst;

type hpara_dll = hpara_dll0 inst;


/** Comparison function for hpred.
    The inst:: parameter specifies whether instumentations should also
    be considered (false by default).  */
let compare_hpred: inst::bool? => hpred => hpred => int;


/** Equality function for hpred.
    The inst:: parameter specifies whether instumentations should also
    be considered (false by default).  */
let equal_hpred: inst::bool? => hpred => hpred => bool;


/** Sets of heap predicates */
let module HpredSet: Caml.Set.S with type elt = hpred;


/** {2 Compaction} */
type sharing_env;


/** Create a sharing env to store canonical representations */
let create_sharing_env: unit => sharing_env;


/** Return a canonical representation of the exp */
let exp_compact: sharing_env => Exp.t => Exp.t;


/** Return a compact representation of the exp */
let hpred_compact: sharing_env => hpred => hpred;


/** {2 Comparision And Inspection Functions} */
let has_objc_ref_counter: Tenv.t => hpred => bool;


/** Returns the zero value of a type, for int, float and ptr types, None othwewise */
let zero_value_of_numerical_type_option: Typ.t => option Exp.t;


/** Returns the zero value of a type, for int, float and ptr types, fail otherwise */
let zero_value_of_numerical_type: Typ.t => Exp.t;


/** Make a static local name in objc */
let mk_static_local_name: string => string => string;


/** Check if a pvar is a local static in objc */
let is_static_local_name: string => Pvar.t => bool;

/* A block pvar used to explain retain cycles */
let block_pvar: Pvar.t;


/** Check if a pvar is a local pointing to a block in objc */
let is_block_pvar: Pvar.t => bool;


/** Return the lhs expression of a hpred */
let hpred_get_lhs: hpred => Exp.t;


/** {2 Pretty Printing} */

/** Begin change color if using diff printing, return updated printenv and change status */
let color_pre_wrapper: Pp.env => F.formatter => 'a => (Pp.env, bool);


/** Close color annotation if changed */
let color_post_wrapper: bool => Pp.env => F.formatter => unit;


/** Pretty print an expression. */
let pp_exp_printenv: Pp.env => F.formatter => Exp.t => unit;


/** Pretty print an expression with type. */
let pp_exp_typ: Pp.env => F.formatter => (Exp.t, Typ.t) => unit;


/** dump an expression. */
let d_exp: Exp.t => unit;


/** Pretty print a type. */
let pp_texp: Pp.env => F.formatter => Exp.t => unit;


/** Pretty print a type with all the details. */
let pp_texp_full: Pp.env => F.formatter => Exp.t => unit;


/** Dump a type expression with all the details. */
let d_texp_full: Exp.t => unit;


/** Pretty print a list of expressions. */
let pp_exp_list: Pp.env => F.formatter => list Exp.t => unit;


/** Dump a list of expressions. */
let d_exp_list: list Exp.t => unit;


/** Pretty print an offset */
let pp_offset: Pp.env => F.formatter => offset => unit;


/** Convert an offset to a string */
let offset_to_string: offset => string;


/** Dump an offset */
let d_offset: offset => unit;


/** Pretty print a list of offsets */
let pp_offset_list: Pp.env => F.formatter => list offset => unit;


/** Dump a list of offsets */
let d_offset_list: list offset => unit;


/** Get the location of the instruction */
let instr_get_loc: instr => Location.t;


/** get the expressions occurring in the instruction */
let instr_get_exps: instr => list Exp.t;


/** Pretty print an instruction. */
let pp_instr: Pp.env => F.formatter => instr => unit;


/** Dump an instruction. */
let d_instr: instr => unit;


/** Pretty print a list of instructions. */
let pp_instr_list: Pp.env => F.formatter => list instr => unit;


/** Dump a list of instructions. */
let d_instr_list: list instr => unit;


/** Pretty print an atom. */
let pp_atom: Pp.env => F.formatter => atom => unit;


/** Dump an atom. */
let d_atom: atom => unit;


/** return a string representing the inst */
let inst_to_string: inst => string;


/** Pretty print a strexp. */
let pp_sexp: Pp.env => F.formatter => strexp => unit;


/** Dump a strexp. */
let d_sexp: strexp => unit;


/** Pretty print a strexp list. */
let pp_sexp_list: Pp.env => F.formatter => list strexp => unit;


/** Dump a strexp. */
let d_sexp_list: list strexp => unit;


/** Pretty print a hpred. */
let pp_hpred: Pp.env => F.formatter => hpred => unit;


/** Dump a hpred. */
let d_hpred: hpred => unit;


/** Pretty print a hpara. */
let pp_hpara: Pp.env => F.formatter => hpara => unit;


/** Pretty print a list of hparas. */
let pp_hpara_list: Pp.env => F.formatter => list hpara => unit;


/** Pretty print a hpara_dll. */
let pp_hpara_dll: Pp.env => F.formatter => hpara_dll => unit;


/** Pretty print a list of hpara_dlls. */
let pp_hpara_dll_list: Pp.env => F.formatter => list hpara_dll => unit;


/** Module Predicates records the occurrences of predicates as parameters
    of (doubly -)linked lists and Epara.
    Provides unique numbering for predicates and an iterator. */
let module Predicates: {

  /** predicate environment */
  type env;

  /** create an empty predicate environment */
  let empty_env: unit => env;

  /** return true if the environment is empty */
  let is_empty: env => bool;

  /** return the id of the hpara */
  let get_hpara_id: env => hpara => int;

  /** return the id of the hpara_dll */
  let get_hpara_dll_id: env => hpara_dll => int;

  /** [iter env f f_dll] iterates [f] and [f_dll] on all the hpara and hpara_dll,
      passing the unique id to the functions. The iterator can only be used once. */
  let iter: env => (int => hpara => unit) => (int => hpara_dll => unit) => unit;

  /** Process one hpred, updating the predicate environment */
  let process_hpred: env => hpred => unit;
};


/** Pretty print a hpred with optional predicate env */
let pp_hpred_env: Pp.env => option Predicates.env => F.formatter => hpred => unit;


/** {2 Functions for traversing SIL data types} */

/** This function should be used before adding a new
    index to Earray. The [exp] is the newly created
    index. This function "cleans" [exp] according to whether it is the
    footprint or current part of the prop.
    The function faults in the re - execution mode, as an internal check of the tool. */
let array_clean_new_index: bool => Exp.t => Exp.t;


/** Change exps in strexp using [f]. */

/** WARNING: the result might not be normalized. */
let strexp_expmap: ((Exp.t, option inst) => (Exp.t, option inst)) => strexp => strexp;


/** Change exps in hpred by [f]. */

/** WARNING: the result might not be normalized. */
let hpred_expmap: ((Exp.t, option inst) => (Exp.t, option inst)) => hpred => hpred;


/** Change instrumentations in hpred using [f]. */
let hpred_instmap: (inst => inst) => hpred => hpred;


/** Change exps in hpred list by [f]. */

/** WARNING: the result might not be normalized. */
let hpred_list_expmap: ((Exp.t, option inst) => (Exp.t, option inst)) => list hpred => list hpred;


/** Change exps in atom by [f]. */

/** WARNING: the result might not be normalized. */
let atom_expmap: (Exp.t => Exp.t) => atom => atom;


/** Change exps in atom list by [f]. */

/** WARNING: the result might not be normalized. */
let atom_list_expmap: (Exp.t => Exp.t) => list atom => list atom;


/** {2 Function for computing lexps in sigma} */
let hpred_list_get_lexps: (Exp.t => bool) => list hpred => list Exp.t;


/** {2 Functions for computing program variables} */
let exp_fpv: Exp.t => list Pvar.t;

let strexp_fpv: strexp => list Pvar.t;

let atom_fpv: atom => list Pvar.t;

let hpred_fpv: hpred => list Pvar.t;

let hpara_fpv: hpara => list Pvar.t;


/** {2 Functions for computing free non-program variables} */

/** Type of free variables. These include primed, normal and footprint variables.
    We remember the order in which variables are added. */
type fav;


/** flag to indicate whether fav's are stored in duplicate form.
    Only to be used with fav_to_list */
let fav_duplicates: ref bool;


/** Pretty print a fav. */
let pp_fav: Pp.env => F.formatter => fav => unit;


/** Create a new [fav]. */
let fav_new: unit => fav;


/** Emptyness check. */
let fav_is_empty: fav => bool;


/** Check whether a predicate holds for all elements. */
let fav_for_all: fav => (Ident.t => bool) => bool;


/** Check whether a predicate holds for some elements. */
let fav_exists: fav => (Ident.t => bool) => bool;


/** Membership test fot [fav] */
let fav_mem: fav => Ident.t => bool;


/** Convert a list to a fav. */
let fav_from_list: list Ident.t => fav;


/** Convert a [fav] to a list of identifiers while preserving the order
    that identifiers were added to [fav]. */
let fav_to_list: fav => list Ident.t;


/** Copy a [fav]. */
let fav_copy: fav => fav;


/** Turn a xxx_fav_add function into a xxx_fav function */
let fav_imperative_to_functional: (fav => 'a => unit) => 'a => fav;


/** [fav_filter_ident fav f] only keeps [id] if [f id] is true. */
let fav_filter_ident: fav => (Ident.t => bool) => unit;


/** Like [fav_filter_ident] but return a copy. */
let fav_copy_filter_ident: fav => (Ident.t => bool) => fav;


/** [fav_subset_ident fav1 fav2] returns true if every ident in [fav1]
    is in [fav2].*/
let fav_subset_ident: fav => fav => bool;


/** add identifier list to fav */
let ident_list_fav_add: list Ident.t => fav => unit;


/** [exp_fav_add fav exp] extends [fav] with the free variables of [exp] */
let exp_fav_add: fav => Exp.t => unit;

let exp_fav: Exp.t => fav;

let exp_fav_list: Exp.t => list Ident.t;

let ident_in_exp: Ident.t => Exp.t => bool;

let strexp_fav_add: fav => strexp => unit;

let atom_fav_add: fav => atom => unit;

let atom_fav: atom => fav;

let hpred_fav_add: fav => hpred => unit;

let hpred_fav: hpred => fav;


/** Variables in hpara, excluding bound vars in the body */
let hpara_shallow_av: hpara => fav;


/** Variables in hpara_dll, excluding bound vars in the body */
let hpara_dll_shallow_av: hpara_dll => fav;


/** {2 Functions for computing all free or bound non-program variables} */

/** Non-program variables include all of primed, normal and footprint
    variables. Thus, the functions essentially compute all the
    identifiers occuring in a parameter. Some variables can appear more
    than once in the result. */
let exp_av_add: fav => Exp.t => unit;

let strexp_av_add: fav => strexp => unit;

let atom_av_add: fav => atom => unit;

let hpred_av_add: fav => hpred => unit;

let hpara_av_add: fav => hpara => unit;


/** {2 Substitution} */
type subst [@@deriving compare];


/** Equality for substitutions. */
let equal_subst: subst => subst => bool;


/** Create a substitution from a list of pairs.
    For all (id1, e1), (id2, e2) in the input list,
    if id1 = id2, then e1 = e2. */
let sub_of_list: list (Ident.t, Exp.t) => subst;


/** like sub_of_list, but allow duplicate ids and only keep the first occurrence */
let sub_of_list_duplicates: list (Ident.t, Exp.t) => subst;


/** Convert a subst to a list of pairs. */
let sub_to_list: subst => list (Ident.t, Exp.t);


/** The empty substitution. */
let sub_empty: subst;


/** Compute the common id-exp part of two inputs [subst1] and [subst2].
    The first component of the output is this common part.
    The second and third components are the remainder of [subst1]
    and [subst2], respectively. */
let sub_join: subst => subst => subst;


/** Compute the common id-exp part of two inputs [subst1] and [subst2].
    The first component of the output is this common part.
    The second and third components are the remainder of [subst1]
    and [subst2], respectively. */
let sub_symmetric_difference: subst => subst => (subst, subst, subst);


/** [sub_find filter sub] returns the expression associated to the first identifier
    that satisfies [filter].
    Raise [Not_found] if there isn't one. */
let sub_find: (Ident.t => bool) => subst => Exp.t;


/** [sub_filter filter sub] restricts the domain of [sub] to the
    identifiers satisfying [filter]. */
let sub_filter: (Ident.t => bool) => subst => subst;


/** [sub_filter_exp filter sub] restricts the domain of [sub] to the
    identifiers satisfying [filter(id, sub(id))]. */
let sub_filter_pair: subst => f::((Ident.t, Exp.t) => bool) => subst;


/** [sub_range_partition filter sub] partitions [sub] according to
    whether range expressions satisfy [filter]. */
let sub_range_partition: (Exp.t => bool) => subst => (subst, subst);


/** [sub_domain_partition filter sub] partitions [sub] according to
    whether domain identifiers satisfy [filter]. */
let sub_domain_partition: (Ident.t => bool) => subst => (subst, subst);


/** Return the list of identifiers in the domain of the substitution. */
let sub_domain: subst => list Ident.t;


/** Return the list of expressions in the range of the substitution. */
let sub_range: subst => list Exp.t;


/** [sub_range_map f sub] applies [f] to the expressions in the range of [sub]. */
let sub_range_map: (Exp.t => Exp.t) => subst => subst;


/** [sub_map f g sub] applies the renaming [f] to identifiers in the domain
    of [sub] and the substitution [g] to the expressions in the range of [sub]. */
let sub_map: (Ident.t => Ident.t) => (Exp.t => Exp.t) => subst => subst;


/** Checks whether [id] belongs to the domain of [subst]. */
let mem_sub: Ident.t => subst => bool;


/** Extend substitution and return [None] if not possible. */
let extend_sub: subst => Ident.t => Exp.t => option subst;


/** Free auxilary variables in the domain and range of the
    substitution. */
let sub_fav_add: fav => subst => unit;


/** Free or bound auxilary variables in the domain and range of the
    substitution. */
let sub_av_add: fav => subst => unit;


/** Compute free pvars in a sub */
let sub_fpv: subst => list Pvar.t;


/** substitution functions */

/** WARNING: these functions do not ensure that the results are normalized. */
let exp_sub: subst => Exp.t => Exp.t;

let atom_sub: subst => atom => atom;


/** apply [subst] to all id's in [instr], including LHS id's */
let instr_sub: subst => instr => instr;

let hpred_sub: subst => hpred => hpred;

let exp_sub_ids: (Ident.t => Exp.t) => Exp.t => Exp.t;


/** apply [f] to id's in [instr]. if [sub_id_binders] is false, [f] is only applied to bound id's */
let instr_sub_ids: sub_id_binders::bool => (Ident.t => Exp.t) => instr => instr;


/** {2 Functions for replacing occurrences of expressions.} */

/** The first parameter should define a partial function.
    No parts of hpara are replaced by these functions. */
let exp_replace_exp: list (Exp.t, Exp.t) => Exp.t => Exp.t;

let strexp_replace_exp: list (Exp.t, Exp.t) => strexp => strexp;

let atom_replace_exp: list (Exp.t, Exp.t) => atom => atom;

let hpred_replace_exp: list (Exp.t, Exp.t) => hpred => hpred;


/** {2 Functions for constructing or destructing entities in this module} */

/** Compute the offset list of an expression */
let exp_get_offsets: Exp.t => list offset;


/** Add the offset list to an expression */
let exp_add_offsets: Exp.t => list offset => Exp.t;

let sigma_to_sigma_ne: list hpred => list (list atom, list hpred);


/** [hpara_instantiate para e1 e2 elist] instantiates [para] with [e1],
    [e2] and [elist]. If [para = lambda (x, y, xs). exists zs. b],
    then the result of the instantiation is [b\[e1 / x, e2 / y, elist / xs, _zs'/ zs\]]
    for some fresh [_zs'].*/
let hpara_instantiate: hpara => Exp.t => Exp.t => list Exp.t => (list Ident.t, list hpred);


/** [hpara_dll_instantiate para cell blink flink  elist] instantiates [para] with [cell],
    [blink], [flink], and [elist]. If [para = lambda (x, y, z, xs). exists zs. b],
    then the result of the instantiation is
    [b\[cell / x, blink / y, flink / z, elist / xs, _zs'/ zs\]]
    for some fresh [_zs'].*/
let hpara_dll_instantiate:
  hpara_dll => Exp.t => Exp.t => Exp.t => list Exp.t => (list Ident.t, list hpred);

let custom_error: Pvar.t;
