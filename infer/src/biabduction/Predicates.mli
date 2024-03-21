(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Offset for an lvalue. *)
type offset = Off_fld of Fieldname.t * Typ.t | Off_index of Exp.t

(** {2 Components of Propositions} *)

(** an atom is a pure atomic formula *)
type atom =
  | Aeq of Exp.t * Exp.t  (** equality *)
  | Aneq of Exp.t * Exp.t  (** disequality *)
  | Apred of PredSymb.t * Exp.t list  (** predicate symbol applied to exps *)
  | Anpred of PredSymb.t * Exp.t list  (** negated predicate symbol applied to exps *)
[@@deriving compare]

val equal_atom : atom -> atom -> bool

val atom_has_local_addr : atom -> bool

(** kind of lseg or dllseg predicates *)
type lseg_kind =
  | Lseg_NE  (** nonempty (possibly circular) listseg *)
  | Lseg_PE  (** possibly empty (possibly circular) listseg *)
[@@deriving compare]

val equal_lseg_kind : lseg_kind -> lseg_kind -> bool

(** The boolean is true when the pointer was dereferenced without testing for zero. *)
type zero_flag = bool option

(** True when the value was obtained by doing case analysis on null in a procedure call. *)
type null_case_flag = bool

(** instrumentation of heap values *)
type inst =
  | Iabstraction
  | Iactual_precondition
  | Ialloc
  | Iformal of zero_flag * null_case_flag
  | Iinitial
  | Ilookup
  | Inone
  | Inullify
  | Irearrange of zero_flag * null_case_flag * int * PredSymb.path_pos
  | Itaint
  | Iupdate of zero_flag * null_case_flag * int * PredSymb.path_pos
  | Ireturn_from_call of int
[@@deriving compare]

val equal_inst : inst -> inst -> bool

val inst_actual_precondition : inst

val inst_formal : inst

val inst_initial : inst
(** for formal parameters and heap values at the beginning of the function *)

val inst_lookup : inst
(** for initial values *)

val inst_none : inst

val inst_nullify : inst

val inst_rearrange : bool -> Location.t -> PredSymb.path_pos -> inst
(** the boolean indicates whether the pointer is known nonzero *)

val inst_update : Location.t -> PredSymb.path_pos -> inst

val inst_set_null_case_flag : inst -> inst
(** Set the null case flag of the inst. *)

val inst_new_loc : Location.t -> inst -> inst
(** update the location of the instrumentation *)

val update_inst : inst -> inst -> inst
(** Update [inst_old] to [inst_new] preserving the zero flag *)

exception JoinFail

val inst_partial_join : inst -> inst -> inst
(** join of instrumentations, can raise JoinFail *)

val inst_partial_meet : inst -> inst -> inst
(** meet of instrumentations *)

(** structured expressions represent a value of structured type, such as an array or a struct. *)
type 'inst strexp0 =
  | Eexp of Exp.t * 'inst  (** Base case: expression with instrumentation *)
  | Estruct of (Fieldname.t * 'inst strexp0) list * 'inst  (** C structure *)
  | Earray of Exp.t * (Exp.t * 'inst strexp0) list * 'inst
      (** Array of given length There are two conditions imposed / used in the array case. First, if
          some index and value pair appears inside an array in a strexp, then the index is less than
          the length of the array. For instance, [x |->[10 | e1: v1]] implies that [e1 <= 9].
          Second, if two indices appear in an array, they should be different. For instance,
          [x |->[10 | e1: v1, e2: v2]] implies that [e1 != e2]. *)
[@@deriving compare]

type strexp = inst strexp0

val compare_strexp : ?inst:bool -> strexp -> strexp -> int
(** Comparison function for strexp. The inst:: parameter specifies whether instumentations should
    also be considered (false by default). *)

val equal_strexp : ?inst:bool -> strexp -> strexp -> bool
(** Equality function for strexp. The inst:: parameter specifies whether instumentations should also
    be considered (false by default). *)

(** an atomic heap predicate *)
type 'inst hpred0 =
  | Hpointsto of Exp.t * 'inst strexp0 * Exp.t
      (** represents [exp|->strexp:typexp] where [typexp] is an expression representing a type, e.h.
          [sizeof(t)]. *)
  | Hlseg of lseg_kind * 'inst hpara0 * Exp.t * Exp.t * Exp.t list
      (** higher - order predicate for singly - linked lists. Should ensure that exp1!= exp2 implies
          that exp1 is allocated. This assumption is used in the rearrangement. The last [exp list]
          parameter is used to denote the shared links by all the nodes in the list. *)
  | Hdllseg of lseg_kind * 'inst hpara_dll0 * Exp.t * Exp.t * Exp.t * Exp.t * Exp.t list
      (** higher-order predicate for doubly-linked lists. Parameter for the higher-order
          singly-linked list predicate. Means "lambda (root,next,svars). Exists evars. body". Assume
          that root, next, svars, evars are disjoint sets of primed identifiers, and include all the
          free primed identifiers in body. body should not contain any non - primed identifiers or
          program variables (i.e. pvars). *)
[@@deriving compare]

and 'inst hpara0 =
  {root: Ident.t; next: Ident.t; svars: Ident.t list; evars: Ident.t list; body: 'inst hpred0 list}
[@@deriving compare]

(** parameter for the higher-order doubly-linked list predicates. Assume that all the free
    identifiers in body_dll should belong to cell, blink, flink, svars_dll, evars_dll. *)
and 'inst hpara_dll0 =
  { cell: Ident.t  (** address cell *)
  ; blink: Ident.t  (** backward link *)
  ; flink: Ident.t  (** forward link *)
  ; svars_dll: Ident.t list
  ; evars_dll: Ident.t list
  ; body_dll: 'inst hpred0 list }
[@@deriving compare]

type hpred = inst hpred0

type hpara = inst hpara0

type hpara_dll = inst hpara_dll0

val compare_hpred : ?inst:bool -> hpred -> hpred -> int
(** Comparison function for hpred. The inst:: parameter specifies whether instumentations should
    also be considered (false by default). *)

val equal_hpred : ?inst:bool -> hpred -> hpred -> bool
(** Equality function for hpred. The inst:: parameter specifies whether instumentations should also
    be considered (false by default). *)

(** Sets of heap predicates *)
module HpredSet : Caml.Set.S with type elt = hpred

(** {2 Compaction} *)

type sharing_env

val create_sharing_env : unit -> sharing_env
(** Create a sharing env to store canonical representations *)

val hpred_compact : sharing_env -> hpred -> hpred
(** Return a compact representation of the exp *)

val is_objc_object : hpred -> bool

(** {2 Comparision And Inspection Functions} *)

val pp_offset : Pp.env -> F.formatter -> offset -> unit

val d_offset_list : offset list -> unit
(** Dump a list of offsets *)

val pp_atom : Pp.env -> F.formatter -> atom -> unit
(** Pretty print an atom. *)

val d_atom : atom -> unit
(** Dump an atom. *)

val pp_inst : F.formatter -> inst -> unit
(** pretty-print an inst *)

val pp_sexp : Pp.env -> F.formatter -> strexp -> unit
(** Pretty print a strexp. *)

val d_sexp : strexp -> unit
(** Dump a strexp. *)

val pp_hpred : Pp.env -> F.formatter -> hpred -> unit
(** Pretty print a hpred. *)

val d_hpred : hpred -> unit
(** Dump a hpred. *)

val pp_hpara : Pp.env -> F.formatter -> hpara -> unit
(** Pretty print a hpara. *)

val pp_hpara_dll : Pp.env -> F.formatter -> hpara_dll -> unit
(** Pretty print a hpara_dll. *)

(** record the occurrences of predicates as parameters of (doubly -)linked lists and Epara. Provides
    unique numbering for predicates and an iterator. *)
module Env : sig
  (** predicate environment *)
  type t

  val mk_empty : unit -> t
  (** create an empty predicate environment *)

  val is_empty : t -> bool
  (** return true if the environment is empty *)

  val iter : t -> (int -> hpara -> unit) -> (int -> hpara_dll -> unit) -> unit
  (** [iter env f f_dll] iterates [f] and [f_dll] on all the hpara and hpara_dll, passing the unique
      id to the functions. The iterator can only be used once. *)

  val process_hpred : t -> hpred -> unit
  (** Process one hpred, updating the predicate environment *)
end

val pp_hpred_env : Pp.env -> Env.t option -> F.formatter -> hpred -> unit
(** Pretty print a hpred with optional predicate env *)

(** {2 Functions for traversing SIL data types} *)

val strexp_expmap : (Exp.t * inst option -> Exp.t * inst option) -> strexp -> strexp
(** Change exps in strexp using [f]. WARNING: the result might not be normalized. *)

val hpred_expmap : (Exp.t * inst option -> Exp.t * inst option) -> hpred -> hpred
(** Change exps in hpred by [f]. WARNING: the result might not be normalized. *)

val hpred_instmap : (inst -> inst) -> hpred -> hpred
(** Change instrumentations in hpred using [f]. *)

val hpred_list_expmap : (Exp.t * inst option -> Exp.t * inst option) -> hpred list -> hpred list
(** Change exps in hpred list by [f]. WARNING: the result might not be normalized. *)

val atom_expmap : (Exp.t -> Exp.t) -> atom -> atom
(** Change exps in atom by [f]. WARNING: the result might not be normalized. *)

val hpred_list_get_lexps : (Exp.t -> bool) -> hpred list -> Exp.t list

val hpred_entries : hpred -> Exp.t list

val atom_free_vars : atom -> Ident.t Sequence.t

val atom_gen_free_vars : atom -> (unit, Ident.t) Sequence.Generator.t

val hpred_free_vars : hpred -> Ident.t Sequence.t

val hpred_gen_free_vars : hpred -> (unit, Ident.t) Sequence.Generator.t

val hpara_shallow_free_vars : hpara -> Ident.t Sequence.t

val hpara_dll_shallow_free_vars : hpara_dll -> Ident.t Sequence.t
(** Variables in hpara_dll, excluding bound vars in the body *)

(** {2 Substitution} *)

type subst = private (Ident.t * Exp.t) list [@@deriving compare]

val equal_subst : subst -> subst -> bool
(** Equality for substitutions. *)

val subst_of_list : (Ident.t * Exp.t) list -> subst
(** Create a substitution from a list of pairs. For all (id1, e1), (id2, e2) in the input list, if
    id1 = id2, then e1 = e2. *)

val subst_of_list_duplicates : (Ident.t * Exp.t) list -> subst
(** like subst_of_list, but allow duplicate ids and only keep the first occurrence *)

val sub_to_list : subst -> (Ident.t * Exp.t) list
(** Convert a subst to a list of pairs. *)

val sub_empty : subst
(** The empty substitution. *)

val is_sub_empty : subst -> bool

val sub_join : subst -> subst -> subst
(** Compute the common id-exp part of two inputs [subst1] and [subst2]. The first component of the
    output is this common part. The second and third components are the remainder of [subst1] and
    [subst2], respectively. *)

val sub_symmetric_difference : subst -> subst -> subst * subst * subst
(** Compute the common id-exp part of two inputs [subst1] and [subst2]. The first component of the
    output is this common part. The second and third components are the remainder of [subst1] and
    [subst2], respectively. *)

val sub_find : (Ident.t -> bool) -> subst -> Exp.t
(** [sub_find filter sub] returns the expression associated to the first identifier that satisfies
    [filter]. Raise [Not_found_s/Caml.Not_found] if there isn't one. *)

val sub_filter : (Ident.t -> bool) -> subst -> subst
(** [sub_filter filter sub] restricts the domain of [sub] to the identifiers satisfying [filter]. *)

val sub_filter_pair : subst -> f:(Ident.t * Exp.t -> bool) -> subst
(** [sub_filter_exp filter sub] restricts the domain of [sub] to the identifiers satisfying
    [filter(id, sub(id))]. *)

val sub_range_partition : (Exp.t -> bool) -> subst -> subst * subst
(** [sub_range_partition filter sub] partitions [sub] according to whether range expressions satisfy
    [filter]. *)

val sub_domain_partition : (Ident.t -> bool) -> subst -> subst * subst
(** [sub_domain_partition filter sub] partitions [sub] according to whether domain identifiers
    satisfy [filter]. *)

val sub_domain : subst -> Ident.t list
(** Return the list of identifiers in the domain of the substitution. *)

val sub_range : subst -> Exp.t list
(** Return the list of expressions in the range of the substitution. *)

val sub_range_map : (Exp.t -> Exp.t) -> subst -> subst
(** [sub_range_map f sub] applies [f] to the expressions in the range of [sub]. *)

val sub_map : (Ident.t -> Ident.t) -> (Exp.t -> Exp.t) -> subst -> subst
(** [sub_map f g sub] applies the renaming [f] to identifiers in the domain of [sub] and the
    substitution [g] to the expressions in the range of [sub]. *)

val extend_sub : subst -> Ident.t -> Exp.t -> subst option
(** Extend substitution and return [None] if not possible. *)

val subst_free_vars : subst -> Ident.t Sequence.t

val subst_gen_free_vars : subst -> (unit, Ident.t) Sequence.Generator.t

(** substitution functions WARNING: these functions do not ensure that the results are normalized. *)

val exp_sub : subst -> Exp.t -> Exp.t

val atom_sub : subst -> atom -> atom

val hpred_sub : subst -> hpred -> hpred

(** {2 Functions for replacing occurrences of expressions.} *)

val atom_replace_exp : (Exp.t * Exp.t) list -> atom -> atom

val hpred_replace_exp : (Exp.t * Exp.t) list -> hpred -> hpred

(** {2 Functions for constructing or destructing entities in this module} *)

val exp_get_offsets : Exp.t -> offset list
(** Compute the offset list of an expression *)

val exp_add_offsets : Exp.t -> offset list -> Exp.t
(** Add the offset list to an expression *)

val sigma_to_sigma_ne : hpred list -> (atom list * hpred list) list

val hpara_instantiate : hpara -> Exp.t -> Exp.t -> Exp.t list -> Ident.t list * hpred list
(** [hpara_instantiate para e1 e2 elist] instantiates [para] with [e1], [e2] and [elist]. If
    [para = lambda (x, y, xs). exists zs. b], then the result of the instantiation is
    [b[e1 / x, e2 / y, elist / xs, _zs'/ zs]] for some fresh [_zs'].*)

val hpara_dll_instantiate :
  hpara_dll -> Exp.t -> Exp.t -> Exp.t -> Exp.t list -> Ident.t list * hpred list
(** [hpara_dll_instantiate para cell blink flink elist] instantiates [para] with [cell], [blink],
    [flink], and [elist]. If [para = lambda (x, y, z, xs). exists zs. b], then the result of the
    instantiation is [b[cell / x, blink / y, flink / z, elist / xs, _zs'/ zs]] for some fresh
    [_zs'].*)

val custom_error : Pvar.t
