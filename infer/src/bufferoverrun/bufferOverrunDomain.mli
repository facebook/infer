(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbstractDomain.Types

(** Set of integers for threshold widening *)
module ItvThresholds : AbstractDomain.FiniteSetS with type elt = Z.t

(** Domain for recording which operations are used for evaluating interval values *)
module ItvUpdatedBy : sig
  type t
end

(** [ModeledRange] represents how many times the interval value can be updated by modeled functions.
    This domain is to support the case where there are mismatches between value of a control
    variable and actual number of loop iterations. For example,

    [while((c = file_channel.read(buf)) != -1) { ... }]

    the loop will iterate as many times as the file size, but the control variable [c] does not have
    that value. In these cases, it assigns a symbolic value of the file size to the modeled range of
    [c], which is used when calculating the overall cost. *)
module ModeledRange : sig
  type t

  val of_big_int : trace:Bounds.BoundTrace.t -> Z.t -> t

  val of_modeled_function : Procname.t -> Location.t -> Bounds.Bound.t -> t
end

(** type for on-demand symbol evaluation in Inferbo *)
type eval_sym_trace =
  { eval_sym: Bounds.Bound.eval_sym  (** evaluating symbol *)
  ; eval_locpath: AbsLoc.PowLoc.eval_locpath  (** evaluating path *)
  ; eval_func_ptrs: FuncPtr.Set.eval_func_ptrs  (** evaluating function pointers *)
  ; trace_of_sym: Symb.Symbol.t -> BufferOverrunTrace.Set.t  (** getting traces of symbol *) }

module Val : sig
  type t =
    { itv: Itv.t  (** Interval *)
    ; itv_thresholds: ItvThresholds.t
    ; itv_updated_by: ItvUpdatedBy.t
    ; modeled_range: ModeledRange.t
    ; powloc: AbsLoc.PowLoc.t  (** Simple pointers *)
    ; arrayblk: ArrayBlk.t  (** Array blocks *)
    ; func_ptrs: FuncPtr.Set.t  (** Function pointers *)
    ; traces: BufferOverrunTrace.Set.t }

  include AbstractDomain.S with type t := t

  val bot : t

  val unknown : t

  val of_big_int : ItvThresholds.elt -> t

  val of_c_array_alloc :
       AbsLoc.Allocsite.t
    -> stride:int option
    -> offset:Itv.t
    -> size:Itv.t
    -> traces:BufferOverrunTrace.Set.t
    -> t
  (** Construct C array allocation. [stride] is a byte size of cell, [offset] is initial offset of
      pointer which is usually zero, and [size] is a total number of cells. *)

  val of_java_array_alloc :
    AbsLoc.Allocsite.t -> length:Itv.t -> traces:BufferOverrunTrace.Set.t -> t
  (** Construct Java array allocation. [size] is a total number of cells *)

  val of_int : int -> t

  val of_int_lit : IntLit.t -> t

  val of_itv : ?traces:BufferOverrunTrace.Set.t -> Itv.t -> t

  val of_literal_string : IntegerWidths.t -> string -> t

  val of_loc : ?traces:BufferOverrunTrace.Set.t -> AbsLoc.Loc.t -> t
  (** Create a value for a pointer pointing to x.*)

  val of_pow_loc : traces:BufferOverrunTrace.Set.t -> AbsLoc.PowLoc.t -> t
  (** Create a value for a pointer pointing to locations in powloc.*)

  val of_func_ptrs : FuncPtr.Set.t -> t

  val unknown_locs : t

  val unknown_from : Typ.t -> callee_pname:Procname.t option -> location:Location.t -> t
  (** Unknown return value of [callee_pname] *)

  val is_bot : t -> bool
  (** Check if the value is bottom *)

  val is_unknown : t -> bool
  (** Return true if the value represents an unknown value. Note that this does not mean that it is
      identical with Dom.Val.unknown. This is because an unknown value is bound to a particular
      location (this affects fields sym, offset_sym, and size_sym - see MemReach.add_heap) and a to
      particular assignment (this affects the field traces - see Val.add_assign_trace_elem). *)

  val is_mone : t -> bool
  (** Check if the value is [[-1,-1]] *)

  val array_sizeof : t -> Itv.t
  (** Get array size *)

  val get_all_locs : t -> AbsLoc.PowLoc.t
  (** Get all locations, including pointers and array blocks *)

  val get_array_locs : t -> AbsLoc.PowLoc.t
  (** Get locations of array blocks *)

  val get_array_blk : t -> ArrayBlk.t

  val get_range_of_iterator : t -> t
  (** Get a range of an iterator value, for example, if iterator value is [[lb,ub]], it returns
      [[0,ub]]. *)

  val get_itv : t -> Itv.t

  val get_modeled_range : t -> ModeledRange.t

  val get_pow_loc : t -> AbsLoc.PowLoc.t

  val get_func_ptrs : t -> FuncPtr.Set.t

  val get_traces : t -> BufferOverrunTrace.Set.t

  val set_array_length : Location.t -> length:t -> t -> t

  val set_array_offset : Location.t -> Itv.t -> t -> t

  val set_array_stride : Z.t -> t -> t

  val set_itv_updated_by_addition : t -> t

  val set_itv_updated_by_multiplication : t -> t

  val set_itv_updated_by_unknown : t -> t

  val set_modeled_range : ModeledRange.t -> t -> t

  val lnot : t -> t

  val neg : t -> t

  val plus_a :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t
  (** Semantics of [Binop.PlusA]. [f_trace] merges traces of the operands. If [f_trace] is not
      given, it uses a default heuristic merge function. *)

  val plus_pi : t -> t -> t

  val minus_a :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val minus_pi : t -> t -> t

  val minus_pp : t -> t -> t

  val mult :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val div :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val mod_sem :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val shiftlt :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val shiftrt :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val lt_sem : t -> t -> t

  val gt_sem : t -> t -> t

  val le_sem : t -> t -> t

  val ge_sem : t -> t -> t

  val eq_sem : t -> t -> t

  val ne_sem : t -> t -> t

  val band_sem :
       ?f_trace:(BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t -> BufferOverrunTrace.Set.t)
    -> t
    -> t
    -> t

  val land_sem : t -> t -> t

  val lor_sem : t -> t -> t

  val unknown_bit : t -> t
  (** Semantic function for some bit operators which are hard to express in the domain, e.g.,
      [Unop.BNot]. *)

  val prune_eq : t -> t -> t
  (** Pruning semantics for [Binop.Eq]. This prunes value of [x] when given [x == y], i.e.,
      [prune_eq x y]. *)

  val prune_ne : t -> t -> t
  (** Pruning semantics for [Binop.Ne]. This prunes value of [x] when given [x != y], i.e.,
      [prune_ne x y]. *)

  val prune_lt : t -> t -> t
  (** Pruning semantics for [Binop.Lt]. This prunes value of [x] when given [x < y], i.e.,
      [prune_lt x y]. *)

  val prune_ne_zero : t -> t
  (** Prune value of [x] when given [x != 0] *)

  val prune_eq_zero : t -> t
  (** Prune value of [x] when given [x == 0] *)

  val prune_ge_one : t -> t
  (** Prune value of [x] when given [x >= 1] *)

  val prune_length_lt : t -> Itv.t -> t
  (** Prune length of [x] when given [x.length() < i] *)

  val prune_length_le : t -> Itv.t -> t
  (** Prune length of [x] when given [x.length() <= i] *)

  val prune_length_eq : t -> Itv.t -> t
  (** Prune length of [x] when given [x.length() == i] *)

  val prune_length_eq_zero : t -> t
  (** Prune length of [x] when given [x.length() == 0] *)

  val prune_length_ge_one : t -> t
  (** Prune length of [x] when given [x.length() >= 1] *)

  val prune_binop : Binop.t -> t -> t -> t
  (** Prune value of [x] when given [x bop y], i.e., [prune_binop bop x y] *)

  val add_assign_trace_elem : Location.t -> AbsLoc.PowLoc.t -> t -> t
  (** Add assign trace for given abstract locations *)

  val cast : Typ.t -> t -> t
  (** Semantics of cast. This updates type information in pointer values, rather than re-calculating
      sizes of array blocks. *)

  val subst : t -> eval_sym_trace -> Location.t -> t
  (** Substitution of symbols in the value *)

  val transform_array_length : Location.t -> f:(Itv.t -> Itv.t) -> t -> t
  (** Apply [f] on array lengths in the value *)

  module Itv : sig
    val nat : t
    (** [[0,+oo]] *)

    val pos : t
    (** [[1,+oo]] *)

    val top : t
    (** [[-oo,+oo]] *)

    val zero : t
    (** [[0,0]] *)

    val one : t
    (** [[1,1]] *)

    val zero_255 : t
    (** [[0,255]] *)

    val m1_255 : t
    (** [[-1,255]] *)

    val unknown_bool : t
    (** [[0,1]] *)
  end
end

(** Right hand side of the alias domain. See [AliasTarget]. *)
module KeyRhs : sig
  type t = AbsLoc.Loc.t [@@deriving equal]
end

module AliasTarget : sig
  type alias_typ =
    | Eq  (** The value of alias target is exactly the same to the alias key. *)
    | Le
        (** The value of alias target is less than or equal to the alias key. For example, if there
            is an alias between [%r] and [size(x)+i] with the [Le] type, it means [size(x)+i <= %r]. *)

  (** Relations between values of logical variables(registers) and program variables *)
  type t =
    | Simple of {i: IntLit.t; java_tmp: AbsLoc.Loc.t option}
        (** Since Sil distinguishes logical and program variables, we need a relation for pruning
            values of program variables. For example, a C statement [if(x){...}] is translated to
            [%r=load(x); if(%r){...}] in Sil. At the load statement, we record the alias between the
            values of [%r] and [x], then we can prune not only the value of [%r], but also that of
            [x] inside the if branch. The [java_tmp] field is an additional slot for keeping one
            more alias of temporary variable in Java. The [i] field is to express [%r=load(x)+i]. *)
    | Empty
        (** For pruning [vector.length] with [vector::empty()] results, we adopt a specific relation
            between [%r] and [v->elements], where [%r=v.empty()]. So, if [%r!=0], [v]'s array length
            ([v->elements->length]) is pruned by [=0]. On the other hand, if [%r==0], [v]'s array
            length is pruned by [>=1]. *)
    | Size of {alias_typ: alias_typ; i: IntLit.t; java_tmp: AbsLoc.Loc.t option}
        (** This is for pruning vector's length. When there is a function call, [%r=x.size()], the
            alias target for [%r] becomes [AliasTarget.size {l=x.elements}]. The [java_tmp] field is
            an additional slot for keeping one more alias of temporary variable in Java. The [i]
            field is to express [%r=x.size()+i], which is required to follow the semantics of
            [Array.add] inside loops precisely. *)
    | Fgets
        (** This is for pruning return values of [fgets]. If the result of [fgets] is not null, the
            length of return value will be pruned to greater than or equal to 1. *)
    | IteratorSimple of {i: IntLit.t; java_tmp: AbsLoc.Loc.t option}
        (** This is for tracking a relation between an iterator offset and an integer value. If [%r]
            has an alias to [IteratorSimple {l; i}], which means that [%r's iterator offset] is same
            to [l]. *)
    | IteratorOffset of {alias_typ: alias_typ; i: IntLit.t; java_tmp: AbsLoc.Loc.t option}
        (** This is for tracking a relation between an iterator offset and the length of the
            underlying collection. If [%r] has an alias to [IteratorOffset {l; i}], which means that
            [%r's iterator offset] is same to [length(l)+i]. *)
    | IteratorHasNext of {java_tmp: AbsLoc.Loc.t option}
        (** This is for tracking return values of the [hasNext] function. If [%r] has an alias to
            [HasNext {l}], which means that [%r] is same to [l.hasNext()]. *)
    | IteratorNextObject of {objc_tmp: AbsLoc.Loc.t option}
        (** This is for tracking the return values of [nextObject] function. If [%r] has an alias to
            [nextObject {l}], which means that [%r] is the same to [l.nextObject()]. *)
    | Top

  include AbstractDomain.S with type t := t

  val equal : t -> t -> bool
end

module AliasTargets : sig
  include AbstractDomain.InvertedMapS with type key = KeyRhs.t and type value = AliasTarget.t

  val exists2 : (key -> value -> key -> value -> bool) -> t -> t -> bool

  val find_simple_alias : t -> key option
  (** Find a simple alias from the set of aliases *)

  val subst : subst_loc:(key -> key option) -> t -> t
  (** Substitute alias target value *)
end

(** Alias domain for return values of callees *)
module AliasRet : sig
  type t = AliasTargets.t
end

(** [CoreVal] is similar to [Val], but its compare function is defined only on core parts, interval,
    pointers, and array blocks, of abstract value. This domain is to keep pruned values, where we do
    not need to care about the other fields in the abstract values. *)
module CoreVal : sig
  type t = Val.t
end

(** Domain to keep assumed expressions *)
module PruningExp : sig
  type t = Unknown | Binop of {bop: Binop.t; lhs: CoreVal.t; rhs: CoreVal.t}

  include AbstractDomain.S with type t := t

  val make : Binop.t -> lhs:Val.t -> rhs:Val.t -> t
end

(** Domain to keep pruned history, which are pairs of a pruned value and an assumed expression *)
module PrunedVal : sig
  include AbstractDomain.S

  val make : Val.t -> PruningExp.t -> t
end

(** [PrunePairs] is a map from abstract locations to abstract values that represents pruned results
    in the latest pruning. It uses [InvertedMap] because more pruning means smaller abstract states. *)
module PrunePairs : sig
  include AbstractDomain.InvertedMapS with type key = AbsLoc.Loc.t and type value = PrunedVal.t

  val is_reachable : t -> bool
  (** Check if a path is reachable, by using its pruned values *)
end

(** Domain to keep latest pruned values *)
module LatestPrune : sig
  include AbstractDomain.S

  val is_top : t -> bool

  val subst :
    ret_id:Ident.t -> eval_sym_trace -> Location.t -> t -> (t, [`SubstBottom | `SubstFail]) result
  (** Substitute the latest pruned values. If the result is bottom, which means the path is
      unreachable. The substitution can be failed when a callee variable can be substituted to
      multiple abstract locations. *)
end

(** Domain for reachability check *)
module Reachability : sig
  type t [@@deriving equal]

  val pp : Format.formatter -> t -> unit

  val make : LatestPrune.t -> t

  val add_latest_prune : LatestPrune.t -> t -> t
  (** Add latest pruned information to this domain *)

  val subst : t -> eval_sym_trace -> Location.t -> [`Reachable of t | `Unreachable]
  (** Substitute a reachability value *)
end

module LoopHeadLoc : sig
  type t = Location.t
end

(** Domain for memory of reachable node *)
module MemReach : sig
  (** ['has_oenv] represents an environment for on-demand symbol evaluation, which is required
      during the analysis, but not in the summary *)
  type 'has_oenv t0

  type t = GOption.some t0

  val range :
       filter_loc:(AbsLoc.Loc.t -> LoopHeadLoc.t option)
    -> node_id:Procdesc.Node.id
    -> t
    -> Polynomials.NonNegativePolynomial.t
  (** Return the multiplication of the ranges of all the abstract locations in memory that satisfy
      the function [filter_loc] which filters abstract locations we should care about, e.g., control
      variables that decide how many times a node is executed *)
end

module Mem : sig
  type 'has_oenv t0 =
    | Unreachable  (** Memory of unreachable node *)
    | ExcRaised
        (** Memory of node that can be reachable only with exception raises that we want to ignore *)
    | Reachable of 'has_oenv MemReach.t0

  (** Memory type without an environment for on-demand symbol evaluation *)
  type no_oenv_t = GOption.none t0

  (** Memory type with an environment for on-demand symbol evaluation *)
  type t = GOption.some t0

  val unset_oenv : t -> no_oenv_t

  include AbstractDomain.S with type t := t

  val pp : Format.formatter -> _ t0 -> unit

  val unreachable : t

  val is_reachable : t -> bool

  type get_summary = Procname.t -> no_oenv_t option

  val init : get_summary -> BufferOverrunOndemandEnv.t -> t

  val exc_raised : t

  val is_rep_multi_loc : AbsLoc.Loc.t -> _ t0 -> bool
  (** Check if an abstract location represents multiple concrete locations. *)

  val is_stack_loc : AbsLoc.Loc.t -> _ t0 -> bool
  (** Check if an abstract location is a stack variable, e.g., [n$0]. *)

  val set_prune_pairs : PrunePairs.t -> t -> t

  val set_latest_prune : LatestPrune.t -> t -> t

  val set_first_idx_of_null : AbsLoc.Loc.t -> Val.t -> t -> t
  (** In C string, set the index of the first null character, i.e., end of string, when called by
      [set_first_idx_of_null loc_to_string index_value mem]. *)

  val unset_first_idx_of_null : AbsLoc.Loc.t -> Val.t -> t -> t
  (** In C string, unset the index of the first null character, i.e., end of string, when called by
      [unset_first_idx_of_null loc_to_string index_value mem]. This is unsound because the index can
      be assigned as [previous index + 1] that is a heuristic to keep string lengths in some loops. *)

  val get_c_strlen : AbsLoc.PowLoc.t -> _ t0 -> Val.t
  (** Get C string length that is set/unset by [set_first_idex_of_null] and
      [unset_first_idex_of_null] *)

  val get_latest_prune : _ t0 -> LatestPrune.t

  val get_reachable_locs_from : (Pvar.t * Typ.t) list -> AbsLoc.LocSet.t -> _ t0 -> AbsLoc.LocSet.t
  (** Get reachable locations from [formals] and [locs] when called
      [get_reachable_locs_from formals locs mem] *)

  val add_stack : ?represents_multiple_values:bool -> AbsLoc.Loc.t -> Val.t -> t -> t
  (** Add an abstract value for stack variables such as [n$0] *)

  val add_stack_loc : AbsLoc.Loc.t -> t -> t

  val add_heap : ?represents_multiple_values:bool -> AbsLoc.Loc.t -> Val.t -> t -> t
  (** Add an abstract value for non-stack variables *)

  val add_heap_set : ?represents_multiple_values:bool -> AbsLoc.PowLoc.t -> Val.t -> t -> t

  val add_unknown : Ident.t * Typ.t -> location:Location.t -> t -> t
  (** Add an unknown value for stack variables *)

  val add_unknown_from : Ident.t * Typ.t -> callee_pname:Procname.t -> location:Location.t -> t -> t
  (** Add an unknown return value of [callee_pname] for stack variables *)

  val remove_vars : Var.t list -> t -> t
  (** Remove temporary variables and if Config.bo_exit_frontend_gener_vars is true also frontend
      generated variables *)

  val find : AbsLoc.Loc.t -> _ t0 -> Val.t

  val find_opt : AbsLoc.Loc.t -> _ t0 -> Val.t option

  val find_set : ?typ:Typ.t -> AbsLoc.PowLoc.t -> _ t0 -> Val.t

  val find_stack : AbsLoc.Loc.t -> _ t0 -> Val.t

  val find_alias_id : Ident.t -> _ t0 -> AliasTargets.t
  (** Find aliases between given ident *)

  val find_alias_loc : AbsLoc.Loc.t -> _ t0 -> AliasTargets.t
  (** Find aliases between given abstract location *)

  val find_simple_alias : Ident.t -> _ t0 -> (AbsLoc.Loc.t * IntLit.t) list
  (** Find simple aliases between given ident. It returns a list of pairs of abstract locations and
      integer which represent aliases [id == x + i]. *)

  val find_size_alias :
    Ident.t -> _ t0 -> (AliasTarget.alias_typ * AbsLoc.Loc.t * IntLit.t * AbsLoc.Loc.t option) list
  (** Find size aliases between given ident. It returns a list of four elements, alias type
      [== or >=], location [x], integer [i], java temporary variable [$irvar0]. This represents
      [id == $irvar0 (== or >=) x.size() + i]. *)

  val find_ret_alias : _ t0 -> AliasRet.t bottom_lifted
  (** Find aliases bound to the return variable *)

  val fgets_alias : Ident.t -> AbsLoc.PowLoc.t -> t -> t
  (** Set [fgets] alias between an ident and an abstract location *)

  val apply_latest_prune : Exp.t -> t -> t * PrunePairs.t
  (** Apply latest_prunes when given [e : Exp.t] is true. It returns pruned memory and pairs of
      pruned locations and values. *)

  val load_alias : Ident.t -> AbsLoc.Loc.t -> AliasTarget.t -> t -> t
  (** Add an alias between ident and abstract location with given alias target *)

  val load_empty_alias : Ident.t -> AbsLoc.Loc.t -> t -> t
  (** Add an empty alias between ident and abstract location, i.e., [ident == loc.empty()] *)

  val load_simple_alias : Ident.t -> AbsLoc.Loc.t -> t -> t
  (** Add a simple alias between ident and abstract location, i.e., [ident == loc] *)

  val load_size_alias : Ident.t -> AbsLoc.Loc.t -> t -> t
  (** Add a size alias between ident and abstract location, i.e., [ident == loc.size()] *)

  val store_simple_alias : AbsLoc.Loc.t -> Exp.t -> t -> t
  (** Add a simple alias between abstract location and expression, i.e., [loc == exp] *)

  val forget_size_alias : AbsLoc.PowLoc.t -> t -> t
  (** Forget size aliases of given [locs] *)

  val incr_size_alias : AbsLoc.PowLoc.t -> t -> t
  (** Update size aliases when the size of [loc] is increased by one. For example if there was an
      alias [ident == loc.size() + i], this changes it to [ident == loc.size() + i - 1], since
      [loc.size()] has been increased. *)

  val incr_or_not_size_alias : AbsLoc.PowLoc.t -> t -> t
  (** Update size aliases when the size of [loc] may be increased by one. For example if there was
      an alias [ident == loc.size() + i], this changes it to [ident >= loc.size() + i - 1] *)

  val add_iterator_has_next_alias : Ident.t -> Exp.t -> t -> t
  (** Add an [AliasTarget.IteratorHasNext] alias when [ident = iterator.hasNext()] is called *)

  val add_iterator_next_object_alias : ret_id:Ident.t -> iterator:Ident.t -> t -> t
  (** Add an [AliasTarget.IteratorNextObject] alias when [ident = iterator.nextObject()] is called *)

  val incr_iterator_simple_alias_on_call : eval_sym_trace -> callee_exit_mem:no_oenv_t -> t -> t
  (** Update [AliasTarget.IteratorSimple] alias at function calls *)

  val add_iterator_alias : Ident.t -> t -> t
  (** Add [AliasTarget.IteratorSimple] and [AliasTarget.IteratorOffset] aliases when
      [Iteratable.iterator()] is called *)

  val incr_iterator_offset_alias : Exp.t -> t -> t
  (** Update iterator offset alias when [iterator.next()] is called *)

  val update_mem : ?force_strong_update:Bool.t -> AbsLoc.PowLoc.t -> Val.t -> t -> t
  (** Add a map from locations to a value. If the given set of locations is a singleton set and the
      only element represents one concrete abstract location or force_strong_update is true, it does
      strong update. Otherwise, weak update. *)

  val strong_update : AbsLoc.PowLoc.t -> Val.t -> t -> t

  val update_latest_prune : updated_locs:AbsLoc.PowLoc.t -> Exp.t -> Exp.t -> t -> t
  (** Update latest prunes when [store(x,1)] or [store(x,0)] is called after [assume] statement *)

  val forget_unreachable_locs : formals:(Pvar.t * Typ.t) list -> t -> t
  (** Forget unreachable locations from [formals] *)

  val transform_mem : f:(Val.t -> Val.t) -> AbsLoc.PowLoc.t -> t -> t
  (** Apply [f] to values bound to given [locs] *)

  val add_cpp_iterator_cmp_alias : Ident.t -> iter_lhs:Pvar.t -> iter_rhs:Pvar.t -> t -> t
  (** Add a compare alias from ret_id to [iter_lhs: Pvar.t] and [iter_rhs: Pvar.t] comparison, i.e.,
      [ret_id -> {iter_lhs != iter_rhs}] where the meaning of != is determined by whether iter_rhs
      is coming from begin or end *)

  val add_cpp_iter_begin_alias : Pvar.t -> t -> t
  (** Add a begin alias for pvar *)

  val add_cpp_iter_end_alias : Pvar.t -> t -> t
  (** Add a end alias for pvar *)

  val find_cpp_iterator_alias : Ident.t -> t -> (Pvar.t * Pvar.t * Binop.t) option
  (** Find the cpp iterator alias [ret_id -> {iter_lhs (binop) iter_rhs}] *)

  val propagate_cpp_iter_begin_or_end_alias : new_pvar:Pvar.t -> existing_pvar:Pvar.t -> t -> t
  (** Propagate the being/end alias information from existing to new *)
end
