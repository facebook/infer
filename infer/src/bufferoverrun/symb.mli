(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module BoundEnd : sig
  type t = LowerBound | UpperBound

  val neg : t -> t
end

module SymbolPath : sig
  type deref_kind = Deref_ArrayIndex | Deref_COneValuePointer | Deref_CPointer | Deref_JavaPointer
  [@@deriving compare]

  type partial = private
    | Pvar of Pvar.t
    | Deref of deref_kind * partial
    | Field of {fn: Typ.Fieldname.t; prefix: partial; typ: Typ.t option}
    | Callsite of {ret_typ: Typ.t; cs: CallSite.t}
    | StarField of {last_field: Typ.Fieldname.t; prefix: partial}
        (**
          Represents a path starting with [prefix] and ending with the field [last_field],
          the middle can be anything.
          Invariants:
          - There is at most one StarField
          - StarField excluded, there are no duplicate fieldnames
          - StarField can only be followed by Deref elements
        *)
  [@@deriving compare]

  type t = private
    | Normal of partial
    | Offset of {p: partial; is_void: bool}
    | Length of {p: partial; is_void: bool}
    | Modeled of partial

  val equal_partial : partial -> partial -> bool

  val pp_mark : markup:bool -> F.formatter -> t -> unit

  val pp_partial : F.formatter -> partial -> unit

  val pp_partial_paren : paren:bool -> F.formatter -> partial -> unit

  val pp_pointer : paren:bool -> F.formatter -> partial -> unit

  val of_pvar : Pvar.t -> partial

  val of_callsite : ret_typ:Typ.t -> CallSite.t -> partial

  val deref : deref_kind:deref_kind -> partial -> partial

  val field : ?typ:Typ.t -> partial -> Typ.Fieldname.t -> partial

  val star_field : partial -> Typ.Fieldname.t -> partial

  val normal : partial -> t

  val offset : partial -> is_void:bool -> t

  val length : partial -> is_void:bool -> t

  val modeled : partial -> t

  val is_this : partial -> bool

  val get_pvar : partial -> Pvar.t option

  val represents_multiple_values : partial -> bool

  val represents_multiple_values_sound : partial -> bool

  val represents_callsite_sound_partial : partial -> bool

  val exists_pvar_partial : f:(Pvar.t -> bool) -> partial -> bool

  val exists_str_partial : f:(string -> bool) -> partial -> bool

  val is_void_ptr_path : t -> bool

  val is_cpp_vector_elem : partial -> bool

  val is_global_partial : partial -> bool
end

module Symbol : sig
  type t

  type 'res eval = t -> BoundEnd.t -> 'res AbstractDomain.Types.bottom_lifted

  val compare : t -> t -> int

  val is_unsigned : t -> bool

  val is_non_int : t -> bool

  val is_global : t -> bool

  val pp_mark : markup:bool -> F.formatter -> t -> unit

  val equal : t -> t -> bool

  val paths_equal : t -> t -> bool

  val path : t -> SymbolPath.t

  val check_bound_end : t -> BoundEnd.t -> unit

  type make_t = unsigned:bool -> ?non_int:bool -> SymbolPath.t -> t

  val make_onevalue : make_t

  val make_boundend : BoundEnd.t -> make_t

  val exists_str : f:(string -> bool) -> t -> bool
end

module SymbolSet : sig
  include PrettyPrintable.PPSet with type elt = Symbol.t

  val union3 : t -> t -> t -> t
end

module SymbolMap : sig
  include PrettyPrintable.PPMap with type key = Symbol.t

  val for_all2 : f:(key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool
end
