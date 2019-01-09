(*
 * Copyright (c) 2017-present, Facebook, Inc.
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
  type deref_kind =
    | Deref_ArrayIndex
    | Deref_COneValuePointer
    | Deref_CPointer
    | Deref_JavaPointer
  [@@deriving compare]

  type partial = private
    | Pvar of Pvar.t
    | Deref of deref_kind * partial
    | Field of Typ.Fieldname.t * partial
    | Callsite of {ret_typ: Typ.t; cs: CallSite.t}
  [@@deriving compare]

  type t = private Normal of partial | Offset of partial | Length of partial

  val equal_partial : partial -> partial -> bool

  val pp_mark : markup:bool -> F.formatter -> t -> unit

  val pp_partial : F.formatter -> partial -> unit

  val pp_partial_paren : paren:bool -> F.formatter -> partial -> unit

  val pp_pointer : paren:bool -> F.formatter -> partial -> unit

  val of_pvar : Pvar.t -> partial

  val of_callsite : ret_typ:Typ.t -> CallSite.t -> partial

  val deref : deref_kind:deref_kind -> partial -> partial

  val field : partial -> Typ.Fieldname.t -> partial

  val normal : partial -> t

  val offset : partial -> t

  val length : partial -> t

  val is_this : partial -> bool

  val get_pvar : partial -> Pvar.t option

  val represents_multiple_values : partial -> bool

  val represents_multiple_values_sound : partial -> bool

  val represents_callsite_sound_partial : partial -> bool

  val exists_str_partial : f:(string -> bool) -> partial -> bool
end

module Symbol : sig
  type t

  type 'res eval = t -> BoundEnd.t -> 'res AbstractDomain.Types.bottom_lifted

  val compare : t -> t -> int

  val is_unsigned : t -> bool

  val pp_mark : markup:bool -> F.formatter -> t -> unit

  val equal : t -> t -> bool

  val paths_equal : t -> t -> bool

  val path : t -> SymbolPath.t

  val assert_bound_end : t -> BoundEnd.t -> unit

  val make_onevalue : unsigned:bool -> SymbolPath.t -> t

  val make_boundend : BoundEnd.t -> unsigned:bool -> SymbolPath.t -> t

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
