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
  type c_sym_array_kind = CSymArray_Array | CSymArray_Pointer [@@deriving compare]

  type partial = private
    | Pvar of Pvar.t
    | Index of c_sym_array_kind * partial
    | Field of Typ.Fieldname.t * partial
  [@@deriving compare]

  type t = private Normal of partial | Offset of partial | Length of partial

  val pp : F.formatter -> t -> unit

  val pp_partial : F.formatter -> partial -> unit

  val of_pvar : Pvar.t -> partial

  val index : array_kind:c_sym_array_kind -> partial -> partial

  val field : partial -> Typ.Fieldname.t -> partial

  val normal : partial -> t

  val offset : partial -> t

  val length : partial -> t
end

module Symbol : sig
  type t

  type 'res eval = t -> 'res AbstractDomain.Types.bottom_lifted

  val compare : t -> t -> int

  val is_unsigned : t -> bool

  val pp : F.formatter -> t -> unit

  val equal : t -> t -> bool

  val paths_equal : t -> t -> bool

  val path : t -> SymbolPath.t

  val bound_end : t -> BoundEnd.t
end

module SymbolSet : sig
  include PrettyPrintable.PPSet with type elt = Symbol.t

  val union3 : t -> t -> t -> t
end

module SymbolMap : sig
  include PrettyPrintable.PPMap with type key = Symbol.t

  val for_all2 : f:(key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool
end

module SymbolTable : sig
  type t

  val empty : unit -> t

  val lookup :
    unsigned:bool -> Typ.Procname.t -> SymbolPath.t -> t -> Counter.t -> Symbol.t * Symbol.t
end
