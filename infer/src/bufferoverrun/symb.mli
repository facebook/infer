(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format

module BoundEnd : sig
  type t = LowerBound | UpperBound

  val neg : t -> t
end

module SymbolPath : sig
  type partial

  type t

  val of_pvar : Pvar.t -> partial

  val index : partial -> partial

  val field : partial -> Typ.Fieldname.t -> partial

  val normal : partial -> t

  val offset : partial -> t

  val length : partial -> t
end

module Symbol : sig
  type t

  val compare : t -> t -> int

  val is_unsigned : t -> bool

  val pp : F.formatter -> t -> unit

  val equal : t -> t -> bool

  val paths_equal : t -> t -> bool
end

module SymbolMap : sig
  include PrettyPrintable.PPMap with type key = Symbol.t

  val for_all2 : f:(key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool

  val is_singleton : 'a t -> (key * 'a) option
end

module SymbolTable : sig
  module M : PrettyPrintable.PPMap with type key = SymbolPath.t

  type summary_t

  val pp : F.formatter -> summary_t -> unit

  val find_opt : SymbolPath.t -> summary_t -> (Symbol.t * Symbol.t) option

  type t

  val empty : unit -> t

  val summary_of : t -> summary_t

  val lookup :
    unsigned:bool -> Typ.Procname.t -> M.key -> t -> Counter.t -> SymbolMap.key * SymbolMap.key
end
