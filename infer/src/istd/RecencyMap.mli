(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format

module type Config = sig
  val limit : int
  (** the maximum number [N] of bindings to keep around *)
end

(** A functional map interface where only the [N] most recently-accessed elements are guaranteed to
    be persisted, similarly to an LRU cache. The map stores at most [2*N] elements. *)
module type S = sig
  (** Note that the derived [compare] and [equal] functions are sensitive to the underlying
      implementation and in particular won't equate some objects that denote the same map. *)
  type t [@@deriving compare, equal]

  type key

  type value

  val pp : F.formatter -> t -> unit

  val empty : t

  val add : key -> value -> t -> t

  val bindings : t -> (key * value) list

  val exists : t -> f:(key * value -> bool) -> bool

  val for_all : t -> f:(key * value -> bool) -> bool

  val filter : t -> f:(key * value -> bool) -> t

  val find_opt : key -> t -> value option

  val iter : t -> f:(key * value -> unit) -> unit

  val fold : t -> init:'acc -> f:('acc -> key * value -> 'acc) -> 'acc

  val fold_map : t -> init:'acc -> f:('acc -> key -> value -> 'acc * value) -> 'acc * t

  val is_empty : t -> bool

  val map : t -> f:(value -> value) -> t

  val mapi : t -> f:(key -> value -> value) -> t

  val mem : t -> key -> bool

  val union_left_biased : t -> t -> t

  val to_seq : t -> (key * value) Seq.t
end

module Make
    (Key : PrettyPrintable.PrintableEquatableOrderedType)
    (Value : PrettyPrintable.PrintableOrderedType)
    (Config : Config) : S with type key = Key.t and type value = Value.t
