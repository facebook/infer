(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Types *)

type t = private
  | Function of {return: t option; args: t iarray}
      (** (Global) function names have type Pointer to Function. *)
  | Integer of {bits: int; byts: int}  (** Integer of given bitwidth. *)
  | Float of
      {bits: int; byts: int; enc: [`Brain | `IEEE | `Extended | `Pair]}
      (** Floating-point numbers of given bitwidth and encoding. *)
  | Pointer of {elt: t}  (** Pointer to element type. *)
  | Array of {elt: t; len: int; bits: int; byts: int}
      (** Statically-sized array of [len] elements of type [elt]. *)
  | Tuple of {elts: (int * t) iarray; bits: int; byts: int}
      (** Anonymous aggregate of heterogeneous types. *)
  | Struct of {name: string; elts: (int * t) iarray; bits: int; byts: int}
      (** Uniquely named aggregate of heterogeneous types. Elements are
          specified by their byte offset and their type. Every cycle of
          recursive types contains a [Struct]. NOTE: recursive [Struct]
          types are represented by cyclic values. *)
  | Opaque of {name: string}
      (** Uniquely named aggregate type whose definition is hidden. *)
[@@deriving compare, equal, hash, sexp]

val pp : t pp
val pp_defn : t pp

include Invariant.S with type t := t

(** Constructors *)

val bool : t
val byt : t
val int : t
val siz : t
val ptr : t
val function_ : return:t option -> args:t iarray -> t
val integer : bits:int -> byts:int -> t

val float :
  bits:int -> byts:int -> enc:[`Brain | `Extended | `IEEE | `Pair] -> t

val pointer : elt:t -> t
val array : elt:t -> len:int -> bits:int -> byts:int -> t
val tuple : (int * t) iarray -> bits:int -> byts:int -> t

val struct_ :
  name:string -> bits:int -> byts:int -> (int * t) lazy_t iarray -> t

val opaque : name:string -> t

(** Queries *)

val is_sized : t -> bool
(** Holds of types which are first-class and have a statically-known size. *)

val bit_size_of : t -> int
(** The number of bits required to hold a value of the given type. Raises
    unless [is_sized] holds. *)

val size_of : t -> int
(** The number of bytes between adjacent values of the given type, including
    alignment padding. Raises unless is_sized holds. *)

val offset_length_of_elt : t -> int -> int * int
(** The offset in bytes to, and number of bytes occupied by, the given
    element of an aggretage type. Raises if type is not an aggregate or
    index is invalid. *)

val equivalent : t -> t -> bool
(** Equivalent types are those that denote the same sets of values in the
    semantic model. An equivalence relation. *)

val castable : t -> t -> bool
(** Castable types are those that can be cast between without loss of
    information. An equivalence relation. *)

val convertible : t -> t -> bool
(** Convertible types are those that can be converted between, perhaps with
    some loss of information. Not transitive: some admissible conversions
    must be performed in multiple steps, such as from [Pointer] to [Integer]
    to [Array]. *)
