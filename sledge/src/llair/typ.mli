(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Types *)

type t = private
  | Function of {return: t option; args: t vector}
      (** (Global) function names have type Pointer to Function. *)
  | Integer of {bits: int}  (** Integer of given bitwidth. *)
  | Float of {bits: int; enc: [`IEEE | `Extended | `Pair]}
      (** Floating-point numbers of given bitwidth and encoding. *)
  | Pointer of {elt: t}  (** Pointer to element type. *)
  | Array of {elt: t; len: int}
      (** Statically-sized array of [len] elements of type [elt]. *)
  | Tuple of {elts: t vector; packed: bool}
      (** Anonymous aggregate of heterogeneous types. *)
  | Struct of {name: string; elts: t vector; packed: bool}
      (** Uniquely named aggregate of heterogeneous types. Every cycle of
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
val siz : t
val ptr : t
val function_ : return:t option -> args:t vector -> t
val integer : bits:int -> t
val float : bits:int -> enc:[`Extended | `IEEE | `Pair] -> t
val pointer : elt:t -> t
val array : elt:t -> len:int -> t
val tuple : t vector -> packed:bool -> t
val struct_ : name:string -> packed:bool -> t lazy_t vector -> t
val opaque : name:string -> t

(** Queries *)

val prim_bit_size_of : t -> int option

val is_sized : t -> bool
(** Holds of types which are first-class and have a statically-known size. *)

val castable : t -> t -> bool
(** Castable types are those that can be cast between without loss of
    information. An equivalence relation. *)

val convertible : t -> t -> bool
(** Convertible types are those that can be converted between, perhaps with
    some loss of information. Not transitive: some admissible conversions
    must be performed in multiple steps, such as from [Pointer] to [Integer]
    to [Array]. *)
