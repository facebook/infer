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
  | Integer of {bits: int}  (** Integer of given bitwidth *)
  | Float of {bits: int; enc: [`IEEE | `Extended | `Pair]}
      (** Floating-point numbers of given bitwidth and encoding *)
  | Pointer of {elt: t}  (** Pointer to element type *)
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
  | Bytes  (** Dynamically-sized byte-array. *)

val compare : t -> t -> int

val equal : t -> t -> bool

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

val fmt : t fmt

val fmt_defn : t fmt

(** Constructors *)

val mkFunction : return:t option -> args:t vector -> t

val mkInteger : bits:int -> t

val mkFloat : bits:int -> enc:[`Extended | `IEEE | `Pair] -> t

val mkPointer : elt:t -> t

val mkArray : elt:t -> len:int -> t

val mkTuple : packed:bool -> t vector -> t

val mkStruct : name:string -> packed:bool -> t lazy_t vector -> t

val mkOpaque : name:string -> t

val mkBytes : t

(** Special types *)

val i1 : t
(** Booleans are represented by 1-bit integers. *)

val i8p : t
(** Byte-pointers are effectively a universal type. *)

(** Queries *)

val compatible : t -> t -> bool
(** Compatible types are those that can be cast or converted between,
    perhaps with some loss of information. *)

val is_sized : t -> bool
(** Holds of types which are first-class and have a statically-known size. *)
