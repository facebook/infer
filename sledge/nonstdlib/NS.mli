(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace intended to be opened in each source file *)

(** Support for [@@deriving compare, equal, hash, sexp] on builtin types *)

include module type of Ppx_compare_lib.Builtin
module Hash = Ppx_hash_lib.Std.Hash
include module type of Hash.Builtin
module Sexp = Sexplib.Sexp
include module type of Ppx_sexp_conv_lib.Conv

(** Comparison *)

val min : int -> int -> int
val max : int -> int -> int
external ( = ) : int -> int -> bool = "%equal"
external ( <> ) : int -> int -> bool = "%notequal"
external ( < ) : int -> int -> bool = "%lessthan"
external ( > ) : int -> int -> bool = "%greaterthan"
external ( <= ) : int -> int -> bool = "%lessequal"
external ( >= ) : int -> int -> bool = "%greaterequal"
external compare : int -> int -> int = "%compare"
external equal : int -> int -> bool = "%equal"

(** Polymorphic comparison and hashing *)
module Poly : sig
  external ( = ) : 'a -> 'a -> bool = "%equal"
  external ( <> ) : 'a -> 'a -> bool = "%notequal"
  external ( < ) : 'a -> 'a -> bool = "%lessthan"
  external ( > ) : 'a -> 'a -> bool = "%greaterthan"
  external ( <= ) : 'a -> 'a -> bool = "%lessequal"
  external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
  external compare : 'a -> 'a -> int = "%compare"
  external equal : 'a -> 'a -> bool = "%equal"
  val min : 'a -> 'a -> 'a
  val max : 'a -> 'a -> 'a
  val hash : 'a -> int
end

module Ord = Containers.Ord

(** Function combinators *)

val ( let@ ) : ('a -> 'b) -> 'a -> 'b
(** [let@ x = e in b] is equivalent to [e @@ fun x -> b], that is,
    [e (fun x -> b)] *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Composition of functions: [(f >> g) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Reverse composition of functions: [(g << f) x] is exactly equivalent to
    [g (f (x))]. Left associative. *)

val ( $ ) : ('a -> unit) -> ('a -> 'b) -> 'a -> 'b
(** Sequential composition of functions: [(f $ g) x] is exactly equivalent
    to [(f x) ; (g x)]. Left associative. *)

val ( $> ) : 'a -> ('a -> unit) -> 'a
(** Reverse apply and ignore function: [x $> f] is exactly equivalent to
    [f x ; x]. Left associative. *)

val ( <$ ) : ('a -> unit) -> 'a -> 'a
(** Apply and ignore function: [f <$ x] is exactly equivalent to [f x ; x].
    Left associative. *)

(** Tuple operations *)

val fst3 : 'a * _ * _ -> 'a
(** First projection from a triple. *)

val snd3 : _ * 'b * _ -> 'b
(** Second projection from a triple. *)

val trd3 : _ * _ * 'c -> 'c
(** Third projection from a triple. *)

(** Map-and-construct operations that preserve physical equality *)

val map1 : ('a -> 'a) -> 'b -> ('a -> 'b) -> 'a -> 'b
val map2 : ('a -> 'a) -> 'b -> ('a -> 'a -> 'b) -> 'a -> 'a -> 'b

val map3 :
  ('a -> 'a) -> 'b -> ('a -> 'a -> 'a -> 'b) -> 'a -> 'a -> 'a -> 'b

val mapN : ('a -> 'a) -> 'b -> ('a array -> 'b) -> 'a array -> 'b

val fold_map_from_map :
     ('a -> f:('b -> 'c) -> 'd)
  -> 'a
  -> 's
  -> f:('b -> 's -> 'c * 's)
  -> 'd * 's

(** Pretty-printing *)

(** Pretty-printer for argument type. *)
type 'a pp = Format.formatter -> 'a -> unit

(** Format strings. *)
type ('a, 'b) fmt = ('a, Format.formatter, unit, 'b) format4

(** Monadic syntax *)

module type Applicative_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module type Monad_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

(** Monads *)

module Monad = Monad

(** Data types *)

module Sign = Sign
module Char = Containers.Char
module Int = Int
module Z = Z_ext
module Q = Q_ext
module Float = Float
module String = String

(** Iterators *)

module Iter = Iter
include module type of Iter.Import

(** Containers *)

module Comparer = Comparer
module Option = Option
include module type of Option.Import

type 'a zero_one_many = Zero | One of 'a | Many
type ('a, 'b) zero_one_many2 = Zero2 | One2 of 'a * 'b | Many2

module Either : sig
  type ('a, 'b) t = Left of 'a | Right of 'b

  val left : 'a -> ('a, 'b) t
  val right : 'a -> ('b, 'a) t
end

module Pair = Containers.Pair
module List = List
module Array = Array
module IArray = IArray
include module type of IArray.Import
module Set = NSSet
module Map = NSMap
module Multiset = Multiset
module Bijection = CCBijection [@@warning "-49"]

module FHeap : sig
  include module type of Fheap

  val remove_top_exn : 'a t -> 'a t
end

module HashSet = HashSet
module HashTable = HashTable
module HashQueue = Core_kernel.Hash_queue

(** Input / Output *)

module In_channel = Stdio.In_channel
module Out_channel = Stdio.Out_channel

(** System interfaces *)

module Sys = Sys
module Timer = Timer

(** Invariants *)
module Invariant : sig
  exception Violation of exn * Lexing.position * Sexp.t

  val invariant :
    Lexing.position -> 'a -> ('a -> Sexp.t) -> (unit -> unit) -> unit

  module type S = sig
    type t

    val invariant : t -> unit
  end
end

(** Failures *)

exception Replay of exn * Sexp.t
exception Unimplemented of string

val fail : ('a, unit -> _) fmt -> 'a
(** Emit a message at the current indentation level, and raise a [Failure]
    exception indicating a fatal error. *)

val todo : ('a, unit -> _) fmt -> 'a
(** Raise an [Unimplemented] exception indicating that an input is valid but
    not handled by the current implementation. *)

val warn : ('a, unit -> unit) fmt -> 'a
(** Issue a warning for a survivable problem. *)

(** Assertions *)

val assertf : bool -> ('a, unit -> unit) fmt -> 'a
(** Raise an [Failure] exception if the bool argument is false, indicating
    that the expected condition was not satisfied. *)

val checkf : bool -> ('a, unit -> bool) fmt -> 'a
(** As [assertf] but returns the argument bool. *)

val check : ('a -> unit) -> 'a -> 'a
(** Assert that function does not raise on argument, and return argument. *)

val violates : ('a -> unit) -> 'a -> _
(** Assert that function raises on argument. *)

(**)

module Hashtbl : sig end [@@deprecated "Use HashTable instead of Hashtbl"]
