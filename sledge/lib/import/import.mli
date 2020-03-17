(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace opened in each source file by the build system *)

include module type of Stdio
module Command = Core.Command
include module type of Import0

(** Tuple operations *)

val fst3 : 'a * _ * _ -> 'a
(** First projection from a triple. *)

val snd3 : _ * 'a * _ -> 'a
(** Second projection from a triple. *)

val trd3 : _ * _ * 'a -> 'a
(** Third projection from a triple. *)

(** Function combinators *)

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

(** Failures *)

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

type 'a or_error = ('a, exn * Caml.Printexc.raw_backtrace) result

val or_error : ('a -> 'b) -> 'a -> unit -> 'b or_error
(** [or_error f x] runs [f x] and converts unhandled exceptions to errors. *)

(** Extensions *)

module Invariant : module type of Base.Invariant
module Unit = Base.Unit

type unit = Unit.t [@@deriving compare, equal, hash, sexp]

module Bool = Base.Bool

type bool = Bool.t [@@deriving compare, equal, hash, sexp]

module Char = Base.Char

type char = Char.t [@@deriving compare, equal, hash, sexp]

module Int = Base.Int

type int = Int.t [@@deriving compare, equal, hash, sexp]

module Int64 = Base.Int64

type int64 = Int64.t [@@deriving compare, equal, hash, sexp]

module Z : sig
  include module type of struct include Z end

  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val pp : t pp
  val true_ : t
  val false_ : t
  val of_bool : bool -> t
  val is_true : t -> bool
  val is_false : t -> bool
end

module Q : sig
  include module type of struct include Q end

  val of_z : Z.t -> t
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val pp : t pp
end

module String : sig
  include module type of Base.String

  type t = String.t [@@deriving compare, equal, hash, sexp]

  module Map : Map.S with type key = string
end

type string = String.t [@@deriving compare, equal, hash, sexp]

module Option = Option

type 'a option = 'a Option.t [@@deriving compare, equal, hash, sexp]

include module type of Option.Monad_infix
include module type of Option.Monad_syntax with type 'a t = 'a option
module Result = Base.Result

module Array : sig
  include module type of Base.Array

  val pp : (unit, unit) fmt -> 'a pp -> 'a array pp
end

module Vector = Vector
include module type of Vector.Infix
module List = List

type 'a list = 'a List.t [@@deriving compare, equal, hash, sexp]

module Hash_queue = Core_kernel.Hash_queue
module Set = Set
module Hash_set = Base.Hash_set
module Map = Map
module Qset = Qset
module Hashtbl = Base.Hashtbl
