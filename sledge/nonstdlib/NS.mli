(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace intended to be opened in each source file *)

include module type of NS0
module Monad = Monad

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

(** Failures *)

exception Replay of exn * Printexc.raw_backtrace * Sexp.t
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

(** Extensions *)

module Invariant : sig
  include module type of Core.Invariant

  exception
    Violation of
      exn * Printexc.raw_backtrace * Source_code_position.t * Sexp.t
end

(** Containers *)

module Option = Option
include module type of Option.Import
module List = List

module Array : sig
  include module type of Array

  type 'a t = 'a Array.t [@@deriving compare, equal, hash, sexp]

  module Import : sig
    type 'a array = 'a t [@@deriving compare, equal, hash, sexp]
  end

  val pp : (unit, unit) fmt -> 'a pp -> 'a array pp

  val map_endo : 'a t -> f:('a -> 'a) -> 'a t
  (** Like map, but specialized to require [f] to be an endofunction, which
      enables preserving [==] if [f] preserves [==] of every element. *)

  val fold_map_inplace :
    'a array -> init:'s -> f:('s -> 'a -> 's * 'a) -> 's

  val to_list_rev_map : 'a array -> f:('a -> 'b) -> 'b list
end

include module type of Array.Import
module IArray = IArray
include module type of IArray.Import
module Set = Set
module Map = Map
module Multiset = Multiset

(** Data types *)

module String : sig
  include
    module type of Core.String
      with module Map := Core.String.Map
      with module Set := Core.String.Set

  module Map : Map.S with type key = string
  module Set : Set.S with type elt = string
end

module Int : sig
  include module type of Stdlib.Int

  include
    module type of Core.Int
      with module Map := Core.Int.Map
      with module Set := Core.Int.Set

  module Map : Map.S with type key = int
  module Set : Set.S with type elt = int
end

module Q : sig
  include module type of struct
    include Q
  end

  val of_z : Z.t -> t
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val pp : t pp
  val pow : t -> int -> t
end

module Z : sig
  include module type of struct
    include Z
  end

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

module Timer = Timer
