(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Singleton types for compare functions *)

(** A comparer [('a, 'compare_a) t] for type ['a] is a "compare" function of
    type ['a -> 'a -> int] tagged with a phantom type ['compare_a] acting as
    a singleton type denoting an individual compare function. *)
type ('a, 'compare_a) t = private 'a -> 'a -> int

module type S = sig
  type ('a, 'compare_a) comparer := ('a, 'compare_a) t
  type t

  (** [compare] types are equipped with functions to support use of
      [@@deriving compare, equal, sexp] on types parameterized by such
      singleton types for compare functions. These derived functions are
      never actually called, since the compare type parameters are phantom. *)
  type compare [@@deriving compare, equal, sexp]

  val comparer : (t, compare) comparer
end

(** [Make] takes a [compare] function, mints a fresh [compare] type to act
    as a singleton type denoting that one compare function, and returns the
    [compare] function at a type stamped with its singleton type. In this
    way, [Make] applied to two different compare functions for the same type
    of values yields comparers with incompatible types. *)
module Make (Ord : sig
  type t [@@deriving compare]
end) : S with type t = Ord.t

(** [Counterfeit] takes a compare function and type and yields a comparer
    that asserts that the given [compare] type is a singleton for the given
    [compare] function. This is not checked by the type system. It is the
    client's responsibility to ensure that distinct types are provided for
    distinct compare functions. If the same type is used for multiple
    functions, then [Counterfeit] will produce type-compatible comparers
    even though the wrapped compare functions differ. *)
module Counterfeit (Ord : sig
  type t [@@deriving compare]
  type compare [@@deriving compare, equal, sexp]
end) : S with type t = Ord.t with type compare = Ord.compare

(** [Apply (F) (A)] takes a type [('a, 'compare_a) F.t] with a type
    parameter ['a] and a compare type ['compare_a] for ['a], and a comparer
    [A], and creates a comparer for [F.t] with ['a] instantiated to [A.t]. *)
module Apply (F : sig
  type ('a, 'compare_a) t [@@deriving compare]
  type 'compare_a compare [@@deriving compare, equal, sexp]
end)
(A : S) : sig
  type t = (A.t, A.compare) F.t [@@deriving compare]

  include S with type t := t with type compare = A.compare F.compare
end

module type S1 = sig
  type ('a, 'compare_a) comparer := ('a, 'compare_a) t
  type 'a t
  type 'compare_a compare [@@deriving compare, equal, sexp]

  val comparer :
    ('a, 'compare_a) comparer -> ('a t, 'compare_a compare) comparer
end

(** [Apply1 (F) (A)] takes a type [('a, 'b, 'compare_a) F.t] with two type
    parameters ['a], ['b] and a compare type ['compare_a] for ['a], and a
    comparer [A], and creates a comparer for [F.t] with ['a] instantiated to
    [A.t]. *)
module Apply1 (F : sig
  type ('a, 'b, 'compare_a) t [@@deriving compare]
  type ('compare_a, 'compare_b) compare [@@deriving compare, equal, sexp]
end)
(A : S) : sig
  type 'b t = (A.t, 'b, A.compare) F.t [@@deriving compare]

  include
    S1
      with type 'b t := 'b t
      with type 'compare_b compare = (A.compare, 'compare_b) F.compare
end
