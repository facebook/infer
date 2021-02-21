(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Singleton types for compare functions *)

type ('a, 'compare_a) t = 'a -> 'a -> int

module type S = sig
  type ('a, 'compare_a) comparer := ('a, 'compare_a) t
  type t
  type compare [@@deriving compare, equal, sexp]

  val comparer : (t, compare) comparer
end

module type S1 = sig
  type ('a, 'compare_a) comparer := ('a, 'compare_a) t
  type 'a t
  type 'compare_a compare [@@deriving compare, equal, sexp]

  val comparer :
    ('a, 'compare_a) comparer -> ('a t, 'compare_a compare) comparer
end

module Make (Ord : sig
  type t [@@deriving compare]
end) =
struct
  include Ord

  type compare [@@deriving compare, equal, sexp]

  let comparer = Ord.compare
end
[@@inlined]

module Counterfeit (Ord : sig
  type t [@@deriving compare]
  type compare [@@deriving compare, equal, sexp]
end) =
struct
  include Ord

  let comparer = Ord.compare
end
[@@inlined]

module Apply (F : sig
  type ('a, 'compare_a) t [@@deriving compare]
  type 'compare_a compare [@@deriving compare, equal, sexp]
end)
(A : S) =
struct
  module A = struct
    include A

    let compare = comparer
  end

  type t = (A.t, A.compare) F.t [@@deriving compare]
  type compare = A.compare F.compare [@@deriving compare, equal, sexp]

  let comparer = compare
end
[@@inlined]

module Apply1 (F : sig
  type ('a, 'b, 'compare_a) t [@@deriving compare]
  type ('compare_a, 'compare_b) compare [@@deriving compare, equal, sexp]
end)
(A : S) =
struct
  module A = struct
    include A

    let compare = comparer
  end

  type 'b t = (A.t, 'b, A.compare) F.t [@@deriving compare]

  type 'b compare = (A.compare, 'b) F.compare
  [@@deriving compare, equal, sexp]

  let comparer = compare
end
[@@inlined]
