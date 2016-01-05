(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Named types. *)
type t =
  | TN_typedef of Mangled.t
  | TN_enum of Mangled.t
  | TN_csu of Csu.t * Mangled.t

(** convert the typename to a string *)
val to_string : t -> string

val pp : Format.formatter -> t -> unit

(** name of the typename without qualifier *)
val name : t -> string

(** Comparison for typenames *)
val compare : t -> t -> int

(** Equality for typenames *)
val equal : t -> t -> bool

module Java : sig

  (** Create a typename from a Java classname in the form "package.class" *)
  val from_string : string -> t

end

module Set : Set.S with type elt = t
