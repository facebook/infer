(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Support for localisation *)

module Tags : sig
  type t
end

(** description field of error messages *)
type error_desc =
  {descriptions: string list; suggestion: string option; tags: Tags.t; dotty: string option}
[@@deriving compare]

val verbatim_desc : ?suggestion:string -> string -> error_desc
(** verbatim desc from a string and suggestion, not to be used for user-visible descs *)

val error_desc_get_bucket : error_desc -> string option
(** get the bucket value of an error_desc, if any *)

val error_desc_hash : error_desc -> int
(** hash function for error_desc *)

val error_desc_equal : error_desc -> error_desc -> bool
(** equality for error_desc *)

val pp_error_qualifier : Format.formatter -> error_desc -> unit
(** pretty print an error qualifier *)

val pp_error_desc : Format.formatter -> error_desc -> unit
(** pretty print a full error description with suggestion *)

val error_desc_get_dotty : error_desc -> string option

(** Description functions for error messages *)

val desc_condition_always_true_false : IntLit.t -> string option -> Location.t -> error_desc
