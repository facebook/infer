(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val pp : F.formatter -> t -> unit

val pp_plain_name : F.formatter -> t -> unit

val to_string : t -> string

val mangled : t -> string
(** retrieve the mangled name. Does NOT include any type arguments — use [to_string] for a
    human-readable form that includes them, or [compare]/[equal] for structural identity that does.
*)

val args : t -> t list
(** retrieve the type arguments. Empty for non-parameterized class names (the common case);
    non-empty for parameterized Swift types such as [__infer_tuple_class<X, Y>]. *)

val of_string : ?plain_name:string -> ?args:t list -> string -> t
(** make a class name out of its mangled name and optionally its [plain_name] and type arguments. NB
    only non-empty [plain_names] can be used; [args] defaults to the empty list. *)

val swift_alloc_unknown_type : t
(** placeholder class name for Swift heap allocations whose dynamic type the frontend could not
    recover (e.g. unmodelled [swift_allocObject] calls without a [Sizeof] size argument). Used as a
    stable type token so PulseRefCounting can classify edges through the object. *)
