(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** A helper module responsible for representing nullability information for a single 3rd party
    method, as well with functionality to read this information from the 3rd party nullability
    repository. *)

open! IStd

type t =
  { class_name: fully_qualified_type
  ; method_name: method_name
  ; ret_nullability: type_nullability
  ; params: (fully_qualified_type * type_nullability) list }

and fully_qualified_type = string [@@deriving sexp]

and method_name = Constructor | Method of string [@@deriving sexp]

and type_nullability = Nullable | Nonnull [@@deriving sexp]

type parsing_error

val string_of_parsing_error : parsing_error -> string

val parse : string -> (t, parsing_error) result
(** Given a string representing nullability information for a given third-party method, return the
    method signature and nullability of its params and return values. The string should come from a
    repository storing 3rd party annotations. E.g.
    ["package.name.Class$NestedClass#foo(package.name.SomeClass, @Nullable package.name.OtherClass)
    @Nullable"] *)

val to_canonical_string : t -> string

val pp : Format.formatter -> t -> unit
(** String representation as it can be parsed via [parse]
    <Class>#<method>(<params>)<ret_nullability> *)
