(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type captured_data = {capture_mode: CapturedVar.capture_mode; is_weak: bool}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

(** Names for fields of class/struct/union *)
type t [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val compare_name : t -> t -> int
(** Similar to compare, but compares only names, except template arguments. *)

val make : ?captured_data:captured_data -> Typ.Name.t -> string -> t
(** create a field of the given class and fieldname *)

val get_class_name : t -> Typ.Name.t

val get_field_name : t -> string

val mk_capture_field_in_closure : Mangled.t -> captured_data -> t

val is_capture_field_in_closure : t -> bool

val is_weak_capture_field_in_closure : t -> bool

val is_capture_field_in_closure_by_ref : t -> bool

val is_java : t -> bool

val is_java_synthetic : t -> bool
[@@warning "-unused-value-declaration"]
(** Check if the field is autogenerated/synthetic **)

val is_internal : t -> bool
(** Check if the field has the prefix "__" or "_M_" (internal field of std::thread::id) *)

(** Set for fieldnames *)
module Set : PrettyPrintable.PPSet with type elt = t

(** Map for fieldnames *)
module Map : PrettyPrintable.PPMap with type key = t

val is_java_outer_instance : t -> bool
(** Check if the field is the synthetic this$n of a nested class, used to access the n-th outer
    instance. *)

val to_string : t -> string
(** Convert a field name to a string. *)

val to_full_string : t -> string

val to_simplified_string : t -> string
(** Convert a fieldname to a simplified string with at most one-level path. For example,

    - In C++: "<ClassName>::<FieldName>"
    - In Java, ObjC, C#: "<ClassName>.<FieldName>"
    - In C: "<StructName>.<FieldName>" or "<UnionName>.<FieldName>"
    - In Erlang: "<FieldName>" *)

val patterns_match : Re.Str.regexp list -> t -> bool
(** Test whether a field full string matches to one of the regular expressions. *)

val pp : F.formatter -> t -> unit
(** Pretty print a field name. *)
