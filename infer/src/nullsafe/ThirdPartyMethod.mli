(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** A helper module responsible for representing nullability information for a single 3rd party method,
    as well with functionality to read this information from the 3rd party nullability repository.
    *)

open! IStd

(** E.g. "full.package.name.TypeName$NestedTypeName1$NestedTypeName2"
  *)
type fully_qualified_type = string

(** The minimum information that is needed to _uniquely_ identify the method.
   That why we don't include e.g. return type, access quilifiers, or whether the method is static
   (because Java overload resolution rules ignore these things).
   In contrast, parameter types are essential, because Java allows several methods with different types.
   *)
type unique_repr =
  { class_name: fully_qualified_type
  ; method_name: method_name
  ; param_types: fully_qualified_type list }

and method_name = Constructor | Method of string

val unique_repr_of_java_proc_name : Typ.Procname.Java.t -> unique_repr

val pp_unique_repr : Format.formatter -> unique_repr -> unit

type nullability = {ret_nullability: type_nullability; param_nullability: type_nullability list}

and type_nullability = Nullable | Nonnull

val pp_nullability : Format.formatter -> nullability -> unit

type parsing_error

val string_of_parsing_error : parsing_error -> string

val parse : string -> (unique_repr * nullability, parsing_error) result
(** Given a string representing nullability information for a given third-party method,
   return the method signature and nullability of its params and return values.
   The string should come from a repository storing 3rd party annotations.
    E.g.
    "package.name.Class$NestedClass#foo(package.name.SomeClass, @Nullable package.name.OtherClass) @Nullable" *)

val pp_parse_result : Format.formatter -> unique_repr * nullability -> unit
