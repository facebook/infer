(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | NullConst of Location.t  (** A null literal in the source *)
  | NonnullConst of Location.t  (** A constant (not equal to null) in the source. *)
  | Field of field_origin  (** A field access (result of expression `some_object.some_field`) *)
  | Formal of Mangled.t  (** A formal parameter *)
  | MethodCall of method_call_origin  (** A result of a method call *)
  | New  (** A new object creation *)
  | ArrayLengthResult  (** integer value - result of accessing array.length *)
  | ONone  (** No origin is known *)
  | Undef  (** Undefined value before initialization *)
[@@deriving compare]

and field_origin =
  { object_origin: t  (** field's object origin (object is before field access operator `.`)  *)
  ; field_name: Typ.Fieldname.t
  ; access_loc: Location.t }

and method_call_origin =
  { pname: Typ.Procname.t
  ; call_loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_library: bool }

val equal : t -> t -> bool

val get_description : t -> TypeErr.origin_descr option
(** Get a description to be used for error messages. *)

val join : t -> t -> t
(** Join with left priority *)

val to_string : t -> string
(** Raw string representation. *)
