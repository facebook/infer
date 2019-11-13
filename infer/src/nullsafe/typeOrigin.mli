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
  | MethodParameter of AnnotatedSignature.param_signature  (** A method's parameter *)
  | This (* `this` object. Can not be null, according to Java rules. *)
  | MethodCall of method_call_origin  (** A result of a method call *)
  | New  (** A new object creation *)
  | ArrayLengthResult  (** integer value - result of accessing array.length *)
  | InferredNonnull of {previous_origin: t}
      (** The value is inferred as non-null during flow-sensitive type inference
          (most commonly from relevant condition branch or assertion explicitly comparing the value with `null`) *)
  (* Below are two special values. *)
  | OptimisticFallback
      (** Something went wrong during typechecking.
          We fall back to optimistic (not-nullable) type to reduce potential non-actionable false positives.
          Ideally we should not see these instances. They should be either processed gracefully
          (and a dedicated type constructor should be added), or fixed.
          T54687014 tracks unsoundness issues caused by this type. *)
  | Undef  (** Undefined value before initialization *)
[@@deriving compare]

and field_origin =
  { object_origin: t  (** field's object origin (object is before field access operator `.`)  *)
  ; field_name: Typ.Fieldname.t
  ; field_type: AnnotatedType.t
  ; access_loc: Location.t }

and method_call_origin =
  { pname: Typ.Procname.t
  ; call_loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_library: bool }

val equal : t -> t -> bool

val get_nullability : t -> Nullability.t

val get_description : t -> TypeErr.origin_descr option
(** Get a description to be used for error messages. *)

val join : t -> t -> t
(** Join with left priority *)

val to_string : t -> string
(** Raw string representation. *)
