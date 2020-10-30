(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type method_parameter_origin = Normal of AnnotatedSignature.param_signature | ObjectEqualsOverride

type method_call_origin =
  { pname: Procname.Java.t
  ; call_loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_defined: bool }
[@@deriving compare]

type t =
  | NullConst of Location.t  (** A null literal in the source *)
  | NonnullConst of Location.t  (** A constant (not equal to null) in the source. *)
  | Field of
      { object_origin: t  (** field's object origin (object is before field access operator `.`) *)
      ; field_name: Fieldname.t
      ; field_type: AnnotatedType.t
      ; access_loc: Location.t }
      (** A field access (result of expression `some_object.some_field`) *)
  | CurrMethodParameter of method_parameter_origin
      (** Parameter of a method we are currently in, *)
  | This (* `this` object. Can not be null, according to Java rules. *)
  | MethodCall of method_call_origin
  | CallToGetKnownToContainsKey
      (** This is a result of accessing a map element that is known to contains this particular key,
          normally because it was explicitly checked for presense before *)
  | New  (** A new object creation *)
  | ArrayLengthResult  (** integer value - result of accessing array.length *)
  | ArrayAccess  (** Result of accessing an array by index *)
  | InferredNonnull of {previous_origin: t}
      (** The value is inferred as non-null during flow-sensitive type inference (most commonly from
          relevant condition branch or assertion explicitly comparing the value with `null`) *)
  | OptimisticFallback
      (** A special case: technical type variant. Indicates either cases when something went wrong
          during typechecking, and some cases that should be expressed in a better way than using
          this type. We fall back to optimistic (not-nullable) type to reduce potential
          non-actionable false positives. Ideally we should not see these instances. They should be
          either processed gracefully (and a dedicated type constructor should be added), or fixed.
          T54687014 tracks unsoundness issues caused by this type. *)
[@@deriving compare]

val get_nullability : t -> Nullability.t

val get_provisional_annotation : t -> ProvisionalAnnotation.t option
(** If the origin is associated with provisional annotation, return it *)

val get_description : t -> string option
(** Get a description to be used for error messages. *)

val to_string : t -> string
(** Raw string representation. *)
