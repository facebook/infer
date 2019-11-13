(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Describe the origin of values propagated by the checker. *)

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
          T54687014 tracks unsoundness issues caused by this type.
      *)
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

let equal = [%compare.equal: t]

let get_nullability = function
  | NullConst _ ->
      Nullability.Nullable
  | NonnullConst _
  | This (* `this` can not be null according to Java rules *)
  | New (* In Java `new` always create a non-null object  *)
  | ArrayLengthResult (* integer hence non-nullable *)
  | InferredNonnull _
  | OptimisticFallback (* non-null is the most optimistic type *)
  | Undef (* This is a very special case, assigning non-null is a technical trick *) ->
      Nullability.Nonnull
  | Field {field_type= {nullability}} ->
      AnnotatedNullability.get_nullability nullability
  | MethodParameter {param_annotated_type= {nullability}} ->
      AnnotatedNullability.get_nullability nullability
  | MethodCall {annotated_signature= {ret= {ret_annotated_type= {nullability}}}} ->
      AnnotatedNullability.get_nullability nullability


let rec to_string = function
  | NullConst _ ->
      "null"
  | NonnullConst _ ->
      "Const (nonnull)"
  | Field {object_origin; field_name} ->
      "Field "
      ^ Typ.Fieldname.to_simplified_string field_name
      ^ " (object: " ^ to_string object_origin ^ ")"
  | MethodParameter {mangled; param_annotated_type= {nullability}} ->
      Format.asprintf "Param %s <%a>" (Mangled.to_string mangled) AnnotatedNullability.pp
        nullability
  | This ->
      "this"
  | MethodCall {pname} ->
      Printf.sprintf "Fun %s" (Typ.Procname.to_simplified_string pname)
  | New ->
      "New"
  | ArrayLengthResult ->
      "ArrayLength"
  | InferredNonnull _ ->
      "InferredNonnull"
  | OptimisticFallback ->
      "OptimisticFallback"
  | Undef ->
      "Undef"


let get_description origin =
  let atline loc = " at line " ^ string_of_int loc.Location.line in
  match origin with
  | NullConst loc ->
      Some ("null constant" ^ atline loc, Some loc, None)
  | Field {field_name; access_loc} ->
      Some
        ( "field " ^ Typ.Fieldname.to_simplified_string field_name ^ atline access_loc
        , Some access_loc
        , None )
  | MethodParameter {mangled} ->
      Some ("method parameter " ^ Mangled.to_string mangled, None, None)
  | MethodCall {pname; call_loc; annotated_signature} ->
      let modelled_in =
        (* TODO(T54088319) don't calculate this info and propagate it from AnnotatedNullability instead *)
        if Models.is_modelled_for_nullability_as_internal pname then
          " modelled in " ^ ModelTables.this_file
        else ""
      in
      let description =
        Printf.sprintf "call to %s%s%s"
          (Typ.Procname.to_simplified_string pname)
          modelled_in (atline call_loc)
      in
      Some (description, Some call_loc, Some annotated_signature)
  (* These are origins of non-nullable expressions that are result of evaluating of some rvalue.
     Because they are non-nullable and they are rvalues, we won't get normal type violations
     With them. All we could get is things like condition redundant or overannotated.
     But for these issues we currently don't print origins in the error string.
     It is a good idea to change this and start printing origins for these origins as well.
  *)
  | This | New | NonnullConst _ | ArrayLengthResult | InferredNonnull _ ->
      None
  (* Two special cases - should not really occur in normal code *)
  | OptimisticFallback | Undef ->
      None


let join o1 o2 =
  match (o1, o2) with
  (* left priority *)
  | Undef, _ | _, Undef ->
      Undef
  | Field _, (NullConst _ | NonnullConst _ | MethodParameter _ | This | MethodCall _ | New) ->
      (* low priority to Field, to support field initialization patterns *)
      o2
  | _ ->
      o1
