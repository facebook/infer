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
  | MethodParameter of method_parameter_origin  (** A method's parameter *)
  | This (* `this` object. Can not be null, according to Java rules. *)
  | MethodCall of method_call_origin  (** A result of a method call *)
  | CallToGetKnownToContainsKey
      (** This is a result of accessing a map element that is known to contains this particular key,
          normally because it was explicitly checked for presense before *)
  | New  (** A new object creation *)
  | ArrayLengthResult  (** integer value - result of accessing array.length *)
  | ArrayAccess  (** Result of accessing an array by index *)
  | InferredNonnull of {previous_origin: t}
      (** The value is inferred as non-null during flow-sensitive type inference (most commonly from
          relevant condition branch or assertion explicitly comparing the value with `null`) *)
  (* Below are two special values. *)
  | OptimisticFallback
      (** Something went wrong during typechecking. We fall back to optimistic (not-nullable) type
          to reduce potential non-actionable false positives. Ideally we should not see these
          instances. They should be either processed gracefully (and a dedicated type constructor
          should be added), or fixed. T54687014 tracks unsoundness issues caused by this type. *)
  | Undef  (** Undefined value before initialization *)
[@@deriving compare]

and method_parameter_origin = Normal of AnnotatedSignature.param_signature | ObjectEqualsOverride

and field_origin =
  { object_origin: t  (** field's object origin (object is before field access operator `.`) *)
  ; field_name: Fieldname.t
  ; field_type: AnnotatedType.t
  ; access_loc: Location.t }

and method_call_origin =
  { pname: Procname.t
  ; call_loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_library: bool }

let equal = [%compare.equal: t]

let get_nullability = function
  | NullConst _ ->
      Nullability.Null
  | NonnullConst _
  | This (* `this` can not be null according to Java rules *)
  | New (* In Java `new` always create a non-null object  *)
  | ArrayLengthResult (* integer hence non-nullable *)
  | CallToGetKnownToContainsKey (* non-nullable by definition *)
  | InferredNonnull _
  (* WARNING: we trade soundness for usability.
     In Java, arrays are initialized with null, so accessing array is nullable until it was initialized.
     However we assume array access is going to always return non-nullable.
     This is because in real life arrays are often initialized straight away.
     We currently don't have a nice way to detect initialization, neither automatical nor manual.
     Hence we make potentially dangerous choice in favor of pragmatism.
  *)
  | ArrayAccess
  | OptimisticFallback (* non-null is the most optimistic type *)
  | Undef (* This is a very special case, assigning non-null is a technical trick *) ->
      Nullability.StrictNonnull
  | Field {field_type= {nullability}} ->
      AnnotatedNullability.get_nullability nullability
  | MethodParameter (Normal {param_annotated_type= {nullability}}) ->
      AnnotatedNullability.get_nullability nullability
  | MethodParameter ObjectEqualsOverride ->
      (* `Object.equals(obj)` should expect to be called with null `obj` *)
      Nullability.Nullable
  | MethodCall {annotated_signature= {ret= {ret_annotated_type= {nullability}}}} ->
      AnnotatedNullability.get_nullability nullability


let rec to_string = function
  | NullConst _ ->
      "null"
  | NonnullConst _ ->
      "Const (nonnull)"
  | Field {object_origin; field_name} ->
      "Field " ^ Fieldname.to_string field_name ^ " (object: " ^ to_string object_origin ^ ")"
  | MethodParameter (Normal {mangled; param_annotated_type= {nullability}}) ->
      Format.asprintf "Param %s <%a>" (Mangled.to_string mangled) AnnotatedNullability.pp
        nullability
  | MethodParameter ObjectEqualsOverride ->
      "Param(ObjectEqualsOverride)"
  | This ->
      "this"
  | MethodCall {pname} ->
      Printf.sprintf "Fun %s" (Procname.to_simplified_string pname)
  | CallToGetKnownToContainsKey ->
      "CallToGetKnownToContainsKey"
  | New ->
      "New"
  | ArrayLengthResult ->
      "ArrayLength"
  | ArrayAccess ->
      "ArrayAccess"
  | InferredNonnull {previous_origin} ->
      Format.sprintf "InferredNonnull(prev:%s)" (to_string previous_origin)
  | OptimisticFallback ->
      "OptimisticFallback"
  | Undef ->
      "Undef"


let atline loc = " at line " ^ string_of_int loc.Location.line

let get_method_ret_description pname call_loc
    AnnotatedSignature.{model_source; ret= {ret_annotated_type= {nullability}}} =
  let should_show_class_name =
    (* Class name is generally redundant: the user knows line number and
       can immediatelly go to definition and see the function annotation.
       When something is modelled though, let's show the class name as well, so it is
       super clear what exactly is modelled.
    *)
    Option.is_some model_source
  in
  let ret_nullability =
    match nullability with
    | AnnotatedNullability.Nullable _ ->
        "nullable"
    | AnnotatedNullability.UncheckedNonnull _
    | AnnotatedNullability.LocallyCheckedNonnull
    | AnnotatedNullability.StrictNonnull _ ->
        "non-nullable"
  in
  let model_info =
    match model_source with
    | None ->
        ""
    | Some InternalModel ->
        Format.sprintf " (%s according to nullsafe internal models)" ret_nullability
    | Some (ThirdPartyRepo {filename; line_number}) ->
        let filename_to_show =
          ThirdPartyAnnotationGlobalRepo.get_user_friendly_third_party_sig_file_name ~filename
        in
        Format.sprintf " (declared %s in %s at line %d)" ret_nullability filename_to_show
          line_number
  in
  Format.sprintf "call to %s%s%s"
    (Procname.to_simplified_string ~withclass:should_show_class_name pname)
    (atline call_loc) model_info


let get_description origin =
  match origin with
  | NullConst loc ->
      Some ("null constant" ^ atline loc)
  | Field {field_name; access_loc} ->
      Some ("field " ^ Fieldname.get_field_name field_name ^ atline access_loc)
  | MethodParameter (Normal {mangled}) ->
      Some ("method parameter " ^ Mangled.to_string mangled)
  | MethodParameter ObjectEqualsOverride ->
      Some "Object.equals() should be able to accept `null`, according to the Java specification"
  | MethodCall {pname; call_loc; annotated_signature} ->
      Some (get_method_ret_description pname call_loc annotated_signature)
  (* These are origins of non-nullable expressions that are result of evaluating of some rvalue.
     Because they are non-nullable and they are rvalues, we won't get normal type violations
     With them. All we could get is things like condition redundant or overannotated.
     But for these issues we currently don't print origins in the error string.
     It is a good idea to change this and start printing origins for these origins as well.
  *)
  | This
  | New
  | NonnullConst _
  | ArrayLengthResult
  | ArrayAccess
  | InferredNonnull _
  | CallToGetKnownToContainsKey ->
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
