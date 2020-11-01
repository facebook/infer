(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Describe the origin of values propagated by the checker. *)

type method_parameter_origin = Normal of AnnotatedSignature.param_signature | ObjectEqualsOverride
[@@deriving compare]

type method_call_origin =
  { pname: Procname.Java.t
  ; call_loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_defined: bool }
[@@deriving compare]

type t =
  | NullConst of Location.t
  | NonnullConst of Location.t
  | Field of
      { object_origin: t  (** field's object origin (object is before field access operator `.`) *)
      ; field_name: Fieldname.t
      ; field_type: AnnotatedType.t
      ; access_loc: Location.t }
  | CurrMethodParameter of method_parameter_origin
  | This
  | MethodCall of method_call_origin
  | CallToGetKnownToContainsKey
  | New
  | ArrayLengthResult
  | ArrayAccess
  | InferredNonnull of {previous_origin: t}
  | OptimisticFallback
[@@deriving compare]

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
  | OptimisticFallback (* non-null is the most optimistic type *) ->
      Nullability.StrictNonnull
  | Field {field_type= {nullability}} ->
      AnnotatedNullability.get_nullability nullability
  | CurrMethodParameter (Normal {param_annotated_type= {nullability}}) -> (
    match nullability with
    | AnnotatedNullability.Nullable _ ->
        (* Annotated as Nullable explicitly or implicitly *)
        Nullability.Nullable
    | AnnotatedNullability.ProvisionallyNullable _ ->
        Nullability.ProvisionallyNullable
    | AnnotatedNullability.UncheckedNonnull _
    | AnnotatedNullability.ThirdPartyNonnull
    | AnnotatedNullability.LocallyTrustedNonnull
    | AnnotatedNullability.LocallyCheckedNonnull
    | AnnotatedNullability.StrictNonnull _ ->
        (* Nonnull param should be treated as trusted inside this function context:
           Things like dereferences or conversions should be allowed without any extra check
           independendly of mode.
           NOTE. However, in practice a function should be allowed to check any param for null
           and be defensive, because no function can gurantee it won't ever be called with `null`, so
           in theory we might want to distinct that in the future.
        *)
        Nullability.StrictNonnull )
  | CurrMethodParameter ObjectEqualsOverride ->
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
  | CurrMethodParameter (Normal {mangled; param_annotated_type= {nullability}}) ->
      Format.asprintf "Param %s <%a>" (Mangled.to_string mangled) AnnotatedNullability.pp
        nullability
  | CurrMethodParameter ObjectEqualsOverride ->
      "Param(ObjectEqualsOverride)"
  | This ->
      "this"
  | MethodCall {pname} ->
      Printf.sprintf "Fun %s" (Procname.Java.to_simplified_string pname)
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


let atline loc = " at line " ^ string_of_int loc.Location.line

let get_method_ret_description pname call_loc
    AnnotatedSignature.{kind; ret= {ret_annotated_type= {nullability}}} =
  let should_show_class_name =
    (* Class name is generally redundant: the user knows line number and
       can immediatelly go to definition and see the function annotation.
       When something is modelled though, let's show the class name as well, so it is
       super clear what exactly is modelled.
    *)
    match kind with
    | FirstParty | ThirdParty Unregistered ->
        false
    | ThirdParty ModelledInternally | ThirdParty (InThirdPartyRepo _) ->
        true
  in
  let ret_nullability =
    match nullability with
    | AnnotatedNullability.Nullable _ ->
        "nullable"
    | AnnotatedNullability.ProvisionallyNullable _ ->
        (* There should not be scenario where this is explained to the end user. *)
        Logging.die InternalError
          "get_method_ret_description: Unexpected nullability: ProvisionallyNullable"
    | AnnotatedNullability.ThirdPartyNonnull
    | AnnotatedNullability.UncheckedNonnull _
    | AnnotatedNullability.LocallyTrustedNonnull
    | AnnotatedNullability.LocallyCheckedNonnull
    | AnnotatedNullability.StrictNonnull _ ->
        "non-nullable"
  in
  let model_info =
    match kind with
    | FirstParty | ThirdParty Unregistered ->
        ""
    | ThirdParty ModelledInternally ->
        Format.sprintf " (%s according to nullsafe internal models)" ret_nullability
    | ThirdParty (InThirdPartyRepo {filename; line_number}) ->
        let filename_to_show =
          ThirdPartyAnnotationGlobalRepo.get_user_friendly_third_party_sig_file_name ~filename
        in
        Format.sprintf " (declared %s in %s at line %d)" ret_nullability filename_to_show
          line_number
  in
  Format.sprintf "call to %s%s%s"
    (Procname.Java.to_simplified_string ~withclass:should_show_class_name pname)
    (atline call_loc) model_info


let get_provisional_annotation = function
  | NullConst _
  | NonnullConst _
  | This
  | New
  | ArrayLengthResult
  | CallToGetKnownToContainsKey
  | InferredNonnull _
  | ArrayAccess
  | OptimisticFallback ->
      None
  | Field
      {field_type= {nullability= AnnotatedNullability.ProvisionallyNullable provisional_annotation}}
    ->
      Some provisional_annotation
  | CurrMethodParameter
      (Normal
        { param_annotated_type=
            {nullability= AnnotatedNullability.ProvisionallyNullable provisional_annotation} }) ->
      Some provisional_annotation
  | MethodCall
      { annotated_signature=
          { ret=
              { ret_annotated_type=
                  {nullability= AnnotatedNullability.ProvisionallyNullable provisional_annotation}
              } } } ->
      Some provisional_annotation
  | Field _ | CurrMethodParameter _ | MethodCall _ ->
      None


let get_description origin =
  match origin with
  | NullConst loc ->
      Some ("null constant" ^ atline loc)
  | Field {field_name; access_loc} ->
      Some ("field " ^ Fieldname.get_field_name field_name ^ atline access_loc)
  | CurrMethodParameter (Normal {mangled}) ->
      Some ("method parameter " ^ Mangled.to_string mangled)
  | CurrMethodParameter ObjectEqualsOverride ->
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
  (* A technical origin *)
  | OptimisticFallback ->
      None
