(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
open ModelTables

(** Module for standard library models. *)

let match_method_name pn name =
  match pn with
  | Procname.Java pn_java ->
      String.equal (Procname.Java.get_method pn_java) name
  | _ ->
      false


let table_has_procedure table proc_name =
  let proc_id = Procname.to_unique_id proc_name in
  try
    ignore (Hashtbl.find table proc_id) ;
    true
  with Caml.Not_found -> false


let get_unique_repr proc_name =
  let java_proc_name =
    match proc_name with Procname.Java java_proc_name -> Some java_proc_name | _ -> None
  in
  Option.map java_proc_name ~f:ThirdPartyMethod.unique_repr_of_java_proc_name


let to_modelled_nullability ThirdPartyMethod.{ret_nullability; param_nullability} =
  let is_nullable = function
    | ThirdPartyMethod.Nullable ->
        true
    | ThirdPartyMethod.Nonnull ->
        false
  in
  (is_nullable ret_nullability, List.map param_nullability ~f:is_nullable)


(** Return the annotated signature of the procedure, taking into account models. External models
    take precedence over internal ones. *)
let get_modelled_annotated_signature ~is_callee_in_trust_list tenv proc_attributes =
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  let nullsafe_mode = NullsafeMode.of_procname tenv proc_name in
  let annotated_signature =
    AnnotatedSignature.get ~is_callee_in_trust_list ~nullsafe_mode proc_attributes
  in
  let proc_id = Procname.to_unique_id proc_name in
  (* Look in the infer internal models *)
  let correct_by_internal_models ann_sig =
    try
      let modelled_nullability = Hashtbl.find annotated_table_nullability proc_id in
      AnnotatedSignature.set_modelled_nullability proc_name ann_sig InternalModel
        modelled_nullability
    with Caml.Not_found -> ann_sig
  in
  (* Look at external models *)
  let correct_by_external_models ann_sig =
    get_unique_repr proc_name
    |> Option.bind
         ~f:
           (ThirdPartyAnnotationInfo.find_nullability_info
              (ThirdPartyAnnotationGlobalRepo.get_repo ()))
    |> Option.map ~f:(fun ThirdPartyAnnotationInfo.{nullability; filename; line_number} ->
           (to_modelled_nullability nullability, filename, line_number) )
    |> Option.value_map
       (* If we found information in third-party repo, overwrite annotated signature *)
         ~f:(fun (modelled_nullability, filename, line_number) ->
           AnnotatedSignature.set_modelled_nullability proc_name ann_sig
             (ThirdPartyRepo {filename; line_number})
             modelled_nullability )
         ~default:ann_sig
  in
  (* External models overwrite internal ones *)
  annotated_signature |> correct_by_internal_models |> correct_by_external_models


(** Check if the procedure is one of the known methods asserting nullability of the object. Nullsafe
    should understand that both the argument and return value are non-nullable after the call. *)
let is_check_not_null proc_name =
  table_has_procedure check_not_null_table proc_name || match_method_name proc_name "checkNotNull"


(** Parameter number (starting from 1) for a procedure known to produce a non-nullable assertion.
    [None] if the function is not known to be an aseertion OR the parameter number is not known *)
let get_check_not_null_parameter proc_name =
  let proc_id = Procname.to_unique_id proc_name in
  Hashtbl.find_opt check_not_null_parameter_table proc_id


(** Check if the procedure is one of the known Preconditions.checkState. *)
let is_check_state proc_name =
  table_has_procedure check_state_table proc_name || match_method_name proc_name "checkState"


(** Check if the procedure is one of the known Preconditions.checkArgument. *)
let is_check_argument proc_name =
  table_has_procedure check_argument_table proc_name || match_method_name proc_name "checkArgument"


(** Check if the procedure does not return. *)
let is_noreturn proc_name = table_has_procedure noreturn_table proc_name

(** Check if the procedure returns true on null. *)
let is_true_on_null proc_name = table_has_procedure true_on_null_table proc_name

(** Check if the procedure returns false on null. *)
let is_false_on_null proc_name =
  (* The only usecase for now - consider all overrides of `Object.equals()` correctly
     implementing the Java specification contract (returning false on null). *)
  PatternMatch.is_override_of_java_lang_object_equals proc_name


(** Check if the procedure is Map.containsKey(). *)
let is_containsKey proc_name = table_has_procedure containsKey_table proc_name

(** Check if the procedure is Map.put(). *)
let is_mapPut proc_name = table_has_procedure mapPut_table proc_name

(** Check if a (nullable) method has a non-nullable alternative: A method that does the same as
    [proc_name] but asserts the result is not null before returning to the caller. *)
let find_nonnullable_alternative proc_name =
  (* NOTE: For now we fetch this info from internal models.
     It is a good idea to support this feature in a user-facing third party repository. *)
  let proc_id = Procname.to_unique_id proc_name in
  Hashtbl.find_opt nonnull_alternatives_table proc_id
