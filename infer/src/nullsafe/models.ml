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
  | Typ.Procname.Java pn_java ->
      String.equal (Typ.Procname.Java.get_method pn_java) name
  | _ ->
      false


let table_has_procedure table proc_name =
  let proc_id = Typ.Procname.to_unique_id proc_name in
  try
    ignore (Hashtbl.find table proc_id) ;
    true
  with Caml.Not_found -> false


(* This is used outside of nullsafe for biabduction.
   If biabduction and nullsafe want to depend on common functionality, this functionality
   should be refactored out in a dedicated library.
 *)
let get_modelled_annotated_signature_for_biabduction proc_attributes =
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  let annotated_signature = AnnotatedSignature.get ~is_strict_mode:false proc_attributes in
  let proc_id = Typ.Procname.to_unique_id proc_name in
  let lookup_models_nullable ann_sig =
    try
      let modelled_nullability = Hashtbl.find annotated_table_nullability proc_id in
      AnnotatedSignature.set_modelled_nullability proc_name ann_sig modelled_nullability
    with Caml.Not_found -> ann_sig
  in
  annotated_signature |> lookup_models_nullable


let get_unique_repr proc_name =
  let java_proc_name =
    match proc_name with Typ.Procname.Java java_proc_name -> Some java_proc_name | _ -> None
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


(** Return the annotated signature of the procedure, taking into account models.
    External models take precedence over internal ones.
 *)
let get_modelled_annotated_signature tenv proc_attributes =
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  let is_strict_mode =
    PatternMatch.check_current_class_attributes Annotations.ia_is_nullsafe_strict tenv proc_name
  in
  let annotated_signature = AnnotatedSignature.get ~is_strict_mode proc_attributes in
  let proc_id = Typ.Procname.to_unique_id proc_name in
  (* Look in the infer internal models *)
  let correct_by_internal_models ann_sig =
    try
      let modelled_nullability = Hashtbl.find annotated_table_nullability proc_id in
      AnnotatedSignature.set_modelled_nullability proc_name ann_sig modelled_nullability
    with Caml.Not_found -> ann_sig
  in
  (* Look at external models *)
  let correct_by_external_models ann_sig =
    get_unique_repr proc_name
    |> Option.bind
         ~f:
           (ThirdPartyAnnotationInfo.find_nullability_info
              (ThirdPartyAnnotationGlobalRepo.get_repo ()))
    |> Option.map ~f:to_modelled_nullability
    |> Option.value_map
       (* If we found information in third-party repo, overwrite annotated signature *)
         ~f:(AnnotatedSignature.set_modelled_nullability proc_name ann_sig)
         ~default:ann_sig
  in
  (* External models overwrite internal ones *)
  annotated_signature |> correct_by_internal_models |> correct_by_external_models


(** Return true when the procedure has been modelled for nullability. *)
let is_modelled_for_nullability_as_internal proc_name =
  (* TODO: get rid of this function, and propagate this information in get_modelled_annotated_signature instead
     to avoid double calculation and make the code more clear.
  *)
  let proc_id = Typ.Procname.to_unique_id proc_name in
  try
    ignore (Hashtbl.find annotated_table_nullability proc_id) ;
    true
  with Caml.Not_found -> false


(** Return true when the procedure has been modelled for nullability as external third-party code. *)
let is_modelled_for_nullability_as_external proc_name =
  (* TODO: get rid of this function, and propagate this information in get_modelled_annotated_signature instead
     to avoid double calculation and make the code more clear.
  *)
  get_unique_repr proc_name
  |> Option.map
       ~f:
         (ThirdPartyAnnotationInfo.find_nullability_info
            (ThirdPartyAnnotationGlobalRepo.get_repo ()))
  |> Option.is_some


(** Check if the procedure is one of the known Preconditions.checkNotNull. *)
let is_check_not_null proc_name =
  table_has_procedure check_not_null_table proc_name || match_method_name proc_name "checkNotNull"


(** Parameter number for a procedure known to be a checkNotNull *)
let get_check_not_null_parameter proc_name =
  let proc_id = Typ.Procname.to_unique_id proc_name in
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

(** Check if the procedure is Map.containsKey(). *)
let is_containsKey proc_name = table_has_procedure containsKey_table proc_name

(** Check if the procedure is Map.put(). *)
let is_mapPut proc_name = table_has_procedure mapPut_table proc_name
