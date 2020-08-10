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
  Option.map java_proc_name ~f:ThirdPartyAnnotationInfo.unique_repr_of_java_proc_name


let to_modelled_nullability ThirdPartyMethod.{ret_nullability; params} =
  let is_nullable = function
    | ThirdPartyMethod.Nullable ->
        true
    | ThirdPartyMethod.Nonnull ->
        false
  in
  (is_nullable ret_nullability, List.map params ~f:(fun (_, nullability) -> is_nullable nullability))


(* Some methods *)
let get_special_method_modelled_nullability tenv proc_name =
  let open IOption.Let_syntax in
  let* class_name = Procname.get_class_type_name proc_name in
  if PatternMatch.Java.is_enum tenv class_name then
    match (Procname.get_method proc_name, Procname.get_parameters proc_name) with
    (* values() is a synthetic enum method that is never null *)
    | "values", [] ->
        Some (false, [])
    (* valueOf() is a synthetic enum method that is never null *)
    | "valueOf", [Procname.Parameter.JavaParameter param_type_name]
      when Typ.equal param_type_name Typ.pointer_to_java_lang_string ->
        Some (false, [false])
    | _ ->
        None
  else None


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
    let modelled_nullability =
      (* Look at internal model tables *)
      Hashtbl.find_opt annotated_table_nullability proc_id
      (* Maybe it is a special method whose nullability is predefined *)
      |> IOption.if_none_evalopt ~f:(fun () ->
             get_special_method_modelled_nullability tenv proc_name )
    in
    Option.value_map modelled_nullability
      ~f:(AnnotatedSignature.set_modelled_nullability proc_name ann_sig ModelledInternally)
      ~default:ann_sig
  in
  (* Look at external models *)
  let correct_by_external_models ann_sig =
    get_unique_repr proc_name
    |> Option.bind
         ~f:
           (ThirdPartyAnnotationInfo.find_nullability_info
              (ThirdPartyAnnotationGlobalRepo.get_repo ()))
    |> Option.map ~f:(fun ThirdPartyAnnotationInfo.{signature; filename; line_number} ->
           (to_modelled_nullability signature, filename, line_number) )
    |> Option.value_map
       (* If we found information in third-party repo, overwrite annotated signature *)
         ~f:(fun (modelled_nullability, filename, line_number) ->
           AnnotatedSignature.set_modelled_nullability proc_name ann_sig
             (InThirdPartyRepo {filename; line_number})
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
  PatternMatch.Java.is_override_of_lang_object_equals proc_name


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


(* Check if a given field is known to be a non-nullable *)
let is_field_nonnullable field_name =
  Hashtbl.find_opt field_nullability_table (Fieldname.to_full_string field_name)
  |> Option.value_map ~f:(fun is_nullable -> not is_nullable) ~default:false
