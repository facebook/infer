(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
open ModelTables

(** Unique representation of a method signature in form of a string. *)
let to_unique_id proc_name = Procname.to_unique_id (Procname.Java proc_name)

let match_method_name proc_name name = String.equal (Procname.Java.get_method proc_name) name

let table_has_procedure table proc_name =
  let proc_id = to_unique_id proc_name in
  try
    ignore (Hashtbl.find table proc_id) ;
    true
  with Caml.Not_found -> false


let to_modelled_nullability ThirdPartyMethod.{ret_nullability; params} =
  let is_nullable = function
    | ThirdPartyMethod.Nullable ->
        true
    | ThirdPartyMethod.Nonnull ->
        false
  in
  (is_nullable ret_nullability, List.map params ~f:(fun (_, nullability) -> is_nullable nullability))


(* Some methods *)
let get_special_method_modelled_nullability tenv java_proc_name =
  let open IOption.Let_syntax in
  (* TODO: convert the implementation that does not use PatternMatch *)
  let proc_name = Procname.Java java_proc_name in
  let* class_name = Procname.get_class_type_name proc_name in
  if PatternMatch.Java.is_enum tenv class_name then
    match (Procname.get_method proc_name, Procname.get_parameters proc_name) with
    (* values() is a synthetic enum method that is never null *)
    | "values", [] ->
        Some (false, [])
    (* valueOf() is a synthetic enum method that is never null *)
    | "valueOf", [Procname.Parameter.JavaParameter param_type_name]
      when Typ.equal param_type_name StdTyp.Java.pointer_to_java_lang_string ->
        Some (false, [false])
    | _ ->
        None
  else None


let get_modelled_annotated_signature ~is_callee_in_trust_list tenv proc_attributes =
  let proc_name =
    Procname.as_java_exn proc_attributes.ProcAttributes.proc_name
      ~explanation:"get_modelled_annotated_signature should be called for Java methods only"
  in
  let nullsafe_mode = NullsafeMode.of_java_procname tenv proc_name in
  let annotated_signature =
    AnnotatedSignature.get ~is_callee_in_trust_list ~nullsafe_mode proc_attributes
  in
  let proc_id = to_unique_id proc_name in
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
      ~f:
        (AnnotatedSignature.set_modelled_nullability (Procname.Java proc_name) ann_sig
           ModelledInternally)
      ~default:ann_sig
  in
  (* Look at external models *)
  let correct_by_external_models ann_sig =
    ThirdPartyAnnotationInfo.unique_repr_of_java_proc_name proc_name
    |> ThirdPartyAnnotationInfo.find_nullability_info (ThirdPartyAnnotationGlobalRepo.get_repo ())
    |> Option.map ~f:(fun ThirdPartyAnnotationInfo.{signature; filename; line_number} ->
           (to_modelled_nullability signature, filename, line_number) )
    |> Option.value_map
       (* If we found information in third-party repo, overwrite annotated signature *)
         ~f:(fun (modelled_nullability, filename, line_number) ->
           AnnotatedSignature.set_modelled_nullability (Procname.Java proc_name) ann_sig
             (InThirdPartyRepo {filename; line_number})
             modelled_nullability )
         ~default:ann_sig
  in
  (* External models overwrite internal ones *)
  annotated_signature |> correct_by_internal_models |> correct_by_external_models


let is_check_not_null proc_name =
  table_has_procedure check_not_null_table proc_name || match_method_name proc_name "checkNotNull"


let get_check_not_null_parameter proc_name =
  let proc_id = to_unique_id proc_name in
  Hashtbl.find_opt check_not_null_parameter_table proc_id


let is_check_state proc_name =
  table_has_procedure check_state_table proc_name || match_method_name proc_name "checkState"


let is_check_argument proc_name =
  table_has_procedure check_argument_table proc_name || match_method_name proc_name "checkArgument"


let is_noreturn proc_name = table_has_procedure noreturn_table proc_name

let is_true_on_null proc_name = table_has_procedure true_on_null_table proc_name

let is_false_on_null proc_name =
  (* The only usecase for now - consider all overrides of `Object.equals()` correctly
     implementing the Java specification contract (returning false on null). *)
  PatternMatch.Java.is_override_of_lang_object_equals (Procname.Java proc_name)


let is_containsKey proc_name = table_has_procedure containsKey_table proc_name

let is_mapPut proc_name = table_has_procedure mapPut_table proc_name

let find_nonnullable_alternative proc_name =
  (* NOTE: For now we fetch this info from internal models.
     It is a good idea to support this feature in a user-facing third party repository. *)
  let proc_id = to_unique_id proc_name in
  Hashtbl.find_opt nonnull_alternatives_table proc_id


let is_field_nonnullable field_name =
  Hashtbl.find_opt field_nullability_table (Fieldname.to_full_string field_name)
  |> Option.value_map ~f:(fun is_nullable -> not is_nullable) ~default:false
