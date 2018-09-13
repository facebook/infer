(*
 * Copyright (c) 2013-present, Facebook, Inc.
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


(** Return the annotated signature of the procedure, taking into account models. *)
let get_modelled_annotated_signature proc_attributes =
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  let annotated_signature = AnnotatedSignature.get proc_attributes in
  let proc_id = Typ.Procname.to_unique_id proc_name in
  let lookup_models_nullable ann_sig =
    try
      let mark = Hashtbl.find annotated_table_nullable proc_id in
      AnnotatedSignature.mark proc_name AnnotatedSignature.Nullable ann_sig mark
    with Caml.Not_found -> ann_sig
  in
  let lookup_models_present ann_sig =
    try
      let mark = Hashtbl.find annotated_table_present proc_id in
      AnnotatedSignature.mark proc_name AnnotatedSignature.Present ann_sig mark
    with Caml.Not_found -> ann_sig
  in
  annotated_signature |> lookup_models_nullable |> lookup_models_present


(** Return true when the procedure has been modelled for nullable. *)
let is_modelled_nullable proc_name =
  let proc_id = Typ.Procname.to_unique_id proc_name in
  try
    ignore (Hashtbl.find annotated_table_nullable proc_id) ;
    true
  with Caml.Not_found -> false


(** Check if the procedure is one of the known Preconditions.checkNotNull. *)
let is_check_not_null proc_name =
  table_has_procedure check_not_null_table proc_name || match_method_name proc_name "checkNotNull"


(** Parameter number for a procedure known to be a checkNotNull *)
let get_check_not_null_parameter proc_name =
  let proc_id = Typ.Procname.to_unique_id proc_name in
  try Hashtbl.find check_not_null_parameter_table proc_id with Caml.Not_found ->
    (* Assume the check is on the first parameter unless modeled otherwise *)
    1


(** Check if the procedure is one of the known Preconditions.checkState. *)
let is_check_state proc_name = table_has_procedure check_state_table proc_name

(** Check if the procedure is one of the known Preconditions.checkArgument. *)
let is_check_argument proc_name = table_has_procedure check_argument_table proc_name

(** Check if the procedure does not return. *)
let is_noreturn proc_name = table_has_procedure noreturn_table proc_name

(** Check if the procedure is Optional.get(). *)
let is_optional_get proc_name = table_has_procedure optional_get_table proc_name

(** Check if the procedure is Optional.isPresent(). *)
let is_optional_isPresent proc_name = table_has_procedure optional_isPresent_table proc_name

(** Check if the procedure returns true on null. *)
let is_true_on_null proc_name = table_has_procedure true_on_null_table proc_name

(** Check if the procedure is Map.containsKey(). *)
let is_containsKey proc_name = table_has_procedure containsKey_table proc_name

(** Check if the procedure is Map.put(). *)
let is_mapPut proc_name = table_has_procedure mapPut_table proc_name
