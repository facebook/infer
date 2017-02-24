(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

open ModelTables
module L = Logging

(** Module for standard library models. *)

(* use model annotations for library functions *)
let use_models = true

(** Module for inference of parameter and return annotations. *)
module Inference = struct
  let enabled = false

  let get_dir () = Filename.concat Config.results_dir "eradicate"

  let field_get_dir_fname fn =
    let fname = Ident.fieldname_to_string fn in
    (get_dir (), fname)

  let field_is_marked fn =
    let dir, fname = field_get_dir_fname fn in
    DB.read_file_with_lock dir fname <> None

  let proc_get_ret_dir_fname pname =
    let fname = Procname.to_filename pname ^ "_ret" in
    (get_dir (), fname)

  let proc_get_param_dir_fname pname =
    let fname = Procname.to_filename pname ^ "_params" in
    (get_dir (), fname)

  let update_count_str s_old =
    let n =
      if String.is_empty s_old then 0
      else try int_of_string s_old with
        | Failure _ ->
            L.stderr "int_of_string %s@." s_old;
            assert false in
    string_of_int (n + 1)

  let update_boolvec_str _s size index bval =
    let s = if String.is_empty _s then String.make size '0' else _s in
    String.set s index (if bval then '1' else '0');
    s

  let mark_file update_str dir fname =
    DB.update_file_with_lock dir fname update_str;
    match DB.read_file_with_lock dir fname with
    | Some buf -> L.stderr "Read %s: %s@." fname buf
    | None -> L.stderr "Read %s: None@." fname

  let mark_file_count = mark_file update_count_str

  (** Mark the field @Nullable indirectly by writing to a global file. *)
  let field_add_nullable_annotation fn =
    let dir, fname = field_get_dir_fname fn in
    mark_file_count dir fname

  (** Mark the return type @Nullable indirectly by writing to a global file. *)
  let proc_mark_return_nullable pn =
    let dir, fname = proc_get_ret_dir_fname pn in
    mark_file_count dir fname

  (** Return true if the return type is marked @Nullable in the global file *)
  let proc_return_is_marked pname =
    let dir, fname = proc_get_ret_dir_fname pname in
    DB.read_file_with_lock dir fname <> None

  (** Mark the n-th parameter @Nullable indirectly by writing to a global file. *)
  let proc_add_parameter_nullable pn n tot =
    let dir, fname = proc_get_param_dir_fname pn in
    let update_str s = update_boolvec_str s tot n true in
    mark_file update_str dir fname

  (** Return None if the parameters are not marked, or a vector of marked parameters *)
  let proc_parameters_marked pn =
    let dir, fname = proc_get_param_dir_fname pn in
    match DB.read_file_with_lock dir fname with
    | None -> None
    | Some buf ->
        let boolvec = ref [] in
        String.iter ~f:(fun c -> boolvec := (Char.equal c '1') :: !boolvec) buf;
        Some (List.rev !boolvec)
end (* Inference *)


let table_has_procedure table proc_name =
  let proc_id = Procname.to_unique_id proc_name in
  try ignore (Hashtbl.find table proc_id); true
  with Not_found -> false

(** Return the annotated signature of the procedure, taking into account models. *)
let get_modelled_annotated_signature proc_attributes =
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  let annotated_signature = AnnotatedSignature.get proc_attributes in
  let proc_id = Procname.to_unique_id proc_name in
  let infer_parameters ann_sig =
    let mark_par =
      if Inference.enabled then Inference.proc_parameters_marked proc_name
      else None in
    match mark_par with
    | None -> ann_sig
    | Some bs ->
        let mark = (false, bs) in
        AnnotatedSignature.mark proc_name AnnotatedSignature.Nullable ann_sig mark in
  let infer_return ann_sig =
    let mark_r =
      Inference.enabled &&
      Inference.proc_return_is_marked proc_name in
    if mark_r
    then AnnotatedSignature.mark_return AnnotatedSignature.Nullable ann_sig
    else ann_sig in
  let lookup_models_nullable ann_sig =
    if use_models then
      try
        let mark = Hashtbl.find annotated_table_nullable proc_id in
        AnnotatedSignature.mark proc_name AnnotatedSignature.Nullable ann_sig mark
      with Not_found ->
        ann_sig
    else ann_sig in
  let lookup_models_present ann_sig =
    if use_models then
      try
        let mark = Hashtbl.find annotated_table_present proc_id in
        AnnotatedSignature.mark proc_name AnnotatedSignature.Present ann_sig mark
      with Not_found ->
        ann_sig
    else ann_sig in

  annotated_signature
  |> lookup_models_nullable
  |> lookup_models_present
  |> infer_return
  |> infer_parameters


(** Return true when the procedure has been modelled for nullable. *)
let is_modelled_nullable proc_name =
  if use_models then
    let proc_id = Procname.to_unique_id proc_name in
    try ignore (Hashtbl.find annotated_table_nullable proc_id ); true
    with Not_found -> false
  else false

(** Check if the procedure is one of the known Preconditions.checkNotNull. *)
let is_check_not_null proc_name =
  table_has_procedure check_not_null_table proc_name

(** Parameter number for a procedure known to be a checkNotNull *)
let get_check_not_null_parameter proc_name =
  let proc_id = Procname.to_unique_id proc_name in
  try Hashtbl.find check_not_null_parameter_table proc_id
  with Not_found -> 0

(** Check if the procedure is one of the known Preconditions.checkState. *)
let is_check_state proc_name =
  table_has_procedure check_state_table proc_name

(** Check if the procedure is one of the known Preconditions.checkArgument. *)
let is_check_argument proc_name =
  table_has_procedure check_argument_table proc_name

(** Check if the procedure does not return. *)
let is_noreturn proc_name =
  table_has_procedure noreturn_table proc_name

(** Check if the procedure is Optional.get(). *)
let is_optional_get proc_name =
  table_has_procedure optional_get_table proc_name

(** Check if the procedure is Optional.isPresent(). *)
let is_optional_isPresent proc_name =
  table_has_procedure optional_isPresent_table proc_name

(** Check if the procedure returns true on null. *)
let is_true_on_null proc_name =
  table_has_procedure true_on_null_table proc_name


(** Check if the procedure is Map.containsKey(). *)
let is_containsKey proc_name =
  table_has_procedure containsKey_table proc_name

(** Check if the procedure is Map.put(). *)
let is_mapPut proc_name =
  table_has_procedure mapPut_table proc_name
