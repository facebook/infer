(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging

type attributes_kind = ProcUndefined | ProcObjCAccessor | ProcDefined [@@deriving compare]

let least_relevant_up_to_proc_kind_exclusive = function
  | ProcUndefined
   -> []
  | ProcObjCAccessor
   -> [ProcUndefined]
  | ProcDefined
   -> [ProcUndefined; ProcObjCAccessor]

let most_relevant_down_to_proc_kind_inclusive = function
  | ProcUndefined
   -> [ProcDefined; ProcObjCAccessor; ProcUndefined]
  | ProcObjCAccessor
   -> [ProcDefined; ProcObjCAccessor]
  | ProcDefined
   -> [ProcDefined]

let proc_kind_of_attr (proc_attributes: ProcAttributes.t) =
  if proc_attributes.is_defined then ProcDefined
  else if Option.is_some proc_attributes.objc_accessor then ProcObjCAccessor
  else ProcUndefined

let should_override_attr attr1 attr2 =
  (* use the source file to be more deterministic in case the same procedure name is defined in several files *)
  [%compare : attributes_kind * SourceFile.t]
    (proc_kind_of_attr attr1, attr1.ProcAttributes.loc.file)
    (proc_kind_of_attr attr2, attr2.ProcAttributes.loc.file)
  > 0

module Table = struct
  type key = Typ.Procname.t * attributes_kind

  type value = ProcAttributes.t

  let table = ResultsDir.attributes_table
end

module Store = KeyValue.Make (Table)

let load_aux ?(min_kind= ProcUndefined) pname =
  List.find_map (most_relevant_down_to_proc_kind_inclusive min_kind) ~f:(fun pkind ->
      Store.find (pname, pkind) )

let load pname : ProcAttributes.t option = load_aux pname

let store (attr: ProcAttributes.t) =
  let pkind = proc_kind_of_attr attr in
  let key = (attr.proc_name, pkind) in
  if load attr.proc_name |> Option.value_map ~default:true ~f:(should_override_attr attr) then (
    (* NOTE: We need to do this dance of adding the proc_kind to the key because there's a race condition between the time we load the attributes from the db and the time we write possibly better ones. We could avoid this by making the db schema richer than just key/value and turning the SELECT + REPLACE into an atomic transaction. *)
    Store.replace key attr ;
    least_relevant_up_to_proc_kind_exclusive pkind
    |> List.iter ~f:(fun k -> Store.delete (attr.proc_name, k)) )

let load_defined pname = load_aux ~min_kind:ProcDefined pname

let get_correct_type_from_objc_class_name type_name =
  (* ToDo: this function should return a type that includes a reference to the tenv computed by:
     let class_method = Typ.Procname.get_default_objc_class_method (Typ.Name.name type_name);
     switch (find_tenv_from_class_of_proc class_method) {
     | Some tenv =>
    *)
  Some (Typ.mk (Tstruct type_name))

let find_file_capturing_procedure pname =
  match load pname with
  | None
   -> None
  | Some proc_attributes
   -> let source_file = proc_attributes.ProcAttributes.source_file_captured in
      let source_dir = DB.source_dir_from_source_file source_file in
      let origin =
        (* Procedure coming from include files if it has different location
         than the file where it was captured. *)
        match SourceFile.compare source_file proc_attributes.ProcAttributes.loc.file <> 0 with
        | true
         -> `Include
        | false
         -> `Source
      in
      let cfg_fname = DB.source_dir_get_internal_file source_dir ".cfg" in
      let cfg_fname_exists =
        PVariant.( = ) `Yes (Sys.file_exists (DB.filename_to_string cfg_fname))
      in
      if cfg_fname_exists then Some (source_file, origin) else None
