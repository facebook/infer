(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
open! PVariant
module Hashtbl = Caml.Hashtbl
module F = Format
module L = Logging

type attr_kind = ProcDefined | ProcObjCAccessor | ProcUndefined

(** Module to manage the table of attributes. *)
let serializer : ProcAttributes.t Serialization.serializer =
  Serialization.create_serializer Serialization.Key.attributes

let attributes_filename ~proc_kind pname_file =
  let file_suffix =
    match proc_kind with
    | ProcDefined
     -> ".attr"
    | ProcObjCAccessor
     -> ".objc_acc.attr"
    | ProcUndefined
     -> ".decl.attr"
  in
  pname_file ^ file_suffix

(** path to the .attr file for the given procedure in the current results directory *)
let res_dir_attr_filename ~create_dir ~proc_kind pname =
  let pname_file = Typ.Procname.to_filename pname in
  let attr_fname = attributes_filename ~proc_kind pname_file in
  let bucket_dir =
    let base = pname_file in
    let len = String.length base in
    if len < 2 then Filename.current_dir_name else String.sub base ~pos:(len - 2) ~len:2
  in
  let filename =
    DB.Results_dir.path_to_filename DB.Results_dir.Abs_root
      [Config.attributes_dir_name; bucket_dir; attr_fname]
  in
  if create_dir then DB.filename_create_dir filename ;
  filename

(* Load the proc attribute for the defined filename if it exists,
   otherwise try to load the declared filename. *)
let load_attr ~defined_only proc_name =
  let attributes_file ~proc_kind proc_name =
    Multilinks.resolve (res_dir_attr_filename ~create_dir:false ~proc_kind proc_name)
  in
  let attr =
    Serialization.read_from_file serializer (attributes_file ~proc_kind:ProcDefined proc_name)
  in
  if is_none attr && not defined_only then
    (* We try to load the objc accesor one if they exist, if not then we load the undefined one *)
    let attr =
      Serialization.read_from_file serializer
        (attributes_file ~proc_kind:ProcObjCAccessor proc_name)
    in
    match attr with
    | Some attr
     -> Some attr
    | None
     -> Serialization.read_from_file serializer
          (attributes_file ~proc_kind:ProcUndefined proc_name)
  else attr

let create_proc_kind (proc_attributes: ProcAttributes.t) =
  if proc_attributes.is_defined then ProcDefined
  else if Option.is_some proc_attributes.objc_accessor then ProcObjCAccessor
  else ProcUndefined

let less_relevant_proc_kinds proc_kind =
  match proc_kind with
  | ProcDefined
   -> [ProcObjCAccessor; ProcUndefined]
  | ProcObjCAccessor
   -> [ProcUndefined]
  | ProcUndefined
   -> []

(* Write a proc attributes to file.
   If defined, delete the declared file if it exists. *)
let write_and_delete proc_name (proc_attributes: ProcAttributes.t) =
  let proc_kind = create_proc_kind proc_attributes in
  let attributes_file proc_kind = res_dir_attr_filename ~create_dir:true ~proc_kind proc_name in
  Serialization.write_to_file serializer (attributes_file proc_kind) ~data:proc_attributes ;
  let upgrade_relevance less_relevant_proc_kind =
    let fname_declared = DB.filename_to_string (attributes_file less_relevant_proc_kind) in
    if Sys.file_exists fname_declared = `Yes then
      try Unix.unlink fname_declared
      with Unix.Unix_error _ -> ()
  in
  List.iter ~f:upgrade_relevance (less_relevant_proc_kinds proc_kind)

(* This creates an ordering in the attribute files: 1.defined, 2.objc accessor, 3.else.
   To be used to figure out if we should override an existing attribute file with a new
   one, if relevant information will be updated, or lost.
   If the relevance is not upgraded, choose based on whether its associated file has higher
   rank (alphabetically) than the other. *)
let should_override_attr (new_attr: ProcAttributes.t) (old_attr: ProcAttributes.t) =
  if new_attr.is_defined then
    if old_attr.is_defined then SourceFile.compare new_attr.loc.file old_attr.loc.file > 0
    else true (* new becomes defined, override  *)
  else if old_attr.is_defined then false (* old was defined, new isn't, don't override *)
  else if Option.is_some new_attr.objc_accessor then
    if Option.is_some old_attr.objc_accessor then
      SourceFile.compare new_attr.loc.file old_attr.loc.file > 0
    else true (* new becomes objc accessor, override *)
  else false

(* new isn't defined or objc accessor, don't overide *)

let store_attributes (proc_attributes: ProcAttributes.t) =
  let proc_name = proc_attributes.proc_name in
  let should_write =
    match load_attr ~defined_only:false proc_name with
    | None
     -> true
    | Some proc_attributes_on_disk
     -> should_override_attr proc_attributes proc_attributes_on_disk
  in
  if should_write then write_and_delete proc_name proc_attributes

let attr_tbl = Typ.Procname.Hash.create 16

let defined_attr_tbl = Typ.Procname.Hash.create 16

let load_attributes ~cache proc_name =
  try Typ.Procname.Hash.find attr_tbl proc_name
  with Not_found ->
    let proc_attributes = load_attr ~defined_only:false proc_name in
    ( match proc_attributes with
    | Some attrs
     -> if cache then (
          Typ.Procname.Hash.add attr_tbl proc_name proc_attributes ;
          if attrs.is_defined then Typ.Procname.Hash.add defined_attr_tbl proc_name proc_attributes )
    | None
     -> () ) ;
    proc_attributes

let load_defined_attributes ~cache_none proc_name =
  try Typ.Procname.Hash.find defined_attr_tbl proc_name
  with Not_found ->
    let proc_attributes = load_attr ~defined_only:true proc_name in
    if proc_attributes <> None then (
      (* procedure just got defined, replace attribute in attr_tbl with defined version *)
      Typ.Procname.Hash.replace attr_tbl proc_name proc_attributes ;
      Typ.Procname.Hash.add defined_attr_tbl proc_name proc_attributes )
    else if cache_none then Typ.Procname.Hash.add defined_attr_tbl proc_name proc_attributes ;
    proc_attributes

(** Given the name of an ObjC class, extract the type from the tenv where the class was defined. We
    do this by adding a method that is unique to each class, and then finding the tenv that
    corresponds to the class definition. *)
let get_correct_type_from_objc_class_name type_name =
  (* ToDo: this function should return a type that includes a reference to the tenv computed by:
     let class_method = Typ.Procname.get_default_objc_class_method (Typ.Name.name type_name);
     switch (find_tenv_from_class_of_proc class_method) {
     | Some tenv =>
      *)
  Some (Typ.mk (Tstruct type_name))

type t =
  {num_bindings: int; num_buckets: int; max_bucket_length: int; serialized_size_kb: int option}

let to_json at =
  let extra_field =
    match at.serialized_size_kb with Some v -> [("serialized_size_kb", `Int v)] | None -> []
  in
  `Assoc
    ( [ ("num_bindings", `Int at.num_bindings)
      ; ("num_buckets", `Int at.num_buckets)
      ; ("max_bucket_length", `Int at.max_bucket_length) ]
    @ extra_field )

let from_json json =
  let open! Yojson.Basic.Util in
  { num_bindings= json |> member "num_bindings" |> to_int
  ; num_buckets= json |> member "num_buckets" |> to_int
  ; max_bucket_length= json |> member "max_bucket_length" |> to_int
  ; serialized_size_kb= json |> member "serialized_size_kb" |> to_option to_int }

let aggregate s =
  let all_num_bindings = List.map ~f:(fun stats -> float_of_int stats.num_bindings) s in
  let all_num_buckets = List.map ~f:(fun stats -> float_of_int stats.num_buckets) s in
  let all_max_bucket_length = List.map ~f:(fun stats -> float_of_int stats.max_bucket_length) s in
  let aggr_num_bindings = StatisticsToolbox.compute_statistics all_num_bindings in
  let aggr_num_buckets = StatisticsToolbox.compute_statistics all_num_buckets in
  let aggr_max_bucket_length = StatisticsToolbox.compute_statistics all_max_bucket_length in
  `Assoc
    [ ("num_bindings", StatisticsToolbox.to_json aggr_num_bindings)
    ; ("num_buckets", StatisticsToolbox.to_json aggr_num_buckets)
    ; ("max_bucket_length", StatisticsToolbox.to_json aggr_max_bucket_length) ]

let stats () =
  let stats = Typ.Procname.Hash.stats attr_tbl in
  let {Hashtbl.num_bindings; num_buckets; max_bucket_length} = stats in
  let serialized_size_kb =
    match Config.developer_mode with
    | true
     -> Some (Marshal.data_size (Marshal.to_bytes attr_tbl []) 0 / 1024)
    | false
     -> None
  in
  {num_bindings; num_buckets; max_bucket_length; serialized_size_kb}

(* Find the file where the procedure was captured, if a cfg for that file exists.
   Return also a boolean indicating whether the procedure is defined in an
   include file. *)
let find_file_capturing_procedure ?(cache= true) pname =
  match load_attributes ~cache pname with
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
      let cfg_fname_exists = Sys.file_exists (DB.filename_to_string cfg_fname) = `Yes in
      if cfg_fname_exists then Some (source_file, origin) else None
