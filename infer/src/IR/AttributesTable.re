/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;

let module F = Format;

let module L = Logging;


/** Module to manage the table of attributes. */
let serializer: Serialization.serializer ProcAttributes.t = Serialization.create_serializer Serialization.attributes_key;

let attributes_filename pname => {
  let pname_file = Procname.to_filename pname;
  pname_file ^ ".attr"
};


/** path to the .attr file for the given procedure in the current results directory */
let res_dir_attr_filename pname => {
  let attr_fname = attributes_filename pname;
  let bucket_dir = {
    let base = Filename.chop_extension attr_fname;
    let len = String.length base;
    if (len < 2) {
      Filename.current_dir_name
    } else {
      String.sub base (len - 2) 2
    }
  };
  let filename =
    DB.Results_dir.path_to_filename
      DB.Results_dir.Abs_root [Config.attributes_dir_name, bucket_dir, attr_fname];
  DB.filename_create_dir filename;
  filename
};

let store_attributes (proc_attributes: ProcAttributes.t) => {
  let proc_name = proc_attributes.proc_name;
  let attributes_file = res_dir_attr_filename proc_name;
  let should_write = not (DB.file_exists attributes_file) || (
    switch (Serialization.from_file serializer attributes_file) {
    | None => true
    | Some proc_attributes_on_disk =>
      let higher_rank_than_on_disk () =>
        proc_attributes.is_defined &&
          DB.source_file_compare proc_attributes.loc.file proc_attributes_on_disk.loc.file > 0;
      let becomes_defined = proc_attributes.is_defined && not proc_attributes_on_disk.is_defined;
      /* Only overwrite the attribute file if the procedure becomes defined
         or its associated file has higher rank (alphabetically) than on disk. */
      becomes_defined || higher_rank_than_on_disk ()
    }
  );
  if should_write {
    Serialization.to_file serializer attributes_file proc_attributes
  }
};

let attr_tbl = Procname.Hash.create 16;

let load_attributes proc_name =>
  try (Procname.Hash.find attr_tbl proc_name) {
  | Not_found =>
    let attributes_file = res_dir_attr_filename proc_name;
    let attr = Serialization.from_file serializer attributes_file;
    if (attr != None) {
      Procname.Hash.add attr_tbl proc_name attr
    };
    attr
  };


/** Given a procedure name, find the file where it is defined and its corresponding type
    environment */
let find_tenv_from_class_of_proc procname =>
  switch (load_attributes procname) {
  | None => None
  | Some attrs =>
    let source_file = attrs.ProcAttributes.loc.Location.file;
    let source_dir = DB.source_dir_from_source_file source_file;
    let tenv_fname = DB.source_dir_get_internal_file source_dir ".tenv";
    Tenv.load_from_file tenv_fname
  };


/** Given a procedure name, find the file where it is defined and its corresponding type
    environment, or create an empty tenv if necessary. */
let get_tenv proc_name =>
  switch (find_tenv_from_class_of_proc proc_name) {
  | Some tenv => tenv
  /* ToDo: a tenv should always be found, it should not be necessary to create one here */
  | None => Tenv.create ()
  | exception _ => Tenv.create ()
  };


/** Given the name of an ObjC class, extract the type from the tenv where the class was defined. We
    do this by adding a method that is unique to each class, and then finding the tenv that
    corresponds to the class definition. */
let get_correct_type_from_objc_class_name type_name =>
  /* ToDo: this function should return a type that includes a reference to the tenv computed by:
     let class_method = Procname.get_default_objc_class_method (Typename.name type_name);
     switch (find_tenv_from_class_of_proc class_method) {
     | Some tenv =>
      */
  Some (Typ.Tstruct type_name);


/** Returns true if the method is defined as a C++ model */
let pname_is_cpp_model callee_pname =>
  switch (load_attributes callee_pname) {
  | Some attrs =>
    let file = DB.source_file_to_string attrs.ProcAttributes.loc.Location.file;
    DB.file_is_in_cpp_model file
  | None => false
  };

let is_whitelisted_cpp_method method_name =>
  IList.exists
    (
      fun whitelisting_class =>
        IList.for_all
          (
            fun whitelisting_class_substring =>
              Utils.string_contains whitelisting_class_substring method_name
          )
          whitelisting_class
    )
    Config.whitelisted_cpp_methods;

type t = {
  num_bindings: int,
  num_buckets: int,
  max_bucket_length: int,
  serialized_size_kb: option int
};

let to_json at => {
  let extra_field =
    switch at.serialized_size_kb {
    | Some v => [("serialized_size_kb", `Int v)]
    | None => []
    };
  `Assoc (
    [
      ("num_bindings", `Int at.num_bindings),
      ("num_buckets", `Int at.num_buckets),
      ("max_bucket_length", `Int at.max_bucket_length)
    ]
      @ extra_field
  )
};

let from_json json => {
  let open! Yojson.Basic.Util;
  {
    num_bindings: json |> member "num_bindings" |> to_int,
    num_buckets: json |> member "num_buckets" |> to_int,
    max_bucket_length: json |> member "max_bucket_length" |> to_int,
    serialized_size_kb: json |> member "serialized_size_kb" |> to_option to_int
  }
};

let aggregate s => {
  let all_num_bindings = IList.map (fun stats => float_of_int stats.num_bindings) s;
  let all_num_buckets = IList.map (fun stats => float_of_int stats.num_buckets) s;
  let all_max_bucket_length = IList.map (fun stats => float_of_int stats.max_bucket_length) s;
  let aggr_num_bindings = StatisticsToolbox.compute_statistics all_num_bindings;
  let aggr_num_buckets = StatisticsToolbox.compute_statistics all_num_buckets;
  let aggr_max_bucket_length = StatisticsToolbox.compute_statistics all_max_bucket_length;
  `Assoc [
    ("num_bindings", StatisticsToolbox.to_json aggr_num_bindings),
    ("num_buckets", StatisticsToolbox.to_json aggr_num_buckets),
    ("max_bucket_length", StatisticsToolbox.to_json aggr_max_bucket_length)
  ]
};

let stats () => {
  let stats = Procname.Hash.stats attr_tbl;
  let {Hashtbl.num_bindings: num_bindings, num_buckets, max_bucket_length} = stats;
  let serialized_size_kb =
    Config.developer_mode ? Some (Marshal.data_size (Marshal.to_bytes attr_tbl []) 0 / 1024) : None;
  {num_bindings, num_buckets, max_bucket_length, serialized_size_kb}
};

/* Find the file where the procedure is defined according to the attributes,
   if a cfg for that file exist. */
let file_defining_procedure pname =>
  switch (load_attributes pname) {
  | None => None
  | Some proc_attributes =>
    let loc = proc_attributes.ProcAttributes.loc;
    let source_file = loc.file;
    let source_dir = DB.source_dir_from_source_file source_file;
    let cfg_fname = DB.source_dir_get_internal_file source_dir ".cfg";
    let cfg_fname_exists = Sys.file_exists (DB.filename_to_string cfg_fname);
    if cfg_fname_exists {
      Some source_file
    } else {
      None
    }
  };
