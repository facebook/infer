(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging

(** Module to manage the table of attributes. *)

let serializer : ProcAttributes.t Serialization.serializer =
  Serialization.create_serializer Serialization.attributes_key

let attributes_filename pname =
  let pname_file = Procname.to_filename pname in
  pname_file ^ ".attr"

(** path to the .attr file for the given procedure in the current results directory *)
let res_dir_attr_filename pname =
  DB.Results_dir.path_to_filename
    DB.Results_dir.Abs_root [Config.attributes_dir_name; attributes_filename pname]

let store_attributes proc_attributes =
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  let attributes_file = res_dir_attr_filename proc_name in
  let should_write = (* only overwrite defined procedures *)
    proc_attributes.ProcAttributes.is_defined ||
    not (DB.file_exists attributes_file) in
  if should_write then
    Serialization.to_file serializer attributes_file proc_attributes

let load_attributes proc_name =
  let attributes_file = res_dir_attr_filename proc_name in
  Serialization.from_file serializer attributes_file
