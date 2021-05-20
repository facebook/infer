(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module ConfigProcnameSet = Caml.Set.Make (struct
  (* workaround: since there is no way to have @@deriving directive
     in atd declaration, we redefine the type *)
  type t = Config_impact_data_t.config_item = {method_name: string; class_name: string}
  [@@deriving compare]
end)

let cut_objc_parameters name = List.hd_exn (String.split name ~on:':')

let read_file_config_data fname =
  let config_list =
    try Atdgen_runtime.Util.Json.from_file Config_impact_data_j.read_config_data fname
    with e ->
      L.user_warning "Failed to read file '%s': %s@." fname (Exn.to_string e) ;
      []
  in
  List.fold config_list ~init:ConfigProcnameSet.empty
    ~f:(fun acc {Config_impact_data_t.method_name; class_name} ->
      ConfigProcnameSet.add
        { method_name= cut_objc_parameters method_name
        ; class_name= Procname.replace_java_inner_class_prefix_regex class_name }
        acc )


let is_in_config_data_file =
  let config_data =
    Option.value_map Config.config_impact_data_file ~default:ConfigProcnameSet.empty
      ~f:read_file_config_data
  in
  fun proc_name ->
    let config_item =
      { Config_impact_data_t.method_name= Procname.get_method proc_name |> cut_objc_parameters
      ; class_name=
          Procname.get_class_name proc_name
          |> Option.value_map ~default:"" ~f:Procname.replace_java_inner_class_prefix_regex }
    in
    ConfigProcnameSet.mem config_item config_data
