(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type labeled_profiler_sample = string * Typ.Procname.Set.t [@@deriving compare]

let equal_labeled_profiler_sample = [%compare.equal: labeled_profiler_sample]

let from_java_profiler_samples j ~use_signature =
  let process_methods methods =
    Typ.Procname.Set.of_list
      (List.map
         ~f:(fun {Java_profiler_samples_t.classname; methodname; signature} ->
           JProcname.create_procname ~classname ~methodname ~signature ~use_signature )
         methods)
  in
  List.map j ~f:(fun {Java_profiler_samples_t.test; methods} -> (test, process_methods methods))


let from_json_string str ~use_signature =
  from_java_profiler_samples
    (Java_profiler_samples_j.java_profiler_samples_of_string str)
    ~use_signature


let from_json_file file ~use_signature =
  from_java_profiler_samples
    (Atdgen_runtime.Util.Json.from_file Java_profiler_samples_j.read_java_profiler_samples file)
    ~use_signature
