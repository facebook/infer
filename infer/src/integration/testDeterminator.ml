(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format
module JPS = JavaProfilerSamples
module YB = Yojson.Basic

(* a flag used to make the method search signature sensitive *)
let use_method_signature = false

module MethodRangeMap = struct
  let split_class_method_name qualified_method_name =
    String.rsplit2_exn qualified_method_name ~on:'.'


  let create_java_method_range_map code_graph_file_opt =
    match code_graph_file_opt with
    | Some code_graph_file ->
        let open Java_method_decl_j in
        let json_string =
          match Utils.read_file code_graph_file with
          | Ok cl_list ->
              let json = List.fold cl_list ~init:"" ~f:(fun acc s -> acc ^ s) in
              json
          | Error _ ->
              L.die UserError "Could not read file %s" code_graph_file
        in
        let method_decls = java_method_decls_of_string json_string in
        List.fold method_decls ~init:Typ.Procname.Map.empty ~f:(fun acc decl ->
            let start_location =
              { Location.line= decl.start_line
              ; col= -1
              ; file= SourceFile.create ~warn_on_error:false decl.source_file }
            in
            let end_location =
              { Location.line= decl.end_line
              ; col= -1
              ; file= SourceFile.create ~warn_on_error:false decl.source_file }
            in
            let range = (start_location, end_location) in
            let classname, methodname = split_class_method_name decl.method_name in
            match decl.signature with
            | Some signature ->
                let signature =
                  if use_method_signature then signature
                  else
                    (* When we should not use the signature we use 'void ()' *)
                    JPS.JNI.void_method_with_no_arguments
                in
                let key = JPS.create_procname ~classname ~methodname ~signature in
                Typ.Procname.Map.add key range acc
            | None ->
                acc )
    | _ ->
        L.die UserError "Missing method declaration info argument"
end

module DiffLines = struct
  (* This is a map
        file name |--> {set of changed line }
  *)

  (* Read the file containing info on changed lines and populate the map *)
  let create_changed_lines_map changed_lines_file_opt =
    match changed_lines_file_opt with
    | Some changed_lines_file -> (
      match Utils.read_file changed_lines_file with
      | Ok cl_list ->
          List.fold cl_list ~init:String.Map.empty ~f:(fun acc cl_item ->
              let fname, cl = String.rsplit2_exn ~on:':' cl_item in
              String.Map.set acc ~key:fname ~data:(FileDiff.parse_unix_diff cl) )
      | Error _ ->
          L.die UserError "Could not read file %s" changed_lines_file )
    | None ->
        L.die UserError "Missing modified lines argument"


  [@@@warning "-32"]

  let pp_changed_lines fmt map =
    F.fprintf fmt "--- Changed Lines Map ---@\n" ;
    String.Map.iteri map ~f:(fun ~key ~data ->
        F.fprintf fmt "%s --> [%a]@\n" key (Pp.seq ~sep:", " F.pp_print_int) data )
end

[@@@warning "-32"]

let pp_profiler_sample_set fmt s =
  F.fprintf fmt " (set size = %i) " (Typ.Procname.Set.cardinal s) ;
  Typ.Procname.Set.iter (fun m -> F.fprintf fmt "@\n      <Method:>  %a " Typ.Procname.pp m) s


module TestSample = struct
  let read_test_sample test_samples_file_opt =
    match test_samples_file_opt with
    | Some test_samples_file ->
        L.(debug TestDeterminator Medium)
          "Reading Profiler Samples File '%s'....@\n" test_samples_file ;
        JPS.from_json_file test_samples_file ~use_signature:use_method_signature
    | None ->
        L.die UserError "Missing profiler samples argument"


  [@@@warning "-32"]

  let pp_map fmt labeled_test_samples =
    List.iter labeled_test_samples ~f:(fun (label, profiler_samples) ->
        F.fprintf fmt "=== Samples for %s ===@\n%a@\n=== End Samples for %s ===@\n" label
          pp_profiler_sample_set profiler_samples label )
end

let in_range l range = l >= (fst range).Location.line && l <= (snd range).Location.line

let affected_methods method_range_map file_changed_lines changed_lines =
  Typ.Procname.Map.fold
    (fun key ((l1, _) as range) acc ->
      let method_file = SourceFile.to_string l1.Location.file in
      if
        String.equal method_file file_changed_lines
        && List.exists ~f:(fun l -> in_range l range) changed_lines
      then Typ.Procname.Set.add key acc
      else acc )
    method_range_map Typ.Procname.Set.empty


let compute_affected_methods_java changed_lines_map method_range_map =
  String.Map.fold changed_lines_map ~init:Typ.Procname.Set.empty
    ~f:(fun ~key:file_changed_lines ~data acc ->
      let am = affected_methods method_range_map file_changed_lines data in
      Typ.Procname.Set.union am acc )


let compute_affected_methods_clang ~clang_range_map ~source_file ~changed_lines_map =
  let fname = SourceFile.to_rel_path source_file in
  match String.Map.find changed_lines_map fname with
  | Some changed_lines ->
      affected_methods clang_range_map fname changed_lines
  | None ->
      Typ.Procname.Set.empty


let emit_relevant_methods relevant_methods =
  let cleaned_methods =
    List.dedup_and_sort ~compare:String.compare
      (List.map (Typ.Procname.Set.elements relevant_methods) ~f:Typ.Procname.to_string)
  in
  let json = `List (List.map ~f:(fun t -> `String t) cleaned_methods) in
  let outpath = Config.results_dir ^/ Config.export_changed_functions_output in
  YB.to_file outpath json


let compute_and_emit_relevant_methods ~clang_range_map ~source_file =
  let changed_lines_file = Config.modified_lines in
  let changed_lines_map = DiffLines.create_changed_lines_map changed_lines_file in
  let relevant_methods =
    compute_affected_methods_clang ~clang_range_map ~source_file ~changed_lines_map
  in
  emit_relevant_methods relevant_methods


(* test_to_run = { n | Affected_Method /\ ts_n != 0 } *)
let test_to_run ?clang_range_map ?source_file () =
  let test_samples_file = Config.profiler_samples in
  let code_graph_file = Config.method_decls_info in
  let changed_lines_file = Config.modified_lines in
  let changed_lines_map = DiffLines.create_changed_lines_map changed_lines_file in
  let affected_methods =
    match (clang_range_map, source_file) with
    | Some clang_range_map, Some source_file ->
        compute_affected_methods_clang ~source_file ~clang_range_map ~changed_lines_map
    | _ (* Java case *) ->
        let method_range = MethodRangeMap.create_java_method_range_map code_graph_file in
        compute_affected_methods_java changed_lines_map method_range
  in
  let profiler_samples = TestSample.read_test_sample test_samples_file in
  if Typ.Procname.Set.is_empty affected_methods then []
  else
    List.fold profiler_samples ~init:[] ~f:(fun acc (label, profiler_samples) ->
        let intersection = Typ.Procname.Set.inter affected_methods profiler_samples in
        if Typ.Procname.Set.is_empty intersection then acc else label :: acc )


let emit_tests_to_run relevant_tests =
  let json = `List (List.map ~f:(fun t -> `String t) relevant_tests) in
  let outpath = Config.results_dir ^/ Config.test_determinator_output in
  YB.to_file outpath json


let compute_and_emit_test_to_run ?clang_range_map ?source_file () =
  let relevant_tests = test_to_run ?clang_range_map ?source_file () in
  emit_tests_to_run relevant_tests
