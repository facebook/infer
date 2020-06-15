(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format
module YB = Yojson.Basic
module YBU = Yojson.Basic.Util

(* a flag used to make the method search signature sensitive *)
let use_signature = false

module MethodRangeMap = struct
  let split_class_method_name qualified_method_name = String.rsplit2 qualified_method_name ~on:'.'

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
        List.fold method_decls ~init:Procname.Map.empty ~f:(fun acc decl ->
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
            match (split_class_method_name decl.method_name, decl.signature) with
            | Some (classname, methodname), Some signature ->
                let key =
                  JProcname.create_procname ~use_signature ~classname ~methodname ~signature
                in
                Procname.Map.add key (range, ()) acc
            | _ ->
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
              match String.rsplit2 ~on:':' cl_item with
              | None ->
                  acc
              | Some (fname, cl) ->
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
  F.fprintf fmt " (set size = %i) " (Procname.Set.cardinal s) ;
  Procname.Set.iter (fun m -> F.fprintf fmt "@\n      <Method:>  %a " Procname.pp m) s


module TestSample = struct
  let read_java_test_sample test_samples_file_opt =
    match test_samples_file_opt with
    | Some test_samples_file ->
        L.(debug TestDeterminator Medium)
          "Reading Profiler Samples File '%s'....@\n" test_samples_file ;
        JavaProfilerSamples.from_json_file test_samples_file ~use_signature
    | None ->
        L.die UserError "Missing profiler samples argument"


  let read_clang_test_sample test_samples_file_opt =
    match test_samples_file_opt with
    | Some test_samples_file ->
        Atdgen_runtime.Util.Json.from_file Clang_profiler_samples_j.read_profiler_samples
          test_samples_file
    | None ->
        L.die UserError "Missing profiler samples argument"


  [@@@warning "-32"]

  let pp_map fmt labeled_test_samples =
    List.iter labeled_test_samples ~f:(fun (label, profiler_samples) ->
        F.fprintf fmt "=== Samples for %s ===@\n%a@\n=== End Samples for %s ===@\n" label
          pp_profiler_sample_set profiler_samples label )
end

let in_range l range = l >= (fst range).Location.line && l <= (snd range).Location.line

let is_file_in_changed_lines file_changed_lines changed_lines range =
  let l1, _ = range in
  let method_file = SourceFile.to_string l1.Location.file in
  String.equal method_file file_changed_lines
  && List.exists ~f:(fun l -> in_range l range) changed_lines


let affected_methods method_range_map file_changed_lines changed_lines =
  Procname.Map.fold
    (fun key (range, _) acc ->
      if is_file_in_changed_lines file_changed_lines changed_lines range then
        Procname.Set.add key acc
      else acc )
    method_range_map Procname.Set.empty


let compute_affected_methods_java changed_lines_map method_range_map =
  String.Map.fold changed_lines_map ~init:Procname.Set.empty
    ~f:(fun ~key:file_changed_lines ~data acc ->
      let am = affected_methods method_range_map file_changed_lines data in
      Procname.Set.union am acc )


let compute_affected_methods_clang ~clang_range_map ~source_file ~changed_lines_map =
  let fname = SourceFile.to_rel_path source_file in
  match String.Map.find changed_lines_map fname with
  | Some changed_lines ->
      affected_methods clang_range_map fname changed_lines
  | None ->
      Procname.Set.empty


let compute_affected_proc_names_clang ~clang_range_map ~source_file ~changed_lines_map =
  let fname = SourceFile.to_rel_path source_file in
  match String.Map.find changed_lines_map fname with
  | Some changed_lines ->
      Procname.Map.fold
        (fun _ (range, clang_proc) acc ->
          if is_file_in_changed_lines fname changed_lines range then clang_proc :: acc else acc )
        clang_range_map []
  | None ->
      []


let emit_relevant_methods relevant_methods =
  let cleaned_methods =
    List.dedup_and_sort ~compare:String.compare
      (List.map (Procname.Set.elements relevant_methods) ~f:Procname.to_string)
  in
  let json = `List (List.map ~f:(fun t -> `String t) cleaned_methods) in
  let outpath = ResultsDir.get_path ChangedFunctions in
  YB.to_file outpath json


let compute_and_emit_relevant_methods ~clang_range_map ~source_file =
  let changed_lines_file = Config.modified_lines in
  let changed_lines_map = DiffLines.create_changed_lines_map changed_lines_file in
  let relevant_methods =
    compute_affected_methods_clang ~clang_range_map ~source_file ~changed_lines_map
  in
  emit_relevant_methods relevant_methods


(* test_to_run = { n | Affected_Method /\ ts_n != 0 } *)
let java_test_to_run () =
  let test_samples_file = Config.profiler_samples in
  let code_graph_file = Config.method_decls_info in
  let changed_lines_file = Config.modified_lines in
  let changed_lines_map = DiffLines.create_changed_lines_map changed_lines_file in
  let method_range = MethodRangeMap.create_java_method_range_map code_graph_file in
  let affected_methods = compute_affected_methods_java changed_lines_map method_range in
  let profiler_samples = TestSample.read_java_test_sample test_samples_file in
  if Procname.Set.is_empty affected_methods then []
  else
    List.fold profiler_samples ~init:[] ~f:(fun acc (label, profiler_samples) ->
        let intersection = Procname.Set.inter affected_methods profiler_samples in
        if Procname.Set.is_empty intersection then acc else label :: acc )


let remove_llvm_suffix_native_symbols native_symbols =
  let remove_llvm_suffix_native_symbol native_symbol =
    let remove_llvm_suffix name =
      (* The first dot of .llvm.... is the first dot after the name,
         because neither names nor mangled names can have dots. *)
      match String.lsplit2 name ~on:'.' with
      | Some (name_no_suffix, _) ->
          name_no_suffix
      | None ->
          name
    in
    let native_symbol_name = remove_llvm_suffix native_symbol.Clang_profiler_samples_t.name in
    let native_symbol_mangled_name =
      Option.map ~f:remove_llvm_suffix native_symbol.Clang_profiler_samples_t.mangled_name
    in
    { native_symbol with
      Clang_profiler_samples_t.name= native_symbol_name
    ; mangled_name= native_symbol_mangled_name }
  in
  List.map ~f:remove_llvm_suffix_native_symbol native_symbols


(* The clang plugin exports C++ mangled names in hashed form for perf reasons, so here we
hash the incoming mangled names in the profiler samples, so that we can compare them. *)
let add_hash_mangled_native_symbols native_symbols =
  let add_hash_mangled_native_symbol native_symbol =
    let hash_mangled mangled_name =
      match mangled_name with
      | Some mangled_name ->
          Some (Fnv64Hash.fnv64_hash mangled_name)
      | None ->
          None
    in
    let native_symbol_hashed_mangled_name =
      hash_mangled native_symbol.Clang_profiler_samples_t.mangled_name
    in
    { native_symbol with
      Clang_profiler_samples_t.hashed_mangled_name= native_symbol_hashed_mangled_name }
  in
  List.map ~f:add_hash_mangled_native_symbol native_symbols


let match_mangled_names affected_function_mangled_name native_symbol =
  match
    (affected_function_mangled_name, native_symbol.Clang_profiler_samples_t.hashed_mangled_name)
  with
  | Some affected_function_mangled_name, Some hashed_mangled_name ->
      String.equal affected_function_mangled_name hashed_mangled_name
  | _ ->
      false


let match_profiler_samples_affected_methods native_symbols affected_methods =
  let match_samples_method affected_method =
    let match_sample_method affected_method native_symbol =
      match affected_method with
      | Some
          (ClangProc.CFunction
            {name= affected_function_name; mangled_name= affected_function_mangled_name}) ->
          (* This is needed because for simple c functions from the standard library, the name
             matches but the mangled name doesn't quite match, there is a __1 at the beginning
             in the name we get from the plugin, but not in the name we get from the profiler samples. *)
          if String.equal affected_function_name native_symbol.Clang_profiler_samples_t.name then
            true
          else match_mangled_names affected_function_mangled_name native_symbol
      | Some (ClangProc.ObjcMethod {mangled_name= affected_method_mangled_name}) ->
          String.equal affected_method_mangled_name native_symbol.Clang_profiler_samples_t.name
      | Some (ClangProc.ObjcBlock {mangled_name= affected_block_mangled_name}) -> (
        match native_symbol.Clang_profiler_samples_t.mangled_name with
        | Some native_symbol_mangled_name ->
            (* Assuming mangled name is there for blocks *)
            String.equal native_symbol_mangled_name affected_block_mangled_name
        | None ->
            false )
      | Some (ClangProc.CppMethod {mangled_name= affected_function_mangled_name}) ->
          match_mangled_names (Some affected_function_mangled_name) native_symbol
      | _ ->
          false
      (* TODO: deal with mangled names, other method kinds *)
    in
    List.exists ~f:(match_sample_method affected_method) native_symbols
  in
  List.exists ~f:match_samples_method affected_methods


(* test_to_run = { n | Affected_Method /\ ts_n != 0 } *)
let clang_test_to_run ~clang_range_map ~source_file () =
  let test_samples_file = Config.profiler_samples in
  let changed_lines_file = Config.modified_lines in
  let changed_lines_map = DiffLines.create_changed_lines_map changed_lines_file in
  let affected_methods =
    compute_affected_proc_names_clang ~source_file ~clang_range_map ~changed_lines_map
  in
  let profiler_samples = TestSample.read_clang_test_sample test_samples_file in
  if List.is_empty affected_methods then []
  else
    List.fold profiler_samples ~init:[]
      ~f:(fun acc ({test; native_symbols} : Clang_profiler_samples_t.profiler_sample) ->
        let native_symbols_no_suffix = remove_llvm_suffix_native_symbols native_symbols in
        let native_symbols_with_hash_mangled =
          add_hash_mangled_native_symbols native_symbols_no_suffix
        in
        if match_profiler_samples_affected_methods native_symbols_with_hash_mangled affected_methods
        then test :: acc
        else acc )


let emit_tests_to_run_java relevant_tests =
  let json = `List (List.map ~f:(fun t -> `String t) relevant_tests) in
  YB.to_file (ResultsDir.get_path TestDeterminatorReport) json


let emit_tests_to_run_clang source_file relevant_tests =
  if not (List.is_empty relevant_tests) then (
    let json = `List (List.map ~f:(fun t -> `String t) relevant_tests) in
    let abbrev_source_file = DB.source_file_encoding source_file in
    let test_determinator_results_path = ResultsDir.get_path TestDeterminatorTempResults in
    let outpath = test_determinator_results_path ^/ abbrev_source_file ^ ".json" in
    Utils.create_dir test_determinator_results_path ;
    Utils.write_json_to_file outpath json )


let compute_and_emit_test_to_run ?clang_range_map ?source_file () =
  match (clang_range_map, source_file) with
  | Some clang_range_map, Some source_file ->
      let relevant_tests = clang_test_to_run ~clang_range_map ~source_file () in
      emit_tests_to_run_clang source_file relevant_tests
  | _ ->
      let relevant_tests = java_test_to_run () in
      emit_tests_to_run_java relevant_tests


let merge_test_determinator_results () =
  let main_results_list = ref [] in
  let merge_json_results intermediate_result =
    let changed_json =
      try YB.from_file intermediate_result |> YBU.to_list with Sys_error _ -> []
    in
    main_results_list := List.append changed_json !main_results_list
  in
  let test_determinator_results_path = ResultsDir.get_path TestDeterminatorTempResults in
  Utils.directory_iter merge_json_results test_determinator_results_path ;
  let main_results_list_sorted =
    List.dedup_and_sort
      ~compare:(fun s1 s2 ->
        match (s1, s2) with `String s1, `String s2 -> String.compare s1 s2 | _ -> 0 )
      !main_results_list
  in
  let main_results_file = ResultsDir.get_path TestDeterminatorReport in
  YB.to_file main_results_file (`List main_results_list_sorted) ;
  Logging.progress "Finished executing Test Determinator successfully, results are in %s@."
    main_results_file
