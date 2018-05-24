(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
module L = Logging
module F = Format
open JavaProfilerSamples
open! IStd

module RangeMap = Caml.Map.Make (struct
  type t = Typ.Procname.t

  let compare = Typ.Procname.compare
end)

let initialized_test_determinator = ref false

let is_test_determinator_init () = !initialized_test_determinator

module MethodRangeMap = struct
  let map : (Location.t * Location.t) RangeMap.t ref = ref RangeMap.empty

  let method_range_map () = !map

  let split_class_method_name qualified_method_name =
    String.rsplit2_exn qualified_method_name ~on:'.'


  let create_java_method_range_map code_graph_file' =
    match code_graph_file' with
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
        let map' =
          List.fold method_decls ~init:RangeMap.empty ~f:(fun acc decl ->
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
                  let key = JavaProfilerSamples.create ~classname ~methodname ~signature in
                  RangeMap.add key range acc
              | None ->
                  acc )
        in
        map := map'
    | _ ->
        L.die UserError "Missing method declaration info argument"


  let create_clang_method_range_map cfg =
    let update_method_range_map pdesc =
      let start_node = Procdesc.get_start_node pdesc in
      let exit_node = Procdesc.get_exit_node pdesc in
      let range = (Procdesc.Node.get_loc start_node, Procdesc.Node.get_loc exit_node) in
      let key = Procdesc.get_proc_name pdesc in
      map := RangeMap.add key range !map
    in
    Typ.Procname.Hash.iter (fun _ pdesc -> update_method_range_map pdesc) cfg


  let pp_map fmt () =
    let pp_map' fmt m =
      RangeMap.iter
        (fun key range ->
          F.fprintf fmt "@\n      %a --> (%a,%a)" Typ.Procname.pp key Location.pp (fst range)
            Location.pp (snd range) )
        m
    in
    F.fprintf fmt "@\n--- Method Range Map ---%a@\n--- End Method Range Map --- @\n" pp_map' !map
end

module DiffLines = struct
  (* This is a map 
        file name |--> {set of changed line } 
  *)
  let map : int list String.Map.t ref = ref String.Map.empty

  let changed_lines_map () = !map

  (* Read the file containing info on changed lines and populate the map *)
  let init_changed_lines_map changed_lines_file' =
    match changed_lines_file' with
    | Some changed_lines_file
      -> (
        L.progress "@\n Initializing modified lines map from file '%s'... " changed_lines_file ;
        match Utils.read_file changed_lines_file with
        | Ok cl_list ->
            let changed_lines =
              List.fold cl_list ~init:String.Map.empty ~f:(fun acc cl_item ->
                  let fname, cl = String.rsplit2_exn ~on:':' cl_item in
                  String.Map.set acc ~key:fname ~data:(FileDiff.parse_unix_diff cl) )
            in
            L.progress " done! @\n" ;
            map := changed_lines
        | Error _ ->
            L.die UserError "Could not read file %s" changed_lines_file )
    | _ ->
        L.die UserError "Missing modified lines argument"


  let print_changed_lines () =
    L.(debug Analysis Medium) "@\n--- Changed Lines Map --- " ;
    String.Map.iteri !map ~f:(fun ~key:k ~data:d ->
        L.(debug Analysis Medium) "\n     %s --> [" k ;
        List.iter d ~f:(L.(debug Analysis Medium) " %i ") ;
        L.(debug Analysis Medium) " ] " ) ;
    L.(debug Analysis Medium) "@\n--- End Changed Lines Map --- @\n"
end

let pp_profiler_sample_set fmt s =
  F.fprintf fmt "size = %i" (ProfilerSample.cardinal s) ;
  ProfilerSample.iter (fun m -> F.fprintf fmt "@\n      >  %a " Typ.Procname.pp m) s


module TestSample = struct
  let labeled_test_samples = ref []

  let test_sample () = !labeled_test_samples

  let init_test_sample test_samples_file' =
    match test_samples_file' with
    | Some test_samples_file ->
        L.progress "@\nReading Profiler Samples File '%s'...." test_samples_file ;
        let ts = JavaProfilerSamples.from_json_file test_samples_file in
        labeled_test_samples := ts
    | _ ->
        L.die UserError "Missing profiler samples argument"


  let pp_map fmt () =
    List.iter !labeled_test_samples ~f:(fun (label, profiler_samples) ->
        F.fprintf fmt "=== Samples for %s ===@\n%a@\n=== End Samples for %s ===@\n" label
          pp_profiler_sample_set profiler_samples label )
end

let in_range l range = l >= (fst range).Location.line && l <= (snd range).Location.line

let affected_methods method_range_map changed_lines =
  RangeMap.fold
    (fun key range acc ->
      if List.exists ~f:(fun l -> in_range l range) changed_lines then
        (*L.progress "@\n ADDING '%a' in affected methods..." Typ.Procname.pp key ; *)
        ProfilerSample.add key acc
      else acc )
    method_range_map ProfilerSample.empty


let compute_affected_methods_java changed_lines_map method_range_map =
  let affected_methods =
    String.Map.fold changed_lines_map ~init:ProfilerSample.empty ~f:(fun ~key:_ ~data acc ->
        let am = affected_methods method_range_map data in
        ProfilerSample.union am acc )
  in
  L.progress "== Resulting Affected Methods ==@\n%a@\n== End Affected Methods ==@\n"
    pp_profiler_sample_set affected_methods ;
  affected_methods


let compute_affected_methods_clang source_file changed_lines_map method_range_map =
  let fname = SourceFile.to_rel_path source_file in
  L.progress "@\n Looking for file %s in changed-line map..." fname ;
  match String.Map.find changed_lines_map fname with
  | Some changed_lines ->
      L.progress " found!@\n" ;
      let affected_methods = affected_methods method_range_map changed_lines in
      L.progress "== Resulting Affected Methods ==@\n%a@\n== End Affected Methods ==@\n"
        pp_profiler_sample_set affected_methods ;
      affected_methods
  | None ->
      L.progress "@\n%s not found in changed-line map. Nothing else to do for it.@\n" fname ;
      ProfilerSample.empty


let relevant_tests = ref []

let _get_relevant_test_to_run () = !relevant_tests

let print_test_to_run () =
  L.progress "@\n [TEST DETERMINATOR] Relevant Tests to run = [" ;
  List.iter ~f:(L.progress " %s ") !relevant_tests ;
  L.progress " ] @\n" ;
  let json = `List (List.map ~f:(fun t -> `String t) !relevant_tests) in
  Yojson.Basic.to_file (Config.results_dir ^/ "test_determinator.json") json


let init_clang cfg changed_lines_file test_samples_file =
  DiffLines.init_changed_lines_map changed_lines_file ;
  DiffLines.print_changed_lines () ;
  MethodRangeMap.create_clang_method_range_map cfg ;
  L.(debug Analysis Medium) "%a@\n" MethodRangeMap.pp_map () ;
  TestSample.init_test_sample test_samples_file ;
  L.(debug Analysis Medium) "%a@\n" TestSample.pp_map () ;
  initialized_test_determinator := true


let init_java changed_lines_file test_samples_file code_graph_file =
  DiffLines.init_changed_lines_map changed_lines_file ;
  DiffLines.print_changed_lines () ;
  MethodRangeMap.create_java_method_range_map code_graph_file ;
  L.(debug Analysis Medium) "%a@\n" MethodRangeMap.pp_map () ;
  TestSample.init_test_sample test_samples_file ;
  L.(debug Analysis Medium) "%a@\n" TestSample.pp_map () ;
  initialized_test_determinator := true


(* test_to_run = { n | Affected_Method /\ ts_n != 0 } *)
let _test_to_run_clang source_file cfg changed_lines_file test_samples_file =
  L.progress "@\n ***** Start Test Determinator for  %s ***** @\n"
    (SourceFile.to_string source_file) ;
  if is_test_determinator_init () then () else init_clang cfg changed_lines_file test_samples_file ;
  let affected_methods =
    compute_affected_methods_clang source_file (DiffLines.changed_lines_map ())
      (MethodRangeMap.method_range_map ())
  in
  let test_to_run =
    if ProfilerSample.is_empty affected_methods then []
    else
      List.fold (TestSample.test_sample ()) ~init:[] ~f:(fun acc (label, profiler_samples) ->
          let intersection = ProfilerSample.inter affected_methods profiler_samples in
          if ProfilerSample.is_empty intersection then acc else label :: acc )
  in
  relevant_tests := List.append test_to_run !relevant_tests


let test_to_run_java changed_lines_file test_samples_file code_graph_file =
  L.progress "@\n ***** Start Test Determinator ***** @\n" ;
  if is_test_determinator_init () then ()
  else init_java changed_lines_file test_samples_file code_graph_file ;
  let affected_methods =
    compute_affected_methods_java (DiffLines.changed_lines_map ())
      (MethodRangeMap.method_range_map ())
  in
  let test_to_run =
    if ProfilerSample.is_empty affected_methods then []
    else
      List.fold (TestSample.test_sample ()) ~init:[] ~f:(fun acc (label, profiler_samples) ->
          let intersection = ProfilerSample.inter affected_methods profiler_samples in
          if ProfilerSample.is_empty intersection then acc else label :: acc )
  in
  relevant_tests := List.append test_to_run !relevant_tests
