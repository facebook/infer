(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
module L = Logging
open JavaProfilerSamples
open! IStd

module LineRangeMap = Caml.Map.Make (struct
  type t = Typ.Procname.t

  let compare = Typ.Procname.compare
end)

let update_line_range_map pdesc map =
  let start_node = Procdesc.get_start_node pdesc in
  let exit_node = Procdesc.get_exit_node pdesc in
  let range = (Procdesc.Node.get_loc start_node, Procdesc.Node.get_loc exit_node) in
  let key = Procdesc.get_proc_name pdesc in
  LineRangeMap.add key range map


let in_range l range = l >= (fst range).Location.line && l <= (snd range).Location.line

let affected_methods line_range_map changed_lines =
  LineRangeMap.fold
    (fun key range acc ->
      if List.exists ~f:(fun l -> in_range l range) changed_lines then ProfilerSample.add key acc
      else acc )
    line_range_map ProfilerSample.empty


let compute_affected_methods fname cfg files_changed_lines_map =
  L.(debug Capture Verbose) "@\n Looking for file %s in files_changed_line_map @\n" fname ;
  match String.Map.find files_changed_lines_map fname with
  | Some changed_lines ->
      let line_range_map : (Location.t * Location.t) LineRangeMap.t = LineRangeMap.empty in
      let line_range_map' =
        Typ.Procname.Hash.fold
          (fun _ pdesc acc -> update_line_range_map pdesc acc)
          cfg line_range_map
      in
      L.(debug Capture Verbose) "@\n Line Range Map" ;
      LineRangeMap.iter
        (fun key range ->
          L.(debug Capture Verbose)
            "@\n      %a --> (%a,%a)" Typ.Procname.pp key Location.pp (fst range) Location.pp
            (snd range) )
        line_range_map' ;
      L.(debug Capture Verbose) "@\n End Line Range Map @\n" ;
      let affected_methods = affected_methods line_range_map' changed_lines in
      L.(debug Capture Verbose) "@\n == Start Printing Affected Methods == " ;
      ProfilerSample.iter
        (fun m -> L.(debug Capture Verbose) "@\n     METHOD>  %a " Typ.Procname.pp m)
        affected_methods ;
      L.(debug Capture Verbose) "@\n == End Printing Affected Methods == @\n" ;
      affected_methods
  | None ->
      L.(debug Capture Verbose)
        "@\n File name %s was not found in files_changed_line_map @\n" fname ;
      ProfilerSample.empty


let read_changed_lines_file changed_lines_file =
  match Utils.read_file changed_lines_file with
  | Ok cl_list ->
      let changed_lines =
        List.fold cl_list ~init:String.Map.empty ~f:(fun acc cl_item ->
            let fname, cl = String.rsplit2_exn ~on:':' cl_item in
            String.Map.set acc ~key:fname ~data:(FileDiff.parse_unix_diff cl) )
      in
      changed_lines
  | Error _ ->
      String.Map.empty


let print_test_to_run test_to_run =
  L.result "@\n [Result Test Determinator:] Test to run = [" ;
  List.iter ~f:(L.result " %s ") test_to_run ;
  L.result " ] @\n"


let print_changed_lines changed_lines =
  L.(debug Capture Verbose) "@\n Changed lines = {" ;
  String.Map.iteri changed_lines ~f:(fun ~key:k ~data:d ->
      L.(debug Capture Verbose) "\n     %s --> [" k ;
      List.iter d ~f:(L.(debug Capture Verbose) " %i ") ;
      L.(debug Capture Verbose) " ] " ) ;
  L.(debug Capture Verbose) "@\n } @\n"


(* test_to_run = { n | Affected_Method /\ ts_n != 0 } *)
let test_to_run source_file cfg changed_lines_file test_samples_file =
  L.(debug Capture Verbose) "@\n ***** Start Test Determinator ***** @\n" ;
  let fname = SourceFile.to_rel_path source_file in
  let changed_lines = read_changed_lines_file changed_lines_file in
  print_changed_lines changed_lines ;
  let test_samples = JavaProfilerSamples.from_json_file test_samples_file in
  let affected_methods = compute_affected_methods fname cfg changed_lines in
  let test_to_run =
    List.fold test_samples ~init:[] ~f:(fun acc (label, profiler_samples) ->
        let intersection = ProfilerSample.inter affected_methods profiler_samples in
        if ProfilerSample.is_empty intersection then acc else label :: acc )
  in
  print_test_to_run test_to_run
