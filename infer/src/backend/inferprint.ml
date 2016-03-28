(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format
open Jsonbug_j

(** Outfile to save the latex report *)
let latex = ref None

(** command line flag: if true, print whole seconds only *)
let whole_seconds = ref false

(** If true, read all .specs files from the results dir *)
let results_dir_cmdline = ref false

(** Outfile to save bugs stats in csv format *)
let bugs_csv = ref None

(** Outfile to save bugs stats in JSON format *)
let bugs_json = ref None

(** Outfile to save bugs stats in txt format *)
let bugs_txt = ref None

(** Outfile to save bugs stats in xml format *)
let bugs_xml = ref None

(** Outfile to save procedures stats in csv format *)
let procs_csv = ref None

(** Outfile to save procedures stats in xml format *)
let procs_xml = ref None

(** Outfile to save call stats in csv format *)
let calls_csv = ref None

(** Outfile to save the analysis report *)
let report = ref None

(** command line flag: if true, produce a svg file *)
let svg = ref false

(** command line flag: if true, export specs to xml files *)
let xml_specs = ref false

(** command line flag: if true, produce unit test for each spec *)
let unit_test = ref false

(** command line flag: if true, do not print the spec to standard output *)
let quiet = ref false

(** command line flag: if true, print stats about preconditions to standard output *)
let precondition_stats = ref false

(** name of the file to load analysis results from *)
let load_analysis_results = ref None

(** If true then include Infer source code locations in json reports *)
let reports_include_ml_loc = ref false

(** name of the file to load save results to *)
let save_analysis_results = ref None

(** command-line option to print the location of the copy of a source file *)
let source_file_copy = ref None

(** command line option to test the filtering based on .inferconfig *)
let test_filtering = ref false

(** Setup the analyzer in order to filter out errors for this analyzer only *)
let analyzer = ref None

let handle_source_file_copy_option () = match !source_file_copy with
  | None -> ()
  | Some source_file ->
      let source_in_resdir = DB.source_file_in_resdir source_file in
      F.fprintf F.std_formatter "%s@." (DB.filename_to_string source_in_resdir);
      exit 0

let canonic_path_from_string s =
  if s = Filename.dir_sep then s
  else Filename.concat (Filename.dirname s) (Filename.basename s) ^ Filename.dir_sep

let arg_desc =
  let base_arg =
    let desc =
      [
        "-bugs",
        Arg.String (fun s -> bugs_csv := create_outfile s),
        Some "bugs.csv",
        "create file bugs.csv containing a list of bugs in CSV format"
        ;
        "-bugs_json",
        Arg.String (fun s -> bugs_json := create_outfile s),
        Some "bugs.json",
        "create file bugs.json containing a list of bugs in JSON format"
        ;
        "-bugs_txt",
        Arg.String (fun s -> bugs_txt := create_outfile s),
        Some "bugs.txt",
        "create file bugs.txt containing a list of bugs in text format"
        ;
        "-bugs_xml",
        Arg.String (fun s -> bugs_xml := create_outfile s),
        Some "bugs.xml",
        "create file bugs.xml containing a list of bugs in XML format"
        ;
        "-calls",
        Arg.String (fun s -> calls_csv := create_outfile s),
        Some "calls.csv",
        "write individual calls in csv format to file.csv"
        ;
        "-load_results",
        Arg.String (fun s -> load_analysis_results := Some s),
        Some "file.iar",
        "load analysis results from Infer Analysis Results file file.iar"
        ;
        "-procs",
        Arg.String (fun s -> procs_csv := create_outfile s),
        Some "procs.csv",
        "create file procs.csv containing statistics for each procedure in CSV format"
        ;
        "-procs_xml",
        Arg.String (fun s -> procs_xml := create_outfile s),
        Some "procs.xml",
        "create file procs.xml containing statistics for each procedure in XML format"
        ;
        "-results_dir",
        Arg.String (fun s -> results_dir_cmdline := true; Config.results_dir := s),
        Some "dir",
        "read all the .specs files in the results dir"
        ;
        "-lib",
        Arg.String (fun s ->
            Config.specs_library := filename_to_absolute s :: !Config.specs_library),
        Some "dir",
        "add dir to the list of directories to be searched for spec files"
        ;
        "-q",
        Arg.Set quiet,
        None,
        "quiet: do not print specs on standard output"
        ;
        "-save_results",
        Arg.String (fun s -> save_analysis_results := Some s),
        Some "file.iar",
        "save analysis results to Infer Analysis Results file file.iar"
        ;
        "-unit_test",
        Arg.Set unit_test,
        None,
        "print unit test code"
        ;
        "-xml",
        Arg.Set xml_specs,
        None,
        "export specs into XML files file1.xml ... filen.xml"
        ;
        "-test_filtering",
        Arg.Set test_filtering,
        None,
        "list all the files Infer can report on (should be call at the root of the procject, \
        where .inferconfig lives)."
        ;
        "-analyzer",
        Arg.String (fun s -> analyzer := Some (Utils.analyzer_of_string s)),
        Some "analyzer",
        "setup the analyzer for the path filtering"
        ;
        "-inferconfig_home",
        Arg.String (fun s -> Config.inferconfig_home := Some s),
        Some "dir",
        "Path to the .inferconfig file"
        ;
        "-with_infer_src_loc",
        Arg.Set reports_include_ml_loc,
        None,
        "include the location (in the Infer source code) from where reports are generated"
        ;
      ] in
    Arg.create_options_desc false "Options" desc in
  let reserved_arg =
    let desc =
      [
        "-latex",
        Arg.String (fun s -> latex := create_outfile s),
        Some "file.tex",
        "print latex report to file.tex"
        ;
        "-print_types",
        Arg.Set Config.print_types,
        None,
        "print types in symbolic heaps"
        ;
        "-precondition_stats",
        Arg.Set precondition_stats,
        None,
        "print stats about preconditions to standard output"
        ;
        "-report",
        Arg.String (fun s -> report := create_outfile s),
        Some "report_file",
        "create file report_file containing a report of the analysis results"
        ;
        "-source_file_copy",
        Arg.String (fun s -> source_file_copy := Some (DB.abs_source_file_from_path s)),
        Some "source_file",
        "print the path of the copy of source_file in the results directory"
        ;
        "-svg",
        Arg.Set svg,
        None,
        "generate .dot and .svg"
        ;
        "-whole_seconds",
        Arg.Set whole_seconds,
        None,
        "print whole seconds only"
        ;
      ] in
    Arg.create_options_desc false "Reserved Options" desc in
  base_arg @ reserved_arg

let usage =
  "Usage: InferPrint [options] name1.specs ... namen.specs\n\
   Read, convert, and print .specs files. \
   To process all the .specs in the current directory, pass . as only parameter \
   To process all the .specs in the results directory, use option -results_dir \
   Each spec is printed to standard output unless option -q is used."

let print_usage_exit err_s =
  L.err "Load Error: %s@.@." err_s;
  Arg.usage arg_desc usage;
  exit(1)

(** return the list of the .specs files in the results dir and libs, if they're defined *)
let load_specfiles () =
  let specs_files_in_dir dir =
    let is_specs_file fname =
      not (Sys.is_directory fname) && Filename.check_suffix fname ".specs" in
    let all_filenames = Array.to_list (Sys.readdir dir) in
    let all_filepaths = IList.map (fun fname -> Filename.concat dir fname) all_filenames in
    IList.filter is_specs_file all_filepaths in
  let specs_dirs =
    if !results_dir_cmdline then
      let result_specs_dir = DB.filename_to_string (DB.Results_dir.specs_dir ()) in
      result_specs_dir :: !Config.specs_library
    else
      !Config.specs_library in
  IList.flatten (IList.map specs_files_in_dir specs_dirs)

(** Create and initialize latex file *)
let begin_latex_file fmt =
  let author = "Infer " ^ Version.versionString in
  let title = "Report on Analysis Results" in
  let table_of_contents = true in
  Latex.pp_begin fmt (author, title, table_of_contents)

(** Write proc summary to latex file *)
let write_summary_latex fmt summary =
  let proc_name = Specs.get_proc_name summary in
  Latex.pp_section fmt ("Analysis of function "
                        ^ (Latex.convert_string (Procname.to_string proc_name)));
  F.fprintf fmt "@[<v>%a@]" (Specs.pp_summary (pe_latex Black) !whole_seconds) summary

let error_desc_to_csv_string error_desc =
  let pp fmt () = F.fprintf fmt "%a" Localise.pp_error_desc error_desc in
  Escape.escape_csv (pp_to_string pp ())

let error_advice_to_csv_string error_desc =
  let pp fmt () = F.fprintf fmt "%a" Localise.pp_error_advice error_desc in
  Escape.escape_csv (pp_to_string pp ())

let error_desc_to_plain_string error_desc =
  let pp fmt () = F.fprintf fmt "%a" Localise.pp_error_desc error_desc in
  pp_to_string pp ()

let error_desc_to_dotty_string error_desc =
  Localise.error_desc_get_dotty error_desc

let error_desc_to_xml_string error_desc =
  let pp fmt () = F.fprintf fmt "%a" Localise.pp_error_desc error_desc in
  Escape.escape_xml (pp_to_string pp ())

let error_desc_to_xml_tags error_desc =
  let tags = Localise.error_desc_get_tags error_desc in
  let subtree label contents =
    Io_infer.Xml.create_tree label [] [(Io_infer.Xml.String contents)] in
  IList.map (fun (tag, value) -> subtree tag (Escape.escape_xml value)) tags

let get_bug_hash
    (kind: string) (type_str: string) (procedure_id: string) (filename: string)
    (node_key: int) (error_desc: Localise.error_desc) =
  let qualifier_tag_call_procedure = Localise.error_desc_get_tag_call_procedure error_desc in
  let qualifier_tag_value = Localise.error_desc_get_tag_value error_desc in
  Hashtbl.hash
    (kind, type_str, procedure_id, filename, node_key,
     qualifier_tag_call_procedure, qualifier_tag_value)

let loc_trace_to_jsonbug_record trace_list ekind =
  match ekind with
  | Exceptions.Kinfo -> []
  | _ ->
      (* writes a trace as a record for atdgen conversion *)
      let node_tags_to_records tags_list =
        IList.map (fun tag -> { tag = fst tag; value = snd tag }) tags_list in
      let trace_item_to_record trace_item =
        { level = trace_item.Errlog.lt_level;
          filename = DB.source_file_to_string trace_item.Errlog.lt_loc.Location.file;
          line_number = trace_item.Errlog.lt_loc.Location.line;
          description = trace_item.Errlog.lt_description;
          node_tags = node_tags_to_records trace_item.Errlog.lt_node_tags;
        } in
      let record_list = IList.rev (IList.rev_map trace_item_to_record trace_list) in
      record_list

let error_desc_to_qualifier_tags_records error_desc =
  let tag_value_pairs = Localise.error_desc_to_tag_value_pairs error_desc in
  let tag_value_to_record (tag, value) =
    { tag = tag; value = value } in
  IList.map (fun tag_value -> tag_value_to_record tag_value) tag_value_pairs

type summary_val =
  { vname : string;
    vname_id : string;
    vspecs : int;
    vtime : string;
    vto : string;
    vsymop : int;
    verr : int;
    vfile : string;
    vflags : proc_flags;
    vline : int;
    vloc : int;
    vtop : string;
    vsignature : string;
    vweight : int;
    vproof_coverage : string;
    vrank : string;
    vin_calls : int;
    vout_calls : int;
    vproof_trace : string;
  }

(** compute values from summary data to export to csv and xml format *)
let summary_values top_proc_set summary =
  let stats = summary.Specs.stats in
  let attributes = summary.Specs.attributes in
  let err_log = attributes.ProcAttributes.err_log in
  let proc_name = Specs.get_proc_name summary in
  let is_top = Procname.Set.mem proc_name top_proc_set in
  let signature = Specs.get_signature summary in
  let nodes_nr = IList.length summary.Specs.nodes in
  let specs = Specs.get_specs_from_payload summary in
  let nr_nodes_visited, lines_visited =
    let visited = ref Specs.Visitedset.empty in
    let do_spec spec = visited := Specs.Visitedset.union spec.Specs.visited !visited in
    IList.iter do_spec specs;
    let visited_lines = ref IntSet.empty in
    Specs.Visitedset.iter (fun (_, ls) ->
        IList.iter (fun l -> visited_lines := IntSet.add l !visited_lines) ls)
      !visited;
    Specs.Visitedset.cardinal !visited, IntSet.elements !visited_lines in
  let proof_trace =
    let pp_line fmt l = F.fprintf fmt "%d" l in
    let pp fmt () = F.fprintf fmt "%a" (pp_seq pp_line) lines_visited in
    pp_to_string pp () in
  let node_coverage =
    if nodes_nr = 0 then 0.0
    else float_of_int nr_nodes_visited /. float_of_int nodes_nr in
  let logscale x =
    log10 (float_of_int (x + 1)) in
  let in_calls, out_calls =
    let calls = stats.Specs.stats_calls in
    calls.Cg.in_calls, calls.Cg.out_calls in
  let call_rank =
    let c1 = 1 and c2 = 1 in
    logscale (c1 * in_calls + c2 * out_calls) in

  let pp_failure failure =
    pp_to_string pp_failure_kind failure in


  { vname = Procname.to_string proc_name;
    vname_id = Procname.to_filename proc_name;
    vspecs = IList.length specs;
    vtime = Printf.sprintf "%.0f" stats.Specs.stats_time;
    vto = Option.map_default pp_failure "NONE" stats.Specs.stats_failure;
    vsymop = stats.Specs.symops;
    verr = Errlog.size
        (fun ekind in_footprint -> ekind = Exceptions.Kerror && in_footprint)
        err_log;
    vflags = attributes.ProcAttributes.proc_flags;
    vfile = DB.source_file_to_string attributes.ProcAttributes.loc.Location.file;
    vline = attributes.ProcAttributes.loc.Location.line;
    vloc = attributes.ProcAttributes.loc.Location.nLOC;
    vtop = if is_top then "Y" else "N";
    vsignature = signature;
    vweight = nodes_nr;
    vproof_coverage = Printf.sprintf "%2.2f" node_coverage;
    vrank = Printf.sprintf "%2.2f" call_rank;
    vin_calls = in_calls;
    vout_calls = out_calls;
    vproof_trace = proof_trace;
  }


module ProcsCsv = struct
  (** Print the header of the procedures csv file, with column names *)
  let pp_header fmt () =
    Format.fprintf fmt
      "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s@\n"
      Io_infer.Xml.tag_name
      Io_infer.Xml.tag_name_id
      Io_infer.Xml.tag_specs
      Io_infer.Xml.tag_time
      Io_infer.Xml.tag_to
      Io_infer.Xml.tag_symop
      Io_infer.Xml.tag_err
      Io_infer.Xml.tag_file
      Io_infer.Xml.tag_line
      Io_infer.Xml.tag_loc
      Io_infer.Xml.tag_top
      Io_infer.Xml.tag_signature
      Io_infer.Xml.tag_weight
      Io_infer.Xml.tag_proof_coverage
      Io_infer.Xml.tag_rank
      Io_infer.Xml.tag_in_calls
      Io_infer.Xml.tag_out_calls
      Io_infer.Xml.tag_proof_trace

  (** Write proc summary stats in csv format *)
  let pp_summary top_proc_set fmt summary =
    let pp x = F.fprintf fmt x in
    let sv = summary_values top_proc_set summary in
    pp "\"%s\"," (Escape.escape_csv sv.vname);
    pp "\"%s\"," (Escape.escape_csv sv.vname_id);
    pp "%d," sv.vspecs;
    pp "%s," sv.vtime;
    pp "%s," sv.vto;
    pp "%d," sv.vsymop;
    pp "%d," sv.verr;
    pp "%s," sv.vfile;
    pp "%d," sv.vline;
    pp "%d," sv.vloc;
    pp "%s," sv.vtop;
    pp "\"%s\"," (Escape.escape_csv sv.vsignature);
    pp "%d," sv.vweight;
    pp "%s," sv.vproof_coverage;
    pp "%s," sv.vrank;
    pp "%d," sv.vin_calls;
    pp "%d," sv.vout_calls;
    pp "%s@\n" sv.vproof_trace;
end

module ProcsXml = struct
  let xml_procs_id = ref 0

  (** print proc in xml *)
  let pp_proc top_proc_set fmt summary =
    let sv = summary_values top_proc_set summary in
    let subtree label contents =
      Io_infer.Xml.create_tree label [] [(Io_infer.Xml.String contents)] in
    let tree =
      incr xml_procs_id;
      let attributes = [("id", string_of_int !xml_procs_id) ] in
      let forest =
        [
          subtree Io_infer.Xml.tag_name (Escape.escape_xml sv.vname);
          subtree Io_infer.Xml.tag_name_id (Escape.escape_xml sv.vname_id);
          subtree Io_infer.Xml.tag_specs (string_of_int sv.vspecs);
          subtree Io_infer.Xml.tag_time sv.vtime;
          subtree Io_infer.Xml.tag_to sv.vto;
          subtree Io_infer.Xml.tag_symop (string_of_int sv.vsymop);
          subtree Io_infer.Xml.tag_err (string_of_int sv.verr);
          subtree Io_infer.Xml.tag_file sv.vfile;
          subtree Io_infer.Xml.tag_line (string_of_int sv.vline);
          subtree Io_infer.Xml.tag_loc (string_of_int sv.vloc);
          subtree Io_infer.Xml.tag_top sv.vtop;
          subtree Io_infer.Xml.tag_signature (Escape.escape_xml sv.vsignature);
          subtree Io_infer.Xml.tag_weight (string_of_int sv.vweight);
          subtree Io_infer.Xml.tag_proof_coverage sv.vproof_coverage;
          subtree Io_infer.Xml.tag_rank sv.vrank;
          subtree Io_infer.Xml.tag_in_calls (string_of_int sv.vin_calls);
          subtree Io_infer.Xml.tag_out_calls (string_of_int sv.vin_calls);
          subtree Io_infer.Xml.tag_proof_trace sv.vproof_trace;
          subtree Io_infer.Xml.tag_flags (string_of_int (Hashtbl.length sv.vflags));
        ] in
      Io_infer.Xml.create_tree "procedure" attributes forest in
    Io_infer.Xml.pp_inner_node fmt tree

  (** print the opening of the procedures xml file *)
  let pp_procs_open fmt () =
    Io_infer.Xml.pp_open fmt "procedures"

  (** print the closing of the procedures xml file *)
  let pp_procs_close fmt () =
    Io_infer.Xml.pp_close fmt "procedures"

end

module BugsCsv = struct
  let csv_bugs_id = ref 0

  let timestamp = Unix.time ()
  let pp_header fmt () =
    Format.fprintf fmt "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s@\n"
      Io_infer.Xml.tag_class
      Io_infer.Xml.tag_kind
      Io_infer.Xml.tag_type
      Io_infer.Xml.tag_qualifier
      Io_infer.Xml.tag_severity
      Io_infer.Xml.tag_line
      Io_infer.Xml.tag_procedure
      Io_infer.Xml.tag_procedure_id
      Io_infer.Xml.tag_file
      Io_infer.Xml.tag_trace
      Io_infer.Xml.tag_key
      Io_infer.Xml.tag_qualifier_tags
      Io_infer.Xml.tag_hash
      "bug_id"
      "always_report"
      "advice"

  (** Write bug report in csv format *)
  let pp_bugs error_filter fmt summary =
    let pp x = F.fprintf fmt x in
    let err_log = summary.Specs.attributes.ProcAttributes.err_log in
    let pp_row (_, node_key) loc _ ekind in_footprint error_name error_desc severity ltr _ eclass =
      if in_footprint && error_filter error_desc error_name then
        let err_desc_string = error_desc_to_csv_string error_desc in
        let err_advice_string = error_advice_to_csv_string error_desc in
        let qualifier_tag_xml =
          let xml_node =
            Io_infer.Xml.create_tree
              Io_infer.Xml.tag_qualifier_tags [] (error_desc_to_xml_tags error_desc) in
          let p fmt () = F.fprintf fmt "%a" (Io_infer.Xml.pp_document false) xml_node in
          let s = pp_to_string p () in
          Escape.escape_csv s in
        let kind = Exceptions.err_kind_string ekind in
        let type_str = Localise.to_string error_name in
        let procedure_id = Procname.to_filename (Specs.get_proc_name summary) in
        let filename =
          DB.source_file_to_string summary.Specs.attributes.ProcAttributes.loc.Location.file in
        let always_report =
          match Localise.error_desc_extract_tag_value error_desc "always_report" with
          | "" -> "false"
          | v -> v in

        let trace = string_of_json_trace { trace = loc_trace_to_jsonbug_record ltr ekind } in
        incr csv_bugs_id;
        pp "%s," (Exceptions.err_class_string eclass);
        pp "%s," kind;
        pp "%s," type_str;
        pp "\"%s\"," err_desc_string;
        pp "%s," severity;
        pp "%d," loc.Location.line;
        pp "\"%s\"," (Escape.escape_csv (Procname.to_string (Specs.get_proc_name summary)));
        pp "\"%s\"," (Escape.escape_csv procedure_id);
        pp "%s," filename;
        pp "\"%s\"," (Escape.escape_csv trace);
        pp "\"%d\"," node_key;
        pp "\"%s\"," qualifier_tag_xml;
        pp "\"%d\"," (get_bug_hash kind type_str procedure_id filename node_key error_desc);
        pp "\"%d\"," !csv_bugs_id; (** bug id *)
        pp "\"%s\"," always_report;
        pp "\"%s\"@\n" err_advice_string; in
    Errlog.iter pp_row err_log
end

module BugsJson = struct
  let is_first_item = ref true
  let pp_json_open fmt () = F.fprintf fmt "[@?"
  let pp_json_close fmt () = F.fprintf fmt "]\n@?"

  (** Write bug report in JSON format *)
  let pp_bugs error_filter fmt summary =
    let pp x = F.fprintf fmt x in
    let err_log = summary.Specs.attributes.ProcAttributes.err_log in
    let pp_row
        (_, node_key) loc ml_loc_opt ekind in_footprint error_name error_desc severity ltr _ eclass
      =
      if in_footprint && error_filter error_desc error_name then
        let kind = Exceptions.err_kind_string ekind in
        let bug_type = Localise.to_string error_name in
        let procedure_id = Procname.to_filename (Specs.get_proc_name summary) in
        let file =
          DB.source_file_to_string summary.Specs.attributes.ProcAttributes.loc.Location.file in
        let json_ml_loc = match ml_loc_opt with
          | Some (file, lnum, cnum, enum)  when !reports_include_ml_loc ->
              Some Jsonbug_j.{ file; lnum; cnum; enum; }
          | _ -> None in
        let bug = {
          bug_class = Exceptions.err_class_string eclass;
          kind = kind;
          bug_type = bug_type;
          qualifier = error_desc_to_plain_string error_desc;
          severity = severity;
          line = loc.Location.line;
          procedure = Procname.to_string (Specs.get_proc_name summary);
          procedure_id = procedure_id;
          file = file;
          bug_trace = loc_trace_to_jsonbug_record ltr ekind;
          key = node_key;
          qualifier_tags = error_desc_to_qualifier_tags_records error_desc;
          hash = get_bug_hash kind bug_type procedure_id file node_key error_desc;
          dotty = error_desc_to_dotty_string error_desc;
          infer_source_loc = json_ml_loc;
        } in
        if not !is_first_item then pp "," else is_first_item := false;
        pp "%s@?" (string_of_jsonbug bug) in
    Errlog.iter pp_row err_log
end

module BugsTxt = struct
  (** Write bug report in text format *)
  let pp_bugs error_filter fmt summary =
    let err_log = summary.Specs.attributes.ProcAttributes.err_log in
    let pp_row (node_id, node_key) loc _ ekind in_footprint error_name error_desc _ _ _ _ =
      if in_footprint && error_filter error_desc error_name then
        Exceptions.pp_err (node_id, node_key) loc ekind error_name error_desc None fmt () in
    Errlog.iter pp_row err_log
end

module BugsXml = struct
  let xml_bugs_id = ref 0
  let include_precondition_tree = false

  let loc_trace_to_xml linereader ltr =
    let subtree label contents =
      Io_infer.Xml.create_tree label [] [(Io_infer.Xml.String contents)] in
    let level_to_xml level = subtree Io_infer.Xml.tag_level (string_of_int level) in
    let line_to_xml line = subtree Io_infer.Xml.tag_line (string_of_int line) in
    let file_to_xml file = subtree Io_infer.Xml.tag_file file in
    let code_to_xml code = subtree Io_infer.Xml.tag_code code in
    let description_to_xml descr = subtree Io_infer.Xml.tag_description (Escape.escape_xml descr) in
    let node_tags_to_xml node_tags =
      let escaped_tags = IList.map (fun (tag, value) -> (tag, Escape.escape_xml value)) node_tags in
      Io_infer.Xml.create_tree Io_infer.Xml.tag_node escaped_tags [] in
    let num = ref 0 in
    let loc_to_xml lt =
      incr num;
      let loc = lt.Errlog.lt_loc in
      let code = match Printer.LineReader.from_loc linereader loc with
        | Some s -> Escape.escape_xml s
        | None -> "" in
      Io_infer.Xml.create_tree Io_infer.Xml.tag_loc [("num", string_of_int !num)]
        [(level_to_xml lt.Errlog.lt_level);
         (file_to_xml (DB.source_file_to_string loc.Location.file));
         (line_to_xml loc.Location.line);
         (code_to_xml code);
         (description_to_xml lt.Errlog.lt_description);
         (node_tags_to_xml lt.Errlog.lt_node_tags)] in
    IList.rev (IList.rev_map loc_to_xml ltr)

  (** print bugs from summary in xml *)
  let pp_bugs error_filter linereader fmt summary =
    let err_log = summary.Specs.attributes.ProcAttributes.err_log in
    let do_row
        (_, node_key) loc _ ekind in_footprint error_name error_desc severity ltr pre_opt eclass =
      if in_footprint && error_filter error_desc error_name then
        let err_desc_string = error_desc_to_xml_string error_desc in
        let precondition_tree () = match pre_opt with
          | None -> []
          | Some pre ->
              Dotty.reset_node_counter ();
              [Dotty.prop_to_xml pre Io_infer.Xml.tag_precondition 1] in
        let subtree label contents =
          Io_infer.Xml.create_tree label [] [(Io_infer.Xml.String contents)] in
        let kind = Exceptions.err_kind_string ekind in
        let type_str = Localise.to_string error_name in
        let tree =
          incr xml_bugs_id;
          let attributes = [("id", string_of_int !xml_bugs_id) ] in
          let error_class = Exceptions.err_class_string eclass in
          let error_line = string_of_int loc.Location.line in
          let proc_name = Specs.get_proc_name summary in
          let procedure_name = Procname.to_string proc_name in
          let procedure_id = Procname.to_filename proc_name in
          let filename =
            DB.source_file_to_string summary.Specs.attributes.ProcAttributes.loc.Location.file in
          let bug_hash = get_bug_hash kind type_str procedure_id filename node_key error_desc in
          let forest =
            [
              subtree Io_infer.Xml.tag_class error_class;
              subtree Io_infer.Xml.tag_kind kind;
              subtree Io_infer.Xml.tag_type type_str;
              subtree Io_infer.Xml.tag_qualifier err_desc_string;
              subtree Io_infer.Xml.tag_severity severity;
              subtree Io_infer.Xml.tag_line error_line;
              subtree Io_infer.Xml.tag_procedure (Escape.escape_xml procedure_name);
              subtree Io_infer.Xml.tag_procedure_id (Escape.escape_xml procedure_id);
              subtree Io_infer.Xml.tag_file filename;
              Io_infer.Xml.create_tree Io_infer.Xml.tag_trace [] (loc_trace_to_xml linereader ltr);
              subtree Io_infer.Xml.tag_key (string_of_int node_key);
              Io_infer.Xml.create_tree
                Io_infer.Xml.tag_qualifier_tags [] (error_desc_to_xml_tags error_desc);
              subtree Io_infer.Xml.tag_hash (string_of_int bug_hash)
            ]
            @
            (if include_precondition_tree then precondition_tree () else []) in
          Io_infer.Xml.create_tree "bug" attributes forest in
        Io_infer.Xml.pp_inner_node fmt tree in
    Errlog.iter do_row err_log

  (** print the opening of the bugs xml file *)
  let pp_bugs_open fmt () =
    Io_infer.Xml.pp_open fmt "bugs"

  (** print the closing of the bugs xml file *)
  let pp_bugs_close fmt () =
    Io_infer.Xml.pp_close fmt "bugs"
end

module CallsCsv = struct
  (** Print the header of the calls csv file, with column names *)
  let pp_header fmt () =
    Format.fprintf fmt "%s,%s,%s,%s,%s,%s,%s@\n"
      Io_infer.Xml.tag_caller
      Io_infer.Xml.tag_caller_id
      Io_infer.Xml.tag_callee
      Io_infer.Xml.tag_callee_id
      Io_infer.Xml.tag_file
      Io_infer.Xml.tag_line
      Io_infer.Xml.tag_call_trace

  (** Write proc summary stats in csv format *)
  let pp_calls fmt summary =
    let pp x = F.fprintf fmt x in
    let stats = summary.Specs.stats in
    let caller_name = Specs.get_proc_name summary in
    let do_call (callee_name, loc) trace =
      pp "\"%s\"," (Escape.escape_csv (Procname.to_string caller_name));
      pp "\"%s\"," (Escape.escape_csv (Procname.to_filename caller_name));
      pp "\"%s\"," (Escape.escape_csv (Procname.to_string callee_name));
      pp "\"%s\"," (Escape.escape_csv (Procname.to_filename callee_name));
      pp "%s," (DB.source_file_to_string summary.Specs.attributes.ProcAttributes.loc.Location.file);
      pp "%d," loc.Location.line;
      pp "%a@\n" Specs.CallStats.pp_trace trace in
    Specs.CallStats.iter do_call stats.Specs.call_stats
end

module UnitTest = struct
  (** Store the unit test functions generated, so that they can be called by main at the end *)
  let procs_done = ref []

  (** Print unit test for every spec in the summary *)
  let print_unit_test proc_name summary =
    let cnt = ref 0 in
    let fmt = F.std_formatter in
    let do_spec spec =
      incr cnt;
      let c_file = Filename.basename
          (DB.source_file_to_string summary.Specs.attributes.ProcAttributes.loc.Location.file) in
      let code =
        Autounit.genunit c_file proc_name !cnt (Specs.get_formals summary) spec in
      F.fprintf fmt "%a@." Autounit.pp_code code in
    let specs = Specs.get_specs_from_payload summary in
    IList.iter do_spec specs;
    procs_done := (proc_name, IList.length specs) :: !procs_done

  (** Print main function which calls all the unit test functions generated *)
  let print_unit_test_main () =
    let fmt = F.std_formatter in
    let code = Autounit.genmain !procs_done in
    F.fprintf fmt "%a@." Autounit.pp_code code
end

(** Module to compute the top procedures.
    A procedure is top if it has specs and any procedure calling it has no specs *)
module TopProcedures : sig
  type t
  val create : unit -> t
  val process_summary : t -> string * Specs.summary -> unit
  val top_set: t -> Procname.Set.t
end = struct
  type t =
    { mutable possible: Procname.Set.t;
      mutable impossible: Procname.Set.t }
  let create () =
    { possible = Procname.Set.empty;
      impossible = Procname.Set.empty }
  let mark_possible x pname =
    x.possible <- Procname.Set.add pname x.possible
  let mark_impossible x pname =
    x.impossible <- Procname.Set.add pname x.impossible
  let top_set x =
    Procname.Set.diff x.possible x.impossible
  let process_summary x (_, summary) =
    let proc_name = Specs.get_proc_name summary in
    let nspecs = IList.length (Specs.get_specs_from_payload summary) in
    if nspecs > 0 then
      begin
        mark_possible x proc_name;
        Procname.Map.iter (fun p _ -> mark_impossible x p) summary.Specs.dependency_map
      end
end

module Stats = struct
  type t = {
    files : (DB.source_file, unit) Hashtbl.t;
    mutable nchecked : int;
    mutable ndefective : int;
    mutable nerrors : int;
    mutable ninfos : int;
    mutable nLOC : int;
    mutable nprocs : int;
    mutable nspecs : int;
    mutable ntimeouts : int;
    mutable nverified : int;
    mutable nwarnings : int;
    mutable saved_errors : string list;
  }

  let create () = {
    files = Hashtbl.create 3;
    nchecked = 0;
    ndefective = 0;
    nerrors = 0;
    ninfos = 0;
    nLOC = 0;
    nprocs = 0;
    nspecs = 0;
    ntimeouts = 0;
    nverified = 0;
    nwarnings = 0;
    saved_errors = [];
  }

  let process_loc loc stats =
    try Hashtbl.find stats.files loc.Location.file
    with Not_found ->
      stats.nLOC <- stats.nLOC + loc.Location.nLOC;
      Hashtbl.add stats.files loc.Location.file ()

  let loc_trace_to_string_list linereader indent_num ltr =
    let res = ref [] in
    let indent_string n =
      let s = ref "" in
      for _ = 1 to n do s := "  " ^ !s done;
      !s in
    let num = ref 0 in
    let loc_to_string lt =
      incr num;
      let loc = lt.Errlog.lt_loc in
      let level = lt.Errlog.lt_level in
      let description = lt.Errlog.lt_description in
      let code = match Printer.LineReader.from_loc linereader loc with
        | Some s -> s
        | None -> "" in
      let line =
        let pp fmt () =
          if description <> ""
          then F.fprintf fmt "%s%4s  // %s@\n" (indent_string (level + indent_num)) " " description;
          F.fprintf fmt "%s%04d: %s" (indent_string (level + indent_num)) loc.Location.line code in
        pp_to_string pp () in
      res := line :: "" :: !res in
    IList.iter loc_to_string ltr;
    IList.rev !res

  let process_err_log error_filter linereader err_log stats =
    let found_errors = ref false in
    let process_row _ loc _ ekind in_footprint error_name error_desc _ ltr _ _ =
      let type_str = Localise.to_string error_name in
      if in_footprint && error_filter error_desc error_name
      then match ekind with
        | Exceptions.Kerror ->
            found_errors := true;
            stats.nerrors <- stats.nerrors + 1;
            let error_strs =
              let pp1 fmt () = F.fprintf fmt "%d: %s" stats.nerrors type_str in
              let pp2 fmt () = F.fprintf fmt "  %s:%d"
                  (DB.source_file_to_string loc.Location.file) loc.Location.line in
              let pp3 fmt () = F.fprintf fmt "  (%a)" Localise.pp_error_desc error_desc in
              [pp_to_string pp1 (); pp_to_string pp2 (); pp_to_string pp3 ()] in
            let trace = loc_trace_to_string_list linereader 1 ltr in
            stats.saved_errors <- IList.rev_append (error_strs @ trace @ [""]) stats.saved_errors
        | Exceptions.Kwarning -> stats.nwarnings <- stats.nwarnings + 1
        | Exceptions.Kinfo -> stats.ninfos <- stats.ninfos + 1 in
    Errlog.iter process_row err_log;
    !found_errors

  let process_summary error_filter summary linereader stats =
    let specs = Specs.get_specs_from_payload summary in
    let found_errors = process_err_log
        error_filter linereader summary.Specs.attributes.ProcAttributes.err_log stats in
    let is_defective = found_errors in
    let is_verified = specs <> [] && not is_defective in
    let is_checked = not (is_defective || is_verified) in
    let is_timeout = match Specs.(summary.stats.stats_failure) with
      | None | Some (FKcrash _) -> false
      | _ -> true in
    stats.nprocs <- stats.nprocs + 1;
    stats.nspecs <- stats.nspecs + (IList.length specs);
    if is_verified then stats.nverified <- stats.nverified + 1;
    if is_checked then stats.nchecked <- stats.nchecked + 1;
    if is_timeout then stats.ntimeouts <- stats.ntimeouts + 1;
    if is_defective then stats.ndefective <- stats.ndefective + 1;
    process_loc summary.Specs.attributes.ProcAttributes.loc stats

  let num_files stats =
    Hashtbl.length stats.files

  let pp fmt stats =
    F.fprintf fmt "Files: %d@\n" (num_files stats);
    F.fprintf fmt "LOC: %d@\n" stats.nLOC;
    F.fprintf fmt "Specs: %d@\n" stats.nspecs;
    F.fprintf fmt "Timeouts: %d@\n" stats.ntimeouts;
    F.fprintf fmt "Procedures: %d@\n" stats.nprocs;
    F.fprintf fmt "  Verified: %d@\n" stats.nverified;
    F.fprintf fmt "  Checked: %d@\n" stats.nchecked;
    F.fprintf fmt "  Defective: %d@\n" stats.ndefective;
    F.fprintf fmt "Errors: %d@\n" stats.nerrors;
    F.fprintf fmt "Warnings: %d@\n" stats.nwarnings;
    F.fprintf fmt "Infos: %d@\n" stats.ninfos;
    F.fprintf fmt "@\n -------------------@\n";
    F.fprintf fmt "@\nDetailed Errors@\n@\n";
    IList.iter (fun s -> F.fprintf fmt "%s@\n" s) (IList.rev stats.saved_errors);
end

module Report = struct
  let pp_header fmt () =
    F.fprintf fmt "Infer Analysis Results -- generated %a@\n@\n" pp_current_time ();
    F.fprintf fmt "Summary Report@\n@\n"

  let pp_stats fmt stats =
    Stats.pp fmt stats
end

(** Categorize the preconditions of specs and print stats *)
module PreconditionStats = struct
  let nr_nopres = ref 0
  let nr_empty = ref 0
  let nr_onlyallocation = ref 0
  let nr_dataconstraints = ref 0

  let do_summary proc_name summary =
    let specs = Specs.get_specs_from_payload summary in
    let preconditions = IList.map (fun spec -> Specs.Jprop.to_prop spec.Specs.pre) specs in
    match Prop.CategorizePreconditions.categorize preconditions with
    | Prop.CategorizePreconditions.Empty ->
        incr nr_empty;
        L.out "Procedure: %a footprint:Empty@." Procname.pp proc_name
    | Prop.CategorizePreconditions.OnlyAllocation ->
        incr nr_onlyallocation;
        L.out "Procedure: %a footprint:OnlyAllocation@." Procname.pp proc_name
    | Prop.CategorizePreconditions.NoPres ->
        incr nr_nopres;
        L.out "Procedure: %a footprint:NoPres@." Procname.pp proc_name
    | Prop.CategorizePreconditions.DataConstraints ->
        incr nr_dataconstraints;
        L.out "Procedure: %a footprint:DataConstraints@." Procname.pp proc_name

  let pp_stats () =
    L.out "@.Precondition stats@.";
    L.out "Procedures with no preconditions: %d@." !nr_nopres;
    L.out "Procedures with empty precondition: %d@." !nr_empty;
    L.out "Procedures with only allocation conditions: %d@." !nr_onlyallocation;
    L.out "Procedures with data constraints: %d@." !nr_dataconstraints
end

(** Process a summary *)
let process_summary filters linereader stats (top_proc_set: Procname.Set.t) (fname, summary) =
  let proc_name = Specs.get_proc_name summary in
  let base = DB.chop_extension (DB.filename_from_string fname) in
  let pp_simple_saved = !Config.pp_simple in
  Config.pp_simple := true;
  if !quiet then ()
  else L.out "Procedure: %a@\n%a@."
      Procname.pp proc_name (Specs.pp_summary pe_text !whole_seconds) summary;
  let error_filter error_desc error_name =
    let always_report () =
      Localise.error_desc_extract_tag_value error_desc "always_report" = "true" in
    (filters.Inferconfig.path_filter summary.Specs.attributes.ProcAttributes.loc.Location.file
     || always_report ()) &&
    filters.Inferconfig.error_filter error_name && filters.Inferconfig.proc_filter proc_name in
  do_outf procs_csv (fun outf -> ProcsCsv.pp_summary top_proc_set outf.fmt summary);
  do_outf calls_csv (fun outf -> CallsCsv.pp_calls outf.fmt summary);
  do_outf procs_xml (fun outf -> ProcsXml.pp_proc top_proc_set outf.fmt summary);
  do_outf bugs_csv (fun outf -> BugsCsv.pp_bugs error_filter outf.fmt summary);
  do_outf bugs_json (fun outf -> BugsJson.pp_bugs error_filter outf.fmt summary);
  do_outf bugs_txt (fun outf -> BugsTxt.pp_bugs error_filter outf.fmt summary);
  do_outf bugs_xml (fun outf -> BugsXml.pp_bugs error_filter linereader outf.fmt summary);
  do_outf report (fun _ -> Stats.process_summary error_filter summary linereader stats);
  if !precondition_stats then PreconditionStats.do_summary proc_name summary;
  if !unit_test then UnitTest.print_unit_test proc_name summary;
  Config.pp_simple := pp_simple_saved;
  do_outf latex (fun outf -> write_summary_latex outf.fmt summary);
  if !svg then begin
    let specs = Specs.get_specs_from_payload summary in
    let dot_file = DB.filename_add_suffix base ".dot" in
    let svg_file = DB.filename_add_suffix base ".svg" in
    if not (DB.file_exists dot_file)
    || DB.file_modified_time (DB.filename_from_string fname) > DB.file_modified_time dot_file
    then
      Dotty.pp_speclist_dotty_file base specs;
    if not (DB.file_exists svg_file)
    || DB.file_modified_time dot_file > DB.file_modified_time svg_file
    then
      ignore (Sys.command ("dot -Tsvg \"" ^
                           (DB.filename_to_string dot_file) ^
                           "\" >\"" ^
                           (DB.filename_to_string svg_file) ^"\""))
  end;
  if !xml_specs then begin
    let xml_file = DB.filename_add_suffix base ".xml" in
    let specs = Specs.get_specs_from_payload summary in
    if not (DB.file_exists xml_file)
    || DB.file_modified_time (DB.filename_from_string fname) > DB.file_modified_time xml_file
    then
      begin
        let xml_out = ref (create_outfile (DB.filename_to_string xml_file)) in
        do_outf xml_out (fun outf ->
            Dotty.print_specs_xml
              (Specs.get_signature summary)
              specs summary.Specs.attributes.ProcAttributes.loc outf.fmt;
            close_outf outf)
      end
  end
(* ignore (Sys.command ("open " ^ base ^ ".svg")) *)

module AnalysisResults = struct
  type t = (string * Specs.summary) list

  let spec_files_from_cmdline =
    (* parse command-line arguments, and find spec files specified there *)
    let args = ref [] in
    let f arg =
      if not (Filename.check_suffix arg ".specs") && arg <> "."
      then print_usage_exit "arguments must be .specs files"
      else args := arg::!args in
    Arg.parse "INFERPRINT_ARGS" arg_desc f usage;
    if !test_filtering then
      begin
        Inferconfig.test ();
        exit(0)
      end;
    IList.append (if !args = ["."] then begin
        let arr = Sys.readdir "." in
        let all_files = Array.to_list arr in
        IList.filter (fun fname -> (Filename.check_suffix fname ".specs")) all_files
      end
       else !args) (load_specfiles ())

  (** apply [f] to [arg] with the gc compaction disabled during the execution *)
  let apply_without_gc f arg =
    let stat = Gc.get () in
    let space_oh = stat.Gc.space_overhead in
    Gc.set { stat with Gc.space_overhead = 10000 };
    let res = f arg in
    Gc.set { stat with Gc.space_overhead = space_oh };
    res

  (** Load .specs files in memory and return list of summaries *)
  let load_summaries_in_memory () : t =
    let summaries = ref [] in
    let load_file fname =
      match Specs.load_summary (DB.filename_from_string fname) with
      | None ->
          L.err "Error: cannot open file %s@." fname;
          exit 0
      | Some summary ->
          summaries := (fname, summary) :: !summaries in
    apply_without_gc (IList.iter load_file) spec_files_from_cmdline;
    let summ_cmp (_, summ1) (_, summ2) =
      let n =
        DB.source_file_compare
          summ1.Specs.attributes.ProcAttributes.loc.Location.file
          summ2.Specs.attributes.ProcAttributes.loc.Location.file in
      if n <> 0 then n
      else int_compare
          summ1.Specs.attributes.ProcAttributes.loc.Location.line
          summ2.Specs.attributes.ProcAttributes.loc.Location.line in
    IList.sort summ_cmp !summaries

  (** Create an iterator which loads spec files one at a time *)
  let iterator_of_spec_files () =
    let sorted_spec_files = IList.sort string_compare spec_files_from_cmdline in
    let do_spec f fname =
      match Specs.load_summary (DB.filename_from_string fname) with
      | None ->
          L.err "Error: cannot open file %s@." fname;
          exit 0
      | Some summary ->
          f (fname, summary) in
    let iterate f =
      IList.iter (do_spec f) sorted_spec_files in
    iterate

  (** Serializer for analysis results *)
  let analysis_results_serializer : t Serialization.serializer =
    Serialization.create_serializer Serialization.analysis_results_key

  (** Load analysis_results from a file *)
  let load_analysis_results_from_file (filename : DB.filename) : t option =
    Serialization.from_file analysis_results_serializer filename

  (** Save analysis_results into a file *)
  let store_analysis_results_to_file (filename : DB.filename) (analysis_results: t) =
    Serialization.to_file analysis_results_serializer filename analysis_results

  (** Return an iterator over all the summaries.
      If options - load_results or - save_results are used,
      all the summaries are loaded in memory *)
  let get_summary_iterator () =
    let iterator_of_summary_list r =
      fun f -> IList.iter f r in
    match !load_analysis_results with
    | None ->
        begin
          match !save_analysis_results with
          | None ->
              iterator_of_spec_files ()
          | Some s ->
              let r = load_summaries_in_memory () in
              store_analysis_results_to_file (DB.filename_from_string s) r;
              iterator_of_summary_list r
        end
    | Some fname ->
        begin
          match load_analysis_results_from_file (DB.filename_from_string fname) with
          | Some r ->
              iterator_of_summary_list r
          | None ->
              L.err "Error: cannot open analysis results file %s@." fname;
              exit 0
        end
end

(* warning: computing top procedures iterates over summaries twice *)
let compute_top_procedures = ref false

let () =
  Config.developer_mode := true;
  Config.print_using_diff := true;
  handle_source_file_copy_option ();
  let iterate_summaries = AnalysisResults.get_summary_iterator () in
  let filters =
    match !analyzer with
    | None -> Inferconfig.do_not_filter
    | Some analyzer -> Inferconfig.create_filters analyzer in

  let pdflatex fname = ignore (Sys.command ("pdflatex " ^ fname)) in
  do_outf latex (fun outf -> begin_latex_file outf.fmt);
  do_outf procs_csv (fun outf -> ProcsCsv.pp_header outf.fmt ());
  do_outf procs_xml (fun outf -> ProcsXml.pp_procs_open outf.fmt ());
  do_outf calls_csv (fun outf -> CallsCsv.pp_header outf.fmt ());
  do_outf bugs_csv (fun outf -> BugsCsv.pp_header outf.fmt ());
  do_outf bugs_json (fun outf -> BugsJson.pp_json_open outf.fmt ());
  do_outf bugs_xml (fun outf -> BugsXml.pp_bugs_open outf.fmt ());
  do_outf report (fun outf -> Report.pp_header outf.fmt ());
  let top_proc = TopProcedures.create () in
  if !compute_top_procedures && (!procs_csv != None || !procs_xml != None)
  then iterate_summaries (TopProcedures.process_summary top_proc);
  let top_proc_set = TopProcedures.top_set top_proc in
  let linereader = Printer.LineReader.create () in
  let stats = Stats.create () in
  iterate_summaries (process_summary filters linereader stats top_proc_set);
  if !unit_test then UnitTest.print_unit_test_main ();
  do_outf procs_csv close_outf;
  do_outf procs_xml (fun outf -> ProcsXml.pp_procs_close outf.fmt (); close_outf outf);
  do_outf bugs_csv close_outf;
  do_outf bugs_json (fun outf -> BugsJson.pp_json_close outf.fmt (); close_outf outf);
  do_outf bugs_json close_outf;
  do_outf calls_csv close_outf;
  do_outf bugs_txt close_outf;
  do_outf bugs_xml (fun outf -> BugsXml.pp_bugs_close outf.fmt (); close_outf outf);
  do_outf latex (fun outf ->
      Latex.pp_end outf.fmt ();
      close_outf outf;
      pdflatex outf.fname;
      let pdf_name = (Filename.chop_extension outf.fname) ^ ".pdf" in
      ignore (Sys.command ("open " ^ pdf_name)));
  do_outf report (fun outf -> F.fprintf outf.fmt "%a@?" Report.pp_stats stats; close_outf outf);
  if !precondition_stats then PreconditionStats.pp_stats ()
