/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

let module Hashtbl = Caml.Hashtbl;

let module L = Logging;

let module F = Format;

let print_usage_exit err_s => {
  L.stderr "Load Error: %s@.@." err_s;
  Config.print_usage_exit ()
};


/** return the list of the .specs files in the results dir and libs, if they're defined */
let load_specfiles () => {
  let specs_files_in_dir dir => {
    let is_specs_file fname =>
      Sys.is_directory fname != `Yes && Filename.check_suffix fname Config.specs_files_suffix;
    let all_filenames =
      try (Array.to_list (Sys.readdir dir)) {
      | Sys_error _ => []
      };
    let all_filepaths = IList.map (fun fname => Filename.concat dir fname) all_filenames;
    IList.filter is_specs_file all_filepaths
  };
  let specs_dirs = {
    let result_specs_dir = DB.filename_to_string DB.Results_dir.specs_dir;
    [result_specs_dir, ...Config.specs_library]
  };
  IList.flatten (IList.map specs_files_in_dir specs_dirs)
};


/** Create and initialize latex file */
let begin_latex_file fmt => {
  let author = "Infer " ^ Version.versionString;
  let title = "Report on Analysis Results";
  let table_of_contents = true;
  Latex.pp_begin fmt (author, title, table_of_contents)
};

let error_desc_to_csv_string error_desc => {
  let pp fmt => F.fprintf fmt "%a" Localise.pp_error_desc error_desc;
  Escape.escape_csv (F.asprintf "%t" pp)
};

let error_advice_to_csv_string error_desc => {
  let pp fmt => F.fprintf fmt "%a" Localise.pp_error_advice error_desc;
  Escape.escape_csv (F.asprintf "%t" pp)
};

let error_desc_to_plain_string error_desc => {
  let pp fmt => F.fprintf fmt "%a" Localise.pp_error_desc error_desc;
  F.asprintf "%t" pp
};

let error_desc_to_dotty_string error_desc => Localise.error_desc_get_dotty error_desc;

let error_desc_to_xml_string error_desc => {
  let pp fmt => F.fprintf fmt "%a" Localise.pp_error_desc error_desc;
  Escape.escape_xml (F.asprintf "%t" pp)
};

let error_desc_to_xml_tags error_desc => {
  let tags = Localise.error_desc_get_tags error_desc;
  let subtree label contents => Io_infer.Xml.create_tree label [] [Io_infer.Xml.String contents];
  IList.map (fun (tag, value) => subtree tag (Escape.escape_xml value)) tags
};

let get_bug_hash
    (kind: string)
    (type_str: string)
    (procedure_id: string)
    (filename: string)
    (node_key: int)
    (error_desc: Localise.error_desc) => {
  let qualifier_tag_call_procedure = Localise.error_desc_get_tag_call_procedure error_desc;
  let qualifier_tag_value = Localise.error_desc_get_tag_value error_desc;
  Hashtbl.hash (
    kind,
    type_str,
    procedure_id,
    filename,
    node_key,
    qualifier_tag_call_procedure,
    qualifier_tag_value
  )
};

let loc_trace_to_jsonbug_record trace_list ekind =>
  switch ekind {
  | Exceptions.Kinfo => []
  | _ =>
    /* writes a trace as a record for atdgen conversion */
    let node_tags_to_records tags_list =>
      IList.map (fun tag => {Jsonbug_j.tag: fst tag, value: snd tag}) tags_list;
    let trace_item_to_record trace_item => {
      Jsonbug_j.level: trace_item.Errlog.lt_level,
      filename: SourceFile.to_string trace_item.Errlog.lt_loc.Location.file,
      line_number: trace_item.Errlog.lt_loc.Location.line,
      description: trace_item.Errlog.lt_description,
      node_tags: node_tags_to_records trace_item.Errlog.lt_node_tags
    };
    let record_list = IList.rev (IList.rev_map trace_item_to_record trace_list);
    record_list
  };

let error_desc_to_qualifier_tags_records error_desc => {
  let tag_value_pairs = Localise.error_desc_to_tag_value_pairs error_desc;
  let tag_value_to_record (tag, value) => {Jsonbug_j.tag: tag, value};
  IList.map (fun tag_value => tag_value_to_record tag_value) tag_value_pairs
};

type summary_val = {
  vname: string,
  vname_id: string,
  vspecs: int,
  vtime: string,
  vto: string,
  vsymop: int,
  verr: int,
  vfile: string,
  vflags: ProcAttributes.proc_flags,
  vline: int,
  vtop: string,
  vsignature: string,
  vweight: int,
  vproof_coverage: string,
  vrank: string,
  vin_calls: int,
  vout_calls: int,
  vproof_trace: string
};


/** compute values from summary data to export to csv and xml format */
let summary_values top_proc_set summary => {
  let stats = summary.Specs.stats;
  let attributes = summary.Specs.attributes;
  let err_log = attributes.ProcAttributes.err_log;
  let proc_name = Specs.get_proc_name summary;
  let is_top = Procname.Set.mem proc_name top_proc_set;
  let signature = Specs.get_signature summary;
  let nodes_nr = IList.length summary.Specs.nodes;
  let specs = Specs.get_specs_from_payload summary;
  let (nr_nodes_visited, lines_visited) = {
    let visited = ref Specs.Visitedset.empty;
    let do_spec spec => visited := Specs.Visitedset.union spec.Specs.visited !visited;
    IList.iter do_spec specs;
    let visited_lines = ref Int.Set.empty;
    Specs.Visitedset.iter
      (fun (_, ls) => IList.iter (fun l => visited_lines := Int.Set.add !visited_lines l) ls)
      !visited;
    (Specs.Visitedset.cardinal !visited, Int.Set.elements !visited_lines)
  };
  let proof_trace = {
    let pp_line fmt l => F.fprintf fmt "%d" l;
    let pp fmt => F.fprintf fmt "%a" (Pp.seq pp_line) lines_visited;
    F.asprintf "%t" pp
  };
  let node_coverage =
    if (nodes_nr == 0) {
      0.0
    } else {
      float_of_int nr_nodes_visited /. float_of_int nodes_nr
    };
  let logscale x => log10 (float_of_int (x + 1));
  let (in_calls, out_calls) = {
    let calls = stats.Specs.stats_calls;
    (calls.Cg.in_calls, calls.Cg.out_calls)
  };
  let call_rank = {
    let c1 = 1
    and c2 = 1;
    logscale (c1 * in_calls + c2 * out_calls)
  };
  let pp_failure failure => F.asprintf "%a" SymOp.pp_failure_kind failure;
  {
    vname: Procname.to_string proc_name,
    vname_id: Procname.to_filename proc_name,
    vspecs: IList.length specs,
    vtime: Printf.sprintf "%.0f" stats.Specs.stats_time,
    vto: Option.value_map f::pp_failure default::"NONE" stats.Specs.stats_failure,
    vsymop: stats.Specs.symops,
    verr:
      Errlog.size (fun ekind in_footprint => ekind == Exceptions.Kerror && in_footprint) err_log,
    vflags: attributes.ProcAttributes.proc_flags,
    vfile: SourceFile.to_string attributes.ProcAttributes.loc.Location.file,
    vline: attributes.ProcAttributes.loc.Location.line,
    vtop: if is_top {"Y"} else {"N"},
    vsignature: signature,
    vweight: nodes_nr,
    vproof_coverage: Printf.sprintf "%2.2f" node_coverage,
    vrank: Printf.sprintf "%2.2f" call_rank,
    vin_calls: in_calls,
    vout_calls: out_calls,
    vproof_trace: proof_trace
  }
};

let module ProcsCsv = {

  /** Print the header of the procedures csv file, with column names */
  let pp_header fmt () =>
    Format.fprintf
      fmt
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
      Io_infer.Xml.tag_proof_trace;

  /** Write proc summary stats in csv format */
  let pp_summary fmt top_proc_set summary => {
    let pp x => F.fprintf fmt x;
    let sv = summary_values top_proc_set summary;
    pp "\"%s\"," (Escape.escape_csv sv.vname);
    pp "\"%s\"," (Escape.escape_csv sv.vname_id);
    pp "%d," sv.vspecs;
    pp "%s," sv.vtime;
    pp "%s," sv.vto;
    pp "%d," sv.vsymop;
    pp "%d," sv.verr;
    pp "%s," sv.vfile;
    pp "%d," sv.vline;
    pp "%s," sv.vtop;
    pp "\"%s\"," (Escape.escape_csv sv.vsignature);
    pp "%d," sv.vweight;
    pp "%s," sv.vproof_coverage;
    pp "%s," sv.vrank;
    pp "%d," sv.vin_calls;
    pp "%d," sv.vout_calls;
    pp "%s@\n" sv.vproof_trace
  };
};

let module ProcsXml = {
  let xml_procs_id = ref 0;

  /** print proc in xml */
  let pp_proc fmt top_proc_set summary => {
    let sv = summary_values top_proc_set summary;
    let subtree label contents => Io_infer.Xml.create_tree label [] [Io_infer.Xml.String contents];
    let tree = {
      incr xml_procs_id;
      let attributes = [("id", string_of_int !xml_procs_id)];
      let forest = [
        subtree Io_infer.Xml.tag_name (Escape.escape_xml sv.vname),
        subtree Io_infer.Xml.tag_name_id (Escape.escape_xml sv.vname_id),
        subtree Io_infer.Xml.tag_specs (string_of_int sv.vspecs),
        subtree Io_infer.Xml.tag_time sv.vtime,
        subtree Io_infer.Xml.tag_to sv.vto,
        subtree Io_infer.Xml.tag_symop (string_of_int sv.vsymop),
        subtree Io_infer.Xml.tag_err (string_of_int sv.verr),
        subtree Io_infer.Xml.tag_file sv.vfile,
        subtree Io_infer.Xml.tag_line (string_of_int sv.vline),
        subtree Io_infer.Xml.tag_top sv.vtop,
        subtree Io_infer.Xml.tag_signature (Escape.escape_xml sv.vsignature),
        subtree Io_infer.Xml.tag_weight (string_of_int sv.vweight),
        subtree Io_infer.Xml.tag_proof_coverage sv.vproof_coverage,
        subtree Io_infer.Xml.tag_rank sv.vrank,
        subtree Io_infer.Xml.tag_in_calls (string_of_int sv.vin_calls),
        subtree Io_infer.Xml.tag_out_calls (string_of_int sv.vin_calls),
        subtree Io_infer.Xml.tag_proof_trace sv.vproof_trace,
        subtree Io_infer.Xml.tag_flags (string_of_int (Hashtbl.length sv.vflags))
      ];
      Io_infer.Xml.create_tree "procedure" attributes forest
    };
    Io_infer.Xml.pp_inner_node fmt tree
  };

  /** print the opening of the procedures xml file */
  let pp_procs_open fmt () => Io_infer.Xml.pp_open fmt "procedures";

  /** print the closing of the procedures xml file */
  let pp_procs_close fmt () => Io_infer.Xml.pp_close fmt "procedures";
};

let should_report (issue_kind: Exceptions.err_kind) issue_type error_desc =>
  if (not Config.filtering) {
    true
  } else {
    let analyzer_is_whitelisted =
      switch Config.analyzer {
      | Checkers
      | Eradicate
      | Tracing => true
      | Capture
      | Compile
      | Crashcontext
      | Infer
      | Linters
      | Quandary
      | Threadsafety => false
      };
    if analyzer_is_whitelisted {
      true
    } else {
      let issue_kind_is_blacklisted =
        switch issue_kind {
        | Kinfo => true
        | Kerror
        | Kwarning
        | Kadvice => false
        };
      if issue_kind_is_blacklisted {
        false
      } else {
        let issue_type_is_null_deref = {
          let null_deref_issue_types =
            Localise.[
              field_not_null_checked,
              null_dereference,
              parameter_not_null_checked,
              premature_nil_termination
            ];
          IList.mem Localise.equal issue_type null_deref_issue_types
        };
        if issue_type_is_null_deref {
          let issue_bucket_is_high = {
            let issue_bucket = Localise.error_desc_get_bucket error_desc;
            let high_buckets = Localise.BucketLevel.[b1, b2];
            let eq o y =>
              switch (o, y) {
              | (None, _) => false
              | (Some x, y) => String.equal x y
              };
            IList.mem eq issue_bucket high_buckets
          };
          issue_bucket_is_high
        } else {
          let issue_type_is_reportable = {
            let reportable_issue_types =
              Localise.[
                Localise.from_string Config.default_failure_name,
                assign_pointer_warning,
                bad_pointer_comparison,
                component_factory_function,
                component_initializer_with_side_effects,
                component_with_multiple_factory_methods,
                component_with_unconventional_superclass,
                context_leak,
                cxx_reference_captured_in_objc_block,
                direct_atomic_property_access,
                empty_vector_access,
                global_variable_initialized_with_function_or_method_call,
                memory_leak,
                mutable_local_variable_in_component_file,
                quandary_taint_error,
                registered_observer_being_deallocated,
                resource_leak,
                retain_cycle,
                static_initialization_order_fiasco,
                strong_delegate_warning,
                tainted_value_reaching_sensitive_function,
                thread_safety_error,
                unsafe_guarded_by_access
              ];
            IList.mem Localise.equal issue_type reportable_issue_types
          };
          issue_type_is_reportable
        }
      }
    }
  };

let module IssuesCsv = {
  let csv_issues_id = ref 0;
  let pp_header fmt () =>
    Format.fprintf
      fmt
      "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s@\n"
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
      "advice";

  /** Write bug report in csv format */
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt procname err_log => {
    let pp x => F.fprintf fmt x;
    let pp_row (_, node_key) loc _ ekind in_footprint error_name error_desc severity ltr eclass _ => {
      let source_file =
        switch proc_loc_opt {
        | Some proc_loc => proc_loc.Location.file
        | None => loc.Location.file
        };
      if (
        in_footprint &&
        error_filter source_file error_desc error_name && should_report ekind error_name error_desc
      ) {
        let err_desc_string = error_desc_to_csv_string error_desc;
        let err_advice_string = error_advice_to_csv_string error_desc;
        let qualifier_tag_xml = {
          let xml_node =
            Io_infer.Xml.create_tree
              Io_infer.Xml.tag_qualifier_tags [] (error_desc_to_xml_tags error_desc);
          let p fmt => F.fprintf fmt "%a" (Io_infer.Xml.pp_document false) xml_node;
          let s = F.asprintf "%t" p;
          Escape.escape_csv s
        };
        let kind = Exceptions.err_kind_string ekind;
        let type_str = Localise.to_string error_name;
        let procedure_id = Procname.to_filename procname;
        let filename = SourceFile.to_string source_file;
        let always_report =
          switch (Localise.error_desc_extract_tag_value error_desc "always_report") {
          | "" => "false"
          | v => v
          };
        let trace = Jsonbug_j.string_of_json_trace {trace: loc_trace_to_jsonbug_record ltr ekind};
        incr csv_issues_id;
        pp "%s," (Exceptions.err_class_string eclass);
        pp "%s," kind;
        pp "%s," type_str;
        pp "\"%s\"," err_desc_string;
        pp "%s," severity;
        pp "%d," loc.Location.line;
        pp "\"%s\"," (Escape.escape_csv (Procname.to_string procname));
        pp "\"%s\"," (Escape.escape_csv procedure_id);
        pp "%s," filename;
        pp "\"%s\"," (Escape.escape_csv trace);
        pp "\"%d\"," node_key;
        pp "\"%s\"," qualifier_tag_xml;
        pp "\"%d\"," (get_bug_hash kind type_str procedure_id filename node_key error_desc);
        pp "\"%d\"," !csv_issues_id; /* bug id */
        pp "\"%s\"," always_report;
        pp "\"%s\"@\n" err_advice_string
      }
    };
    Errlog.iter pp_row err_log
  };
};

let module IssuesJson = {
  let is_first_item = ref true;
  let pp_json_open fmt () => F.fprintf fmt "[@?";
  let pp_json_close fmt () => F.fprintf fmt "]\n@?";

  /** Write bug report in JSON format */
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt procname err_log => {
    let pp x => F.fprintf fmt x;
    let pp_row
        (_, node_key)
        loc
        ml_loc_opt
        ekind
        in_footprint
        error_name
        error_desc
        severity
        ltr
        eclass
        visibility => {
      let (source_file, procedure_start_line) =
        switch proc_loc_opt {
        | Some proc_loc => (proc_loc.Location.file, proc_loc.Location.line)
        | None => (loc.Location.file, 0)
        };
      let should_report_source_file =
        not (SourceFile.is_infer_model source_file) ||
        Config.debug_mode || Config.debug_exceptions;
      if (
        in_footprint &&
        error_filter source_file error_desc error_name &&
        should_report_source_file && should_report ekind error_name error_desc
      ) {
        let kind = Exceptions.err_kind_string ekind;
        let bug_type = Localise.to_string error_name;
        let procedure_id = Procname.to_filename procname;
        let file = SourceFile.to_string source_file;
        let json_ml_loc =
          switch ml_loc_opt {
          | Some (file, lnum, cnum, enum) when Config.reports_include_ml_loc =>
            Some Jsonbug_j.{file, lnum, cnum, enum}
          | _ => None
          };
        let visibility = Exceptions.string_of_exception_visibility visibility;
        let bug = {
          Jsonbug_j.bug_class: Exceptions.err_class_string eclass,
          kind,
          bug_type,
          qualifier: error_desc_to_plain_string error_desc,
          severity,
          visibility,
          line: loc.Location.line,
          column: loc.Location.col,
          procedure: Procname.to_string procname,
          procedure_id,
          procedure_start_line,
          file,
          bug_trace: loc_trace_to_jsonbug_record ltr ekind,
          key: node_key,
          qualifier_tags: error_desc_to_qualifier_tags_records error_desc,
          hash: get_bug_hash kind bug_type procedure_id file node_key error_desc,
          dotty: error_desc_to_dotty_string error_desc,
          infer_source_loc: json_ml_loc
        };
        if (not !is_first_item) {
          pp ","
        } else {
          is_first_item := false
        };
        pp "%s@?" (Jsonbug_j.string_of_jsonbug bug)
      }
    };
    Errlog.iter pp_row err_log
  };
};

let pp_tests_of_report fmt report => {
  open Jsonbug_t;
  let pp_trace_elem fmt {description} => F.fprintf fmt "%s" description;
  let pp_trace fmt trace =>
    if Config.print_traces_in_tests {
      let trace_without_empty_descs = IList.filter (fun {description} => description != "") trace;
      F.fprintf fmt ", [%a]" (Pp.comma_seq pp_trace_elem) trace_without_empty_descs
    };
  let pp_row jsonbug =>
    F.fprintf
      fmt
      "%s, %s, %d, %s%a@."
      jsonbug.file
      jsonbug.procedure
      (jsonbug.line - jsonbug.procedure_start_line)
      jsonbug.bug_type
      pp_trace
      jsonbug.bug_trace;
  IList.iter pp_row report
};

let tests_jsonbug_compare bug1 bug2 =>
  Jsonbug_t.(
    [%compare : (string, string, int, string, int)]
      (bug1.file, bug1.procedure, bug1.line - bug1.procedure_start_line, bug1.bug_type, bug1.hash)
      (bug2.file, bug2.procedure, bug2.line - bug2.procedure_start_line, bug2.bug_type, bug2.hash)
  );

let module IssuesTxt = {

  /** Write bug report in text format */
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt _ err_log => {
    let pp_row (node_id, node_key) loc _ ekind in_footprint error_name error_desc _ _ _ _ => {
      let source_file =
        switch proc_loc_opt {
        | Some proc_loc => proc_loc.Location.file
        | None => loc.Location.file
        };
      if (in_footprint && error_filter source_file error_desc error_name) {
        Exceptions.pp_err (node_id, node_key) loc ekind error_name error_desc None fmt ()
      }
    };
    Errlog.iter pp_row err_log
  };
};

let pp_text_of_report fmt report => {
  let pp_row jsonbug =>
    Jsonbug_t.(
      F.fprintf
        fmt
        "%s:%d: %s: %s %s@\n"
        jsonbug.file
        jsonbug.line
        jsonbug.kind
        jsonbug.bug_type
        jsonbug.qualifier
    );
  IList.iter pp_row report
};

let module IssuesXml = {
  let xml_issues_id = ref 0;
  let loc_trace_to_xml linereader ltr => {
    let subtree label contents => Io_infer.Xml.create_tree label [] [Io_infer.Xml.String contents];
    let level_to_xml level => subtree Io_infer.Xml.tag_level (string_of_int level);
    let line_to_xml line => subtree Io_infer.Xml.tag_line (string_of_int line);
    let file_to_xml file => subtree Io_infer.Xml.tag_file file;
    let code_to_xml code => subtree Io_infer.Xml.tag_code code;
    let description_to_xml descr => subtree Io_infer.Xml.tag_description (Escape.escape_xml descr);
    let node_tags_to_xml node_tags => {
      let escaped_tags = IList.map (fun (tag, value) => (tag, Escape.escape_xml value)) node_tags;
      Io_infer.Xml.create_tree Io_infer.Xml.tag_node escaped_tags []
    };
    let num = ref 0;
    let loc_to_xml lt => {
      incr num;
      let loc = lt.Errlog.lt_loc;
      let code =
        switch (Printer.LineReader.from_loc linereader loc) {
        | Some s => Escape.escape_xml s
        | None => ""
        };
      Io_infer.Xml.create_tree
        Io_infer.Xml.tag_loc
        [("num", string_of_int !num)]
        [
          level_to_xml lt.Errlog.lt_level,
          file_to_xml (SourceFile.to_string loc.Location.file),
          line_to_xml loc.Location.line,
          code_to_xml code,
          description_to_xml lt.Errlog.lt_description,
          node_tags_to_xml lt.Errlog.lt_node_tags
        ]
    };
    IList.rev (IList.rev_map loc_to_xml ltr)
  };

  /** print issues from summary in xml */
  let pp_issues_of_error_log fmt error_filter linereader proc_loc_opt proc_name err_log => {
    let do_row (_, node_key) loc _ ekind in_footprint error_name error_desc severity ltr eclass _ => {
      let source_file =
        switch proc_loc_opt {
        | Some proc_loc => proc_loc.Location.file
        | None => loc.Location.file
        };
      if (in_footprint && error_filter source_file error_desc error_name) {
        let err_desc_string = error_desc_to_xml_string error_desc;
        let subtree label contents =>
          Io_infer.Xml.create_tree label [] [Io_infer.Xml.String contents];
        let kind = Exceptions.err_kind_string ekind;
        let type_str = Localise.to_string error_name;
        let tree = {
          incr xml_issues_id;
          let attributes = [("id", string_of_int !xml_issues_id)];
          let error_class = Exceptions.err_class_string eclass;
          let error_line = string_of_int loc.Location.line;
          let procedure_name = Procname.to_string proc_name;
          let procedure_id = Procname.to_filename proc_name;
          let filename = SourceFile.to_string source_file;
          let bug_hash = get_bug_hash kind type_str procedure_id filename node_key error_desc;
          let forest = [
            subtree Io_infer.Xml.tag_class error_class,
            subtree Io_infer.Xml.tag_kind kind,
            subtree Io_infer.Xml.tag_type type_str,
            subtree Io_infer.Xml.tag_qualifier err_desc_string,
            subtree Io_infer.Xml.tag_severity severity,
            subtree Io_infer.Xml.tag_line error_line,
            subtree Io_infer.Xml.tag_procedure (Escape.escape_xml procedure_name),
            subtree Io_infer.Xml.tag_procedure_id (Escape.escape_xml procedure_id),
            subtree Io_infer.Xml.tag_file filename,
            Io_infer.Xml.create_tree Io_infer.Xml.tag_trace [] (loc_trace_to_xml linereader ltr),
            subtree Io_infer.Xml.tag_key (string_of_int node_key),
            Io_infer.Xml.create_tree
              Io_infer.Xml.tag_qualifier_tags [] (error_desc_to_xml_tags error_desc),
            subtree Io_infer.Xml.tag_hash (string_of_int bug_hash)
          ];
          Io_infer.Xml.create_tree "bug" attributes forest
        };
        Io_infer.Xml.pp_inner_node fmt tree
      }
    };
    Errlog.iter do_row err_log
  };

  /** print the opening of the issues xml file */
  let pp_issues_open fmt () => Io_infer.Xml.pp_open fmt "bugs";

  /** print the closing of the issues xml file */
  let pp_issues_close fmt () => Io_infer.Xml.pp_close fmt "bugs";
};

let module CallsCsv = {

  /** Write proc summary stats in csv format */
  let pp_calls fmt summary => {
    let pp x => F.fprintf fmt x;
    let stats = summary.Specs.stats;
    let caller_name = Specs.get_proc_name summary;
    let do_call (callee_name, loc) trace => {
      pp "\"%s\"," (Escape.escape_csv (Procname.to_string caller_name));
      pp "\"%s\"," (Escape.escape_csv (Procname.to_filename caller_name));
      pp "\"%s\"," (Escape.escape_csv (Procname.to_string callee_name));
      pp "\"%s\"," (Escape.escape_csv (Procname.to_filename callee_name));
      pp "%s," (SourceFile.to_string summary.Specs.attributes.ProcAttributes.loc.Location.file);
      pp "%d," loc.Location.line;
      pp "%a@\n" Specs.CallStats.pp_trace trace
    };
    Specs.CallStats.iter do_call stats.Specs.call_stats
  };
};


/** Module to compute the top procedures.
    A procedure is top if it has specs and any procedure calling it has no specs */
let module TopProcedures: {
  type t;
  let create: unit => t;
  let process_summary: t => (string, Specs.summary) => unit;
  let top_set: t => Procname.Set.t;
} = {
  type t = {mutable possible: Procname.Set.t, mutable impossible: Procname.Set.t};
  let create () => {possible: Procname.Set.empty, impossible: Procname.Set.empty};
  let mark_possible x pname => x.possible = Procname.Set.add pname x.possible;
  let mark_impossible x pname => x.impossible = Procname.Set.add pname x.impossible;
  let top_set x => Procname.Set.diff x.possible x.impossible;
  let process_summary x (_, summary) => {
    let proc_name = Specs.get_proc_name summary;
    let nspecs = IList.length (Specs.get_specs_from_payload summary);
    if (nspecs > 0) {
      mark_possible x proc_name;
      Procname.Map.iter (fun p _ => mark_impossible x p) summary.Specs.dependency_map
    }
  };
};

let module Stats = {
  type t = {
    files: Hashtbl.t SourceFile.t unit,
    mutable nchecked: int,
    mutable ndefective: int,
    mutable nerrors: int,
    mutable ninfos: int,
    mutable nadvice: int,
    mutable nprocs: int,
    mutable nspecs: int,
    mutable ntimeouts: int,
    mutable nverified: int,
    mutable nwarnings: int,
    mutable saved_errors: list string
  };
  let create () => {
    files: Hashtbl.create 3,
    nchecked: 0,
    ndefective: 0,
    nerrors: 0,
    ninfos: 0,
    nadvice: 0,
    nprocs: 0,
    nspecs: 0,
    ntimeouts: 0,
    nverified: 0,
    nwarnings: 0,
    saved_errors: []
  };
  let process_loc loc stats =>
    try (Hashtbl.find stats.files loc.Location.file) {
    | Not_found => Hashtbl.add stats.files loc.Location.file ()
    };
  let loc_trace_to_string_list linereader indent_num ltr => {
    let res = ref [];
    let indent_string n => {
      let s = ref "";
      for _ in 1 to n {
        s := "  " ^ !s
      };
      !s
    };
    let num = ref 0;
    let loc_to_string lt => {
      incr num;
      let loc = lt.Errlog.lt_loc;
      let level = lt.Errlog.lt_level;
      let description = lt.Errlog.lt_description;
      let code =
        switch (Printer.LineReader.from_loc linereader loc) {
        | Some s => s
        | None => ""
        };
      let line = {
        let pp fmt => {
          if (description != "") {
            F.fprintf fmt "%s%4s  // %s@\n" (indent_string (level + indent_num)) " " description
          };
          F.fprintf fmt "%s%04d: %s" (indent_string (level + indent_num)) loc.Location.line code
        };
        F.asprintf "%t" pp
      };
      res := [line, "", ...!res]
    };
    IList.iter loc_to_string ltr;
    IList.rev !res
  };
  let process_err_log error_filter linereader err_log stats => {
    let found_errors = ref false;
    let process_row _ loc _ ekind in_footprint error_name error_desc _ ltr _ _ => {
      let type_str = Localise.to_string error_name;
      if (in_footprint && error_filter error_desc error_name) {
        switch ekind {
        | Exceptions.Kerror =>
          found_errors := true;
          stats.nerrors = stats.nerrors + 1;
          let error_strs = {
            let pp1 fmt => F.fprintf fmt "%d: %s" stats.nerrors type_str;
            let pp2 fmt =>
              F.fprintf fmt "  %a:%d" SourceFile.pp loc.Location.file loc.Location.line;
            let pp3 fmt => F.fprintf fmt "  (%a)" Localise.pp_error_desc error_desc;
            [F.asprintf "%t" pp1, F.asprintf "%t" pp2, F.asprintf "%t" pp3]
          };
          let trace = loc_trace_to_string_list linereader 1 ltr;
          stats.saved_errors = IList.rev_append (error_strs @ trace @ [""]) stats.saved_errors
        | Exceptions.Kwarning => stats.nwarnings = stats.nwarnings + 1
        | Exceptions.Kinfo => stats.ninfos = stats.ninfos + 1
        | Exceptions.Kadvice => stats.nadvice = stats.nadvice + 1
        }
      }
    };
    Errlog.iter process_row err_log;
    !found_errors
  };
  let process_summary error_filter summary linereader stats => {
    let specs = Specs.get_specs_from_payload summary;
    let found_errors =
      process_err_log
        error_filter linereader summary.Specs.attributes.ProcAttributes.err_log stats;
    let is_defective = found_errors;
    let is_verified = specs != [] && not is_defective;
    let is_checked = not (is_defective || is_verified);
    let is_timeout =
      switch Specs.(summary.stats.stats_failure) {
      | None
      | Some (FKcrash _) => false
      | _ => true
      };
    stats.nprocs = stats.nprocs + 1;
    stats.nspecs = stats.nspecs + IList.length specs;
    if is_verified {
      stats.nverified = stats.nverified + 1
    };
    if is_checked {
      stats.nchecked = stats.nchecked + 1
    };
    if is_timeout {
      stats.ntimeouts = stats.ntimeouts + 1
    };
    if is_defective {
      stats.ndefective = stats.ndefective + 1
    };
    process_loc summary.Specs.attributes.ProcAttributes.loc stats
  };
  let num_files stats => Hashtbl.length stats.files;
  let pp fmt stats => {
    F.fprintf fmt "Files: %d@\n" (num_files stats);
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
    IList.iter (fun s => F.fprintf fmt "%s@\n" s) (IList.rev stats.saved_errors)
  };
};

let module Report = {
  let pp_header fmt () => {
    F.fprintf fmt "Infer Analysis Results -- generated %a@\n@\n" Pp.current_time ();
    F.fprintf fmt "Summary Report@\n@\n"
  };
  let pp_stats fmt stats => Stats.pp fmt stats;
};

let module Summary = {
  let pp_summary_out summary => {
    let proc_name = Specs.get_proc_name summary;
    if Config.quiet {
      ()
    } else {
      L.stdout
        "Procedure: %a@\n%a@."
        Procname.pp
        proc_name
        (Specs.pp_summary_text whole_seconds::Config.whole_seconds)
        summary
    }
  };

  /** Write proc summary to latex file */
  let write_summary_latex fmt summary => {
    let proc_name = Specs.get_proc_name summary;
    Latex.pp_section
      fmt ("Analysis of function " ^ Latex.convert_string (Procname.to_string proc_name));
    F.fprintf
      fmt "@[<v>%a@]" (Specs.pp_summary_latex Black whole_seconds::Config.whole_seconds) summary
  };
  let pp_summary_xml summary fname =>
    if Config.xml_specs {
      let base = DB.chop_extension (DB.filename_from_string fname);
      let xml_file = DB.filename_add_suffix base ".xml";
      let specs = Specs.get_specs_from_payload summary;
      if (
        not (DB.file_exists xml_file) ||
        DB.file_modified_time (DB.filename_from_string fname) > DB.file_modified_time xml_file
      ) {
        let xml_out = Utils.create_outfile (DB.filename_to_string xml_file);
        Utils.do_outf
          xml_out
          (
            fun outf => {
              Dotty.print_specs_xml
                (Specs.get_signature summary)
                specs
                summary.Specs.attributes.ProcAttributes.loc
                outf.fmt;
              Utils.close_outf outf
            }
          )
      }
    };
  let print_summary_dot_svg summary fname =>
    if Config.svg {
      let base = DB.chop_extension (DB.filename_from_string fname);
      let specs = Specs.get_specs_from_payload summary;
      let dot_file = DB.filename_add_suffix base ".dot";
      let svg_file = DB.filename_add_suffix base ".svg";
      if (
        not (DB.file_exists dot_file) ||
        DB.file_modified_time (DB.filename_from_string fname) > DB.file_modified_time dot_file
      ) {
        Dotty.pp_speclist_dotty_file base specs
      };
      if (
        not (DB.file_exists svg_file) ||
        DB.file_modified_time dot_file > DB.file_modified_time svg_file
      ) {
        ignore (
          Sys.command (
            "dot -Tsvg \"" ^
            DB.filename_to_string dot_file ^ "\" >\"" ^ DB.filename_to_string svg_file ^ "\""
          )
        )
      }
    };
};


/** Categorize the preconditions of specs and print stats */
let module PreconditionStats = {
  let nr_nopres = ref 0;
  let nr_empty = ref 0;
  let nr_onlyallocation = ref 0;
  let nr_dataconstraints = ref 0;
  let do_summary proc_name summary => {
    let specs = Specs.get_specs_from_payload summary;
    let preconditions = IList.map (fun spec => Specs.Jprop.to_prop spec.Specs.pre) specs;
    switch (Prop.CategorizePreconditions.categorize preconditions) {
    | Prop.CategorizePreconditions.Empty =>
      incr nr_empty;
      L.stdout "Procedure: %a footprint:Empty@." Procname.pp proc_name
    | Prop.CategorizePreconditions.OnlyAllocation =>
      incr nr_onlyallocation;
      L.stdout "Procedure: %a footprint:OnlyAllocation@." Procname.pp proc_name
    | Prop.CategorizePreconditions.NoPres =>
      incr nr_nopres;
      L.stdout "Procedure: %a footprint:NoPres@." Procname.pp proc_name
    | Prop.CategorizePreconditions.DataConstraints =>
      incr nr_dataconstraints;
      L.stdout "Procedure: %a footprint:DataConstraints@." Procname.pp proc_name
    }
  };
  let pp_stats () => {
    L.stdout "@.Precondition stats@.";
    L.stdout "Procedures with no preconditions: %d@." !nr_nopres;
    L.stdout "Procedures with empty precondition: %d@." !nr_empty;
    L.stdout "Procedures with only allocation conditions: %d@." !nr_onlyallocation;
    L.stdout "Procedures with data constraints: %d@." !nr_dataconstraints
  };
};

let error_filter filters proc_name file error_desc error_name => {
  let always_report () =>
    Localise.error_desc_extract_tag_value error_desc "always_report" == "true";
  (Config.write_html || not (Localise.equal error_name Localise.skip_function)) &&
  (filters.Inferconfig.path_filter file || always_report ()) &&
  filters.Inferconfig.error_filter error_name && filters.Inferconfig.proc_filter proc_name
};

type report_kind =
  | Issues
  | Procs
  | Stats
  | Calls
  | Summary;

type bug_format_kind =
  | Json
  | Csv
  | Tests
  | Text
  | Xml
  | Latex;

let pp_issues_in_format (format_kind, outf: Utils.outfile) =>
  switch format_kind {
  | Json => IssuesJson.pp_issues_of_error_log outf.fmt
  | Csv => IssuesCsv.pp_issues_of_error_log outf.fmt
  | Tests => failwith "Print issues as tests is not implemented"
  | Text => IssuesTxt.pp_issues_of_error_log outf.fmt
  | Xml => IssuesXml.pp_issues_of_error_log outf.fmt
  | Latex => failwith "Printing issues in latex is not implemented"
  };

let pp_procs_in_format (format_kind, outf: Utils.outfile) =>
  switch format_kind {
  | Csv => ProcsCsv.pp_summary outf.fmt
  | Xml => ProcsXml.pp_proc outf.fmt
  | Json
  | Latex
  | Tests
  | Text => failwith "Printing procs in json/latex/tests/text is not implemented"
  };

let pp_calls_in_format (format_kind, outf: Utils.outfile) =>
  switch format_kind {
  | Csv => CallsCsv.pp_calls outf.fmt
  | Json
  | Tests
  | Text
  | Xml
  | Latex => failwith "Printing calls in json/tests/text/xml/latex is not implemented"
  };

let pp_stats_in_format (format_kind, _) =>
  switch format_kind {
  | Csv => Stats.process_summary
  | Json
  | Tests
  | Text
  | Xml
  | Latex => failwith "Printing stats in json/tests/text/xml/latex is not implemented"
  };

let pp_summary_in_format (format_kind, outf: Utils.outfile) =>
  switch format_kind {
  | Latex => Summary.write_summary_latex outf.fmt
  | Json
  | Csv
  | Tests
  | Text
  | Xml => failwith "Printing summary in json/csv/tests/text/xml is not implemented"
  };

let pp_issues_of_error_log error_filter linereader proc_loc_opt procname err_log bug_format_list => {
  let pp_issues_in_format format =>
    pp_issues_in_format format error_filter linereader proc_loc_opt procname err_log;
  IList.iter pp_issues_in_format bug_format_list
};

let pp_issues error_filter linereader summary bug_format_list => {
  let err_log = summary.Specs.attributes.ProcAttributes.err_log;
  let procname = Specs.get_proc_name summary;
  let loc = summary.Specs.attributes.ProcAttributes.loc;
  pp_issues_of_error_log error_filter linereader (Some loc) procname err_log bug_format_list
};

let pp_procs top_proc_set summary procs_format_list => {
  let pp_procs_in_format format => {
    let pp_procs = pp_procs_in_format format;
    pp_procs top_proc_set summary
  };
  IList.iter pp_procs_in_format procs_format_list
};

let pp_calls summary calls_format_list => {
  let pp_calls_in_format format => {
    let pp_calls = pp_calls_in_format format;
    pp_calls summary
  };
  IList.iter pp_calls_in_format calls_format_list
};

let pp_stats error_filter linereader summary stats stats_format_list => {
  let pp_stats_in_format format => {
    let pp_stats = pp_stats_in_format format;
    pp_stats error_filter summary linereader stats
  };
  IList.iter pp_stats_in_format stats_format_list
};

let pp_summary summary fname summary_format_list => {
  let pp_summary_in_format format => {
    let pp_summary = pp_summary_in_format format;
    pp_summary summary
  };
  IList.iter pp_summary_in_format summary_format_list;
  Summary.pp_summary_out summary;
  Summary.pp_summary_xml summary fname;
  Summary.print_summary_dot_svg summary fname
};

let pp_summary_by_report_kind
    formats_by_report_kind
    summary
    top_proc_set
    fname
    error_filter
    linereader
    stats
    file => {
  let pp_summary_by_report_kind (report_kind, format_list) =>
    switch (report_kind, format_list) {
    | (Issues, [_, ..._]) => pp_issues error_filter linereader summary format_list
    | (Procs, [_, ..._]) => pp_procs top_proc_set summary format_list
    | (Stats, [_, ..._]) => pp_stats (error_filter file) linereader summary stats format_list
    | (Calls, [_, ..._]) => pp_calls summary format_list
    | (Summary, _) => pp_summary summary fname format_list
    | _ => ()
    };
  IList.iter pp_summary_by_report_kind formats_by_report_kind
};

let pp_json_report_by_report_kind formats_by_report_kind fname =>
  switch (Utils.read_file fname) {
  | Some report_lines =>
    let pp_json_issues format_list report => {
      let pp_json_issue (format_kind, outf: Utils.outfile) =>
        switch format_kind {
        | Tests => pp_tests_of_report outf.fmt report
        | Text => pp_text_of_report outf.fmt report
        | Json => failwith "Printing issues from json does not support json output"
        | Csv => failwith "Printing issues from json does not support csv output"
        | Xml => failwith "Printing issues from json does not support xml output"
        | Latex => failwith "Printing issues from json does not support latex output"
        };
      IList.iter pp_json_issue format_list
    };
    let sorted_report = {
      let report = Jsonbug_j.report_of_string (String.concat sep::"\n" report_lines);
      IList.sort tests_jsonbug_compare report
    };
    let pp_report_by_report_kind (report_kind, format_list) =>
      switch (report_kind, format_list) {
      | (Issues, [_, ..._]) => pp_json_issues format_list sorted_report
      | _ => ()
      };
    IList.iter pp_report_by_report_kind formats_by_report_kind
  | None => failwithf "Error reading %s. Does the file exist?" fname
  };

let pp_lint_issues_by_report_kind formats_by_report_kind error_filter linereader procname error_log => {
  let pp_summary_by_report_kind (report_kind, format_list) =>
    switch (report_kind, format_list) {
    | (Issues, [_, ..._]) =>
      pp_issues_of_error_log error_filter linereader None procname error_log format_list
    | _ => ()
    };
  IList.iter pp_summary_by_report_kind formats_by_report_kind
};


/** Process lint issues of a procedure */
let pp_lint_issues filters formats_by_report_kind linereader procname error_log => {
  let error_filter = error_filter filters procname;
  pp_lint_issues_by_report_kind formats_by_report_kind error_filter linereader procname error_log
};


/** Process a summary */
let process_summary filters formats_by_report_kind linereader stats top_proc_set (fname, summary) => {
  let file = summary.Specs.attributes.ProcAttributes.loc.Location.file;
  let proc_name = Specs.get_proc_name summary;
  let error_filter = error_filter filters proc_name;
  let pp_simple_saved = !Config.pp_simple;
  Config.pp_simple := true;
  pp_summary_by_report_kind
    formats_by_report_kind summary top_proc_set fname error_filter linereader stats file;
  if Config.precondition_stats {
    PreconditionStats.do_summary proc_name summary
  };
  Config.pp_simple := pp_simple_saved
};

let module AnalysisResults = {
  type t = list (string, Specs.summary);
  let spec_files_from_cmdline () => {
    /* Find spec files specified by command-line arguments.  Not run at init time since the specs
       files may be generated between init and report time. */
    IList.iter
      (
        fun arg =>
          if (not (Filename.check_suffix arg Config.specs_files_suffix) && arg != ".") {
            print_usage_exit ("file " ^ arg ^ ": arguments must be .specs files")
          }
      )
      Config.anon_args;
    if Config.test_filtering {
      Inferconfig.test ();
      exit 0
    };
    if (Config.anon_args == []) {
      load_specfiles ()
    } else {
      List.rev Config.anon_args
    }
  };

  /** apply [f] to [arg] with the gc compaction disabled during the execution */
  let apply_without_gc f arg => {
    let stat = Gc.get ();
    let space_oh = stat.space_overhead;
    Gc.set {...stat, space_overhead: 10000};
    let res = f arg;
    Gc.set {...stat, space_overhead: space_oh};
    res
  };

  /** Load .specs files in memory and return list of summaries */
  let load_summaries_in_memory () :t => {
    let summaries = ref [];
    let load_file fname =>
      switch (Specs.load_summary (DB.filename_from_string fname)) {
      | None =>
        L.stderr "Error: cannot open file %s@." fname;
        exit 0
      | Some summary => summaries := [(fname, summary), ...!summaries]
      };
    apply_without_gc (IList.iter load_file) (spec_files_from_cmdline ());
    let summ_cmp (_, summ1) (_, summ2) => {
      let n =
        SourceFile.compare
          summ1.Specs.attributes.ProcAttributes.loc.Location.file
          summ2.Specs.attributes.ProcAttributes.loc.Location.file;
      if (n != 0) {
        n
      } else {
        Int.compare
          summ1.Specs.attributes.ProcAttributes.loc.Location.line
          summ2.Specs.attributes.ProcAttributes.loc.Location.line
      }
    };
    IList.sort summ_cmp !summaries
  };

  /** Create an iterator which loads spec files one at a time */
  let iterator_of_spec_files () => {
    let sorted_spec_files = IList.sort String.compare (spec_files_from_cmdline ());
    let do_spec f fname =>
      switch (Specs.load_summary (DB.filename_from_string fname)) {
      | None =>
        L.stderr "Error: cannot open file %s@." fname;
        exit 0
      | Some summary => f (fname, summary)
      };
    let iterate f => IList.iter (do_spec f) sorted_spec_files;
    iterate
  };

  /** Serializer for analysis results */
  let analysis_results_serializer: Serialization.serializer t = Serialization.create_serializer Serialization.analysis_results_key;

  /** Load analysis_results from a file */
  let load_analysis_results_from_file (filename: DB.filename) :option t =>
    Serialization.from_file analysis_results_serializer filename;

  /** Save analysis_results into a file */
  let store_analysis_results_to_file (filename: DB.filename) (analysis_results: t) =>
    Serialization.to_file analysis_results_serializer filename analysis_results;

  /** Return an iterator over all the summaries.
      If options - load_results or - save_results are used,
      all the summaries are loaded in memory */
  let get_summary_iterator () => {
    let iterator_of_summary_list r f => IList.iter f r;
    switch Config.load_analysis_results {
    | None =>
      switch Config.save_analysis_results {
      | None => iterator_of_spec_files ()
      | Some s =>
        let r = load_summaries_in_memory ();
        store_analysis_results_to_file (DB.filename_from_string s) r;
        iterator_of_summary_list r
      }
    | Some fname =>
      switch (load_analysis_results_from_file (DB.filename_from_string fname)) {
      | Some r => iterator_of_summary_list r
      | None =>
        L.stderr "Error: cannot open analysis results file %s@." fname;
        exit 0
      }
    }
  };
};

/* warning: computing top procedures iterates over summaries twice */
let compute_top_procedures = ref false;

let register_perf_stats_report () => {
  let stats_dir = Filename.concat Config.results_dir Config.reporting_stats_dir_name;
  let stats_file = Filename.concat stats_dir (Config.perf_stats_prefix ^ ".json");
  PerfStats.register_report_at_exit stats_file
};

let mk_format format_kind fname =>
  Option.value_map
    f::(fun out_file => [(format_kind, out_file)]) default::[] (Utils.create_outfile fname);

let init_issues_format_list report_csv report_json => {
  let csv_format = Option.value_map f::(mk_format Csv) default::[] report_csv;
  let json_format = Option.value_map f::(mk_format Json) default::[] report_json;
  let tests_format = Option.value_map f::(mk_format Tests) default::[] Config.bugs_tests;
  let txt_format = Option.value_map f::(mk_format Text) default::[] Config.bugs_txt;
  let xml_format = Option.value_map f::(mk_format Xml) default::[] Config.bugs_xml;
  csv_format @ json_format @ tests_format @ txt_format @ xml_format
};

let init_procs_format_list () => {
  let csv_format = Option.value_map f::(mk_format Csv) default::[] Config.procs_csv;
  let xml_format = Option.value_map f::(mk_format Xml) default::[] Config.procs_xml;
  csv_format @ xml_format
};

let init_calls_format_list () => {
  let csv_format = Option.value_map f::(mk_format Csv) default::[] Config.calls_csv;
  csv_format
};

let init_stats_format_list () => {
  let csv_format = Option.value_map f::(mk_format Csv) default::[] Config.report;
  csv_format
};

let init_summary_format_list () => {
  let latex_format = Option.value_map f::(mk_format Latex) default::[] Config.latex;
  latex_format
};

let init_files format_list_by_kind => {
  let init_files_of_report_kind (report_kind, format_list) => {
    let init_files_of_format (format_kind, outfile: Utils.outfile) =>
      switch (format_kind, report_kind) {
      | (Csv, Issues) => IssuesCsv.pp_header outfile.fmt ()
      | (Csv, Procs) => ProcsCsv.pp_header outfile.fmt ()
      | (Csv, Stats) => Report.pp_header outfile.fmt ()
      | (Json, Issues) => IssuesJson.pp_json_open outfile.fmt ()
      | (Xml, Issues) => IssuesXml.pp_issues_open outfile.fmt ()
      | (Xml, Procs) => ProcsXml.pp_procs_open outfile.fmt ()
      | (Latex, Summary) => begin_latex_file outfile.fmt
      | (Csv | Json | Latex | Tests | Text | Xml, _) => ()
      };
    IList.iter init_files_of_format format_list
  };
  IList.iter init_files_of_report_kind format_list_by_kind
};

let finalize_and_close_files format_list_by_kind stats pdflatex => {
  let close_files_of_report_kind (report_kind, format_list) => {
    let close_files_of_format (format_kind, outfile: Utils.outfile) => {
      switch (format_kind, report_kind) {
      | (Csv, Stats) => F.fprintf outfile.fmt "%a@?" Report.pp_stats stats
      | (Json, Issues) => IssuesJson.pp_json_close outfile.fmt ()
      | (Xml, Issues) => IssuesXml.pp_issues_close outfile.fmt ()
      | (Xml, Procs) => ProcsXml.pp_procs_close outfile.fmt ()
      | (Latex, Summary) => Latex.pp_end outfile.fmt ()
      | (Csv | Latex | Tests | Text | Xml | Json, _) => ()
      };
      Utils.close_outf outfile;
      if ((format_kind, report_kind) == (Latex, Summary)) {
        pdflatex outfile.fname;
        let pdf_name = Filename.chop_extension outfile.fname ^ ".pdf";
        ignore (Sys.command ("open " ^ pdf_name))
      }
    };
    IList.iter close_files_of_format format_list;
    ()
  };
  IList.iter close_files_of_report_kind format_list_by_kind
};

let pp_summary_and_issues formats_by_report_kind => {
  let pdflatex fname => ignore (Sys.command ("pdflatex " ^ fname));
  let stats = Stats.create ();
  let linereader = Printer.LineReader.create ();
  let filters = Inferconfig.create_filters Config.analyzer;
  let iterate_summaries = AnalysisResults.get_summary_iterator ();
  let top_proc = TopProcedures.create ();
  let top_proc_set = TopProcedures.top_set top_proc;
  if (!compute_top_procedures && (Config.procs_csv != None || Config.procs_xml != None)) {
    iterate_summaries (TopProcedures.process_summary top_proc)
  };
  iterate_summaries (process_summary filters formats_by_report_kind linereader stats top_proc_set);
  if Config.precondition_stats {
    PreconditionStats.pp_stats ()
  };
  {
    LintIssues.load_issues_to_errlog_map Config.lint_issues_dir_name;
    Procname.Map.iter
      (pp_lint_issues filters formats_by_report_kind linereader) !LintIssues.errLogMap
  };
  finalize_and_close_files formats_by_report_kind stats pdflatex
};

let print_issues formats_by_report_kind => {
  init_files formats_by_report_kind;
  switch Config.from_json_report {
  | Some fname => pp_json_report_by_report_kind formats_by_report_kind fname
  | None => pp_summary_and_issues formats_by_report_kind
  }
};

let main report_csv::report_csv report_json::report_json => {
  let formats_by_report_kind = [
    (Issues, init_issues_format_list report_csv report_json),
    (Procs, init_procs_format_list ()),
    (Calls, init_calls_format_list ()),
    (Stats, init_stats_format_list ()),
    (Summary, init_summary_format_list ())
  ];
  register_perf_stats_report ();
  print_issues formats_by_report_kind
};
