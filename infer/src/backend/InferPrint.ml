(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CLOpt = CommandLineOption
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

let print_usage_exit err_s =
  L.user_error "Load Error: %s@\n@." err_s ;
  Config.print_usage_exit ()


(** return the list of the .specs files in the results dir and libs, if they're defined *)
let load_specfiles () =
  let specs_files_in_dir dir =
    let is_specs_file fname =
      Sys.is_directory fname <> `Yes && Filename.check_suffix fname Config.specs_files_suffix
    in
    let all_filenames = try Array.to_list (Sys.readdir dir) with Sys_error _ -> [] in
    let all_filepaths = List.map ~f:(fun fname -> Filename.concat dir fname) all_filenames in
    List.filter ~f:is_specs_file all_filepaths
  in
  let result_specs_dir = DB.filename_to_string DB.Results_dir.specs_dir in
  specs_files_in_dir result_specs_dir


let error_desc_to_plain_string error_desc =
  let pp fmt = Localise.pp_error_desc fmt error_desc in
  let s = F.asprintf "%t" pp in
  let s = String.strip s in
  let s =
    (* end error description with a dot *)
    if String.is_suffix ~suffix:"." s then s else s ^ "."
  in
  s


let error_desc_to_dotty_string error_desc = Localise.error_desc_get_dotty error_desc

let compute_key (bug_type : string) (proc_name : Typ.Procname.t) (filename : string) =
  let base_filename = Filename.basename filename
  and simple_procedure_name = Typ.Procname.get_method proc_name in
  String.concat ~sep:"|" [base_filename; simple_procedure_name; bug_type]


let compute_hash ~(severity : string) ~(bug_type : string) ~(proc_name : Typ.Procname.t)
    ~(file : string) ~(qualifier : string) =
  let base_filename = Filename.basename file in
  let hashable_procedure_name = Typ.Procname.hashable_name proc_name in
  let location_independent_qualifier =
    (* Removing the line,column, and infer temporary variable (e.g., n$67) information from the
       error message as well as the index of the annonymmous class to make the hash invariant
       when moving the source code in the file *)
    Str.global_replace (Str.regexp "\\(line \\|column \\|\\$\\)[0-9]+") "$_" qualifier
  in
  Utils.better_hash
    (severity, bug_type, hashable_procedure_name, base_filename, location_independent_qualifier)
  |> Caml.Digest.to_hex


let loc_trace_to_jsonbug_record trace_list ekind =
  match ekind with
  | Exceptions.Info ->
      []
  | _ ->
      let trace_item_to_record trace_item =
        { Jsonbug_j.level= trace_item.Errlog.lt_level
        ; filename= SourceFile.to_string trace_item.Errlog.lt_loc.Location.file
        ; line_number= trace_item.Errlog.lt_loc.Location.line
        ; column_number= trace_item.Errlog.lt_loc.Location.col
        ; description= trace_item.Errlog.lt_description }
      in
      let record_list = List.rev (List.rev_map ~f:trace_item_to_record trace_list) in
      record_list


type summary_val =
  { vname: string
  ; vname_id: string
  ; vspecs: int
  ; vto: string
  ; vsymop: int
  ; verr: int
  ; vfile: string
  ; vline: int
  ; vsignature: string
  ; vproof_trace: string }

(** compute values from summary data to export to csv format *)
let summary_values summary =
  let stats = summary.Summary.stats in
  let attributes = Summary.get_attributes summary in
  let err_log = Summary.get_err_log summary in
  let proc_name = Summary.get_proc_name summary in
  let vsignature = Summary.get_signature summary in
  let specs = Tabulation.get_specs_from_payload summary in
  let lines_visited =
    let visited = ref BiabductionSummary.Visitedset.empty in
    let do_spec spec =
      visited := BiabductionSummary.Visitedset.union spec.BiabductionSummary.visited !visited
    in
    List.iter ~f:do_spec specs ;
    let visited_lines = ref Int.Set.empty in
    BiabductionSummary.Visitedset.iter
      (fun (_, ls) -> List.iter ~f:(fun l -> visited_lines := Int.Set.add !visited_lines l) ls)
      !visited ;
    Int.Set.elements !visited_lines
  in
  let vproof_trace =
    let pp_line fmt l = F.pp_print_int fmt l in
    let pp fmt = Pp.seq pp_line fmt lines_visited in
    F.asprintf "%t" pp
  in
  { vname= Typ.Procname.to_string proc_name
  ; vname_id= Typ.Procname.to_filename proc_name
  ; vspecs= List.length specs
  ; vto= Summary.Stats.failure_kind_to_string stats
  ; vsymop= Summary.Stats.symops stats
  ; verr= Errlog.size (Exceptions.equal_severity Exceptions.Error) err_log
  ; vfile= SourceFile.to_string attributes.ProcAttributes.loc.Location.file
  ; vline= attributes.ProcAttributes.loc.Location.line
  ; vsignature
  ; vproof_trace }


module ProcsCsv = struct
  (** Print the header of the procedures csv file, with column names *)
  let pp_header fmt () =
    Format.fprintf fmt "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s@\n"
      Io_infer.Xml.tag_name Io_infer.Xml.tag_name_id Io_infer.Xml.tag_specs Io_infer.Xml.tag_time
      Io_infer.Xml.tag_to Io_infer.Xml.tag_symop Io_infer.Xml.tag_err Io_infer.Xml.tag_file
      Io_infer.Xml.tag_line Io_infer.Xml.tag_loc Io_infer.Xml.tag_top Io_infer.Xml.tag_signature
      Io_infer.Xml.tag_weight Io_infer.Xml.tag_proof_coverage Io_infer.Xml.tag_rank
      Io_infer.Xml.tag_in_calls Io_infer.Xml.tag_out_calls Io_infer.Xml.tag_proof_trace


  (** Write proc summary stats in csv format *)
  let pp_summary fmt summary =
    let pp x = F.fprintf fmt x in
    let sv = summary_values summary in
    pp "\"%s\"," (Escape.escape_csv sv.vname) ;
    pp "\"%s\"," (Escape.escape_csv sv.vname_id) ;
    pp "%d," sv.vspecs ;
    pp "%s," sv.vto ;
    pp "%d," sv.vsymop ;
    pp "%d," sv.verr ;
    pp "%s," sv.vfile ;
    pp "%d," sv.vline ;
    pp "\"%s\"," (Escape.escape_csv sv.vsignature) ;
    pp "%s@\n" sv.vproof_trace
end

let should_report (issue_kind : Exceptions.severity) issue_type error_desc eclass =
  if (not Config.filtering) || Exceptions.equal_err_class eclass Exceptions.Linters then true
  else
    let issue_kind_is_blacklisted =
      match issue_kind with Info -> true | Advice | Error | Like | Warning -> false
    in
    if issue_kind_is_blacklisted then false
    else
      let issue_type_is_null_deref =
        let null_deref_issue_types =
          let open IssueType in
          [ field_not_null_checked
          ; null_dereference
          ; parameter_not_null_checked
          ; premature_nil_termination
          ; empty_vector_access
          ; use_after_free ]
        in
        List.mem ~equal:IssueType.equal null_deref_issue_types issue_type
      in
      if issue_type_is_null_deref then Localise.error_desc_is_reportable_bucket error_desc
      else true


(* The reason an issue should be censored (that is, not reported). The empty
   string (that is "no reason") means that the issue should be reported. *)
let censored_reason (issue_type : IssueType.t) source_file =
  let filename = SourceFile.to_rel_path source_file in
  let rejected_by ((issue_type_polarity, issue_type_re), (filename_polarity, filename_re), reason)
      =
    let accepted =
      (* matches issue_type_re implies matches filename_re *)
      (not (Bool.equal issue_type_polarity (Str.string_match issue_type_re issue_type.unique_id 0)))
      || Bool.equal filename_polarity (Str.string_match filename_re filename 0)
    in
    Option.some_if (not accepted) reason
  in
  Option.value ~default:"" (List.find_map Config.filter_report ~f:rejected_by)


let potential_exception_message = "potential exception at line"

module type Printer = sig
  type elt

  val pp_open : F.formatter -> unit -> unit

  val pp_close : F.formatter -> unit -> unit

  val pp : F.formatter -> elt -> unit
end

module MakeJsonListPrinter (P : sig
  type elt

  val to_string : elt -> string option
end) : Printer with type elt = P.elt = struct
  include P

  let is_first_item = ref true

  let pp_open fmt () =
    is_first_item := true ;
    F.fprintf fmt "[@?"


  let pp_close fmt () = F.fprintf fmt "]@\n@?"

  let pp fmt elt =
    match to_string elt with
    | Some s ->
        if !is_first_item then is_first_item := false else F.pp_print_char fmt ',' ;
        F.fprintf fmt "%s@?" s
    | None ->
        ()
end

type json_issue_printer_typ =
  { error_filter: SourceFile.t -> IssueType.t -> bool
  ; proc_name: Typ.Procname.t
  ; proc_loc_opt: Location.t option
  ; err_key: Errlog.err_key
  ; err_data: Errlog.err_data }

let procedure_id_of_procname proc_name =
  match Typ.Procname.get_language proc_name with
  | Language.Java ->
      Typ.Procname.to_unique_id proc_name
  | _ ->
      Typ.Procname.to_string proc_name


module JsonIssuePrinter = MakeJsonListPrinter (struct
  type elt = json_issue_printer_typ

  let to_string ({error_filter; proc_name; proc_loc_opt; err_key; err_data} : elt) =
    let source_file, procedure_start_line =
      match proc_loc_opt with
      | Some proc_loc ->
          (proc_loc.Location.file, proc_loc.Location.line)
      | None ->
          (err_data.loc.Location.file, 0)
    in
    if SourceFile.is_invalid source_file then
      L.(die InternalError)
        "Invalid source file for %a %a@.Trace: %a@." IssueType.pp err_key.err_name
        Localise.pp_error_desc err_key.err_desc Errlog.pp_loc_trace err_data.loc_trace ;
    let should_report_source_file =
      (not (SourceFile.is_infer_model source_file)) || Config.debug_mode || Config.debug_exceptions
    in
    if
      error_filter source_file err_key.err_name
      && should_report_source_file
      && should_report err_key.severity err_key.err_name err_key.err_desc err_data.err_class
    then
      let severity = Exceptions.severity_string err_key.severity in
      let bug_type = err_key.err_name.IssueType.unique_id in
      let file =
        SourceFile.to_string ~force_relative:Config.report_force_relative_path source_file
      in
      let json_ml_loc =
        match err_data.loc_in_ml_source with
        | Some (file, lnum, cnum, enum) when Config.reports_include_ml_loc ->
            Some Jsonbug_j.{file; lnum; cnum; enum}
        | _ ->
            None
      in
      let visibility = Some (Exceptions.string_of_visibility err_data.visibility) in
      let qualifier =
        let base_qualifier = error_desc_to_plain_string err_key.err_desc in
        if IssueType.(equal resource_leak) err_key.err_name then
          match Errlog.compute_local_exception_line err_data.loc_trace with
          | None ->
              base_qualifier
          | Some line ->
              let potential_exception_message =
                Format.asprintf "%a: %s %d" MarkupFormatter.pp_bold "Note"
                  potential_exception_message line
              in
              Format.sprintf "%s@\n%s" base_qualifier potential_exception_message
        else base_qualifier
      in
      let bug =
        { Jsonbug_j.bug_type
        ; qualifier
        ; severity
        ; visibility
        ; line= err_data.loc.Location.line
        ; column= err_data.loc.Location.col
        ; procedure= procedure_id_of_procname proc_name
        ; procedure_start_line
        ; file
        ; bug_trace= loc_trace_to_jsonbug_record err_data.loc_trace err_key.severity
        ; node_key= Option.map ~f:Procdesc.NodeKey.to_string err_data.node_key
        ; key= compute_key bug_type proc_name file
        ; hash= compute_hash ~severity ~bug_type ~proc_name ~file ~qualifier
        ; dotty= error_desc_to_dotty_string err_key.err_desc
        ; infer_source_loc= json_ml_loc
        ; bug_type_hum= err_key.err_name.IssueType.hum
        ; linters_def_file= err_data.linters_def_file
        ; doc_url= err_data.doc_url
        ; traceview_id= None
        ; censored_reason= censored_reason err_key.err_name source_file
        ; access= err_data.access
        ; extras= err_data.extras }
      in
      Some (Jsonbug_j.string_of_jsonbug bug)
    else None
end)

module IssuesJson = struct
  include JsonIssuePrinter

  (** Write bug report in JSON format *)
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt proc_name err_log =
    Errlog.iter
      (fun err_key err_data -> pp fmt {error_filter; proc_name; proc_loc_opt; err_key; err_data})
      err_log
end

type json_costs_printer_typ =
  {loc: Location.t; proc_name: Typ.Procname.t; cost_opt: CostDomain.summary option}

module JsonCostsPrinter = MakeJsonListPrinter (struct
  type elt = json_costs_printer_typ

  let to_string {loc; proc_name; cost_opt} =
    match cost_opt with
    | Some {post} ->
        let hum =
          if Config.developer_mode then
            Some
              { Jsonbug_t.hum_polynomial= Format.asprintf "%a" CostDomain.BasicCost.pp post
              ; hum_degree= Format.asprintf "%a" CostDomain.BasicCost.pp_degree post }
          else None
        in
        let cost_item =
          let file = SourceFile.to_rel_path loc.Location.file in
          { Jsonbug_t.hash= compute_hash ~severity:"" ~bug_type:"" ~proc_name ~file ~qualifier:""
          ; loc= {file; lnum= loc.Location.line; cnum= loc.Location.col; enum= -1}
          ; procedure_id= procedure_id_of_procname proc_name
          ; polynomial= CostDomain.BasicCost.encode post
          ; hum }
        in
        Some (Jsonbug_j.string_of_cost_item cost_item)
    | None ->
        None
end)

let pp_custom_of_report fmt report fields =
  let pp_custom_of_issue fmt (issue : Jsonbug_t.jsonbug) =
    let open Jsonbug_t in
    let comma_separator index = if index > 0 then ", " else "" in
    let pp_trace fmt trace comma =
      let pp_trace_elem fmt {description} = F.pp_print_string fmt description in
      let trace_without_empty_descs =
        List.filter ~f:(fun {description} -> description <> "") trace
      in
      F.fprintf fmt "%s[%a]" comma (Pp.comma_seq pp_trace_elem) trace_without_empty_descs
    in
    let pp_field index field =
      match field with
      | `Issue_field_bug_type ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.bug_type
      | `Issue_field_bucket ->
          let bucket =
            match
              String.lsplit2 issue.qualifier ~on:']'
              |> Option.map ~f:fst
              |> Option.bind ~f:(String.chop_prefix ~prefix:"[")
            with
            | Some bucket ->
                bucket
            | None ->
                "no_bucket"
          in
          Format.fprintf fmt "%s%s" (comma_separator index) bucket
      | `Issue_field_qualifier ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.qualifier
      | `Issue_field_severity ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.severity
      | `Issue_field_visibility ->
          Format.fprintf fmt "%s%a" (comma_separator index) (Pp.option String.pp) issue.visibility
      | `Issue_field_line ->
          Format.fprintf fmt "%s%d" (comma_separator index) issue.line
      | `Issue_field_column ->
          Format.fprintf fmt "%s%d" (comma_separator index) issue.column
      | `Issue_field_procedure ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.procedure
      | `Issue_field_procedure_start_line ->
          Format.fprintf fmt "%s%d" (comma_separator index) issue.procedure_start_line
      | `Issue_field_file ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.file
      | `Issue_field_bug_trace ->
          pp_trace fmt issue.bug_trace (comma_separator index)
      | `Issue_field_key ->
          Format.fprintf fmt "%s%s" (comma_separator index) (Caml.Digest.to_hex issue.key)
      | `Issue_field_hash ->
          Format.fprintf fmt "%s%s" (comma_separator index) (Caml.Digest.to_hex issue.hash)
      | `Issue_field_line_offset ->
          Format.fprintf fmt "%s%d" (comma_separator index)
            (issue.line - issue.procedure_start_line)
      | `Issue_field_qualifier_contains_potential_exception_note ->
          Format.pp_print_bool fmt
            (String.is_substring issue.qualifier ~substring:potential_exception_message)
    in
    List.iteri ~f:pp_field fields ; Format.fprintf fmt "@."
  in
  List.iter ~f:(pp_custom_of_issue fmt) report


let tests_jsonbug_compare (bug1 : Jsonbug_t.jsonbug) (bug2 : Jsonbug_t.jsonbug) =
  let open Jsonbug_t in
  [%compare: string * string * int * string * Caml.Digest.t]
    (bug1.file, bug1.procedure, bug1.line - bug1.procedure_start_line, bug1.bug_type, bug1.hash)
    (bug2.file, bug2.procedure, bug2.line - bug2.procedure_start_line, bug2.bug_type, bug2.hash)


module IssuesTxt = struct
  let pp_issue fmt error_filter proc_loc_opt (key : Errlog.err_key) (err_data : Errlog.err_data) =
    let source_file =
      match proc_loc_opt with
      | Some proc_loc ->
          proc_loc.Location.file
      | None ->
          err_data.loc.Location.file
    in
    if
      error_filter source_file key.err_name
      && ((not Config.filtering) || String.is_empty (censored_reason key.err_name source_file))
    then Exceptions.pp_err err_data.loc key.severity key.err_name key.err_desc None fmt ()


  (** Write bug report in text format *)
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt _ err_log =
    Errlog.iter (pp_issue fmt error_filter proc_loc_opt) err_log
end

let pp_text_of_report fmt report =
  let pp_row jsonbug =
    let open Jsonbug_t in
    F.fprintf fmt "%s:%d: %s: %s %s@\n" jsonbug.file jsonbug.line jsonbug.severity jsonbug.bug_type
      jsonbug.qualifier
  in
  List.iter ~f:pp_row report ; F.fprintf fmt "@?"


module Stats = struct
  type t =
    { files: (SourceFile.t, unit) Hashtbl.t
    ; mutable nchecked: int
    ; mutable ndefective: int
    ; mutable nerrors: int
    ; mutable ninfos: int
    ; mutable nadvice: int
    ; mutable nlikes: int
    ; mutable nprocs: int
    ; mutable nspecs: int
    ; mutable ntimeouts: int
    ; mutable nverified: int
    ; mutable nwarnings: int
    ; mutable saved_errors: string list }

  let create () =
    { files= Hashtbl.create 3
    ; nchecked= 0
    ; ndefective= 0
    ; nerrors= 0
    ; ninfos= 0
    ; nadvice= 0
    ; nlikes= 0
    ; nprocs= 0
    ; nspecs= 0
    ; ntimeouts= 0
    ; nverified= 0
    ; nwarnings= 0
    ; saved_errors= [] }


  let process_loc loc stats =
    try Hashtbl.find stats.files loc.Location.file with Caml.Not_found ->
      Hashtbl.add stats.files loc.Location.file ()


  let loc_trace_to_string_list linereader indent_num ltr =
    let res = ref [] in
    let indent_string n =
      let s = ref "" in
      for _ = 1 to n do
        s := "  " ^ !s
      done ;
      !s
    in
    let num = ref 0 in
    let loc_to_string lt =
      incr num ;
      let loc = lt.Errlog.lt_loc in
      let level = lt.Errlog.lt_level in
      let description = lt.Errlog.lt_description in
      let code =
        match Printer.LineReader.from_loc linereader loc with Some s -> s | None -> ""
      in
      let line =
        let pp fmt =
          if description <> "" then
            F.fprintf fmt "%s%4s  // %s@\n" (indent_string (level + indent_num)) " " description ;
          F.fprintf fmt "%s%04d: %s" (indent_string (level + indent_num)) loc.Location.line code
        in
        F.asprintf "%t" pp
      in
      res := line :: "" :: !res
    in
    List.iter ~f:loc_to_string ltr ; List.rev !res


  let process_err_log error_filter linereader err_log stats =
    let found_errors = ref false in
    let process_row (key : Errlog.err_key) (err_data : Errlog.err_data) =
      let type_str = key.err_name.IssueType.unique_id in
      if error_filter key.err_name then
        match key.severity with
        | Exceptions.Error ->
            found_errors := true ;
            stats.nerrors <- stats.nerrors + 1 ;
            let error_strs =
              let pp1 fmt = F.fprintf fmt "%d: %s" stats.nerrors type_str in
              let pp2 fmt =
                F.fprintf fmt "  %a:%d" SourceFile.pp err_data.loc.Location.file
                  err_data.loc.Location.line
              in
              let pp3 fmt = F.fprintf fmt "  (%a)" Localise.pp_error_desc key.err_desc in
              [F.asprintf "%t" pp1; F.asprintf "%t" pp2; F.asprintf "%t" pp3]
            in
            let trace = loc_trace_to_string_list linereader 1 err_data.loc_trace in
            stats.saved_errors <- List.rev_append (error_strs @ trace @ [""]) stats.saved_errors
        | Exceptions.Warning ->
            stats.nwarnings <- stats.nwarnings + 1
        | Exceptions.Info ->
            stats.ninfos <- stats.ninfos + 1
        | Exceptions.Advice ->
            stats.nadvice <- stats.nadvice + 1
        | Exceptions.Like ->
            stats.nlikes <- stats.nlikes + 1
    in
    Errlog.iter process_row err_log ; !found_errors


  let process_summary error_filter summary linereader stats =
    let specs = Tabulation.get_specs_from_payload summary in
    let found_errors =
      process_err_log error_filter linereader (Summary.get_err_log summary) stats
    in
    let is_defective = found_errors in
    let is_verified = specs <> [] && not is_defective in
    let is_checked = not (is_defective || is_verified) in
    let is_timeout =
      match Summary.(Stats.failure_kind summary.stats) with
      | None | Some (FKcrash _) ->
          false
      | _ ->
          true
    in
    stats.nprocs <- stats.nprocs + 1 ;
    stats.nspecs <- stats.nspecs + List.length specs ;
    if is_verified then stats.nverified <- stats.nverified + 1 ;
    if is_checked then stats.nchecked <- stats.nchecked + 1 ;
    if is_timeout then stats.ntimeouts <- stats.ntimeouts + 1 ;
    if is_defective then stats.ndefective <- stats.ndefective + 1 ;
    process_loc (Summary.get_loc summary) stats


  let num_files stats = Hashtbl.length stats.files

  let pp fmt stats =
    F.fprintf fmt "Files: %d@\n" (num_files stats) ;
    F.fprintf fmt "Specs: %d@\n" stats.nspecs ;
    F.fprintf fmt "Timeouts: %d@\n" stats.ntimeouts ;
    F.fprintf fmt "Procedures: %d@\n" stats.nprocs ;
    F.fprintf fmt "  Verified: %d@\n" stats.nverified ;
    F.fprintf fmt "  Checked: %d@\n" stats.nchecked ;
    F.fprintf fmt "  Defective: %d@\n" stats.ndefective ;
    F.fprintf fmt "Errors: %d@\n" stats.nerrors ;
    F.fprintf fmt "Warnings: %d@\n" stats.nwarnings ;
    F.fprintf fmt "Infos: %d@\n" stats.ninfos ;
    F.fprintf fmt "@\n -------------------@\n" ;
    F.fprintf fmt "@\nDetailed Errors@\n@\n" ;
    List.iter ~f:(fun s -> F.fprintf fmt "%s@\n" s) (List.rev stats.saved_errors)
end

module StatsLogs = struct
  let process _ (summary : Summary.t) _ _ =
    let num_preposts =
      match summary.payloads.biabduction with Some {preposts} -> List.length preposts | None -> 0
    in
    let clang_method_kind =
      ClangMethodKind.to_string (Summary.get_attributes summary).clang_method_kind
    in
    let proc_name = Summary.get_proc_name summary in
    let lang = Typ.Procname.get_language proc_name in
    let stats =
      EventLogger.AnalysisStats
        { analysis_nodes_visited= Summary.Stats.nb_visited summary.stats
        ; analysis_status= Summary.Stats.failure_kind summary.stats
        ; analysis_total_nodes= Summary.get_proc_desc summary |> Procdesc.get_nodes_num
        ; clang_method_kind=
            (match lang with Language.Clang -> Some clang_method_kind | _ -> None)
        ; lang= Language.to_explicit_string lang
        ; method_location= Summary.get_loc summary
        ; method_name= Typ.Procname.to_string proc_name
        ; num_preposts
        ; symops= Summary.Stats.symops summary.stats }
    in
    EventLogger.log stats
end

module Report = struct
  let pp_header fmt () =
    F.fprintf fmt "Infer Analysis Results -- generated %a@\n@\n" Pp.current_time () ;
    F.fprintf fmt "Summary Report@\n@\n"


  let pp_stats fmt stats = Stats.pp fmt stats
end

(** Categorize the preconditions of specs and print stats *)
module PreconditionStats = struct
  let nr_nopres = ref 0

  let nr_empty = ref 0

  let nr_onlyallocation = ref 0

  let nr_dataconstraints = ref 0

  let do_summary proc_name summary =
    let specs = Tabulation.get_specs_from_payload summary in
    let preconditions =
      List.map ~f:(fun spec -> BiabductionSummary.Jprop.to_prop spec.BiabductionSummary.pre) specs
    in
    match Prop.CategorizePreconditions.categorize preconditions with
    | Prop.CategorizePreconditions.Empty ->
        incr nr_empty ;
        L.result "Procedure: %a footprint:Empty@." Typ.Procname.pp proc_name
    | Prop.CategorizePreconditions.OnlyAllocation ->
        incr nr_onlyallocation ;
        L.result "Procedure: %a footprint:OnlyAllocation@." Typ.Procname.pp proc_name
    | Prop.CategorizePreconditions.NoPres ->
        incr nr_nopres ;
        L.result "Procedure: %a footprint:NoPres@." Typ.Procname.pp proc_name
    | Prop.CategorizePreconditions.DataConstraints ->
        incr nr_dataconstraints ;
        L.result "Procedure: %a footprint:DataConstraints@." Typ.Procname.pp proc_name


  let pp_stats () =
    L.result "@.Precondition stats@." ;
    L.result "Procedures with no preconditions: %d@." !nr_nopres ;
    L.result "Procedures with empty precondition: %d@." !nr_empty ;
    L.result "Procedures with only allocation conditions: %d@." !nr_onlyallocation ;
    L.result "Procedures with data constraints: %d@." !nr_dataconstraints
end

let error_filter filters proc_name file error_name =
  (Config.write_html || not (IssueType.(equal skip_function) error_name))
  && filters.Inferconfig.path_filter file
  && filters.Inferconfig.error_filter error_name
  && filters.Inferconfig.proc_filter proc_name


type report_kind = Costs | Issues | Procs | Stats | Summary [@@deriving compare]

let _string_of_report_kind = function
  | Costs ->
      "Costs"
  | Issues ->
      "Issues"
  | Procs ->
      "Procs"
  | Stats ->
      "Stats"
  | Summary ->
      "Summary"


type bug_format_kind = Json | Csv | Logs | Tests | Text [@@deriving compare]

let _string_of_bug_format_kind = function
  | Json ->
      "Json"
  | Csv ->
      "Csv"
  | Logs ->
      "Logs"
  | Tests ->
      "Tests"
  | Text ->
      "Text"


let get_outfile outfile =
  match outfile with
  | Some outfile ->
      outfile
  | None ->
      L.(die InternalError) "An outfile is require for this format."


let pp_issue_in_format (format_kind, (outfile_opt : Utils.outfile option)) error_filter
    {Issue.proc_name; proc_location; err_key; err_data} =
  match format_kind with
  | Json ->
      let outf = get_outfile outfile_opt in
      IssuesJson.pp outf.fmt
        {error_filter; proc_name; proc_loc_opt= Some proc_location; err_key; err_data}
  | Csv ->
      L.(die InternalError) "Printing issues in a CSV format is not implemented"
  | Tests ->
      L.(die InternalError) "Printing issues as tests is not implemented"
  | Logs ->
      L.(die InternalError) "Printing issues as logs is not implemented"
  | Text ->
      let outf = get_outfile outfile_opt in
      IssuesTxt.pp_issue outf.fmt error_filter (Some proc_location) err_key err_data


let pp_issues_in_format (format_kind, (outfile_opt : Utils.outfile option)) =
  match format_kind with
  | Json ->
      let outf = get_outfile outfile_opt in
      IssuesJson.pp_issues_of_error_log outf.fmt
  | Csv ->
      L.(die InternalError) "Printing issues in a CSV format is not implemented"
  | Tests ->
      L.(die InternalError) "Printing issues as tests is not implemented"
  | Logs ->
      L.(die InternalError) "Printing issues as logs is not implemented"
  | Text ->
      let outf = get_outfile outfile_opt in
      IssuesTxt.pp_issues_of_error_log outf.fmt


let pp_procs_in_format (format_kind, (outfile_opt : Utils.outfile option)) =
  match format_kind with
  | Csv ->
      let outf = get_outfile outfile_opt in
      ProcsCsv.pp_summary outf.fmt
  | Json | Tests | Text | Logs ->
      L.(die InternalError) "Printing procs in json/tests/text/logs is not implemented"


let pp_stats_in_format (format_kind, _) =
  match format_kind with
  | Csv ->
      Stats.process_summary
  | Logs ->
      StatsLogs.process
  | Json | Tests | Text ->
      L.(die InternalError) "Printing stats in json/tests/text is not implemented"


let pp_issues_of_error_log error_filter linereader proc_loc_opt procname err_log bug_format_list =
  let pp_issues_in_format format =
    pp_issues_in_format format error_filter linereader proc_loc_opt procname err_log
  in
  List.iter ~f:pp_issues_in_format bug_format_list


let collect_issues summary issues_acc =
  let err_log = Summary.get_err_log summary in
  let proc_name = Summary.get_proc_name summary in
  let proc_location = Summary.get_loc summary in
  Errlog.fold
    (fun err_key err_data acc -> {Issue.proc_name; proc_location; err_key; err_data} :: acc)
    err_log issues_acc


let pp_procs summary procs_format_list =
  let pp_procs_in_format format =
    let pp_procs = pp_procs_in_format format in
    pp_procs summary
  in
  List.iter ~f:pp_procs_in_format procs_format_list


let pp_stats error_filter linereader summary stats stats_format_list =
  let pp_stats_in_format format =
    let pp_stats = pp_stats_in_format format in
    pp_stats error_filter summary linereader stats
  in
  List.iter ~f:pp_stats_in_format stats_format_list


let pp_summary summary =
  L.result "Procedure: %a@\n%a@." Typ.Procname.pp (Summary.get_proc_name summary) Summary.pp_text
    summary


let pp_costs_in_format (format_kind, (outfile_opt : Utils.outfile option)) =
  match format_kind with
  | Json ->
      let outf = get_outfile outfile_opt in
      JsonCostsPrinter.pp outf.fmt
  | Csv | Tests | Text | Logs ->
      L.(die InternalError) "Printing costs in csv/tests/text/logs is not implemented"


let pp_costs summary costs_format_list =
  let pp format =
    pp_costs_in_format format
      { loc= Summary.get_loc summary
      ; proc_name= Summary.get_proc_name summary
      ; cost_opt= summary.Summary.payloads.Payloads.cost }
  in
  List.iter ~f:pp costs_format_list


let pp_summary_by_report_kind formats_by_report_kind summary error_filter linereader stats file
    issues_acc =
  let pp_summary_by_report_kind (report_kind, format_list) =
    match (report_kind, format_list) with
    | Costs, _ ->
        pp_costs summary format_list
    | Procs, _ :: _ ->
        pp_procs summary format_list
    | Stats, _ :: _ ->
        pp_stats (error_filter file) linereader summary stats format_list
    | Summary, _ when InferCommand.equal Config.command Report && not Config.quiet ->
        pp_summary summary
    | _ ->
        ()
  in
  List.iter ~f:pp_summary_by_report_kind formats_by_report_kind ;
  collect_issues summary issues_acc


let pp_json_report_by_report_kind formats_by_report_kind fname =
  match Utils.read_file fname with
  | Ok report_lines ->
      let pp_json_issues format_list report =
        let pp_json_issue (format_kind, (outfile_opt : Utils.outfile option)) =
          match format_kind with
          | Tests ->
              let outf = get_outfile outfile_opt in
              pp_custom_of_report outf.fmt report Config.issues_fields
          | Text ->
              let outf = get_outfile outfile_opt in
              pp_text_of_report outf.fmt report
          | Json ->
              L.(die InternalError) "Printing issues from json does not support json output"
          | Csv ->
              L.(die InternalError) "Printing issues from json does not support csv output"
          | Logs ->
              L.(die InternalError) "Printing issues from json does not support logs output"
        in
        List.iter ~f:pp_json_issue format_list
      in
      let sorted_report =
        let report = Jsonbug_j.report_of_string (String.concat ~sep:"\n" report_lines) in
        List.sort ~compare:tests_jsonbug_compare report
      in
      let pp_report_by_report_kind (report_kind, format_list) =
        match (report_kind, format_list) with
        | Issues, _ :: _ ->
            pp_json_issues format_list sorted_report
        | _ ->
            ()
      in
      List.iter ~f:pp_report_by_report_kind formats_by_report_kind
  | Error error ->
      L.(die UserError) "Error reading '%s': %s" fname error


let pp_lint_issues_by_report_kind formats_by_report_kind error_filter linereader procname error_log
    =
  let pp_summary_by_report_kind (report_kind, format_list) =
    match (report_kind, format_list) with
    | Issues, _ :: _ ->
        pp_issues_of_error_log error_filter linereader None procname error_log format_list
    | _ ->
        ()
  in
  List.iter ~f:pp_summary_by_report_kind formats_by_report_kind


(** Process lint issues of a procedure *)
let pp_lint_issues filters formats_by_report_kind linereader procname error_log =
  let error_filter = error_filter filters procname in
  pp_lint_issues_by_report_kind formats_by_report_kind error_filter linereader procname error_log


(** Process a summary *)
let process_summary filters formats_by_report_kind linereader stats summary issues_acc =
  let file = (Summary.get_loc summary).Location.file in
  let proc_name = Summary.get_proc_name summary in
  let error_filter = error_filter filters proc_name in
  let issues_acc' =
    pp_summary_by_report_kind formats_by_report_kind summary error_filter linereader stats file
      issues_acc
  in
  if Config.precondition_stats then PreconditionStats.do_summary proc_name summary ;
  issues_acc'


let spec_files_from_cmdline () =
  if CLOpt.is_originator then (
    (* Find spec files specified by command-line arguments.  Not run at init time since the specs
         files may be generated between init and report time. *)
    List.iter
      ~f:(fun arg ->
        if (not (Filename.check_suffix arg Config.specs_files_suffix)) && arg <> "." then
          print_usage_exit ("file " ^ arg ^ ": arguments must be .specs files") )
      Config.anon_args ;
    if Config.test_filtering then ( Inferconfig.test () ; L.exit 0 ) ;
    if List.is_empty Config.anon_args then load_specfiles () else List.rev Config.anon_args )
  else load_specfiles ()


(** Create an iterator which loads spec files one at a time *)
let get_summary_iterator () =
  let sorted_spec_files = List.sort ~compare:String.compare (spec_files_from_cmdline ()) in
  let do_spec f fname =
    match Summary.load_from_file (DB.filename_from_string fname) with
    | None ->
        L.(die UserError) "Error: cannot open file %s@." fname
    | Some summary ->
        f summary
  in
  let iterate f = List.iter ~f:(do_spec f) sorted_spec_files in
  iterate


(** Although the out_file is an Option type, the None option is strictly meant for the
  logs format_kind, and all other formats should contain an outfile value. *)
let mk_format format_kind fname =
  Option.value_map
    ~f:(fun out_file -> [(format_kind, Some out_file)])
    ~default:[] (Utils.create_outfile fname)


let init_issues_format_list report_json =
  let json_format = Option.value_map ~f:(mk_format Json) ~default:[] report_json in
  let tests_format = Option.value_map ~f:(mk_format Tests) ~default:[] Config.issues_tests in
  let txt_format = Option.value_map ~f:(mk_format Text) ~default:[] Config.issues_txt in
  json_format @ tests_format @ txt_format


let init_procs_format_list () = Option.value_map ~f:(mk_format Csv) ~default:[] Config.procs_csv

let init_stats_format_list () =
  let csv_format = Option.value_map ~f:(mk_format Csv) ~default:[] Config.stats_report in
  let logs_format = if Config.log_events then [(Logs, None)] else [] in
  csv_format @ logs_format


let init_files format_list_by_kind =
  let init_files_of_report_kind (report_kind, format_list) =
    let init_files_of_format (format_kind, (outfile_opt : Utils.outfile option)) =
      match (format_kind, report_kind) with
      | Csv, Issues ->
          L.(die InternalError) "Printing issues in a CSV format is not implemented"
      | Logs, (Issues | Procs | Summary) ->
          L.(die InternalError) "Logging these reports is not implemented"
      | Csv, Procs ->
          let outfile = get_outfile outfile_opt in
          ProcsCsv.pp_header outfile.fmt ()
      | Csv, Stats ->
          let outfile = get_outfile outfile_opt in
          Report.pp_header outfile.fmt ()
      | Json, Costs ->
          let outfile = get_outfile outfile_opt in
          JsonCostsPrinter.pp_open outfile.fmt ()
      | Json, Issues ->
          let outfile = get_outfile outfile_opt in
          IssuesJson.pp_open outfile.fmt ()
      | Csv, (Costs | Summary)
      | Logs, (Costs | Stats)
      | Json, (Procs | Stats | Summary)
      | Tests, _
      | Text, _ ->
          ()
    in
    List.iter ~f:init_files_of_format format_list
  in
  List.iter ~f:init_files_of_report_kind format_list_by_kind


let finalize_and_close_files format_list_by_kind (stats : Stats.t) =
  let close_files_of_report_kind (report_kind, format_list) =
    let close_files_of_format (format_kind, (outfile_opt : Utils.outfile option)) =
      ( match (format_kind, report_kind) with
      | Logs, (Issues | Procs | Summary) ->
          L.(die InternalError) "Logging these reports is not implemented"
      | Csv, Stats ->
          let outfile = get_outfile outfile_opt in
          F.fprintf outfile.fmt "%a@?" Report.pp_stats stats
      | Json, Costs ->
          let outfile = get_outfile outfile_opt in
          JsonCostsPrinter.pp_close outfile.fmt ()
      | Json, Issues ->
          let outfile = get_outfile outfile_opt in
          IssuesJson.pp_close outfile.fmt ()
      | Csv, (Costs | Issues | Procs | Summary)
      | Logs, (Costs | Stats)
      | Json, (Procs | Stats | Summary)
      | Tests, _
      | Text, _ ->
          () ) ;
      match outfile_opt with Some outfile -> Utils.close_outf outfile | None -> ()
    in
    List.iter ~f:close_files_of_format format_list ;
    ()
  in
  List.iter ~f:close_files_of_report_kind format_list_by_kind


let pp_summary_and_issues formats_by_report_kind issue_formats =
  let stats = Stats.create () in
  let linereader = Printer.LineReader.create () in
  let filters = Inferconfig.create_filters () in
  let iterate_summaries = get_summary_iterator () in
  let all_issues = ref [] in
  iterate_summaries (fun summary ->
      all_issues :=
        process_summary filters formats_by_report_kind linereader stats summary !all_issues ) ;
  all_issues := Issue.sort_filter_issues !all_issues ;
  if Config.quandaryBO then all_issues := QuandaryBO.update_issues !all_issues ;
  List.iter
    ~f:(fun ({Issue.proc_name} as issue) ->
      let error_filter = error_filter filters proc_name in
      List.iter
        ~f:(fun issue_format -> pp_issue_in_format issue_format error_filter issue)
        issue_formats )
    !all_issues ;
  if Config.precondition_stats then PreconditionStats.pp_stats () ;
  List.iter
    [Config.lint_issues_dir_name; Config.starvation_issues_dir_name; Config.racerd_issues_dir_name]
    ~f:(fun dir_name ->
      IssueLog.load dir_name ;
      IssueLog.iter (pp_lint_issues filters formats_by_report_kind linereader) ) ;
  finalize_and_close_files formats_by_report_kind stats


let register_perf_stats_report () =
  let rtime_span, initial_times = (Mtime_clock.counter (), Unix.times ()) in
  PerfStats.register_report (PerfStats.Time (rtime_span, initial_times)) PerfStats.Reporting


let main ~report_json =
  ( if Config.loop_hoisting then
    match Config.perf_profiler_data_file with
    | Some fname ->
        LoadPerfData.read_file_perf_data fname
    | _ ->
        () ) ;
  let issue_formats = init_issues_format_list report_json in
  let formats_by_report_kind =
    let costs_report_format_kind =
      match report_json with
      | Some _ ->
          let file = Config.(results_dir ^/ Config.costs_report_json) in
          [(Costs, mk_format Json file)]
      | None ->
          []
    in
    costs_report_format_kind
    @ [ (Issues, issue_formats)
      ; (Procs, init_procs_format_list ())
      ; (Stats, init_stats_format_list ())
      ; (Summary, []) ]
  in
  register_perf_stats_report () ;
  init_files formats_by_report_kind ;
  ( match Config.from_json_report with
  | Some fname ->
      pp_json_report_by_report_kind formats_by_report_kind fname
  | None ->
      pp_summary_and_issues formats_by_report_kind issue_formats ) ;
  PerfStats.get_reporter PerfStats.Reporting ()
