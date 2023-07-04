(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module BStats = Stats

module Stats = struct
  type t =
    { failure_kind: Exception.failure_kind option
          (** what type of failure stopped the analysis (if any) *)
    ; symops: int  (** Number of SymOp's throughout the whole analysis of the function *)
    ; mutable nodes_visited: IntSet.t  (** Nodes visited *) }

  let empty = {failure_kind= None; symops= 0; nodes_visited= IntSet.empty}

  let is_visited stats node_id = IntSet.mem node_id stats.nodes_visited

  let add_visited stats node_id = stats.nodes_visited <- IntSet.add node_id stats.nodes_visited

  let update ?(add_symops = 0) ?failure_kind stats =
    let symops = stats.symops + add_symops in
    let failure_kind = match failure_kind with None -> stats.failure_kind | some -> some in
    {stats with symops; failure_kind}


  let pp_failure_kind_opt fmt failure_kind_opt =
    match failure_kind_opt with
    | Some failure_kind ->
        Exception.pp_failure_kind fmt failure_kind
    | None ->
        F.pp_print_string fmt "NONE"


  let pp fmt {failure_kind; symops} =
    F.fprintf fmt "FAILURE:%a SYMOPS:%d@\n" pp_failure_kind_opt failure_kind symops
end

type t =
  { payloads: Payloads.t
  ; mutable sessions: int
  ; stats: Stats.t
  ; proc_name: Procname.t
  ; err_log: Errlog.t
  ; mutable dependencies: Dependencies.t }

let yojson_of_t {proc_name; payloads} = [%yojson_of: Procname.t * Payloads.t] (proc_name, payloads)

type full_summary = t

let pp_errlog fmt err_log =
  F.fprintf fmt "ERRORS: @[<h>%a@]@\n%!" Errlog.pp_errors err_log ;
  F.fprintf fmt "WARNINGS: @[<h>%a@]" Errlog.pp_warnings err_log


let pp_signature fmt {proc_name} =
  let {ProcAttributes.ret_type; formals} = Attributes.load_exn proc_name in
  let pp_formal fmt (p, typ, _) = F.fprintf fmt "%a %a" (Typ.pp_full Pp.text) typ Mangled.pp p in
  F.fprintf fmt "%a %a(%a)" (Typ.pp_full Pp.text) ret_type Procname.pp proc_name
    (Pp.seq ~sep:", " pp_formal) formals


let pp_no_stats_specs fmt summary = F.fprintf fmt "%a@\n" pp_signature summary

let pp_text fmt ({err_log; payloads; stats; dependencies} as summary) =
  pp_no_stats_specs fmt summary ;
  F.fprintf fmt "%a@\n%a%a%a" pp_errlog err_log Stats.pp stats (Payloads.pp Pp.text) payloads
    Dependencies.pp dependencies


let pp_html source fmt ({err_log; payloads; stats} as summary) =
  let pp_escaped pp fmt x = F.fprintf fmt "%s" (Escape.escape_xml (F.asprintf "%a" pp x)) in
  F.pp_force_newline fmt () ;
  Pp.html_with_color Black (pp_escaped pp_no_stats_specs) fmt summary ;
  F.fprintf fmt "<br />%a<br />@\n" Stats.pp stats ;
  Errlog.pp_html source [] fmt err_log ;
  Io_infer.Html.pp_hline fmt () ;
  F.fprintf fmt "<LISTING>@\n" ;
  pp_escaped (Payloads.pp (Pp.html Black)) fmt payloads ;
  F.fprintf fmt "</LISTING>@\n"


module ReportSummary = struct
  type t =
    { loc: Location.t
    ; cost_opt: CostDomain.summary option
    ; config_impact_opt: ConfigImpactAnalysis.Summary.t option
    ; err_log: Errlog.t }

  let of_full_summary ({proc_name; payloads; err_log} : full_summary) : t =
    { loc= (Attributes.load_exn proc_name).loc
    ; cost_opt= Lazy.force payloads.Payloads.cost
    ; config_impact_opt= Lazy.force payloads.Payloads.config_impact_analysis
    ; err_log }


  let empty () =
    {loc= Location.dummy; cost_opt= None; config_impact_opt= None; err_log= Errlog.empty ()}


  module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
    type nonrec t = t
  end)
end

module SummaryMetadata = struct
  type t =
    {sessions: int; stats: Stats.t; proc_name: Procname.t; dependencies: Dependencies.complete}
  [@@deriving fields]

  let of_full_summary (f : full_summary) : t =
    { sessions= f.sessions
    ; stats= f.stats
    ; proc_name= f.proc_name
    ; dependencies= Dependencies.complete_exn f.dependencies }


  let empty proc_name =
    { sessions= 0
    ; stats= Stats.empty
    ; proc_name
    ; dependencies= Dependencies.reset proc_name |> Dependencies.freeze proc_name }


  module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
    type nonrec t = t
  end)
end

let mk_full_summary payloads (report_summary : ReportSummary.t)
    (summary_metadata : SummaryMetadata.t) : full_summary =
  { payloads
  ; sessions= summary_metadata.sessions
  ; stats= summary_metadata.stats
  ; proc_name= summary_metadata.proc_name
  ; dependencies= Dependencies.Complete summary_metadata.dependencies
  ; err_log= report_summary.err_log }


module OnDisk = struct
  type cache = t Procname.Hash.t

  let cache : cache = Procname.Hash.create 128

  let clear_cache () = Procname.Hash.clear cache

  (** Remove an element from the cache of summaries. Contrast to reset which re-initializes a
      summary keeping the same Procdesc and updates the cache accordingly. *)
  let remove_from_cache pname = Procname.Hash.remove cache pname

  (** Add the summary to the table for the given function *)
  let add (proc_name : Procname.t) (summary : t) : unit =
    Procname.Hash.replace cache proc_name summary


  let spec_of_procname, spec_of_model =
    let mk_lazy_load_stmt table =
      Database.register_statement AnalysisDatabase
        "SELECT report_summary, summary_metadata FROM %s WHERE proc_uid = :k"
        (Database.string_of_analysis_table table)
    in
    let mk_eager_load_stmt table =
      Database.register_statement AnalysisDatabase
        "SELECT report_summary, summary_metadata, %s FROM %s WHERE proc_uid = :k"
        (F.asprintf "%a" (Pp.seq ~sep:", " F.pp_print_string) PayloadId.database_fields)
        (Database.string_of_analysis_table table)
    in
    let load_spec ~lazy_payloads ~load_statement table proc_name =
      Database.with_registered_statement load_statement ~f:(fun db load_stmt ->
          let proc_uid = Procname.to_unique_id proc_name in
          Sqlite3.bind load_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"load proc specs bind proc_name" ;
          SqliteUtils.result_option ~finalize:false db ~log:"load proc specs run" load_stmt
            ~read_row:(fun stmt ->
              let report_summary = Sqlite3.column stmt 0 |> ReportSummary.SQLite.deserialize in
              let summary_metadata = Sqlite3.column stmt 1 |> SummaryMetadata.SQLite.deserialize in
              let payloads =
                if lazy_payloads then Payloads.SQLite.lazy_load table ~proc_uid
                else Payloads.SQLite.eager_load ~first_column:2 stmt
              in
              mk_full_summary payloads report_summary summary_metadata ) )
    in
    let mk_load_spec (table : Database.analysis_table) =
      let is_specs_table = match table with Specs -> true | BiabductionModelsSpecs -> false in
      let lazy_load_statement = mk_lazy_load_stmt table in
      let eager_load_statement = mk_eager_load_stmt table in
      fun ~lazy_payloads proc_name ->
        if is_specs_table then BStats.incr_summary_file_try_load () ;
        let load_statement = if lazy_payloads then lazy_load_statement else eager_load_statement in
        let opt = load_spec ~lazy_payloads ~load_statement table proc_name in
        if is_specs_table && Option.is_some opt then BStats.incr_summary_read_from_disk () ;
        opt
    in
    let spec_of_procname = mk_load_spec Specs in
    let spec_of_model = mk_load_spec BiabductionModelsSpecs in
    (spec_of_procname, spec_of_model)


  (** Load procedure summary for the given procedure name and update spec table *)
  let load_summary_to_spec_table ~lazy_payloads proc_name =
    let summ_opt =
      match spec_of_procname ~lazy_payloads proc_name with
      | None when BiabductionModels.mem proc_name ->
          (* most of the time we don't run biabduction and it's the only analysis with non-NULL
             specs in these models, so load lazily *)
          spec_of_model ~lazy_payloads:true proc_name
      | summ_opt ->
          summ_opt
    in
    Option.iter ~f:(add proc_name) summ_opt ;
    summ_opt


  let get ~lazy_payloads proc_name =
    match Procname.Hash.find cache proc_name with
    | summary ->
        BStats.incr_summary_cache_hits () ;
        Some summary
    | exception Caml.Not_found ->
        BStats.incr_summary_cache_misses () ;
        load_summary_to_spec_table ~lazy_payloads proc_name


  (** Save summary for the procedure into the spec database *)
  let rec store ({proc_name; dependencies; payloads} as summary : t) =
    (* Make sure the summary in memory is identical to the saved one *)
    add proc_name summary ;
    summary.dependencies <- Dependencies.(Complete (freeze proc_name dependencies)) ;
    let report_summary = ReportSummary.of_full_summary summary in
    let summary_metadata = SummaryMetadata.of_full_summary summary in
    try
      DBWriter.store_spec ~proc_uid:(Procname.to_unique_id proc_name)
        ~proc_name:(Procname.SQLite.serialize proc_name)
        ~payloads:(Payloads.SQLite.serialize payloads)
        ~report_summary:(ReportSummary.SQLite.serialize report_summary)
        ~summary_metadata:(SummaryMetadata.SQLite.serialize summary_metadata) ;
      summary
    with SqliteUtils.Error msg when String.is_substring msg ~substring:"TOOBIG" ->
      (* Sqlite failed with [TOOBIG], reset to the empty summary and write it back *)
      L.internal_error
        "Summary for %a caused a TOOBIG Sqlite error, writing empty summary instead.@\n" Procname.pp
        proc_name ;
      let payloads = Payloads.empty in
      let report_summary = ReportSummary.empty () in
      let summary_metadata = SummaryMetadata.empty proc_name in
      let new_summary = mk_full_summary payloads report_summary summary_metadata in
      store new_summary


  let reset proc_name =
    let summary =
      { sessions= 0
      ; payloads= Payloads.empty
      ; stats= Stats.empty
      ; proc_name
      ; err_log= Errlog.empty ()
      ; dependencies= Dependencies.reset proc_name }
    in
    Procname.Hash.replace cache proc_name summary ;
    summary


  let delete pname =
    remove_from_cache pname ;
    DBWriter.delete_spec ~proc_uid:(Procname.to_unique_id pname)


  let delete_all ~procedures = List.iter ~f:delete procedures

  let iter_filtered_specs ~filter ~f =
    let db = Database.get_database AnalysisDatabase in
    let dummy_source_file = SourceFile.invalid __FILE__ in
    (* NB the order is deterministic, but it is over a serialised value, so it is arbitrary *)
    Sqlite3.prepare db
      {|
      SELECT proc_uid, proc_name, report_summary, summary_metadata
      FROM specs
      ORDER BY proc_uid ASC
      |}
    |> Container.iter ~fold:(SqliteUtils.result_fold_rows db ~log:"iter over filtered specs")
         ~f:(fun stmt ->
           let proc_name = Sqlite3.column stmt 1 |> Procname.SQLite.deserialize in
           if filter dummy_source_file proc_name then
             let proc_uid = Sqlite3.column_text stmt 0 in
             let report_summary = Sqlite3.column stmt 2 |> ReportSummary.SQLite.deserialize in
             let summary_metadata = Sqlite3.column stmt 3 |> SummaryMetadata.SQLite.deserialize in
             let payloads = Payloads.SQLite.lazy_load Specs ~proc_uid in
             let spec = mk_full_summary payloads report_summary summary_metadata in
             f spec )


  let iter_filtered_report_summaries ~filter ~f =
    let db = Database.get_database AnalysisDatabase in
    let dummy_source_file = SourceFile.invalid __FILE__ in
    (* NB the order is deterministic, but it is over a serialised value, so it is arbitrary *)
    Sqlite3.prepare db "SELECT proc_name, report_summary FROM specs ORDER BY proc_uid ASC"
    |> Container.iter ~fold:(SqliteUtils.result_fold_rows db ~log:"iter over filtered specs")
         ~f:(fun stmt ->
           let proc_name = Sqlite3.column stmt 0 |> Procname.SQLite.deserialize in
           if filter dummy_source_file proc_name then
             let ({loc; cost_opt; config_impact_opt; err_log} : ReportSummary.t) =
               Sqlite3.column stmt 1 |> ReportSummary.SQLite.deserialize
             in
             f proc_name loc cost_opt config_impact_opt err_log )


  let make_filtered_iterator_from_config ~iter ~f =
    let filter =
      if Option.is_some Config.procedures_filter then Lazy.force Filtering.procedures_filter
      else fun _ _ -> true
    in
    iter ~filter ~f


  let iter_report_summaries_from_config ~f =
    make_filtered_iterator_from_config ~iter:iter_filtered_report_summaries ~f


  let iter_specs ~f = iter_filtered_specs ~filter:(fun _ _ -> true) ~f

  let get_count () =
    let db = Database.get_database AnalysisDatabase in
    Sqlite3.prepare db "SELECT count(1) FROM specs"
    |> SqliteUtils.result_single_column_option db ~log:"count specs"
    |> function Some count -> Sqlite3.Data.to_int_exn count | _ -> 0
end
