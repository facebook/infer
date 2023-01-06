(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
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
  ; proc_desc: Procdesc.t
  ; err_log: Errlog.t
  ; mutable callee_pnames: Procname.Set.t
  ; mutable used_tenv_sources: SourceFile.Set.t }

let yojson_of_t {proc_desc; payloads} =
  [%yojson_of: Procname.t * Payloads.t] (Procdesc.get_proc_name proc_desc, payloads)


type full_summary = t

let get_proc_desc summary = summary.proc_desc

let get_attributes summary = Procdesc.get_attributes summary.proc_desc

let get_proc_name summary = (get_attributes summary).ProcAttributes.proc_name

let get_ret_type summary = (get_attributes summary).ProcAttributes.ret_type

let get_formals summary = (get_attributes summary).ProcAttributes.formals

let get_err_log summary = summary.err_log

let get_loc summary = (get_attributes summary).ProcAttributes.loc

let pp_errlog fmt err_log =
  F.fprintf fmt "ERRORS: @[<h>%a@]@\n%!" Errlog.pp_errors err_log ;
  F.fprintf fmt "WARNINGS: @[<h>%a@]" Errlog.pp_warnings err_log


let pp_signature fmt summary =
  let pp_formal fmt (p, typ, _) = F.fprintf fmt "%a %a" (Typ.pp_full Pp.text) typ Mangled.pp p in
  F.fprintf fmt "%a %a(%a)" (Typ.pp_full Pp.text) (get_ret_type summary) Procname.pp
    (get_proc_name summary) (Pp.seq ~sep:", " pp_formal) (get_formals summary)


let pp_no_stats_specs fmt summary = F.fprintf fmt "%a@\n" pp_signature summary

let pp_text fmt summary =
  pp_no_stats_specs fmt summary ;
  F.fprintf fmt "%a@\n%a%a" pp_errlog (get_err_log summary) Stats.pp summary.stats
    (Payloads.pp Pp.text) summary.payloads


let pp_html source fmt summary =
  let pp_escaped pp fmt x = F.fprintf fmt "%s" (Escape.escape_xml (F.asprintf "%a" pp x)) in
  F.pp_force_newline fmt () ;
  Pp.html_with_color Black (pp_escaped pp_no_stats_specs) fmt summary ;
  F.fprintf fmt "<br />%a<br />@\n" Stats.pp summary.stats ;
  Errlog.pp_html source [] fmt (get_err_log summary) ;
  Io_infer.Html.pp_hline fmt () ;
  F.fprintf fmt "<LISTING>@\n" ;
  pp_escaped (Payloads.pp (Pp.html Black)) fmt summary.payloads ;
  F.fprintf fmt "</LISTING>@\n"


module ReportSummary = struct
  type t =
    { loc: Location.t
    ; cost_opt: CostDomain.summary option
    ; config_impact_opt: ConfigImpactAnalysis.Summary.t option
    ; err_log: Errlog.t }

  let of_full_summary (f : full_summary) : t =
    { loc= get_loc f
    ; cost_opt= Lazy.force f.payloads.Payloads.cost
    ; config_impact_opt= Lazy.force f.payloads.Payloads.config_impact_analysis
    ; err_log= f.err_log }


  module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
    type nonrec t = t
  end)
end

module SummaryMetadata = struct
  type t =
    { sessions: int
    ; stats: Stats.t
    ; proc_desc: Procdesc.t
    ; callee_pnames: Procname.Set.t
    ; used_tenv_sources: SourceFile.Set.t }
  [@@deriving fields]

  let of_full_summary (f : full_summary) : t =
    { sessions= f.sessions
    ; stats= f.stats
    ; proc_desc= f.proc_desc
    ; callee_pnames= f.callee_pnames
    ; used_tenv_sources= f.used_tenv_sources }


  module SQLite = SqliteUtils.MarshalledDataNOTForComparison (struct
    type nonrec t = t
  end)
end

let mk_full_summary payloads (report_summary : ReportSummary.t)
    (summary_metadata : SummaryMetadata.t) : full_summary =
  { payloads
  ; sessions= summary_metadata.sessions
  ; stats= summary_metadata.stats
  ; proc_desc= summary_metadata.proc_desc
  ; callee_pnames= summary_metadata.callee_pnames
  ; used_tenv_sources= summary_metadata.used_tenv_sources
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
        "SELECT rowid, report_summary, summary_metadata FROM %s WHERE proc_uid = :k"
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
          Sqlite3.bind load_stmt 1 (Sqlite3.Data.TEXT (Procname.to_unique_id proc_name))
          |> SqliteUtils.check_result_code db ~log:"load proc specs bind proc_name" ;
          SqliteUtils.result_option ~finalize:false db ~log:"load proc specs run" load_stmt
            ~read_row:(fun stmt ->
              let offset = if lazy_payloads then (* column 0 holds [rowid] *) 1 else 0 in
              let report_summary =
                Sqlite3.column stmt (offset + 0) |> ReportSummary.SQLite.deserialize
              in
              let summary_metadata =
                Sqlite3.column stmt (offset + 1) |> SummaryMetadata.SQLite.deserialize
              in
              let payloads =
                if lazy_payloads then
                  let rowid = Sqlite3.column_int64 stmt 0 in
                  Payloads.SQLite.lazy_load table ~rowid
                else
                  (* NOTE: [offset] = 0 at this point *)
                  Payloads.SQLite.eager_load ~first_column:(offset + 2) stmt
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


  let get_model_proc_desc model_name =
    if not (BiabductionModels.mem model_name) then
      Logging.die InternalError "Requested summary of model that couldn't be found: %a@\n"
        Procname.pp model_name
    else
      (* we only care about the proc_desc so load analysis payloads lazily (i.e. not at all) *)
      Option.map (get ~lazy_payloads:true model_name) ~f:(fun (s : full_summary) -> s.proc_desc)


  (** Save summary for the procedure into the spec database *)
  let store (summary : t) =
    let proc_name = get_proc_name summary in
    (* Make sure the summary in memory is identical to the saved one *)
    add proc_name summary ;
    let report_summary = ReportSummary.of_full_summary summary in
    let summary_metadata = SummaryMetadata.of_full_summary summary in
    DBWriter.store_spec ~proc_uid:(Procname.to_unique_id proc_name)
      ~proc_name:(Procname.SQLite.serialize proc_name)
      ~payloads:(Payloads.SQLite.serialize summary.payloads)
      ~report_summary:(ReportSummary.SQLite.serialize report_summary)
      ~summary_metadata:(SummaryMetadata.SQLite.serialize summary_metadata)


  let reset proc_desc =
    let summary =
      { sessions= 0
      ; payloads= Payloads.empty
      ; stats= Stats.empty
      ; proc_desc
      ; err_log= Errlog.empty ()
      ; callee_pnames= Procname.Set.empty
      ; used_tenv_sources= SourceFile.Set.empty }
    in
    Procname.Hash.replace cache (Procdesc.get_proc_name proc_desc) summary ;
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
      SELECT rowid, proc_name, report_summary, summary_metadata
      FROM specs
      ORDER BY proc_uid ASC
      |}
    |> Container.iter ~fold:(SqliteUtils.result_fold_rows db ~log:"iter over filtered specs")
         ~f:(fun stmt ->
           let proc_name = Sqlite3.column stmt 1 |> Procname.SQLite.deserialize in
           if filter dummy_source_file proc_name then
             let rowid = Sqlite3.column_int64 stmt 0 in
             let report_summary = Sqlite3.column stmt 2 |> ReportSummary.SQLite.deserialize in
             let summary_metadata = Sqlite3.column stmt 3 |> SummaryMetadata.SQLite.deserialize in
             let payloads = Payloads.SQLite.lazy_load Specs ~rowid in
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
