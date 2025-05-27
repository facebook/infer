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
  type t = {mutable nodes_visited: IInt.Set.t  (** Nodes visited *)}

  let empty = {nodes_visited= IInt.Set.empty}

  let is_visited stats node_id = IInt.Set.mem node_id stats.nodes_visited

  let add_visited stats node_id = stats.nodes_visited <- IInt.Set.add node_id stats.nodes_visited
end

type t =
  { payloads: Payloads.t
  ; mutable sessions: int
  ; stats: Stats.t
  ; proc_name: Procname.t
  ; err_log: Errlog.t
  ; mutable dependencies: Dependencies.t
  ; mutable is_complete_result: bool }

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

let pp_text fmt ({proc_name; err_log; payloads; dependencies} as summary) =
  pp_no_stats_specs fmt summary ;
  F.fprintf fmt "%a@\n%a%a" pp_errlog err_log (Payloads.pp Pp.text proc_name) payloads
    Dependencies.pp dependencies


let pp_html source fmt ({proc_name; err_log; payloads} as summary) =
  let pp_escaped pp fmt x = F.fprintf fmt "%s" (Escape.escape_xml (F.asprintf "%a" pp x)) in
  F.pp_force_newline fmt () ;
  Pp.html_with_color Black (pp_escaped pp_no_stats_specs) fmt summary ;
  Errlog.pp_html source [] fmt err_log ;
  Io_infer.Html.pp_hline fmt () ;
  F.fprintf fmt "<button type='button' onclick='toggleDetailsBlock()'>Toggle details</button>" ;
  F.fprintf fmt "<div class='state code'>@\n" ;
  Payloads.pp (Pp.html Black) proc_name fmt payloads ;
  F.fprintf fmt "</div>@\n" ;
  ()


module ReportSummary = struct
  type t = {loc: Location.t; err_log: Errlog.t}

  let of_err_log proc_name err_log = {loc= (Attributes.load_exn proc_name).loc; err_log}

  let of_full_summary {proc_name; err_log} = of_err_log proc_name err_log

  let empty () = {loc= Location.dummy; err_log= Errlog.empty ()}

  let merge ~into x = Errlog.merge ~into:into.err_log x.err_log

  module SQLite = struct
    include SqliteUtils.MarshalledDataNOTForComparison (struct
      type nonrec t = t
    end)

    let serialize report_summary =
      let default = serialize report_summary in
      fun ~old_report_summary ->
        Option.value_map old_report_summary ~default ~f:(fun old_report_summary ->
            match merge ~into:report_summary (deserialize old_report_summary) with
            | `Modified ->
                serialize report_summary
            | `Intact ->
                default )
  end
end

module SummaryMetadata = struct
  type t =
    { sessions: int
    ; stats: Stats.t
    ; proc_name: Procname.t
    ; dependencies: Dependencies.complete
    ; is_complete_result: bool }
  [@@deriving fields]

  let of_full_summary ~is_complete_result (f : full_summary) : t =
    { sessions= f.sessions
    ; stats= f.stats
    ; proc_name= f.proc_name
    ; dependencies= Dependencies.complete_exn f.dependencies
    ; is_complete_result }


  let empty proc_name =
    { sessions= 0
    ; stats= Stats.empty
    ; proc_name
    ; dependencies= Dependencies.reset proc_name |> Dependencies.freeze proc_name
    ; is_complete_result= false }


  let merge x y =
    let dependencies = Dependencies.merge x.dependencies y.dependencies in
    let is_complete_result = x.is_complete_result || y.is_complete_result in
    if phys_equal dependencies x.dependencies && Bool.equal is_complete_result x.is_complete_result
    then x
    else if
      phys_equal dependencies y.dependencies && Bool.equal is_complete_result y.is_complete_result
    then y
    else {x with dependencies; is_complete_result}


  module SQLite = struct
    include SqliteUtils.MarshalledDataNOTForComparison (struct
      type nonrec t = t
    end)

    let serialize summary_metadata =
      let default = serialize summary_metadata in
      fun ~old_summary_metadata ->
        Option.value_map old_summary_metadata ~default ~f:(fun old_summary_metadata ->
            let res = merge summary_metadata (deserialize old_summary_metadata) in
            if phys_equal res summary_metadata then default else serialize res )
  end
end

let mk_full_summary payloads (report_summary : ReportSummary.t)
    (summary_metadata : SummaryMetadata.t) : full_summary =
  { payloads
  ; sessions= summary_metadata.sessions
  ; stats= summary_metadata.stats
  ; proc_name= summary_metadata.proc_name
  ; dependencies= Dependencies.Complete summary_metadata.dependencies
  ; err_log= report_summary.err_log
  ; is_complete_result= summary_metadata.is_complete_result }


module OnDisk = struct
  (* a two-layer cache: procname -> analysis request -> summary *)
  module Cache = Procname.Cache

  let clear_cache, remove_from_cache, add_to_cache, find_in_cache, set_lru_limit =
    let cache = Cache.create ~name:"summaries" in
    let clear_cache () = Cache.clear cache in
    let remove_from_cache pname = Cache.remove cache pname in
    let add proc_name analysis_req summary =
      Cache.update cache proc_name ~f:(fun map_opt ->
          Option.value map_opt ~default:AnalysisRequest.Map.empty
          |> AnalysisRequest.Map.add analysis_req summary
          |> Option.some )
    in
    let find_opt (proc_name, analysis_req) =
      Cache.lookup cache proc_name |> Option.bind ~f:(AnalysisRequest.Map.find_opt analysis_req)
    in
    let set_lru_limit ~lru_limit = Cache.set_lru_mode ~lru_limit cache in
    (clear_cache, remove_from_cache, add, find_opt, set_lru_limit)


  let spec_of_procname =
    let lazy_load_statement =
      Database.register_statement AnalysisDatabase
        "SELECT report_summary, summary_metadata FROM specs WHERE proc_uid = :k"
    in
    let eager_load_statement =
      Database.register_statement AnalysisDatabase
        "SELECT report_summary, summary_metadata, %s FROM specs WHERE proc_uid = :k"
        (F.asprintf "%a" (Pp.seq ~sep:", " F.pp_print_string) PayloadId.database_fields)
    in
    let load_spec ~lazy_payloads ~load_statement proc_name =
      Database.with_registered_statement load_statement ~f:(fun db load_stmt ->
          let proc_uid = Procname.to_unique_id proc_name in
          Sqlite3.bind load_stmt 1 (Sqlite3.Data.TEXT proc_uid)
          |> SqliteUtils.check_result_code db ~log:"load proc specs bind proc_name" ;
          SqliteUtils.result_option ~finalize:false db ~log:"load proc specs run" load_stmt
            ~read_row:(fun stmt ->
              let report_summary = Sqlite3.column stmt 0 |> ReportSummary.SQLite.deserialize in
              let summary_metadata = Sqlite3.column stmt 1 |> SummaryMetadata.SQLite.deserialize in
              let payloads =
                if lazy_payloads then Payloads.SQLite.lazy_load ~proc_uid
                else Payloads.SQLite.eager_load ~first_column:2 stmt
              in
              mk_full_summary payloads report_summary summary_metadata ) )
    in
    fun ~lazy_payloads proc_name ->
      BStats.incr_summary_file_try_load () ;
      let load_statement = if lazy_payloads then lazy_load_statement else eager_load_statement in
      let opt = load_spec ~lazy_payloads ~load_statement proc_name in
      if Option.is_some opt then BStats.incr_summary_read_from_disk () ;
      opt


  (** Load procedure summary for the given procedure name and update spec table *)
  let load_summary_to_spec_table ~lazy_payloads analysis_req proc_name =
    let summ_opt = spec_of_procname ~lazy_payloads proc_name in
    Option.iter ~f:(add_to_cache proc_name analysis_req) summ_opt ;
    summ_opt


  let get ~lazy_payloads analysis_req proc_name =
    let not_found_from_cache () =
      load_summary_to_spec_table ~lazy_payloads analysis_req proc_name
    in
    match find_in_cache (proc_name, analysis_req) with
    | Some _ as some_summary ->
        some_summary
    | None -> (
      match (analysis_req : AnalysisRequest.t) with
      | All ->
          (* We already tried to find the cache for [All]. *)
          not_found_from_cache ()
      | One _ | CheckerWithoutPayload _ -> (
        (* If there is a cache for [All], we use it. *)
        match find_in_cache (proc_name, AnalysisRequest.all) with
        | Some _ as some_summary ->
            some_summary
        | None ->
            not_found_from_cache () ) )


  (** Save summary for the procedure into the spec database *)
  let rec store (analysis_req : AnalysisRequest.t)
      ({proc_name; dependencies; payloads} as summary : t) =
    summary.dependencies <- Dependencies.(Complete (freeze proc_name dependencies)) ;
    let report_summary = ReportSummary.of_full_summary summary in
    let summary_metadata =
      let is_complete_result =
        match analysis_req with All -> true | One _ | CheckerWithoutPayload _ -> false
      in
      SummaryMetadata.of_full_summary ~is_complete_result summary
    in
    try
      DBWriter.store_spec analysis_req ~proc_uid:(Procname.to_unique_id proc_name)
        ~proc_name:(Procname.SQLite.serialize proc_name)
        ~merge_pulse_payload:(Payloads.SQLite.serialize payloads)
        ~merge_report_summary:(ReportSummary.SQLite.serialize report_summary)
        ~merge_summary_metadata:(SummaryMetadata.SQLite.serialize summary_metadata) ;
      (* Make sure the summary in memory is identical to the saved one *)
      add_to_cache proc_name analysis_req summary ;
      summary
    with SqliteUtils.DataTooBig ->
      (* Serialization exceeded size limits, write and return an empty summary  *)
      L.internal_error
        "Summary for %a caused exceeds blob size limit, writing empty summary instead.@\n"
        Procname.pp proc_name ;
      let payloads = Payloads.empty in
      let report_summary = ReportSummary.empty () in
      let summary_metadata = SummaryMetadata.empty proc_name in
      let new_summary = mk_full_summary payloads report_summary summary_metadata in
      store analysis_req new_summary


  let empty proc_name =
    { sessions= 0
    ; payloads= Payloads.empty
    ; stats= Stats.empty
    ; proc_name
    ; err_log= Errlog.empty ()
    ; dependencies= Dependencies.reset proc_name
    ; is_complete_result= false }


  let delete_all ~procedures =
    List.iter ~f:remove_from_cache procedures ;
    DBWriter.delete_specs ~proc_uids:(List.map procedures ~f:Procname.to_unique_id)


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
             let payloads = Payloads.SQLite.lazy_load ~proc_uid in
             let spec = mk_full_summary payloads report_summary summary_metadata in
             f spec )


  let iter_filtered_report_summaries ~filter ~f =
    let db = Database.get_database AnalysisDatabase in
    let dummy_source_file = SourceFile.invalid __FILE__ in
    (* NB the order is deterministic, but it is over a serialised value, so it is arbitrary *)
    Printf.sprintf "SELECT proc_name, report_summary, %s, %s FROM specs ORDER BY proc_uid ASC"
      PayloadId.Variants.cost.Variant.name PayloadId.Variants.configimpactanalysis.Variant.name
    |> Sqlite3.prepare db
    |> Container.iter ~fold:(SqliteUtils.result_fold_rows db ~log:"iter over filtered specs")
         ~f:(fun stmt ->
           let proc_name = Sqlite3.column stmt 0 |> Procname.SQLite.deserialize in
           if filter dummy_source_file proc_name then
             let {ReportSummary.loc; err_log} =
               Sqlite3.column stmt 1 |> ReportSummary.SQLite.deserialize
             in
             let cost_opt : CostDomain.summary option =
               Sqlite3.column stmt 2 |> Payloads.SQLite.deserialize_payload_opt
               |> SafeLazy.force_option
             in
             let config_impact_opt : ConfigImpactAnalysis.Summary.t option =
               Sqlite3.column stmt 3 |> Payloads.SQLite.deserialize_payload_opt
               |> SafeLazy.force_option
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


  let add_errlog proc_name err_log =
    (* Make sure the summary in memory gets the updated err_log *)
    find_in_cache (proc_name, AnalysisRequest.all)
    |> Option.iter ~f:(fun summary ->
           (* side-effects galore! no need to do anything except run [Errlog.merge] to update all
              existing copies of this summary's error log *)
           Errlog.merge ~into:summary.err_log err_log |> ignore ) ;
    let report_summary = ReportSummary.of_err_log proc_name err_log in
    DBWriter.update_report_summary ~proc_uid:(Procname.to_unique_id proc_name)
      ~merge_report_summary:(ReportSummary.SQLite.serialize report_summary)
      ()
end
