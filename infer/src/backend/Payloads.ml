(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  { annot_map: AnnotationReachabilityDomain.t option Lazy.t
  ; biabduction: BiabductionSummary.t option Lazy.t
  ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t option Lazy.t
  ; buffer_overrun_checker: BufferOverrunCheckerSummary.t option Lazy.t
  ; config_impact_analysis: ConfigImpactAnalysis.Summary.t option Lazy.t
  ; cost: CostDomain.summary option Lazy.t
  ; disjunctive_demo: DisjunctiveDemo.domain option Lazy.t
  ; dotnet_resource_leaks: ResourceLeakCSDomain.summary option Lazy.t
  ; lab_resource_leaks: ResourceLeakDomain.summary option Lazy.t
  ; litho_required_props: LithoDomain.summary option Lazy.t
  ; pulse: PulseSummary.t option Lazy.t
  ; purity: PurityDomain.summary option Lazy.t
  ; quandary: QuandarySummary.t option Lazy.t
  ; racerd: RacerDDomain.summary option Lazy.t
  ; scope_leakage: ScopeLeakage.Summary.t option Lazy.t
  ; siof: SiofDomain.Summary.t option Lazy.t
  ; simple_lineage: SimpleLineage.Summary.t option Lazy.t
  ; simple_shape: SimpleShape.Summary.t option Lazy.t
  ; starvation: StarvationDomain.summary option Lazy.t
  ; nullsafe: NullsafeSummary.t option Lazy.t
  ; uninit: UninitDomain.Summary.t option Lazy.t }
[@@deriving fields]

let yojson_of_t {pulse} =
  [%yojson_of: (string * PulseSummary.t option) list] [(Checker.get_id Pulse, Lazy.force pulse)]


type 'a pp = Pp.env -> F.formatter -> 'a -> unit

type field =
  | F : {field: (t, 'a option Lazy.t) Field.t; payload_id: PayloadId.t; pp: 'a pp} -> field

let all_fields =
  let mk_pe field payload_id pp = F {field; payload_id; pp} in
  let mk field payload_id pp = mk_pe field payload_id (fun _ -> pp) in
  Fields.to_list
    ~annot_map:(fun f -> mk f AnnotMap AnnotationReachabilityDomain.pp)
    ~biabduction:(fun f -> mk_pe f Biabduction BiabductionSummary.pp)
    ~buffer_overrun_analysis:(fun f -> mk f BufferOverrunAnalysis BufferOverrunAnalysisSummary.pp)
    ~buffer_overrun_checker:(fun f -> mk f BufferOverrunChecker BufferOverrunCheckerSummary.pp)
    ~config_impact_analysis:(fun f -> mk f ConfigImpactAnalysis ConfigImpactAnalysis.Summary.pp)
    ~cost:(fun f -> mk f Cost CostDomain.pp_summary)
    ~disjunctive_demo:(fun f -> mk f DisjunctiveDemo DisjunctiveDemo.pp_domain)
    ~litho_required_props:(fun f -> mk f LithoRequiredProps LithoDomain.pp_summary)
    ~pulse:(fun f -> mk f Pulse PulseSummary.pp)
    ~purity:(fun f -> mk f Purity PurityDomain.pp_summary)
    ~quandary:(fun f -> mk f Quandary QuandarySummary.pp)
    ~racerd:(fun f -> mk f RacerD RacerDDomain.pp_summary)
    ~lab_resource_leaks:(fun f -> mk f LabResourceLeaks ResourceLeakDomain.pp)
    ~dotnet_resource_leaks:(fun f -> mk f DotnetResourceLeaks ResourceLeakCSDomain.Summary.pp)
    ~scope_leakage:(fun f -> mk f ScopeLeakage ScopeLeakage.Summary.pp)
    ~siof:(fun f -> mk f SIOF SiofDomain.Summary.pp)
    ~simple_lineage:(fun f -> mk f SimpleLineage SimpleLineage.Summary.pp)
    ~simple_shape:(fun f -> mk f SimpleShape SimpleShape.Summary.pp)
    ~starvation:(fun f -> mk f Starvation StarvationDomain.pp_summary)
    ~nullsafe:(fun f -> mk f Nullsafe NullsafeSummary.pp)
    ~uninit:(fun f -> mk f Uninit UninitDomain.Summary.pp)
  (* sorted to help serialization, see {!SQLite.serialize} below *)
  |> List.sort ~compare:(fun (F {payload_id= payload_id1}) (F {payload_id= payload_id2}) ->
         Int.compare
           (PayloadId.Variants.to_rank payload_id1)
           (PayloadId.Variants.to_rank payload_id2) )


let pp pe f payloads =
  List.iter all_fields ~f:(fun (F {field; payload_id; pp}) ->
      Field.get field payloads |> Lazy.force
      |> Option.iter ~f:(fun x ->
             F.fprintf f "%s: %a@\n" (PayloadId.Variants.to_name payload_id) (pp pe) x ) )


let empty =
  let no_payload = Lazy.from_val None in
  { annot_map= no_payload
  ; biabduction= no_payload
  ; buffer_overrun_analysis= no_payload
  ; buffer_overrun_checker= no_payload
  ; config_impact_analysis= no_payload
  ; cost= no_payload
  ; disjunctive_demo= no_payload
  ; dotnet_resource_leaks= no_payload
  ; lab_resource_leaks= no_payload
  ; litho_required_props= no_payload
  ; pulse= no_payload
  ; purity= no_payload
  ; quandary= no_payload
  ; racerd= no_payload
  ; scope_leakage= no_payload
  ; siof= no_payload
  ; simple_lineage= no_payload
  ; simple_shape= no_payload
  ; starvation= no_payload
  ; nullsafe= no_payload
  ; uninit= no_payload }


module SQLite = struct
  (** Each payload is stored in the DB as either [NULL] for the absence of payload, or the payload
      itself. We cannot give a good type to this function because it deserializes several payload
      types. *)
  let deserialize_payload_opt = function[@warning "-partial-match"]
    | Sqlite3.Data.NULL ->
        Lazy.from_val None
    | Sqlite3.Data.BLOB blob ->
        (* lazily deserialize the blob once we have it to save time in case it won't be used. This
           can happen when payloads were loaded eagerly by one analysis when other active analyses
           are not interested in the summaries for the same procedure, i.e. they don't have the same
           dependencies *)
        lazy (Some (Marshal.from_string blob 0))


  (** serialize a payload into the format described in {!deserialize_payload_opt} above *)
  let serialize_payload_opt payload_opt =
    match Lazy.force payload_opt with
    | None ->
        Sqlite3.Data.NULL
    | Some payload ->
        Sqlite3.Data.BLOB (Marshal.to_string payload [])


  let serialize payloads =
    (* use [all_fields] to serialize in rank order so the column names (declared in {!Database})
       match the payloads *)
    List.map all_fields ~f:(fun (F {field}) -> Field.get field payloads |> serialize_payload_opt)


  let make_eager =
    let data_of_sqlite_column _field column =
      ( (fun stmt ->
          let sqlite_data = Sqlite3.column stmt column in
          deserialize_payload_opt sqlite_data )
      , column + 1 )
    in
    Fields.make_creator ~annot_map:data_of_sqlite_column ~biabduction:data_of_sqlite_column
      ~buffer_overrun_analysis:data_of_sqlite_column ~buffer_overrun_checker:data_of_sqlite_column
      ~config_impact_analysis:data_of_sqlite_column ~cost:data_of_sqlite_column
      ~disjunctive_demo:data_of_sqlite_column ~litho_required_props:data_of_sqlite_column
      ~pulse:data_of_sqlite_column ~purity:data_of_sqlite_column ~quandary:data_of_sqlite_column
      ~racerd:data_of_sqlite_column ~lab_resource_leaks:data_of_sqlite_column
      ~dotnet_resource_leaks:data_of_sqlite_column ~scope_leakage:data_of_sqlite_column
      ~siof:data_of_sqlite_column ~simple_lineage:data_of_sqlite_column
      ~simple_shape:data_of_sqlite_column ~starvation:data_of_sqlite_column
      ~nullsafe:data_of_sqlite_column ~uninit:data_of_sqlite_column


  let eager_load stmt ~first_column = (make_eager first_column |> fst) stmt

  (** {3 code for lazily loading payloads} *)

  (** SQLite statements to load a payload from either analysis table *)
  type load_statements =
    {specs: Database.registered_stmt; model_specs: Database.registered_stmt; name: string}

  (** all possible load statements for each payload type and analysis table, in a rank-indexed array *)
  let all_load_statements =
    let mk_load_statements payload_id =
      let for_table table =
        Database.register_statement AnalysisDatabase "SELECT %s FROM %s WHERE rowid = :k" payload_id
          (Database.string_of_analysis_table table)
      in
      {specs= for_table Specs; model_specs= for_table BiabductionModelsSpecs; name= payload_id}
    in
    List.map PayloadId.database_fields ~f:mk_load_statements |> Array.of_list


  let get_load_statement (table : Database.analysis_table) payload_id =
    let {specs; model_specs} = all_load_statements.(PayloadId.Variants.to_rank payload_id) in
    match table with Specs -> specs | BiabductionModelsSpecs -> model_specs


  let load table ~rowid payload_id =
    let load_statement = get_load_statement table payload_id in
    Database.with_registered_statement load_statement ~f:(fun db load_stmt ->
        Sqlite3.bind_int64 load_stmt 1 rowid
        |> SqliteUtils.check_result_code db ~log:"load payloads bind rowid" ;
        SqliteUtils.result_option ~finalize:false db ~log:"load payloads exec" load_stmt
          ~read_row:(fun stmt -> Sqlite3.column stmt 0 |> deserialize_payload_opt |> Lazy.force) )
    |> Option.join


  let lazy_load table ~rowid =
    { annot_map= lazy (load table ~rowid AnnotMap)
    ; biabduction= lazy (load table ~rowid Biabduction)
    ; buffer_overrun_analysis= lazy (load table ~rowid BufferOverrunAnalysis)
    ; buffer_overrun_checker= lazy (load table ~rowid BufferOverrunChecker)
    ; config_impact_analysis= lazy (load table ~rowid ConfigImpactAnalysis)
    ; cost= lazy (load table ~rowid Cost)
    ; disjunctive_demo= lazy (load table ~rowid DisjunctiveDemo)
    ; dotnet_resource_leaks= lazy (load table ~rowid DotnetResourceLeaks)
    ; lab_resource_leaks= lazy (load table ~rowid LabResourceLeaks)
    ; litho_required_props= lazy (load table ~rowid LithoRequiredProps)
    ; pulse= lazy (load table ~rowid Pulse)
    ; purity= lazy (load table ~rowid Purity)
    ; quandary= lazy (load table ~rowid Quandary)
    ; racerd= lazy (load table ~rowid RacerD)
    ; scope_leakage= lazy (load table ~rowid ScopeLeakage)
    ; siof= lazy (load table ~rowid SIOF)
    ; simple_lineage= lazy (load table ~rowid SimpleLineage)
    ; simple_shape= lazy (load table ~rowid SimpleShape)
    ; starvation= lazy (load table ~rowid Starvation)
    ; nullsafe= lazy (load table ~rowid Nullsafe)
    ; uninit= lazy (load table ~rowid Uninit) }
end
