(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

type t =
  { annot_map: AnnotationReachabilityDomain.t Lazy.t option
  ; biabduction: BiabductionSummary.t Lazy.t option
  ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t Lazy.t option
  ; buffer_overrun_checker: BufferOverrunCheckerSummary.t Lazy.t option
  ; config_impact_analysis: ConfigImpactAnalysis.Summary.t Lazy.t option
  ; cost: CostDomain.summary Lazy.t option
  ; disjunctive_demo: DisjunctiveDemo.domain Lazy.t option
  ; lab_resource_leaks: ResourceLeakDomain.summary Lazy.t option
  ; litho_required_props: LithoDomain.summary Lazy.t option
  ; pulse: PulseSummary.t Lazy.t option
  ; purity: PurityDomain.summary Lazy.t option
  ; racerd: RacerDDomain.summary Lazy.t option
  ; scope_leakage: ScopeLeakage.Summary.t Lazy.t option
  ; siof: SiofDomain.Summary.t Lazy.t option
  ; lineage: Lineage.Summary.t Lazy.t option
  ; lineage_shape: LineageShape.Summary.t Lazy.t option
  ; starvation: StarvationDomain.summary Lazy.t option }
[@@deriving fields]

let yojson_of_t {pulse} =
  [%yojson_of: (string * PulseSummary.t option) list]
    [(Checker.get_id Pulse, ILazy.force_option pulse)]


let () =
  if not (Int.equal (List.length Fields.names) (List.length PayloadId.database_fields)) then
    L.die InternalError "Payloads.t and PayloadId.t do not match."


type 'a pp = Pp.env -> F.formatter -> 'a -> unit

type field =
  | F : {field: (t, 'a Lazy.t option) Field.t; payload_id: PayloadId.t; pp: 'a pp} -> field

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
    ~racerd:(fun f -> mk f RacerD RacerDDomain.pp_summary)
    ~lab_resource_leaks:(fun f -> mk f LabResourceLeaks ResourceLeakDomain.pp)
    ~scope_leakage:(fun f -> mk f ScopeLeakage ScopeLeakage.Summary.pp)
    ~siof:(fun f -> mk f SIOF SiofDomain.Summary.pp)
    ~lineage:(fun f -> mk f Lineage Lineage.Summary.pp)
    ~lineage_shape:(fun f -> mk f LineageShape LineageShape.Summary.pp)
    ~starvation:(fun f -> mk f Starvation StarvationDomain.pp_summary)
  (* sorted to help serialization, see {!SQLite.serialize} below *)
  |> List.sort ~compare:(fun (F {payload_id= payload_id1}) (F {payload_id= payload_id2}) ->
         Int.compare
           (PayloadId.Variants.to_rank payload_id1)
           (PayloadId.Variants.to_rank payload_id2) )


let pp pe f payloads =
  List.iter all_fields ~f:(fun (F {field; payload_id; pp}) ->
      Field.get field payloads |> ILazy.force_option
      |> Option.iter ~f:(fun x ->
             F.fprintf f "%s: %a@\n" (PayloadId.Variants.to_name payload_id) (pp pe) x ) )


let empty =
  { annot_map= None
  ; biabduction= None
  ; buffer_overrun_analysis= None
  ; buffer_overrun_checker= None
  ; config_impact_analysis= None
  ; cost= None
  ; disjunctive_demo= None
  ; lab_resource_leaks= None
  ; litho_required_props= None
  ; pulse= None
  ; purity= None
  ; racerd= None
  ; scope_leakage= None
  ; siof= None
  ; lineage= None
  ; lineage_shape= None
  ; starvation= None }


module PayloadIdToField =
  PrettyPrintable.MakePPMonoMap
    (PayloadId)
    (struct
      type t = field

      let pp f (F {field}) = F.pp_print_string f (Field.name field)
    end)

module FieldnameToPayloadId = PrettyPrintable.MakePPMonoMap (String) (PayloadId)

let payload_id_to_field, fieldname_to_payload_id =
  List.fold all_fields ~init:(PayloadIdToField.empty, FieldnameToPayloadId.empty)
    ~f:(fun (payload_id_to_field, field_to_payload_id) (F {field= field_t; payload_id} as field) ->
      ( PayloadIdToField.add payload_id field payload_id_to_field
      , FieldnameToPayloadId.add (Field.name field_t) payload_id field_to_payload_id ) )


let has_payload payload_id payloads =
  let (F {field}) = PayloadIdToField.find payload_id payload_id_to_field in
  Option.is_some (Field.get field payloads)


let analysis_request_of_field field =
  AnalysisRequest.one (FieldnameToPayloadId.find (Field.name field) fieldname_to_payload_id)


module SQLite = struct
  (** Each payload is stored in the DB as either [NULL] for the absence of payload, or the payload
      itself. We cannot give a good type to this function because it deserializes several payload
      types. *)
  let deserialize_payload_opt = function[@warning "-partial-match"]
    | Sqlite3.Data.NULL ->
        None
    | Sqlite3.Data.BLOB blob ->
        (* lazily deserialize the blob once we have it to save time in case it won't be used. This
           can happen when payloads were loaded eagerly by one analysis when other active analyses
           are not interested in the summaries for the same procedure, i.e. they don't have the same
           dependencies *)
        Some (lazy (Marshal.from_string blob 0))


  (** serialize a payload into the format described in {!deserialize_payload_opt} above *)
  let serialize_payload_opt payload_opt =
    match ILazy.force_option payload_opt with
    | None ->
        Sqlite3.Data.NULL
    | Some payload ->
        Sqlite3.Data.BLOB (Marshal.to_string payload [Closures])


  let serialize payloads =
    (* use [all_fields] to serialize in rank order so the column names (declared in {!Database})
       match the payloads *)
    List.map all_fields ~f:(fun (F {field}) -> Field.get field payloads |> serialize_payload_opt)


  let serialize ({pulse} as payloads) =
    let default = serialize payloads in
    fun ~old_pulse_payload ->
      (* All payloads must be null or blob. *)
      match[@warning "-partial-match"] (old_pulse_payload : Sqlite3.Data.t option) with
      | None | Some NULL ->
          (* No row or no pulse payload is in the DB. *)
          default
      | Some (BLOB blob) -> (
          let old_pulse_payload : PulseSummary.t = Marshal.from_string blob 0 in
          match ILazy.force_option pulse with
          | None ->
              serialize {payloads with pulse= Some (lazy old_pulse_payload)}
          | Some pulse_payload ->
              let res = PulseSummary.merge pulse_payload old_pulse_payload in
              if phys_equal res pulse_payload then default
              else serialize {payloads with pulse= Some (lazy res)} )


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
      ~pulse:data_of_sqlite_column ~purity:data_of_sqlite_column ~racerd:data_of_sqlite_column
      ~lab_resource_leaks:data_of_sqlite_column ~scope_leakage:data_of_sqlite_column
      ~siof:data_of_sqlite_column ~lineage:data_of_sqlite_column
      ~lineage_shape:data_of_sqlite_column ~starvation:data_of_sqlite_column


  let eager_load stmt ~first_column = (make_eager first_column |> fst) stmt

  (** {3 code for lazily loading payloads} *)

  (** SQLite statements to load a payload from either analysis table *)
  type load_statements =
    {specs: Database.registered_stmt; model_specs: Database.registered_stmt; name: string}

  (** all possible load statements for each payload type and analysis table, in a rank-indexed array *)
  let all_load_statements =
    let mk_load_statements payload_id =
      let for_table table =
        Database.register_statement AnalysisDatabase "SELECT %s FROM %s WHERE proc_uid = :k"
          payload_id
          (Database.string_of_analysis_table table)
      in
      {specs= for_table Specs; model_specs= for_table BiabductionModelsSpecs; name= payload_id}
    in
    List.map PayloadId.database_fields ~f:mk_load_statements |> Array.of_list


  let get_load_statement (table : Database.analysis_table) payload_id =
    let {specs; model_specs} = all_load_statements.(PayloadId.Variants.to_rank payload_id) in
    match table with Specs -> specs | BiabductionModelsSpecs -> model_specs


  let load table ~proc_uid payload_id =
    let load_statement = get_load_statement table payload_id in
    Database.with_registered_statement load_statement ~f:(fun db load_stmt ->
        Sqlite3.bind_text load_stmt 1 proc_uid
        |> SqliteUtils.check_result_code db ~log:"load payloads bind proc_uid" ;
        SqliteUtils.result_option ~finalize:false db ~log:"load payloads exec" load_stmt
          ~read_row:(fun stmt -> Sqlite3.column stmt 0 |> deserialize_payload_opt ) )
    |> Option.join


  let lazy_load table ~proc_uid =
    { annot_map= load table ~proc_uid AnnotMap
    ; biabduction= load table ~proc_uid Biabduction
    ; buffer_overrun_analysis= load table ~proc_uid BufferOverrunAnalysis
    ; buffer_overrun_checker= load table ~proc_uid BufferOverrunChecker
    ; config_impact_analysis= load table ~proc_uid ConfigImpactAnalysis
    ; cost= load table ~proc_uid Cost
    ; disjunctive_demo= load table ~proc_uid DisjunctiveDemo
    ; lab_resource_leaks= load table ~proc_uid LabResourceLeaks
    ; litho_required_props= load table ~proc_uid LithoRequiredProps
    ; pulse= load table ~proc_uid Pulse
    ; purity= load table ~proc_uid Purity
    ; racerd= load table ~proc_uid RacerD
    ; scope_leakage= load table ~proc_uid ScopeLeakage
    ; siof= load table ~proc_uid SIOF
    ; lineage= load table ~proc_uid Lineage
    ; lineage_shape= load table ~proc_uid LineageShape
    ; starvation= load table ~proc_uid Starvation }
end
