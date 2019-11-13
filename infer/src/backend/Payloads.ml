(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  { annot_map: AnnotationReachabilityDomain.t option
  ; biabduction: BiabductionSummary.t option
  ; buffer_overrun_analysis: BufferOverrunAnalysisSummary.t option
  ; buffer_overrun_checker: BufferOverrunCheckerSummary.t option
  ; class_loads: ClassLoadsDomain.summary option
  ; cost: CostDomain.summary option
  ; impurity: ImpurityDomain.t option
  ; lab_resource_leaks: ResourceLeakDomain.summary option
  ; litho_graphql_field_access: LithoDomain.t option
  ; litho_required_props: LithoDomain.t option
  ; pulse: PulseSummary.t option
  ; purity: PurityDomain.summary option
  ; quandary: QuandarySummary.t option
  ; racerd: RacerDDomain.summary option
  ; siof: SiofDomain.Summary.t option
  ; starvation: StarvationDomain.summary option
  ; typestate: TypeState.t option
  ; uninit: UninitDomain.Summary.t option }
[@@deriving fields]

let poly_fields = PolyFields.make Fields.map_poly

type 'a pp = Pp.env -> F.formatter -> 'a -> unit

type field = F : {field: (t, 'a option) Field.t; name: string; pp: 'a pp} -> field

let fields =
  let mk field name pp = F {field; name; pp= (fun _ -> pp)} in
  let mk_pe field name pp = F {field; name; pp} in
  Fields.to_list
    ~annot_map:(fun f -> mk f "AnnotationReachability" AnnotationReachabilityDomain.pp)
    ~biabduction:(fun f -> mk_pe f "Biabduction" BiabductionSummary.pp)
    ~buffer_overrun_analysis:(fun f -> mk f "BufferOverrunAnalysis" BufferOverrunAnalysisSummary.pp)
    ~buffer_overrun_checker:(fun f -> mk f "BufferOverrunChecker" BufferOverrunCheckerSummary.pp)
    ~class_loads:(fun f -> mk f "ClassLoads" ClassLoadsDomain.pp_summary)
    ~cost:(fun f -> mk f "Cost" CostDomain.pp_summary)
    ~impurity:(fun f -> mk f "Impurity" ImpurityDomain.pp)
    ~litho_graphql_field_access:(fun f -> mk f "Litho GraphQL Field Access" LithoDomain.pp)
    ~litho_required_props:(fun f -> mk f "Litho Required Props" LithoDomain.pp)
    ~pulse:(fun f -> mk f "Pulse" PulseSummary.pp)
    ~purity:(fun f -> mk f "Purity" PurityDomain.pp_summary)
    ~quandary:(fun f -> mk f "Quandary" QuandarySummary.pp)
    ~racerd:(fun f -> mk f "RacerD" RacerDDomain.pp_summary)
    ~lab_resource_leaks:(fun f -> mk f "Resource Leaks Lab" ResourceLeakDomain.pp)
    ~siof:(fun f -> mk f "Siof" SiofDomain.Summary.pp)
    ~starvation:(fun f -> mk f "Starvation" StarvationDomain.pp_summary)
    ~typestate:(fun f -> mk f "TypeState" TypeState.pp)
    ~uninit:(fun f -> mk f "Uninitialised" UninitDomain.Summary.pp)


let pp pe f payloads =
  List.iter fields ~f:(fun (F {field; name; pp}) ->
      Field.get field payloads |> Option.iter ~f:(fun x -> F.fprintf f "%s: %a@\n" name (pp pe) x)
  )


let empty =
  { annot_map= None
  ; biabduction= None
  ; buffer_overrun_analysis= None
  ; buffer_overrun_checker= None
  ; class_loads= None
  ; cost= None
  ; impurity= None
  ; lab_resource_leaks= None
  ; litho_graphql_field_access= None
  ; litho_required_props= None
  ; pulse= None
  ; purity= None
  ; quandary= None
  ; racerd= None
  ; siof= None
  ; starvation= None
  ; typestate= None
  ; uninit= None }
