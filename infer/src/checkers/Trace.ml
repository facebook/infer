(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module type Spec = sig
  module Source : Source.S

  module Sink : Sink.S

  module Sanitizer : Sanitizer.S

  val get_report : Source.t -> Sink.t -> Sanitizer.t list -> IssueType.t option
end

module type S = sig
  include Spec

  include AbstractDomain.WithBottom

  module Sources : sig
    module Known : module type of AbstractDomain.FiniteSet (Source)

    module FootprintConfig : AccessTree.Config

    module Footprint : module type of AccessTree.PathSet (FootprintConfig)

    module Sanitizers : module type of AbstractDomain.FiniteSet (Sanitizer)

    type t = {known: Known.t; footprint: Footprint.t; sanitizers: Sanitizers.t}

    val empty : t

    val is_empty : t -> bool

    val of_source : Source.t -> t

    val of_footprint : AccessPath.Abs.t -> t

    val add : Source.t -> t -> t

    val get_footprint_indexes : t -> IntSet.t
  end

  module Sinks = Sink.Set
  module Passthroughs = Passthrough.Set

  type path = Passthroughs.t * (Source.t * Passthroughs.t) list * (Sink.t * Passthroughs.t) list

  type report =
    { issue: IssueType.t
    ; path_source: Source.t
    ; path_sink: Sink.t
    ; path_passthroughs: Passthroughs.t }

  val empty : t

  val sources : t -> Sources.t
  (** get the sources of the trace. *)

  val sinks : t -> Sinks.t
  (** get the sinks of the trace *)

  val passthroughs : t -> Passthroughs.t
  (** get the passthroughs of the trace *)

  val get_reports : ?cur_site:CallSite.t -> t -> report list
  (** get the reportable source-sink flows in this trace. specifying [cur_site] restricts the
      reported paths to ones introduced by the call at [cur_site]  *)

  val get_reportable_paths :
    ?cur_site:CallSite.t -> t -> trace_of_pname:(Typ.Procname.t -> t) -> path list
  (** get a path for each of the reportable source -> sink flows in this trace. specifying
      [cur_site] restricts the reported paths to ones introduced by the call at [cur_site] *)

  val to_loc_trace :
       ?desc_of_source:(Source.t -> string)
    -> ?source_should_nest:(Source.t -> bool)
    -> ?desc_of_sink:(Sink.t -> string)
    -> ?sink_should_nest:(Sink.t -> bool)
    -> path
    -> Errlog.loc_trace
  (** create a loc_trace from a path; [source_should_nest s] should be true when we are going one
      deeper into a call-chain, ie when lt_level should be bumper in the next loc_trace_elem, and
      similarly for [sink_should_nest] *)

  val of_source : Source.t -> t
  (** create a trace from a source *)

  val of_footprint : AccessPath.Abs.t -> t
  (** create a trace from a footprint access path *)

  val add_source : Source.t -> t -> t
  (** add a source to the current trace *)

  val add_sink : Sink.t -> t -> t
  (** add a sink to the current trace. *)

  val add_sanitizer : Sanitizer.t -> t -> t
  (** add a sanitizer to the current trace *)

  val update_sources : t -> Sources.t -> t

  val update_sinks : t -> Sinks.t -> t

  val get_footprint_indexes : t -> IntSet.t

  val append : t -> t -> CallSite.t -> t
  (** append the trace for given call site to the current caller trace *)

  val is_empty : t -> bool
  (** return true if this trace has no source or sink data *)

  val pp : F.formatter -> t -> unit

  val pp_path : Typ.Procname.t -> F.formatter -> path -> unit
  (** pretty-print a path in the context of the given procname *)
end

(** Expand a trace element (i.e., a source or sink) into a list of trace elements bottoming out in
    the "original" trace element. The list is always non-empty. *)
module Expander (TraceElem : TraceElem.S) = struct
  let expand elem0 ~elems_passthroughs_of_pname ~filter_passthroughs =
    let rec expand_ elem (elems_passthroughs_acc, seen_acc) =
      let caller_elem_site = TraceElem.call_site elem in
      let caller_elem_kind = TraceElem.kind elem in
      let seen_acc' = CallSite.Set.add caller_elem_site seen_acc in
      let elems, passthroughs = elems_passthroughs_of_pname (CallSite.pname caller_elem_site) in
      let is_recursive callee_elem seen =
        CallSite.Set.mem (TraceElem.call_site callee_elem) seen
      in
      (* find sinks that are the same kind as the caller, but have a different procname *)
      let matching_elems =
        List.filter
          ~f:(fun callee_elem ->
            TraceElem.Kind.matches ~caller:caller_elem_kind ~callee:(TraceElem.kind callee_elem)
            && not (is_recursive callee_elem seen_acc') )
          elems
      in
      (* arbitrarily pick one elem and explore it further *)
      match matching_elems with
      | callee_elem :: _ ->
          (* TODO: pick the shortest path to a sink here instead (t14242809) *)
          let filtered_passthroughs =
            filter_passthroughs caller_elem_site (TraceElem.call_site callee_elem) passthroughs
          in
          expand_ callee_elem ((elem, filtered_passthroughs) :: elems_passthroughs_acc, seen_acc')
      | _ ->
          ((elem, Passthrough.Set.empty) :: elems_passthroughs_acc, seen_acc')
    in
    fst (expand_ elem0 ([], CallSite.Set.empty))
end

module Make (Spec : Spec) = struct
  include Spec

  module Sources = struct
    module Known = AbstractDomain.FiniteSet (Source)
    module FootprintConfig = AccessTree.DefaultConfig
    module Footprint = AccessTree.PathSet (FootprintConfig)
    module Sanitizers = AbstractDomain.FiniteSet (Sanitizer)

    type t = {known: Known.t; footprint: Footprint.t; sanitizers: Sanitizers.t}

    let ( <= ) ~lhs ~rhs =
      if phys_equal lhs rhs then true
      else
        Known.( <= ) ~lhs:lhs.known ~rhs:rhs.known
        && Footprint.( <= ) ~lhs:lhs.footprint ~rhs:rhs.footprint
        && Sanitizers.( <= ) ~lhs:lhs.sanitizers ~rhs:rhs.sanitizers


    let join astate1 astate2 =
      if phys_equal astate1 astate2 then astate1
      else
        let known = Known.join astate1.known astate2.known in
        let footprint = Footprint.join astate1.footprint astate2.footprint in
        let sanitizers = Sanitizers.join astate1.sanitizers astate2.sanitizers in
        {known; footprint; sanitizers}


    let widen ~prev ~next ~num_iters =
      if phys_equal prev next then prev
      else
        let known = Known.widen ~prev:prev.known ~next:next.known ~num_iters in
        let footprint = Footprint.widen ~prev:prev.footprint ~next:next.footprint ~num_iters in
        let sanitizers = Sanitizers.widen ~prev:prev.sanitizers ~next:next.sanitizers ~num_iters in
        {known; footprint; sanitizers}


    let pp fmt {known; footprint; sanitizers} =
      let pp_sanitizers fmt sanitizers =
        if not (Sanitizers.is_empty sanitizers) then
          F.fprintf fmt " + Sanitizers(%a)" Sanitizers.pp sanitizers
      in
      if Known.is_empty known then
        if Footprint.is_empty footprint then F.pp_print_string fmt "{}"
        else F.fprintf fmt "Footprint(%a)%a" Footprint.pp footprint pp_sanitizers sanitizers
      else
        F.fprintf fmt "%a + Footprint(%a)%a" Known.pp known Footprint.pp footprint pp_sanitizers
          sanitizers


    let empty = {known= Known.empty; footprint= Footprint.empty; sanitizers= Sanitizers.empty}

    (* note: empty known/footprint implies empty sanitizers *)
    let is_empty {known; footprint} = Known.is_empty known && Footprint.BaseMap.is_empty footprint

    let of_footprint access_path =
      let footprint = Footprint.add_trace access_path true Footprint.empty in
      {empty with footprint}


    let of_source source =
      let known = Known.singleton source in
      {empty with known}


    let add source astate =
      let known = Known.add source astate.known in
      {astate with known}


    let get_footprint_indexes {footprint} =
      Footprint.BaseMap.fold
        (fun base _ acc ->
          match AccessPath.Abs.get_footprint_index_base base with
          | Some footprint_index ->
              IntSet.add footprint_index acc
          | None ->
              acc )
        footprint IntSet.empty
  end

  module Sinks = Sink.Set
  module Passthroughs = Passthrough.Set
  module SourceExpander = Expander (Source)
  module SinkExpander = Expander (Sink)

  type t =
    { sources: Sources.t  (** last functions in the trace that returned tainted data *)
    ; sinks: Sinks.t
          (** last callees in the trace that transitively called a tainted function (if any) *)
    ; passthroughs: Passthrough.Set.t  (** calls that occurred between source and sink *) }

  type path = Passthroughs.t * (Source.t * Passthroughs.t) list * (Sink.t * Passthroughs.t) list

  type report =
    { issue: IssueType.t
    ; path_source: Source.t
    ; path_sink: Sink.t
    ; path_passthroughs: Passthroughs.t }

  let pp fmt {sources; sinks; passthroughs} =
    let pp_passthroughs fmt passthroughs =
      if not (Passthroughs.is_empty passthroughs) then
        F.fprintf fmt " via %a" Passthroughs.pp passthroughs
    in
    let pp_sinks fmt sinks =
      if Sinks.is_empty sinks then F.pp_print_char fmt '?' else Sinks.pp fmt sinks
    in
    (* empty sources implies empty sinks and passthroughs *)
    F.fprintf fmt "%a ~> %a%a" Sources.pp sources pp_sinks sinks pp_passthroughs passthroughs


  let get_path_source_call_site = Source.call_site

  let sources t = t.sources

  let sinks t = t.sinks

  let passthroughs t = t.passthroughs

  let is_empty t =
    (* sources empty => sinks empty and passthroughs empty *)
    Sources.is_empty t.sources


  let get_reports ?cur_site t =
    if Sinks.is_empty t.sinks || Sources.is_empty t.sources then []
    else
      let should_report_at_site source sink =
        match cur_site with
        | None ->
            true
        | Some call_site ->
            (* report when: (1) [cur_site] introduces the sink, and (2) [cur_site] does not also
               introduce the source. otherwise, we'll report paths that don't respect control
               flow. *)
            CallSite.equal call_site (Sink.call_site sink)
            && not (CallSite.equal call_site (Source.call_site source))
      in
      (* written to avoid closure allocations in hot code. change with caution. *)
      let report_source source sinks acc0 =
        let report_one sink acc =
          if should_report_at_site source sink then
            match
              Spec.get_report source sink (Sources.Sanitizers.elements t.sources.sanitizers)
            with
            | Some issue ->
                {issue; path_source= source; path_sink= sink; path_passthroughs= t.passthroughs}
                :: acc
            | None ->
                acc
          else acc
        in
        Sinks.fold report_one sinks acc0
      in
      let report_sources source acc = report_source source t.sinks acc in
      Sources.Known.fold report_sources t.sources.known []


  let pp_path_source = Source.pp

  let pp_path cur_pname fmt (cur_passthroughs, sources_passthroughs, sinks_passthroughs) =
    let pp_passthroughs fmt passthroughs =
      if not (Passthrough.Set.is_empty passthroughs) then
        F.fprintf fmt "(via %a)" Passthrough.Set.pp passthroughs
    in
    let pp_elems elem_to_callsite fmt elems_passthroughs =
      let pp_sep fmt () = F.fprintf fmt "@." in
      let pp_elem fmt (elem, passthroughs) =
        F.fprintf fmt "|=> %a %a" CallSite.pp (elem_to_callsite elem) pp_passthroughs passthroughs
      in
      F.pp_print_list ~pp_sep pp_elem fmt elems_passthroughs
    in
    let pp_sources = pp_elems get_path_source_call_site in
    let pp_sinks = pp_elems Sink.call_site in
    let original_source = fst (List.hd_exn sources_passthroughs) in
    let final_sink = fst (List.hd_exn sinks_passthroughs) in
    F.fprintf fmt "Error: %a -> %a. Full trace:@.%a@.Current procedure %a %a@.%a" pp_path_source
      original_source Sink.pp final_sink pp_sources sources_passthroughs Typ.Procname.pp cur_pname
      pp_passthroughs cur_passthroughs pp_sinks (List.rev sinks_passthroughs)


  type passthrough_kind =
    | Source  (** passthroughs of a source *)
    | Sink  (** passthroughs of a sink *)
    | Top_level  (** passthroughs of a top-level source->sink path *)

  let get_reportable_paths ?cur_site t ~trace_of_pname =
    let filter_passthroughs_ passthrough_kind start_site end_site passthroughs =
      let line_number call_site = (CallSite.loc call_site).Location.line in
      let start_line = line_number start_site in
      let end_line = line_number end_site in
      let between_start_and_end passthrough =
        let passthrough_line = line_number (Passthrough.site passthrough) in
        match passthrough_kind with
        | Source ->
            passthrough_line >= end_line
        | Sink ->
            passthrough_line <= end_line
        | Top_level ->
            passthrough_line >= start_line && passthrough_line <= end_line
      in
      Passthrough.Set.filter between_start_and_end passthroughs
    in
    let expand_path source sink =
      let sources_of_pname pname =
        let trace = trace_of_pname pname in
        (Sources.Known.elements (sources trace).known, passthroughs trace)
      in
      let sinks_of_pname pname =
        let trace = trace_of_pname pname in
        (Sinks.elements (sinks trace), passthroughs trace)
      in
      let sources_passthroughs =
        let filter_passthroughs = filter_passthroughs_ Source in
        SourceExpander.expand source ~elems_passthroughs_of_pname:sources_of_pname
          ~filter_passthroughs
        |> List.map ~f:(fun (source, passthrough) -> (source, passthrough))
      in
      let sinks_passthroughs =
        let filter_passthroughs = filter_passthroughs_ Sink in
        SinkExpander.expand sink ~elems_passthroughs_of_pname:sinks_of_pname ~filter_passthroughs
      in
      (sources_passthroughs, sinks_passthroughs)
    in
    List.map
      ~f:(fun {path_source; path_sink; path_passthroughs} ->
        let sources_passthroughs, sinks_passthroughs = expand_path path_source path_sink in
        let filtered_passthroughs =
          let source_site = Source.call_site path_source in
          filter_passthroughs_ Top_level source_site (Sink.call_site path_sink) path_passthroughs
        in
        (filtered_passthroughs, sources_passthroughs, sinks_passthroughs) )
      (get_reports ?cur_site t)


  let to_loc_trace
      ?(desc_of_source =
        fun source ->
          let callsite = Source.call_site source in
          Format.asprintf "return from %a" Typ.Procname.pp (CallSite.pname callsite))
      ?(source_should_nest = fun _ -> true)
      ?(desc_of_sink =
        fun sink ->
          let callsite = Sink.call_site sink in
          Format.asprintf "call to %a" Typ.Procname.pp (CallSite.pname callsite))
      ?(sink_should_nest = fun _ -> true) (passthroughs, sources, sinks) =
    let trace_elems_of_passthroughs lt_level passthroughs acc0 =
      let trace_elem_of_passthrough passthrough acc =
        let passthrough_site = Passthrough.site passthrough in
        let desc =
          F.asprintf "flow through %a" Typ.Procname.pp (CallSite.pname passthrough_site)
        in
        Errlog.make_trace_element lt_level (CallSite.loc passthrough_site) desc [] :: acc
      in
      (* sort passthroughs by ascending line number to create a coherent trace *)
      let sorted_passthroughs =
        List.sort
          ~compare:(fun passthrough1 passthrough2 ->
            let loc1 = CallSite.loc (Passthrough.site passthrough1) in
            let loc2 = CallSite.loc (Passthrough.site passthrough2) in
            Int.compare loc1.Location.line loc2.Location.line )
          (Passthroughs.elements passthroughs)
      in
      List.fold_right ~f:trace_elem_of_passthrough sorted_passthroughs ~init:acc0
    in
    let get_nesting should_nest elems start_nesting =
      let level = ref start_nesting in
      let get_nesting_ ((elem, _) as pair) =
        if should_nest elem then incr level ;
        (pair, !level)
      in
      List.map ~f:get_nesting_ (List.rev elems)
    in
    let trace_elems_of_path_elem call_site desc ~is_source ((elem, passthroughs), lt_level) acc =
      let desc = desc elem in
      let loc = CallSite.loc (call_site elem) in
      if is_source then
        let trace_elem = Errlog.make_trace_element lt_level loc desc [] in
        trace_elems_of_passthroughs (lt_level + 1) passthroughs (trace_elem :: acc)
      else
        let trace_elem = Errlog.make_trace_element (lt_level - 1) loc desc [] in
        trace_elem :: trace_elems_of_passthroughs lt_level passthroughs acc
    in
    let trace_elems_of_source =
      trace_elems_of_path_elem get_path_source_call_site desc_of_source ~is_source:true
    in
    let trace_elems_of_sink =
      trace_elems_of_path_elem Sink.call_site desc_of_sink ~is_source:false
    in
    let sources_with_level = get_nesting source_should_nest sources (-1) in
    let sinks_with_level = get_nesting sink_should_nest sinks 0 in
    let trace_prefix =
      List.fold_right ~f:trace_elems_of_sink sinks_with_level ~init:[]
      |> trace_elems_of_passthroughs 0 passthroughs
    in
    List.fold
      ~f:(fun acc source -> trace_elems_of_source source acc)
      ~init:trace_prefix sources_with_level


  let of_sources sources =
    let passthroughs = Passthroughs.empty in
    let sinks = Sinks.empty in
    {sources; passthroughs; sinks}


  let of_source source =
    let sources = Sources.of_source source in
    of_sources sources


  let of_footprint access_path =
    let sources = Sources.of_footprint access_path in
    of_sources sources


  let add_source source t =
    let sources = Sources.add source t.sources in
    {t with sources}


  let add_sink sink t =
    let sinks = Sinks.add sink t.sinks in
    {t with sinks}


  let add_sanitizer sanitizer t =
    let sanitizers = Sources.Sanitizers.add sanitizer t.sources.sanitizers in
    let sources = {t.sources with sanitizers} in
    {t with sources}


  let update_sources t sources = {t with sources}

  let update_sinks t sinks = {t with sinks}

  let get_footprint_indexes trace = Sources.get_footprint_indexes trace.sources

  (** compute caller_trace + callee_trace *)
  let append caller_trace callee_trace callee_site =
    if is_empty callee_trace then caller_trace
    else
      let sanitizers =
        Sources.Sanitizers.join callee_trace.sources.sanitizers caller_trace.sources.sanitizers
      in
      let non_footprint_callee_sources = callee_trace.sources.known in
      let sources =
        if Sources.Known.subset non_footprint_callee_sources caller_trace.sources.known then
          if phys_equal sanitizers caller_trace.sources.sanitizers then caller_trace.sources
          else {caller_trace.sources with Sources.sanitizers}
        else
          let known =
            List.map
              ~f:(fun source -> Source.with_callsite source callee_site)
              (Sources.Known.elements non_footprint_callee_sources)
            |> Sources.Known.of_list
            |> Sources.Known.union caller_trace.sources.known
          in
          {caller_trace.sources with Sources.known; sanitizers}
      in
      let sinks =
        if Sinks.subset callee_trace.sinks caller_trace.sinks then caller_trace.sinks
        else
          let footprint_indices =
            Sources.Footprint.BaseMap.fold
              (fun (vname, _) _ s ->
                match Var.get_footprint_index vname with Some ind -> IntSet.add ind s | None -> s
                )
              callee_trace.sources.footprint IntSet.empty
          in
          List.map
            ~f:(fun sink ->
              Sink.with_indexes (Sink.with_callsite sink callee_site) footprint_indices )
            (Sinks.elements callee_trace.sinks)
          |> Sinks.of_list |> Sinks.union caller_trace.sinks
      in
      let passthroughs =
        if Config.passthroughs then
          if phys_equal sources caller_trace.sources && phys_equal sinks caller_trace.sinks then
            (* this callee didn't add any new sources or any news sinks; it's just a passthrough *)
            Passthroughs.add (Passthrough.make callee_site) caller_trace.passthroughs
          else caller_trace.passthroughs
        else Passthroughs.empty
      in
      {sources; sinks; passthroughs}


  let empty =
    let sources = Sources.empty in
    let sinks = Sinks.empty in
    let passthroughs = Passthroughs.empty in
    {sources; sinks; passthroughs}


  let ( <= ) ~lhs ~rhs =
    phys_equal lhs rhs
    || Sources.( <= ) ~lhs:lhs.sources ~rhs:rhs.sources
       && Sinks.subset lhs.sinks rhs.sinks
       && Passthroughs.subset lhs.passthroughs rhs.passthroughs


  let join t1 t2 =
    if phys_equal t1 t2 then t1
    else
      let sources = Sources.join t1.sources t2.sources in
      let sinks = Sinks.union t1.sinks t2.sinks in
      let passthroughs = Passthroughs.union t1.passthroughs t2.passthroughs in
      {sources; sinks; passthroughs}


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      let sources = Sources.widen ~prev:prev.sources ~next:next.sources ~num_iters in
      let sinks = Sinks.union prev.sinks next.sinks in
      let passthroughs = Passthroughs.union prev.passthroughs next.passthroughs in
      {sources; sinks; passthroughs}
end
