(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Callees = struct
  type call_kind = Static | Virtual | Closure [@@deriving equal, compare]

  type resolution =
    | ResolvedUsingDynamicType (* the most precise resolution *)
    | ResolvedUsingStaticType (* may not be exact *)
    | Unresolved
      (* the worst resolution because we don't have enough type
         information or the capture was incomplete *)
  [@@deriving equal, compare]

  module CallSite = struct
    type t =
      { callsite_loc: Location.t
      ; caller_name: (string[@compare.ignore])
      ; caller_loc: (Location.t[@compare.ignore]) }
    [@@deriving compare]

    let pp fmt {callsite_loc} = Location.pp_file_pos fmt callsite_loc
  end

  module Status = struct
    type kind = call_kind [@@deriving equal, compare]

    type t = {kind: kind; resolution: resolution} [@@deriving equal, compare]

    let pp fmt {kind; resolution} =
      let kind =
        match kind with Static -> "static" | Virtual -> "virtual" | Closure -> "closure"
      in
      let resolution =
        match resolution with
        | Unresolved ->
            "unresolved"
        | ResolvedUsingDynamicType ->
            "resolved using receiver dynamic type"
        | ResolvedUsingStaticType ->
            "resolved using receiver static type"
      in
      F.fprintf fmt "%s call was %s" kind resolution


    (* this is a total order *)
    let leq ~lhs ~rhs =
      if equal lhs rhs then true
      else
        match (lhs.resolution, rhs.resolution) with
        | _, ResolvedUsingDynamicType | Unresolved, _ ->
            false
        | _, _ ->
            true


    let join lhs rhs = if leq ~lhs ~rhs then rhs else lhs

    let widen ~prev ~next ~num_iters:_ = join prev next
  end

  include AbstractDomain.Map (CallSite) (Status)

  let compare = compare Status.compare

  let equal = equal Status.equal

  let record ~caller_name ~caller_loc ~callsite_loc kind resolution history =
    let callsite = {CallSite.caller_name; caller_loc; callsite_loc} in
    add callsite {kind; resolution} history


  type item =
    { callsite_loc: Location.t
    ; caller_name: string
    ; caller_loc: Location.t
    ; kind: call_kind
    ; resolution: resolution }

  let report_as_extra_info history =
    fold
      (fun {callsite_loc; caller_name; caller_loc} ({kind; resolution} : Status.t) acc : item list ->
        {callsite_loc; caller_name; caller_loc; kind; resolution} :: acc )
      history []
end

module Accesses = AbstractDomain.FiniteSetOfPPSet (PulseTrace.Set)
module MissedCaptures = AbstractDomain.FiniteSetOfPPSet (Typ.Name.Set)

type t = {accesses: Accesses.t; callees: Callees.t; missed_captures: MissedCaptures.t}
[@@deriving abstract_domain, compare, equal]

let pp fmt {accesses; callees; missed_captures} =
  F.fprintf fmt "@[@[accesses: %a@],@ @[callees: %a@],@ @[missed captures: %a@]@]" Accesses.pp
    accesses Callees.pp callees Typ.Name.Set.pp missed_captures


let bottom = {accesses= Accesses.empty; callees= Callees.bottom; missed_captures= Typ.Name.Set.empty}

let is_bottom {accesses; callees; missed_captures} =
  Accesses.is_bottom accesses && Callees.is_bottom callees && Typ.Name.Set.is_empty missed_captures


let remember_dropped_elements ~dropped {accesses; callees; missed_captures} =
  let accesses = Accesses.union dropped.accesses accesses in
  let callees = Callees.join dropped.callees callees in
  let missed_captures = Typ.Name.Set.union dropped.missed_captures missed_captures in
  {accesses; callees; missed_captures}


let apply_summary ~callee_pname ~call_loc ~summary {accesses; callees; missed_captures} =
  let accesses =
    PulseTrace.Set.map_callee (PulseCallEvent.Call callee_pname) call_loc summary.accesses
    |> PulseTrace.Set.union accesses
  in
  let callees = Callees.join callees summary.callees in
  let missed_captures = MissedCaptures.join missed_captures summary.missed_captures in
  {accesses; callees; missed_captures}


let transfer_transitive_info_to_caller ~caller callee_proc_name call_loc ~callee_summary =
  let accesses =
    PulseTrace.Set.map_callee (Call callee_proc_name) call_loc callee_summary.accesses
    |> PulseTrace.Set.union caller.accesses
  in
  let callees = Callees.join caller.callees callee_summary.callees in
  let missed_captures = Typ.Name.Set.union caller.missed_captures callee_summary.missed_captures in
  {accesses; callees; missed_captures}
