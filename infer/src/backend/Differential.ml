(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** type for remembering what we have already reported to avoid duplicates. our policy is to report
    each kind of access (read/write) to the same field reachable from the same procedure only once.
    in addition, if a call to a procedure (transitively) accesses multiple fields, we will only
    report one of each kind of access *)
type reported =
  { reported_sites: CallSite.Set.t
  ; reported_writes: Typ.Procname.Set.t
  ; reported_reads: Typ.Procname.Set.t }

let empty_reported =
  let reported_sites = CallSite.Set.empty in
  let reported_writes = Typ.Procname.Set.empty in
  let reported_reads = Typ.Procname.Set.empty in
  {reported_sites; reported_reads; reported_writes}


module Access = struct
  type t = Typ.Procname.t * RacerDDomain.TraceElem.t [@@deriving compare]
end

(** map from accesses to reported sets for remembering what has been reported for each access *)
module AccessMap = Caml.Map.Make (Access)

let is_duplicate_report (pname, access) {reported_sites; reported_writes; reported_reads} =
  let open RacerDDomain in
  if Config.filtering then CallSite.Set.mem (TraceElem.call_site access) reported_sites
    ||
    match TraceElem.kind access with
    | Access.Write _ | Access.ContainerWrite _ ->
        Typ.Procname.Set.mem pname reported_writes
    | Access.Read _ | Access.ContainerRead _ ->
        Typ.Procname.Set.mem pname reported_reads
    | Access.InterfaceCall _ ->
        false
  else false


let update_reported (pname, access) reported =
  let open RacerDDomain in
  if Config.filtering then
    let reported_sites = CallSite.Set.add (TraceElem.call_site access) reported.reported_sites in
    match TraceElem.kind access with
    | Access.Write _ | Access.ContainerWrite _ ->
        let reported_writes = Typ.Procname.Set.add pname reported.reported_writes in
        {reported with reported_writes; reported_sites}
    | Access.Read _ | Access.ContainerRead _ ->
        let reported_reads = Typ.Procname.Set.add pname reported.reported_reads in
        {reported with reported_reads; reported_sites}
    | Access.InterfaceCall _ ->
        reported
  else reported


let dedup (issues: Jsonbug_t.jsonbug list) =
  List.fold issues ~init:(AccessMap.empty, []) ~f:
    (fun (reported_map, nondup_issues) (issue: Jsonbug_t.jsonbug) ->
      match issue.access with
      | Some access_serial ->
          let access : Access.t = Marshal.from_string (B64.decode access_serial) 0 in
          let reported =
            Option.value (AccessMap.find_opt access reported_map) ~default:empty_reported
          in
          if is_duplicate_report access reported then (reported_map, nondup_issues)
          else
            ( AccessMap.add access (update_reported access reported) reported_map
            , {issue with access= None} :: nondup_issues )
      | None ->
          (reported_map, {issue with access= None} :: nondup_issues) )
  |> snd


type t = {introduced: Jsonbug_t.report; fixed: Jsonbug_t.report; preexisting: Jsonbug_t.report}

(** Set operations should keep duplicated issues with identical hashes *)
let of_reports ~(current_report: Jsonbug_t.report) ~(previous_report: Jsonbug_t.report) : t =
  let to_map report =
    List.fold_left
      ~f:(fun map issue -> Map.add_multi map ~key:issue.Jsonbug_t.hash ~data:issue)
      ~init:String.Map.empty report
  in
  let fold_aux ~key:_ ~data (left, both, right) =
    match data with
    | `Left left' ->
        (List.rev_append left' left, both, right)
    | `Both (both', _) ->
        (left, List.rev_append both' both, right)
    | `Right right' ->
        (left, both, List.rev_append right' right)
  in
  let introduced, preexisting, fixed =
    Map.fold2 (to_map current_report) (to_map previous_report) ~f:fold_aux ~init:([], [], [])
  in
  {introduced= dedup introduced; fixed= dedup fixed; preexisting= dedup preexisting}


let to_files {introduced; fixed; preexisting} destdir =
  Out_channel.write_all (destdir ^/ "introduced.json")
    ~data:(Jsonbug_j.string_of_report introduced) ;
  Out_channel.write_all (destdir ^/ "fixed.json") ~data:(Jsonbug_j.string_of_report fixed) ;
  Out_channel.write_all (destdir ^/ "preexisting.json")
    ~data:(Jsonbug_j.string_of_report preexisting)

