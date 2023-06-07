(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let currently_under_analysis : Procname.t option ref = ref None

type complete =
  { summary_loads: Procname.t list
  ; recursion_edges: Procname.Set.t
  ; other_proc_names: Procname.t list
  ; used_tenv_sources: SourceFile.t list }

type t = Partial | Complete of complete

type partial =
  { partial_summary_loads: Procname.HashSet.t
  ; partial_recursion_edges: Procname.HashSet.t
  ; partial_other_proc_names: Procname.HashSet.t
  ; partial_used_tenv_sources: SourceFile.HashSet.t }

let deps_in_progress : partial Procname.Hash.t = Procname.Hash.create 0

let reset pname =
  let partial =
    { partial_summary_loads= Procname.HashSet.create 0
    ; partial_recursion_edges= Procname.HashSet.create 0
    ; partial_other_proc_names= Procname.HashSet.create 0
    ; partial_used_tenv_sources= SourceFile.HashSet.create 0 }
  in
  Procname.Hash.replace deps_in_progress pname partial ;
  Partial


let freeze pname deps =
  match deps with
  | Partial ->
      let { partial_summary_loads
          ; partial_recursion_edges
          ; partial_other_proc_names
          ; partial_used_tenv_sources } =
        Procname.Hash.find deps_in_progress pname
      in
      (* make sets pairwise disjoint to save space in summaries, in case we first added a procedure
         to "other" and *then* to "summary loads", for example *)
      let iter_partial_summary_loads = Procname.HashSet.iter partial_summary_loads in
      Procname.HashSet.remove_all iter_partial_summary_loads partial_recursion_edges ;
      let iter_partial_recursion_edges = Procname.HashSet.iter partial_recursion_edges in
      Procname.HashSet.remove_all
        (Iter.append iter_partial_recursion_edges iter_partial_summary_loads)
        partial_other_proc_names ;
      (* now freeze them *)
      let summary_loads = Iter.to_list iter_partial_summary_loads in
      let recursion_edges =
        Iter.to_set (module Procname.Set) (Procname.HashSet.iter partial_recursion_edges)
      in
      let other_proc_names = Iter.to_list (Procname.HashSet.iter partial_other_proc_names) in
      let used_tenv_sources = Iter.to_list (SourceFile.HashSet.iter partial_used_tenv_sources) in
      {summary_loads; recursion_edges; other_proc_names; used_tenv_sources}
  | Complete c ->
      c


let complete_exn = function
  | Complete c ->
      c
  | Partial ->
      L.die InternalError "complete dependency info unavailable for partially-computed summary"


type kind = SummaryLoad | RecursionEdge | Other

let record_pname_dep ?caller kind callee =
  let caller = match caller with Some _ -> caller | None -> !currently_under_analysis in
  match caller with
  | Some caller when not (Procname.equal caller callee) ->
      Option.iter (Procname.Hash.find_opt deps_in_progress caller)
        ~f:(fun {partial_summary_loads; partial_recursion_edges; partial_other_proc_names} ->
          match kind with
          | SummaryLoad ->
              Procname.HashSet.add callee partial_summary_loads
          (* HACK: only add to the other (than "summary loads") buckets if the proc name is not
             already accounted for in summary loads as we don't need to precisely account for
             holding another kind of dependency as long as we know it's a dependency already. This
             avoids double counting elsewhere and saves some space. *)
          | RecursionEdge ->
              if not @@ Procname.HashSet.mem partial_summary_loads callee then
                Procname.HashSet.add callee partial_recursion_edges
          | Other ->
              if
                (not @@ Procname.HashSet.mem partial_summary_loads callee)
                && (not @@ Procname.HashSet.mem partial_recursion_edges callee)
              then Procname.HashSet.add callee partial_other_proc_names )
  | _ ->
      ()


let record_srcfile_dep src_file =
  Option.bind !currently_under_analysis ~f:(Procname.Hash.find_opt deps_in_progress)
  |> Option.iter ~f:(fun {partial_used_tenv_sources} ->
         SourceFile.HashSet.add src_file partial_used_tenv_sources )


let clear () =
  Procname.Hash.clear deps_in_progress ;
  currently_under_analysis := None


let pp fmt = function
  | Partial ->
      F.pp_print_string fmt "Partial"
  | ((Complete {summary_loads; recursion_edges; other_proc_names; used_tenv_sources})
  [@warning "+missing-record-field-pattern"] ) ->
      F.fprintf fmt
        "summary_loads= @[<hv>%a@]@\n\
         recursion_edges= @[<hv>%a@]@\n\
         other_proc_names= @[<hv>%a@]@\n\
         type environments from sources= @[<hv>%a@]@\n"
        (Pp.seq ~sep:", " Procname.pp) summary_loads Procname.Set.pp recursion_edges
        (Pp.seq ~sep:", " Procname.pp) other_proc_names (Pp.seq ~sep:", " SourceFile.pp)
        used_tenv_sources
