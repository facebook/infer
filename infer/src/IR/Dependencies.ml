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

let () = AnalysisGlobalState.register_ref_with_proc_name currently_under_analysis ~init:Option.some

type complete =
  { summary_loads: Procname.t list
  ; other_proc_names: Procname.t list
  ; used_tenv_sources: SourceFile.t list }

type t = Partial | Complete of complete

type partial =
  { partial_summary_loads: Procname.HashSet.t
  ; partial_other_proc_names: Procname.HashSet.t
  ; partial_used_tenv_sources: SourceFile.HashSet.t }

let deps_in_progress : partial Procname.Hash.t = Procname.Hash.create 0

let reset pname =
  let partial =
    { partial_summary_loads= Procname.HashSet.create 0
    ; partial_other_proc_names= Procname.HashSet.create 0
    ; partial_used_tenv_sources= SourceFile.HashSet.create 0 }
  in
  Procname.Hash.replace deps_in_progress pname partial ;
  Partial


let freeze pname deps =
  match deps with
  | Partial ->
      let {partial_summary_loads; partial_other_proc_names; partial_used_tenv_sources} =
        Procname.Hash.find deps_in_progress pname
      in
      let summary_loads = Iter.to_list (Procname.HashSet.iter partial_summary_loads) in
      let other_proc_names = Iter.to_list (Procname.HashSet.iter partial_other_proc_names) in
      let used_tenv_sources = Iter.to_list (SourceFile.HashSet.iter partial_used_tenv_sources) in
      {summary_loads; other_proc_names; used_tenv_sources}
  | Complete c ->
      c


let complete_exn = function
  | Complete c ->
      c
  | Partial ->
      L.die InternalError "complete dependency info unavailable for partially-computed summary"


let record_pname_dep ?caller ~is_summary_load callee =
  let caller = match caller with Some _ -> caller | None -> !currently_under_analysis in
  match caller with
  | Some caller when not (Procname.equal caller callee) ->
      Option.iter (Procname.Hash.find_opt deps_in_progress caller)
        ~f:(fun {partial_summary_loads; partial_other_proc_names} ->
          if is_summary_load then Procname.HashSet.add callee partial_summary_loads
          else if not @@ Procname.HashSet.mem partial_summary_loads callee then
            (* HACK: only add to [other] if the proc name is not already accounted for in
               [summary_loads] as we don't need to precisely account for holding an "other"
               dependency as long as we know it's a dependency already. This avoids double counting
               elsewhere and saves some space. *)
            Procname.HashSet.add callee partial_other_proc_names )
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
      L.die InternalError "pretty-printing Partial dependencies unimplemented"
  | Complete {summary_loads; other_proc_names; used_tenv_sources} ->
      F.fprintf fmt
        "summary_loads= @[<hv>%a@]@\n\
         other_proc_names= @[<hv>%a@]@\n\
         type environments from sources= @[<hv>%a@]@\n"
        (Pp.seq ~sep:", " Procname.pp) summary_loads (Pp.seq ~sep:", " Procname.pp) other_proc_names
        (Pp.seq ~sep:", " SourceFile.pp) used_tenv_sources
