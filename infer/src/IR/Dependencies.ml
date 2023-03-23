(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let currently_under_analysis : Procname.t option ref = ref None

let () = AnalysisGlobalState.register_ref_with_proc_name currently_under_analysis ~init:Option.some

type partial = Procname.HashSet.t

type complete = {callees: Procname.t list; used_tenv_sources: SourceFile.t list}

type t = Partial of partial | Complete of complete

let deps_in_progress : (SourceFile.HashSet.t * Procname.HashSet.t) Procname.Hash.t =
  Procname.Hash.create 0


let reset pname =
  let pname_deps = Procname.HashSet.create 0 in
  let srcfile_deps = SourceFile.HashSet.create 0 in
  Procname.Hash.replace deps_in_progress pname (srcfile_deps, pname_deps) ;
  Partial pname_deps


let freeze pname deps =
  match deps with
  | Partial _ ->
      let srcfile_deps, pname_deps = Procname.Hash.find deps_in_progress pname in
      let callees = Iter.to_list (Procname.HashSet.iter pname_deps) in
      let used_tenv_sources = Iter.to_list (SourceFile.HashSet.iter srcfile_deps) in
      {callees; used_tenv_sources}
  | Complete c ->
      c


let complete_exn = function
  | Complete c ->
      c
  | Partial _ ->
      L.die InternalError "complete dependency info unavailable for partially-computed summary"


let record_pname_dep ?caller callee =
  let caller = match caller with Some _ -> caller | None -> !currently_under_analysis in
  match caller with
  | Some caller when not (Procname.equal caller callee) ->
      Option.iter (Procname.Hash.find_opt deps_in_progress caller) ~f:(fun (_, caller_pname_deps) ->
          Procname.HashSet.add callee caller_pname_deps )
  | _ ->
      ()


let record_srcfile_dep src_file =
  Option.bind !currently_under_analysis ~f:(Procname.Hash.find_opt deps_in_progress)
  |> Option.iter ~f:(fun (deps, _) -> SourceFile.HashSet.add src_file deps)


let clear () =
  Procname.Hash.clear deps_in_progress ;
  currently_under_analysis := None
