(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'payload t =
  { proc_desc: Procdesc.t
  ; tenv: Tenv.t
  ; err_log: Errlog.t
  ; exe_env: Exe_env.t
  ; analyze_dependency: ?specialization:Specialization.t -> Procname.t -> 'payload option
  ; update_stats: ?add_symops:int -> ?failure_kind:Exception.failure_kind -> unit -> unit }

type 'payload file_t =
  { source_file: SourceFile.t
  ; procedures: Procname.t list
  ; file_exe_env: Exe_env.t
  ; analyze_file_dependency: Procname.t -> 'payload option }

let bind_payload ~f analysis_data =
  { analysis_data with
    analyze_dependency=
      (fun ?specialization proc_name ->
        Option.bind ~f (analysis_data.analyze_dependency ?specialization proc_name) ) }
