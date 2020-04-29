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
  ; analyze_dependency: Procname.t -> (Procdesc.t * 'payload) option
  ; analyze_pdesc_dependency: Procdesc.t -> 'payload option
  ; update_stats: ?add_symops:int -> ?failure_kind:SymOp.failure_kind -> unit -> unit }
