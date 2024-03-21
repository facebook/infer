(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let proc_desc_ref = ref None

let tenv_ref = ref (None : Tenv.t Option.t)

let () =
  AnalysisGlobalState.register_ref_with_proc_desc_and_tenv proc_desc_ref
    ~init:(fun proc_desc _tenv -> Some proc_desc )


let () =
  AnalysisGlobalState.register_ref_with_proc_desc_and_tenv tenv_ref ~init:(fun _proc_desc tenv ->
      Some tenv )


let proc_desc () = !proc_desc_ref

let tenv () = !tenv_ref

let tenv_exn () =
  match tenv () with
  | None ->
      Logging.die InternalError "No tenv available in global state"
  | Some tenv ->
      tenv


let set_tenv_global_for_testing tenv = tenv_ref := Some tenv
