(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let proc_desc_key = DLS.new_key (fun () -> None)

let tenv_key = DLS.new_key (fun () : Tenv.t Option.t -> None)

let () =
  AnalysisGlobalState.register_dls_with_proc_desc_and_tenv proc_desc_key
    ~init:(fun proc_desc _tenv -> Some proc_desc )


let () =
  AnalysisGlobalState.register_dls_with_proc_desc_and_tenv tenv_key ~init:(fun _proc_desc tenv ->
      Some tenv )


let proc_desc () = DLS.get proc_desc_key

let tenv () = DLS.get tenv_key

let tenv_exn () =
  match tenv () with
  | None ->
      Logging.die InternalError "No tenv available in global state"
  | Some tenv ->
      tenv


let set_tenv_global_for_testing tenv = DLS.set tenv_key (Some tenv)
