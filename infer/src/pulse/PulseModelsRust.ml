(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport
module DSL = PulseModelsDSL

let retag (dst_exp : Exp.t) (src_exp : Exp.t) (is_mut_exp : Exp.t) : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  if not (Config.is_checker_enabled TreeBorrows) then ret ()
  else
    let is_mut =
      match is_mut_exp with Exp.Const (Const.Cint i) -> not (IntLit.iszero i) | _ -> false
    in
    exec_command (PulseTreeBorrowsOperations.exec_retag ~dst_exp ~src_exp ~is_mut)


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [-"__rust_retag" <>$ capt_exp $+ capt_exp $+ capt_exp $--> retag]
