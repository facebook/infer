(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let%expect_test "parse_node" =
  let items =
    [ "module_name:function_name/42.arg14"
    ; "mod:fun/2.ret"
    ; "fun_name/3.arg0"
    ; "fun/10.ret"
    ; "noarity.ret"
    ; "onlyname"
    ; "mod:funonly"
    ; "noargindex.arg" ]
  in
  let pp_item fmt item =
    match LineageTaint.Private.TaintConfig.parse_endpoint item with
    | None ->
        Fmt.pf fmt "INVALID(%s)" item
    | Some {procname; node} ->
        Fmt.pf fmt "(%a, %a)" Procname.pp_verbose procname
          LineageTaint.Private.TaintConfig.Endpoint.pp_node node
  in
  Fmt.pr "%a" (Fmt.list ~sep:(Fmt.any " ") pp_item) items ;
  [%expect
    "(module_name:function_name/42, arg14) (mod:fun/2, ret) (erlang:fun_name/3, arg0) \
     (erlang:fun/10, ret) INVALID(noarity.ret) INVALID(onlyname) INVALID(mod:funonly) \
     INVALID(noargindex.arg)"]
