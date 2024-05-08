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
    [ "module_name:function_name/42$arg14"
    ; "mod:fun/2$ret"
    ; "fun_name/3$arg0"
    ; "fun/10$ret"
    ; "noarity$ret"
    ; "onlyname"
    ; "mod:funonly"
    ; "noargindex$arg" ]
  in
  let pp_item fmt item =
    match LineageTaint.Private.parse_node item with
    | None ->
        Fmt.pf fmt "INVALID(%s)" item
    | Some (procname, vertex) ->
        Fmt.pf fmt "(%a, %a)" Procname.pp_verbose procname Lineage.Vertex.pp vertex
  in
  Fmt.pr "%a" (Fmt.list ~sep:(Fmt.any " ") pp_item) items ;
  [%expect
    "(module_name:function_name/42, arg14) (mod:fun/2, ret) (:fun_name/3, arg0) (:fun/10, ret) \
     INVALID(noarity$ret) INVALID(onlyname) INVALID(mod:funonly) INVALID(noargindex$arg)"]
