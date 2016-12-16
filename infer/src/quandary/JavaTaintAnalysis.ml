(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

include
  TaintAnalysis.Make(struct
    module Trace = JavaTrace
    module AccessTree = AccessTree.Make(Trace)

    let to_summary_access_tree access_tree = QuandarySummary.AccessTree.Java access_tree

    let of_summary_access_tree = function
      | QuandarySummary.AccessTree.Java access_tree -> access_tree
      | _ -> assert false

    let handle_unknown_call pname ret_typ_opt =
      match pname with
      | (Procname.Java java_pname) as pname ->
          begin
            match Procname.java_get_class_name java_pname,
                  Procname.java_get_method java_pname,
                  ret_typ_opt with
            | _ when Procname.is_constructor pname ->
                [TaintSpec.Propagate_to_receiver]
            | ("java.lang.StringBuffer" | "java.lang.StringBuilder" | "java.util.Formatter"), _,
              Some _
              when not (Procname.java_is_static pname) ->
                [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
            | _, _, Some _ ->
                [TaintSpec.Propagate_to_return]
            | _ ->
                []
          end
      | pname when BuiltinDecl.is_declared pname ->
          []
      | pname ->
          failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname
  end)
