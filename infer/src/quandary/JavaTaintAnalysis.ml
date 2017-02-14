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

    let handle_unknown_call pname ret_typ_opt actuals tenv =
      let types_match typ class_string tenv = match typ with
        | Typ.Tptr (Tstruct original_typename, _) ->
            PatternMatch.supertype_exists
              tenv
              (fun typename _ -> String.equal (Typename.name typename) class_string)
              original_typename
        | _ ->
            false in
      match pname with
      | (Procname.Java java_pname) as pname ->
          let is_static = Procname.java_is_static pname in
          begin
            match Procname.java_get_class_name java_pname,
                  Procname.java_get_method java_pname,
                  ret_typ_opt with
            | _ when Procname.is_constructor pname ->
                [TaintSpec.Propagate_to_receiver]
            | _, _, (Some Typ.Tvoid | None) when not is_static ->
                (* for instance methods with no return value, propagate the taint to the receiver *)
                [TaintSpec.Propagate_to_receiver]
            | classname, _, Some (Typ.Tptr _ | Tstruct _) ->
                begin
                  match actuals with
                  | (_, receiver_typ) :: _
                    when not is_static && types_match receiver_typ classname tenv ->
                      (* if the receiver and return type are the same, propagate to both. we're
                         assuming the call is one of the common "builder-style" methods that both
                         updates and returns the receiver *)
                      [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
                  | _ ->
                      (* receiver doesn't match return type; just propagate to the return type *)
                      [TaintSpec.Propagate_to_return]
                end
            | _ ->
                []
          end
      | pname when BuiltinDecl.is_declared pname ->
          []
      | pname ->
          failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname
  end)
