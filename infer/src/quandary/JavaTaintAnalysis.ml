(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

include TaintAnalysis.Make (struct
  module Trace = JavaTrace
  module AccessTree = AccessTree.Make (Trace) (AccessTree.DefaultConfig)

  let to_summary_access_tree access_tree = QuandarySummary.AccessTree.Java access_tree

  let of_summary_access_tree = function
    | QuandarySummary.AccessTree.Java access_tree ->
        access_tree
    | _ ->
        assert false


  let handle_unknown_call pname ret_typ actuals tenv =
    let rec get_receiver_typ tenv = function
      | HilExp.Cast (_, e) ->
          get_receiver_typ tenv e
      | HilExp.AccessExpression access_expr ->
          AccessPath.get_typ (HilExp.AccessExpression.to_access_path access_expr) tenv
      | _ ->
          None
    in
    let types_match typ class_string tenv =
      match typ with
      | Some {Typ.desc= Typ.Tptr ({desc= Tstruct original_typename}, _)} ->
          PatternMatch.supertype_exists tenv
            (fun typename _ -> String.equal (Typ.Name.name typename) class_string)
            original_typename
      | _ ->
          false
    in
    match pname with
    | Typ.Procname.Java java_pname -> (
        let is_static = Typ.Procname.Java.is_static java_pname in
        match
          ( Typ.Procname.Java.get_class_name java_pname
          , Typ.Procname.Java.get_method java_pname
          , ret_typ )
        with
        | "android.content.Intent", ("putExtra" | "putExtras"), _ ->
            (* don't care about tainted extras. instead. we'll check that result of getExtra is
                   always used safely *)
            []
        | _ when Typ.Procname.is_constructor pname ->
            [TaintSpec.Propagate_to_receiver]
        | _, _, {Typ.desc= Tvoid | Tint _ | Tfloat _} when not is_static ->
            (* for instance methods with a non-Object return value, propagate the taint to the
               receiver *)
            [TaintSpec.Propagate_to_receiver]
        | classname, _, {Typ.desc= Tptr _ | Tstruct _} -> (
          match actuals with
          | receiver_exp :: _
            when (not is_static) && types_match (get_receiver_typ tenv receiver_exp) classname tenv
            ->
              (* if the receiver and return type are the same, propagate to both. we're
                         assuming the call is one of the common "builder-style" methods that both
                         updates and returns the receiver *)
              [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
          | _ ->
              (* receiver doesn't match return type; just propagate to the return type *)
              [TaintSpec.Propagate_to_return] )
        | _ ->
            [] )
    | pname when BuiltinDecl.is_declared pname ->
        []
    | pname ->
        L.(die InternalError) "Non-Java procname %a in Java analysis" Typ.Procname.pp pname


  let get_model _ _ _ _ _ = None

  let is_taintable_type typ =
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct (JavaClass typename)}, _) | Tstruct (JavaClass typename) -> (
      match Mangled.to_string_full typename with
      | "android.content.Intent" | "android.net.Uri" | "java.lang.String" | "java.net.URI" ->
          true
      | _ ->
          false )
    | _ ->
        false


  let name = "java"
end)
