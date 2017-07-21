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

include TaintAnalysis.Make (struct
  module Trace = JavaTrace
  module AccessTree = AccessTree.Make (Trace)

  let to_summary_access_tree access_tree = QuandarySummary.AccessTree.Java access_tree

  let of_summary_access_tree = function
    | QuandarySummary.AccessTree.Java access_tree
     -> access_tree
    | _
     -> assert false

  let handle_unknown_call pname ret_typ_opt actuals tenv =
    let get_receiver_typ tenv = function
      | HilExp.AccessPath access_path
       -> AccessPath.Raw.get_typ access_path tenv
      | _
       -> None
    in
    let types_match typ class_string tenv =
      match typ with
      | Some {Typ.desc= Typ.Tptr ({desc= Tstruct original_typename}, _)}
       -> PatternMatch.supertype_exists tenv
            (fun typename _ -> String.equal (Typ.Name.name typename) class_string)
            original_typename
      | _
       -> false
    in
    match pname with
    | Typ.Procname.Java java_pname as pname
     -> (
        let is_static = Typ.Procname.java_is_static pname in
        match
          ( Typ.Procname.java_get_class_name java_pname
          , Typ.Procname.java_get_method java_pname
          , ret_typ_opt )
        with
        | "android.content.Intent", ("putExtra" | "putExtras"), _
         -> (* don't care about tainted extras. instead. we'll check that result of getExtra is
                   always used safely *)
            []
        | _ when Typ.Procname.is_constructor pname
         -> [TaintSpec.Propagate_to_receiver]
        | _, _, (Some {Typ.desc= Tvoid} | None) when not is_static
         -> (* for instance methods with no return value, propagate the taint to the receiver *)
            [TaintSpec.Propagate_to_receiver]
        | classname, _, Some {Typ.desc= Tptr _ | Tstruct _} -> (
          match actuals with
          | receiver_exp :: _
            when not is_static && types_match (get_receiver_typ tenv receiver_exp) classname tenv
           -> (* if the receiver and return type are the same, propagate to both. we're
                         assuming the call is one of the common "builder-style" methods that both
                         updates and returns the receiver *)
              [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
          | _
           -> (* receiver doesn't match return type; just propagate to the return type *)
              [TaintSpec.Propagate_to_return] )
        | _
         -> [] )
    | pname when BuiltinDecl.is_declared pname
     -> []
    | pname
     -> failwithf "Non-Java procname %a in Java analysis@." Typ.Procname.pp pname

  let get_model _ _ _ _ _ = None

  let external_sanitizers =
    List.map
      ~f:(fun {QuandaryConfig.Sanitizer.procedure} -> Str.regexp procedure)
      (QuandaryConfig.Sanitizer.of_json Config.quandary_sanitizers)

  let get_sanitizer = function
    | Typ.Procname.Java java_pname
     -> let procedure_string =
          Printf.sprintf "%s.%s" (Typ.Procname.java_get_class_name java_pname)
            (Typ.Procname.java_get_method java_pname)
        in
        List.find_map
          ~f:(fun procedure_regex ->
            if Str.string_match procedure_regex procedure_string 0 then Some TaintSpec.Return
            else None)
          external_sanitizers
    | _
     -> None

  let is_taintable_type typ =
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct JavaClass typename}, _) | Tstruct JavaClass typename -> (
      match Mangled.to_string_full typename with
      | "android.content.Intent" | "android.net.Uri" | "java.lang.String" | "java.net.URI"
       -> true
      | _
       -> false )
    | _
     -> false
end)
