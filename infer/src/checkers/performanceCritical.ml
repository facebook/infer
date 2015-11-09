(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging


let calls_expensive_method = "CHECKERS_CALLS_EXPENSIVE_METHOD"


let search_expensive_call checked_pnames expensive_callee (pname, _) =
  match expensive_callee with
  | Some callee_pname -> Some callee_pname
  | None ->
      if Procname.Set.mem pname !checked_pnames then None
      else
        begin
          checked_pnames := Procname.Set.add pname !checked_pnames;
          match AttributesTable.load_attributes pname with
          | None -> None
          | Some attributes ->
              let annotated_signature = Annotations.get_annotated_signature attributes in
              let ret_annotation, _ = annotated_signature.Annotations.ret in
              if Annotations.ia_is_expensive ret_annotation then
                Some pname
              else
                None
        end


let is_performance_critical attributes =
  let annotated_signature = Annotations.get_annotated_signature attributes in
  let ret_annotation, _ = annotated_signature.Annotations.ret in
  Annotations.ia_is_performance_critical ret_annotation


let callback_performance_checker _ _ _ tenv pname pdesc : unit =
  let attributes = Cfg.Procdesc.get_attributes pdesc in
  let expensive_call_found =
    let checked_pnames = ref Procname.Set.empty in
    Cfg.Procdesc.fold_calls
      (search_expensive_call checked_pnames)
      None
      pdesc in
  match expensive_call_found with
  | None -> ()
  | Some callee_pname when is_performance_critical attributes ->
      let description =
        Printf.sprintf "Method %s annotated with @%s calls method %s annotated with @%s"
          (Procname.to_simplified_string pname)
          Annotations.performance_critical
          (Procname.to_string callee_pname)
          Annotations.expensive in
      Checkers.ST.report_error
        pname pdesc calls_expensive_method (Cfg.Procdesc.get_loc pdesc) description
  | Some _ -> ()
