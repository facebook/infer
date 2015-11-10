(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging


let calls_expensive_method =
  "CHECKERS_CALLS_EXPENSIVE_METHOD"

let expensive_overrides_unexpensive =
  "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"

let unannotated_overrides_performance_critical =
  "CHECKERS_UNANNOTATED_OVERRIDES_PERFOMANCE_CRITICAL"

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


let check_attributes check attributes =
  let annotated_signature = Annotations.get_annotated_signature attributes in
  let ret_annotation, _ = annotated_signature.Annotations.ret in
  check ret_annotation


let is_performance_critical attributes =
  check_attributes Annotations.ia_is_performance_critical attributes


let is_expensive attributes =
  check_attributes Annotations.ia_is_expensive attributes


let check_method check pname =
  match AttributesTable.load_attributes pname with
  | None -> false
  | Some attributes -> check_attributes check attributes


let method_is_performance_critical pname =
  check_method Annotations.ia_is_performance_critical pname


let method_is_expensive pname =
  check_method Annotations.ia_is_expensive pname


let callback_performance_checker _ _ _ tenv pname pdesc =

  let loc = Cfg.Procdesc.get_loc pdesc in
  let attributes = Cfg.Procdesc.get_attributes pdesc in
  let expensive = is_expensive attributes
  and performance_critical = is_performance_critical attributes in
  let expensive_call_found =
    let checked_pnames = ref Procname.Set.empty in
    Cfg.Procdesc.fold_calls
      (search_expensive_call checked_pnames)
      None
      pdesc in

  let check_expensive_subtyping_rules overridden_pname =
    if not (method_is_expensive overridden_pname) then
      let description =
        Printf.sprintf
          "Method %s overrides unannotated method %s and cannot be annotated with @%s"
          (Procname.to_string pname)
          (Procname.to_string overridden_pname)
          Annotations.expensive in
      Checkers.ST.report_error
        pname pdesc expensive_overrides_unexpensive loc description

  and check_performance_critical_subtyping_rules overridden_pname =
    if method_is_performance_critical overridden_pname then
      let description =
        Printf.sprintf
          "Method %s overrides method %s annotated with %s and should also be annotated"
          (Procname.to_string pname)
          (Procname.to_string overridden_pname)
          Annotations.performance_critical in
      Checkers.ST.report_error
        pname pdesc unannotated_overrides_performance_critical loc description in

  if expensive then
    PatternMatch.proc_iter_overridden_methods check_expensive_subtyping_rules tenv pname;
  if not performance_critical then
    PatternMatch.proc_iter_overridden_methods check_performance_critical_subtyping_rules tenv pname;

  match expensive_call_found with
  | None -> ()
  | Some callee_pname when performance_critical ->
      let description =
        Printf.sprintf "Method %s annotated with @%s calls method %s annotated with @%s"
          (Procname.to_simplified_string pname)
          Annotations.performance_critical
          (Procname.to_string callee_pname)
          Annotations.expensive in
      Checkers.ST.report_error
        pname pdesc calls_expensive_method loc description
  | Some _ when not expensive ->
      let ret_annot, param_annot = attributes.ProcAttributes.method_annotation in
      let updated_method_annot =
        (Annotations.expensive_annotation, true) :: ret_annot, param_annot in
      let updated_attributes =
        { attributes with ProcAttributes.method_annotation = updated_method_annot } in
      AttributesTable.store_attributes updated_attributes
  | Some _ -> () (* Nothing to do if method already annotated with @Expensive *)
