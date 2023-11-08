(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface
open PulseOperationResult.Import

module Config : sig
  val must_be_monitored : Fieldname.t -> bool

  val is_initial_caller : Tenv.t -> Procname.t -> bool
end = struct
  let fieldnames_to_monitor = ["GlobalVARIABLES"]

  let must_be_monitored fieldname =
    List.exists fieldnames_to_monitor ~f:(String.equal (Fieldname.get_field_name fieldname))


  let initial_caller_class_extends = "hack GlobalAccess::EventHandler"

  let initial_caller_class_does_not_extend = ["hack GlobalAccess::Unsafe"]

  let is_initial_caller tenv procname =
    (let open IOption.Let_syntax in
     let+ type_name = Procname.get_class_type_name procname in
     let parents =
       Tenv.fold_supers tenv type_name ~init:String.Set.empty ~f:(fun name _ set ->
           String.Set.add set (Typ.Name.to_string name) )
     in
     String.Set.mem parents initial_caller_class_extends
     && List.for_all initial_caller_class_does_not_extend ~f:(fun str ->
            not (String.Set.mem parents str) ) )
    |> Option.value ~default:false
end

let record_load rhs_exp location astates =
  match rhs_exp with
  | Exp.Lfield (_, fieldname, _) when Config.must_be_monitored fieldname ->
      List.map astates ~f:(function
        | ContinueProgram astate ->
            ContinueProgram (AbductiveDomain.record_transitive_access location astate)
        | execstate ->
            execstate )
  | _ ->
      astates


let report_errors tenv proc_desc err_log summary =
  let procname = Procdesc.get_proc_name proc_desc in
  if Config.is_initial_caller tenv procname then
    List.iter summary ~f:(function
      | ContinueProgram astate ->
          AbductiveDomain.Summary.get_transitive_accesses astate
          |> List.iter ~f:(fun call_trace ->
                 PulseReport.report ~is_suppressed:false ~latent:false tenv proc_desc err_log
                   (Diagnostic.TransitiveAccess {call_trace}) )
      | _ ->
          () )
