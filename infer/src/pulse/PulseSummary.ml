(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface

type t = ExecutionDomain.summary list [@@deriving yojson_of]

let pp fmt pre_posts =
  F.open_vbox 0 ;
  F.fprintf fmt "%d pre/post(s)@;" (List.length pre_posts) ;
  List.iteri pre_posts ~f:(fun i (pre_post : ExecutionDomain.summary) ->
      F.fprintf fmt "#%d: @[%a@]@;" i ExecutionDomain.pp (pre_post :> ExecutionDomain.t) ) ;
  F.close_box ()


let exec_summary_of_post_common tenv ~continue_program proc_desc err_log location
    (exec_astate : ExecutionDomain.t) : _ ExecutionDomain.base_t SatUnsat.t =
  match exec_astate with
  | ExceptionRaised _ ->
      Unsat (* we do not propagate exception interproceduraly yet *)
  | ContinueProgram astate -> (
      let open SatUnsat.Import in
      let+ summary_result =
        AbductiveDomain.Summary.of_post tenv
          (Procdesc.get_proc_name proc_desc)
          (Procdesc.get_attributes proc_desc)
          location astate
      in
      match (summary_result : _ result) with
      | Ok astate ->
          continue_program astate
      | Error (`RetainCycle (astate, assignment_traces, value, path, location)) ->
          PulseReport.report_summary_error tenv proc_desc err_log
            (ReportableErrorSummary
               {astate; diagnostic= RetainCycle {assignment_traces; value; path; location}} )
          |> Option.value ~default:(ExecutionDomain.ContinueProgram astate)
      | Error (`MemoryLeak (astate, allocator, allocation_trace, location)) ->
          PulseReport.report_summary_error tenv proc_desc err_log
            (ReportableErrorSummary
               {astate; diagnostic= MemoryLeak {allocator; allocation_trace; location}} )
          |> Option.value ~default:(ExecutionDomain.ContinueProgram astate)
      | Error (`ResourceLeak (astate, class_name, allocation_trace, location)) ->
          PulseReport.report_summary_error tenv proc_desc err_log
            (ReportableErrorSummary
               {astate; diagnostic= ResourceLeak {class_name; allocation_trace; location}} )
          |> Option.value ~default:(ExecutionDomain.ContinueProgram astate)
      | Error
          (`PotentialInvalidAccessSummary
            ((astate : AbductiveDomain.Summary.t), address, must_be_valid) ) -> (
        match
          let open IOption.Let_syntax in
          let* addr = DecompilerExpr.abstract_value_of_expr address in
          let* _, attrs = AbductiveDomain.find_post_cell_opt addr (astate :> AbductiveDomain.t) in
          Attributes.get_invalid attrs
        with
        | None ->
            ExecutionDomain.LatentInvalidAccess {astate; address; must_be_valid; calling_context= []}
        | Some (invalidation, invalidation_trace) ->
            (* NOTE: this probably leads to the error being dropped as the access trace is unlikely to
               contain the reason for invalidation and thus we will filter out the report. TODO:
               figure out if that's a problem. *)
            PulseReport.report_summary_error tenv proc_desc err_log
              (ReportableErrorSummary
                 { diagnostic=
                     AccessToInvalidAddress
                       { calling_context= []
                       ; invalid_address= address
                       ; invalidation
                       ; invalidation_trace
                       ; access_trace= fst must_be_valid
                       ; must_be_valid_reason= snd must_be_valid }
                 ; astate } )
            |> Option.value ~default:(ExecutionDomain.ContinueProgram astate) ) )
  (* already a summary but need to reconstruct the variants to make the type system happy :( *)
  | AbortProgram astate ->
      Sat (AbortProgram astate)
  | ExitProgram astate ->
      Sat (ExitProgram astate)
  | LatentAbortProgram {astate; latent_issue} ->
      Sat (LatentAbortProgram {astate; latent_issue})
  | LatentInvalidAccess {astate; address; must_be_valid; calling_context} ->
      Sat (LatentInvalidAccess {astate; address; must_be_valid; calling_context})
  | ISLLatentMemoryError astate ->
      Sat (ISLLatentMemoryError astate)


let force_exit_program tenv proc_desc err_log post =
  exec_summary_of_post_common tenv proc_desc err_log post ~continue_program:(fun astate ->
      ExitProgram astate )


let of_posts tenv proc_desc err_log location posts =
  List.filter_mapi posts ~f:(fun i exec_state ->
      L.d_printfln "Creating spec out of state #%d:@\n%a" i ExecutionDomain.pp exec_state ;
      exec_summary_of_post_common tenv proc_desc err_log location exec_state
        ~continue_program:(fun astate -> ContinueProgram astate)
      |> SatUnsat.sat )
