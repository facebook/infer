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

let pp fmt summary =
  F.open_vbox 0 ;
  F.fprintf fmt "%d pre/post(s)@;" (List.length summary) ;
  List.iteri summary ~f:(fun i (pre_post : ExecutionDomain.summary) ->
      F.fprintf fmt "#%d: @[%a@]@;" i ExecutionDomain.pp (pre_post :> ExecutionDomain.t) ) ;
  F.close_box ()


let exec_summary_of_post_common tenv ~continue_program proc_desc err_log
    (exec_astate : ExecutionDomain.t) : _ ExecutionDomain.base_t option =
  match exec_astate with
  | ContinueProgram astate -> (
    match AbductiveDomain.summary_of_post tenv proc_desc astate with
    | Unsat ->
        None
    | Sat (Ok astate) ->
        Some (continue_program astate)
    | Sat (Error (`PotentialInvalidAccessSummary (astate, address, must_be_valid))) -> (
      match
        AbductiveDomain.find_post_cell_opt address (astate :> AbductiveDomain.t)
        |> Option.bind ~f:(fun (_, attrs) -> Attributes.get_invalid attrs)
      with
      | None ->
          Some (LatentInvalidAccess {astate; address; must_be_valid; calling_context= []})
      | Some (invalidation, invalidation_trace) ->
          PulseReport.report_summary_error tenv proc_desc err_log
            (ReportableError
               { diagnostic=
                   AccessToInvalidAddress
                     { calling_context= []
                     ; invalidation
                     ; invalidation_trace
                     ; access_trace= must_be_valid }
               ; astate })
          |> Option.some ) )
  (* already a summary but need to reconstruct the variants to make the type system happy :( *)
  | AbortProgram astate ->
      Some (AbortProgram astate)
  | ExitProgram astate ->
      Some (ExitProgram astate)
  | LatentAbortProgram {astate; latent_issue} ->
      Some (LatentAbortProgram {astate; latent_issue})
  | LatentInvalidAccess {astate; address; must_be_valid; calling_context} ->
      Some (LatentInvalidAccess {astate; address; must_be_valid; calling_context})
  | ISLLatentMemoryError astate ->
      Some (ISLLatentMemoryError astate)


let force_exit_program tenv proc_desc err_log post =
  exec_summary_of_post_common tenv proc_desc err_log post ~continue_program:(fun astate ->
      ExitProgram astate )


let of_posts tenv proc_desc err_log posts =
  List.filter_mapi posts ~f:(fun i exec_state ->
      L.d_printfln "Creating spec out of state #%d:@\n%a" i ExecutionDomain.pp exec_state ;
      exec_summary_of_post_common tenv proc_desc err_log exec_state ~continue_program:(fun astate ->
          ContinueProgram astate ) )
