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


let exec_summary_of_post_common tenv ~continue_program proc_desc (exec_astate : ExecutionDomain.t) :
    _ ExecutionDomain.base_t option =
  match exec_astate with
  | ContinueProgram astate -> (
    match AbductiveDomain.summary_of_post tenv proc_desc astate with
    | Unsat ->
        None
    | Sat astate ->
        Some (continue_program astate) )
  (* already a summary but need to reconstruct the variants to make the type system happy :( *)
  | AbortProgram astate ->
      Some (AbortProgram astate)
  | ExitProgram astate ->
      Some (ExitProgram astate)
  | LatentAbortProgram {astate; latent_issue} ->
      Some (LatentAbortProgram {astate; latent_issue})
  | ISLLatentMemoryError astate ->
      Some (ISLLatentMemoryError astate)


let force_exit_program tenv proc_desc post =
  exec_summary_of_post_common tenv proc_desc post ~continue_program:(fun astate ->
      ExitProgram astate )


let of_posts tenv proc_desc posts =
  List.filter_mapi posts ~f:(fun i exec_state ->
      L.d_printfln "Creating spec out of state #%d:@\n%a" i ExecutionDomain.pp exec_state ;
      exec_summary_of_post_common tenv proc_desc exec_state ~continue_program:(fun astate ->
          ContinueProgram astate ) )
