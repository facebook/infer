(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Env = ErlangEnvironment
module L = Logging
module Node = ErlangNode

type t = {start: Procdesc.Node.t; exit_success: Procdesc.Node.t; exit_failure: Procdesc.Node.t}

(* NOTE: Because this function has side-effects, and because it is common to want to set successors
   for both [exit_success] and [exit_failure], it is dangerous to alias [exit_success] with
   [exit_failure]. Better to use some dummy NOP nodes. *)
let ( |~~> ) from to_ = Procdesc.set_succs from ~normal:(Some to_) ~exn:None

let make_success_general env exit_success =
  {start= exit_success; exit_success; exit_failure= Node.make_nop env}


let make_success env = make_success_general env (Node.make_nop env)

let make_stuck env = make_success_general env (Node.make_if env true Exp.zero)

let make_fail env fail_function = make_success_general env (Node.make_fail env fail_function)

let make_failure env =
  let exit_success, exit_failure = (Node.make_nop env, Node.make_nop env) in
  {start= exit_failure; exit_success; exit_failure}


(** Makes one block of a list of blocks. Meant to be used only by the functions [all] and [any]
    defined immediately below. If [b] comes before [c] in the list [blocks], then an edge is added
    from [continue b] to [c.start]. For all blocks [b] in the list [blocks], an edge is added from
    [stop b] to [new_stop], where [new_stop] is a new node of type join. If there is only one block,
    then it is returned with no modification.*)
let sequence ~(continue : t -> Procdesc.Node.t) ~(stop : t -> Procdesc.Node.t) env (blocks : t list)
    =
  match blocks with
  | [] ->
      L.die InternalError "blocks should not be empty"
  | [one_block] ->
      (one_block.start, continue one_block, stop one_block)
  | first_block :: next_blocks ->
      let continue_node =
        let f previous b =
          previous |~~> [b.start] ;
          continue b
        in
        List.fold ~f ~init:(continue first_block) next_blocks
      in
      let new_stop = Node.make_join env in
      List.iter ~f:(fun b -> stop b |~~> [new_stop]) blocks ;
      (first_block.start, continue_node, new_stop)


let all env (blocks : t list) : t =
  match blocks with
  | [] ->
      make_success env
  | _ ->
      let continue b = b.exit_success in
      let stop b = b.exit_failure in
      let start, exit_success, exit_failure = sequence ~continue ~stop env blocks in
      {start; exit_success; exit_failure}


let any env (blocks : t list) : t =
  match blocks with
  | [] ->
      make_failure env
  | _ ->
      let continue b = b.exit_failure in
      let stop b = b.exit_success in
      let start, exit_failure, exit_success = sequence ~continue ~stop env blocks in
      {start; exit_success; exit_failure}


let make_instruction env ?(kind = Procdesc.Node.ErlangExpression) instructions =
  let exit_success = Node.make_stmt env ~kind instructions in
  let exit_failure = Node.make_nop env in
  {start= exit_success; exit_success; exit_failure}


let make_load env id e typ =
  let exit_success = Node.make_load env id e typ in
  let exit_failure = Node.make_nop env in
  {start= exit_success; exit_success; exit_failure}


let make_branch env condition =
  let start = Node.make_nop env in
  let exit_success = Node.make_if env true condition in
  let exit_failure = Node.make_if env false condition in
  start |~~> [exit_success; exit_failure] ;
  {start; exit_success; exit_failure}
