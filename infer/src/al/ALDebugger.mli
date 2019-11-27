(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module EvaluationTracker : sig
  type eval_result = Eval_undefined | Eval_true | Eval_false

  type content =
    { ast_node: Ctl_parser_types.ast_node
    ; phi: CTLTypes.t
    ; lcxt: CLintersContext.context
    ; eval_result: eval_result
    ; witness: Ctl_parser_types.ast_node option }

  type eval_node = {id: int; content: content}

  type tree = Tree of eval_node * tree list

  type ast_node_to_display =
    (* the node can be used to describe further sub calls in the evaluation stack *)
    | Carry_forward of Ctl_parser_types.ast_node
    (* the node cannot be further used to describe sub calls in the evaluation stack *)
    | Last_occurrence of Ctl_parser_types.ast_node

  type t =
    { next_id: int
    ; eval_stack: (tree * ast_node_to_display) Stack.t
    ; forest: tree list
    ; breakpoint_line: int option
    ; debugger_active: bool }

  val create : SourceFile.t -> t

  val create_content : Ctl_parser_types.ast_node -> CTLTypes.t -> CLintersContext.context -> content

  val eval_begin : t -> content -> t

  val eval_end : t -> Ctl_parser_types.ast_node option -> t

  module DottyPrinter : sig
    val dotty_of_ctl_evaluation : t -> string
  end
end
