(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Utility methods to support the translation of clang ast constructs into sil instructions.  *)

type continuation = {
  break: Cfg.Node.t list;
  continue: Cfg.Node.t list;
  return_temp : bool; (* true if temps should not be removed in the node but returned to ancestors *)
}

type priority_node =
  | Free
  | Busy of Clang_ast_t.pointer

type trans_state = {
  context: CContext.t;
  succ_nodes: Cfg.Node.t list;
  continuation: continuation option;
  priority: priority_node;
  var_exp_typ: (Sil.exp * Sil.typ) option;
}

type trans_result = {
  root_nodes: Cfg.Node.t list;
  leaf_nodes: Cfg.Node.t list;
  ids: Ident.t list;
  instrs: Sil.instr list;
  exps: (Sil.exp * Sil.typ) list;
  initd_exps: Sil.exp list;
  is_cpp_call_virtual : bool;
}

val empty_res_trans: trans_result

val collect_res_trans : trans_result list -> trans_result

val extract_var_exp_or_fail : trans_state -> Sil.exp * Sil.typ

val is_return_temp: continuation option -> bool

val ids_to_parent: continuation option -> Ident.t list -> Ident.t list

val ids_to_node: continuation option -> Ident.t list -> Ident.t list

val mk_cond_continuation : continuation option -> continuation option

val extract_item_from_singleton : 'a list -> string -> 'a -> 'a

val extract_exp_from_list : (Sil.exp * Sil.typ) list -> string -> (Sil.exp * Sil.typ)

val fix_param_exps_mismatch : 'a list -> (Sil.exp * Sil.typ) list -> (Sil.exp * Sil.typ)list

val get_selector_receiver : Clang_ast_t.obj_c_message_expr_info -> string * Clang_ast_t.receiver_kind

val define_condition_side_effects :
  (Sil.exp * Sil.typ) list -> Sil.instr list -> Location.t ->
  (Sil.exp * Sil.typ) list * Sil.instr list

val extract_stmt_from_singleton : Clang_ast_t.stmt list -> string -> Clang_ast_t.stmt

val is_null_stmt : Clang_ast_t.stmt -> bool

val is_enumeration_constant : Clang_ast_t.stmt -> bool

val is_member_exp : Clang_ast_t.stmt -> bool

val get_type_from_exp_stmt : Clang_ast_t.stmt -> Clang_ast_t.type_ptr

(** Given trans_result with ONE expression, create temporary variable with *)
(** dereferenced value of an expression assigned to it *)
val dereference_value_from_result : Location.t -> trans_result -> strip_pointer:bool -> trans_result

val cast_operation :
  CContext.t -> Clang_ast_t.cast_kind -> (Sil.exp * Sil.typ) list -> Sil.typ -> Location.t ->
  bool -> Ident.t list * Sil.instr list * (Sil.exp * Sil.typ)

val trans_assertion_failure : Location.t -> CContext.t -> trans_result

val trans_assume_false : Location.t -> CContext.t -> Cfg.Node.t list ->  trans_result

val is_owning_method : Clang_ast_t.stmt -> bool

val is_owning_name : string -> bool

val is_method_call : Clang_ast_t.stmt -> bool

val contains_opaque_value_expr : Clang_ast_t.stmt -> bool

val get_info_from_decl_ref : Clang_ast_t.decl_ref ->
  Clang_ast_t.named_decl_info * Clang_ast_t.pointer * Clang_ast_t.type_ptr

val get_decl_ref_info : Clang_ast_t.stmt -> Clang_ast_t.decl_ref

val builtin_trans : trans_state -> Location.t -> Clang_ast_t.stmt_info ->
  Sil.typ -> Procname.t option -> trans_result option

val alloc_trans :
  trans_state -> Location.t -> Clang_ast_t.stmt_info -> Sil.typ -> bool -> trans_result

val new_or_alloc_trans : trans_state -> Location.t -> Clang_ast_t.stmt_info ->
  Clang_ast_t.type_ptr -> string option -> string -> trans_result

val cpp_new_trans : trans_state -> Location.t -> Sil.typ -> Sil.exp option -> trans_result

val cast_trans :
  CContext.t -> (Sil.exp * Sil.typ) list -> Location.t -> Procname.t option -> Sil.typ ->
  (Ident.t * Sil.instr * Sil.exp) option

val dereference_var_sil : Sil.exp * Sil.typ -> Location.t -> Ident.t list * Sil.instr list * Sil.exp

(** Module for creating cfg nodes and other utility functions related to them.  *)
module Nodes :
sig
  val is_binary_assign_op : Clang_ast_t.binary_operator_info -> bool

  val need_unary_op_node : Clang_ast_t.unary_operator_info -> bool

  val create_node : Cfg.Node.nodekind -> Ident.t list -> Sil.instr list ->
    Location.t -> CContext.t -> Cfg.Node.t

  val is_join_node : Cfg.Node.t -> bool

  val create_prune_node :
    bool -> (Sil.exp * Sil.typ) list -> Ident.t list -> Sil.instr list -> Location.t ->
    Sil.if_kind -> CContext.t -> Cfg.Node.t

  val is_prune_node : Cfg.Node.t -> bool

  val is_true_prune_node : Cfg.Node.t -> bool

  val prune_kind : bool -> Cfg.Node.nodekind

end

(** priority_node is used to enforce some kind of policy for creating nodes *)
(** in the cfg. Certain elements of the AST _must_ create nodes therefore   *)
(** there is no need for them to use priority_node. Certain elements        *)
(** instead need or need not to create a node depending of certain factors. *)
(** When an element of the latter kind wants to create a node it must claim *)
(** priority first (like taking a lock). priority can be claimes only when  *)
(** it is free. If an element of AST succedes in claiming priority its id   *)
(** (pointer) is recorded in priority. After an element has finished it     *)
(** frees the priority. In general an AST element E checks if an ancestor   *)
(** has claimed priority. If priority is already claimed E does not have to *)
(** create a node. If priority is free then it means E has to create the    *)
(** node. Then E claims priority and release it afterward.                  *)
module PriorityNode :
sig

  type t = priority_node

  val is_priority_free : trans_state -> bool

  val try_claim_priority_node : trans_state -> Clang_ast_t.stmt_info -> trans_state

  val force_claim_priority_node : trans_state -> Clang_ast_t.stmt_info -> trans_state

  val own_priority_node : t -> Clang_ast_t.stmt_info -> bool

  (* Used by translation functions to handle potenatial cfg nodes. *)
  (* It connects nodes returned by translation of stmt children and *)
  (* deals with creating or not a cfg node depending of owning the *)
  (* priority_node. It returns nodes, ids, instrs that should be passed to parent *)
  val compute_results_to_parent : trans_state -> Location.t -> string -> Clang_ast_t.stmt_info ->
    trans_result list -> trans_result

end

(** Module for translating goto instructions by keeping a map of labels. *)
module GotoLabel :
sig
  val find_goto_label : CContext.t -> string -> Location.t -> Cfg.Node.t

  val reset_all_labels : unit -> unit

end

(** Module that provides utility functions for translating different types of loops. *)
module Loops :
sig
  type loop_kind =
    | For of Clang_ast_t.stmt * Clang_ast_t.stmt * Clang_ast_t.stmt * Clang_ast_t.stmt * Clang_ast_t.stmt
    (* init, decl_stmt, condition, increment and body *)
    | While of Clang_ast_t.stmt option * Clang_ast_t.stmt * Clang_ast_t.stmt
    (* decl_stmt, condition and body *)
    | DoWhile of Clang_ast_t.stmt * Clang_ast_t.stmt  (* condition and body *)

  val loop_kind_to_if_kind : loop_kind -> Sil.if_kind

  val get_cond : loop_kind -> Clang_ast_t.stmt

  val get_body : loop_kind -> Clang_ast_t.stmt

end

(** This module handles the translation of the variable self which is challenging because self *)
(** is used both as a variable in instance method calls and also as a type in class method calls. *)
module Self :
sig

  exception SelfClassException of string

  val add_self_parameter_for_super_instance :
    CContext.t -> Procname.t -> Location.t -> Clang_ast_t.obj_c_message_expr_info ->
    trans_result

  val is_var_self : Pvar.t -> bool -> bool
end

val is_logical_negation_of_int :
  Tenv.t -> Clang_ast_t.expr_info -> Clang_ast_t.unary_operator_info -> bool

val is_dispatch_function : Clang_ast_t.stmt list -> int option

val is_block_enumerate_function : Clang_ast_t.obj_c_message_expr_info -> bool

val var_or_zero_in_init_list : Tenv.t -> Sil.exp -> Sil.typ -> return_zero:bool ->
  (Sil.exp * Sil.typ) list
