(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility methods to support the translation of clang ast constructs into sil instructions.  *)

type continuation =
  { break: Procdesc.Node.t list
  ; continue: Procdesc.Node.t list
  ; return_temp: bool
  (* true if temps should not be removed in the node but returned to ancestors *) }

type priority_node = Free | Busy of Clang_ast_t.pointer

type trans_state =
  { context: CContext.t
  ; succ_nodes: Procdesc.Node.t list
  ; continuation: continuation option
  ; priority: priority_node
  ; var_exp_typ: (Exp.t * Typ.t) option
  ; opaque_exp: (Exp.t * Typ.t) option
  ; obj_bridged_cast_typ: Typ.t option }

type trans_result =
  { root_nodes: Procdesc.Node.t list
  ; leaf_nodes: Procdesc.Node.t list
  ; instrs: Sil.instr list
  ; exps: (Exp.t * Typ.t) list
  ; initd_exps: Exp.t list
  ; is_cpp_call_virtual: bool }

exception TemplatedCodeException of Clang_ast_t.stmt

val empty_res_trans : trans_result

val undefined_expression : unit -> Exp.t

val collect_res_trans : Procdesc.t -> trans_result list -> trans_result

val extract_var_exp_or_fail : trans_state -> Exp.t * Typ.t

val is_return_temp : continuation option -> bool

val ids_to_parent : continuation option -> Ident.t list -> Ident.t list

val ids_to_node : continuation option -> Ident.t list -> Ident.t list

val mk_cond_continuation : continuation option -> continuation option

val extract_item_from_singleton : 'a list -> string -> 'a -> 'a

val extract_exp_from_list : (Exp.t * Typ.t) list -> string -> Exp.t * Typ.t

val get_selector_receiver :
  Clang_ast_t.obj_c_message_expr_info -> string * Clang_ast_t.receiver_kind

val define_condition_side_effects :
  (Exp.t * Typ.t) list -> Sil.instr list -> Location.t -> (Exp.t * Typ.t) list * Sil.instr list

val extract_stmt_from_singleton : Clang_ast_t.stmt list -> string -> Clang_ast_t.stmt

val is_null_stmt : Clang_ast_t.stmt -> bool

val is_enumeration_constant : Clang_ast_t.stmt -> bool

val is_member_exp : Clang_ast_t.stmt -> bool

val get_type_from_exp_stmt : Clang_ast_t.stmt -> Clang_ast_t.qual_type

val dereference_value_from_result :
  Location.t -> trans_result -> strip_pointer:bool -> trans_result
(** Given trans_result with ONE expression, create temporary variable with dereferenced value of an
    expression assigned to it *)

val cast_operation :
  trans_state -> Clang_ast_t.cast_kind -> (Exp.t * Typ.t) list -> Typ.t -> Location.t -> bool
  -> Sil.instr list * (Exp.t * Typ.t)

val trans_assertion : trans_state -> Location.t -> trans_result

val is_owning_method : Clang_ast_t.stmt -> bool

val is_owning_name : string -> bool

val is_method_call : Clang_ast_t.stmt -> bool

val contains_opaque_value_expr : Clang_ast_t.stmt -> bool

val get_decl_ref_info : Clang_ast_t.stmt -> Clang_ast_t.decl_ref

val builtin_trans :
  trans_state -> Location.t -> Clang_ast_t.stmt_info -> Typ.t -> trans_result list
  -> Typ.Procname.t -> trans_result option

val cxx_method_builtin_trans :
  trans_state -> Location.t -> trans_result list -> Typ.Procname.t -> trans_result option

val alloc_trans :
  trans_state -> Location.t -> Clang_ast_t.stmt_info -> Typ.t -> bool -> Typ.Procname.t option
  -> trans_result

val new_or_alloc_trans :
  trans_state -> Location.t -> Clang_ast_t.stmt_info -> Clang_ast_t.qual_type -> Typ.Name.t option
  -> string -> trans_result

val cpp_new_trans : Location.t -> Typ.t -> Exp.t option -> trans_result

val cast_trans :
  (Exp.t * Typ.t) list -> Location.t -> Typ.t -> Typ.Procname.t -> (Sil.instr * Exp.t) option

val dereference_var_sil : Exp.t * Typ.t -> Location.t -> Sil.instr list * Exp.t

(** Module for creating cfg nodes and other utility functions related to them.  *)
module Nodes : sig
  val is_binary_assign_op : Clang_ast_t.binary_operator_info -> bool

  val need_unary_op_node : Clang_ast_t.unary_operator_info -> bool

  val create_node :
    Procdesc.Node.nodekind -> Sil.instr list -> Location.t -> CContext.t -> Procdesc.Node.t

  val is_join_node : Procdesc.Node.t -> bool

  val create_prune_node :
    bool -> (Exp.t * Typ.t) list -> Sil.instr list -> Location.t -> Sil.if_kind -> CContext.t
    -> Procdesc.Node.t

  val is_prune_node : Procdesc.Node.t -> bool

  val is_true_prune_node : Procdesc.Node.t -> bool

  val prune_kind : bool -> Procdesc.Node.nodekind
end

(** priority_node is used to enforce some kind of policy for creating nodes in the cfg. Certain
    elements of the AST _must_ create nodes therefore there is no need for them to use
    priority_node. Certain elements instead need or need not to create a node depending of certain
    factors.  When an element of the latter kind wants to create a node it must claim priority first
    (like taking a lock). priority can be claimes only when it is free. If an element of AST
    succedes in claiming priority its id (pointer) is recorded in priority. After an element has
    finished it frees the priority. In general an AST element E checks if an ancestor has claimed
    priority. If priority is already claimed E does not have to create a node. If priority is free
    then it means E has to create the node. Then E claims priority and release it afterward. *)
module PriorityNode : sig
  type t = priority_node

  val is_priority_free : trans_state -> bool

  val try_claim_priority_node : trans_state -> Clang_ast_t.stmt_info -> trans_state

  val force_claim_priority_node : trans_state -> Clang_ast_t.stmt_info -> trans_state

  val own_priority_node : t -> Clang_ast_t.stmt_info -> bool

  (* Used by translation functions to handle potenatial cfg nodes. *)
  (* It connects nodes returned by translation of stmt children and *)
  (* deals with creating or not a cfg node depending of owning the *)
  (* priority_node. It returns nodes, ids, instrs that should be passed to parent *)

  val compute_results_to_parent :
    trans_state -> Location.t -> string -> Clang_ast_t.stmt_info -> trans_result list
    -> trans_result
end

(** Module for translating goto instructions by keeping a map of labels. *)
module GotoLabel : sig
  val find_goto_label : CContext.t -> string -> Location.t -> Procdesc.Node.t
end

(** Module that provides utility functions for translating different types of loops. *)
module Loops : sig
  type loop_kind =
    | For of
        Clang_ast_t.stmt
        * Clang_ast_t.stmt
        * Clang_ast_t.stmt
        * Clang_ast_t.stmt
        * Clang_ast_t.stmt
    (* init, decl_stmt, condition, increment and body *)
    | While of Clang_ast_t.stmt option * Clang_ast_t.stmt * Clang_ast_t.stmt
    (* decl_stmt, condition and body *)
    | DoWhile of Clang_ast_t.stmt * Clang_ast_t.stmt

  (* condition and body *)

  val loop_kind_to_if_kind : loop_kind -> Sil.if_kind

  val get_cond : loop_kind -> Clang_ast_t.stmt

  val get_body : loop_kind -> Clang_ast_t.stmt
end

(** This module handles the translation of the variable self which is challenging because self is
    used both as a variable in instance method calls and also as a type in class method calls. *)
module Self : sig
  exception SelfClassException of Typ.Name.t

  val add_self_parameter_for_super_instance :
    CContext.t -> Typ.Procname.t -> Location.t -> Clang_ast_t.obj_c_message_expr_info
    -> trans_result

  val is_var_self : Pvar.t -> bool -> bool
end

val is_logical_negation_of_int :
  Tenv.t -> Clang_ast_t.expr_info -> Clang_ast_t.unary_operator_info -> bool

val is_dispatch_function : Clang_ast_t.stmt list -> int option

val is_block_enumerate_function : Clang_ast_t.obj_c_message_expr_info -> bool

val var_or_zero_in_init_list : Tenv.t -> Exp.t -> Typ.t -> return_zero:bool -> (Exp.t * Typ.t) list
