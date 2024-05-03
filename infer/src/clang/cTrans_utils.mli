(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Utility methods to support the translation of clang ast constructs into sil instructions. *)

type continuation =
  { break: Procdesc.Node.t list
  ; continue: Procdesc.Node.t list
  ; return_temp: bool
        (** true if temps should not be removed in the node but returned to ancestors *) }

(** Whether we are collecting instructions for a new block in the CFG ([Busy]) or there are no
    blocks being created from enclosing translations ([Free]) *)
type priority_node =
  | Free  (** no node currently being created *)
  | Busy of Clang_ast_t.pointer
      (** the translation of the clang expression or statement at [pointer] will create a node with
          the collected instructions from the sub-expressions (see {!control.instrs} *)

(** The input of the translation constructed from enclosing expressions. *)
type trans_state =
  { context: CContext.t  (** global context of the translation *)
  ; succ_nodes: Procdesc.Node.t list
        (** successor nodes in the CFG, i.e. instructions that will happen *after* the current
            expression or statement being translated (note that the CFG is constructed bottom-up,
            starting from the last instructions) *)
  ; continuation: continuation option
        (** current continuation, used for [break], [continue], and the like *)
  ; priority: priority_node  (** see {!priority_node} *)
  ; var_exp_typ: (Exp.t * Typ.t) option
        (** the expression (usually of the form [Exp.Lvar pvar]) that the enclosing expression or
            statement is trying to initialize, if any *)
  ; opaque_exp: (Exp.t * Typ.t) option  (** needed for translating [OpaqueValueExpr] nodes *)
  ; is_fst_arg_objc_instance_method_call: bool
  ; block_as_arg_attributes: ProcAttributes.block_as_arg_attributes option }

val pp_trans_state : F.formatter -> trans_state -> unit

val default_trans_state : CContext.t -> trans_state

(** Part of the translation result that is (loosely) related to control flow graph construction.
    More importantly, this is the part of a [trans_result] that some internal translation functions
    work on when constructing a [trans_result] before the other components of the translation result
    are available (such as the return expression). This is made into a separate type to make
    intermediate computations easier to write and easier to typecheck. *)
type control =
  { root_nodes: Procdesc.Node.t list  (** Top cfg nodes (root) created by the translation *)
  ; leaf_nodes: Procdesc.Node.t list  (** Bottom cfg nodes (leaf) created by the translate *)
  ; instrs: Sil.instr list
        (** Instructions that need to be placed in the current CFG node being constructed, *after*
            [leaf_nodes]. *)
  ; initd_exps: Exp.t list  (** list of expressions that are initialized by the instructions *)
  ; cxx_temporary_markers_set: Pvar.t list
        (** markers for C++ temporaries that have been set during the translation; used to avoid
            adding the same marker several times *) }

val pp_control : F.formatter -> control -> unit

(** A translation result. It is returned by the translation function. *)
type trans_result =
  { control: control
  ; return: Exp.t * Typ.t  (** value returned by the translated statement *)
  ; method_name: Procname.t option
        (** in the specific case of translating a method call in C++, we get the method name called
            at the same time we get the [this] object that contains the method. The [this] instance
            object is returned as the [return] field, while the method to call is filled in here.
            This field is [None] in all other cases. *)
  ; method_signature: CMethodSignature.t option
        (** in the specific case of translating a function call, we get the method signature. This
            field is [None] in all other cases. *)
  ; is_cpp_call_virtual: bool }

val empty_control : control

val mk_trans_result :
     ?method_name:Procname.t
  -> ?method_signature:CMethodSignature.t
  -> ?is_cpp_call_virtual:bool
  -> Exp.t * Typ.t
  -> control
  -> trans_result

val undefined_expression : unit -> Exp.t

val collect_controls : Procdesc.t -> control list -> control
(** Collect the results of translating a list of instructions, and link up the nodes created. *)

val collect_trans_results : Procdesc.t -> return:Exp.t * Typ.t -> trans_result list -> trans_result

val is_return_temp : continuation option -> bool

val mk_cond_continuation : continuation option -> continuation option

val define_condition_side_effects :
  Exp.t * Typ.t -> Sil.instr list -> Location.t -> (Exp.t * Typ.t) * Sil.instr list

val source_range_of_stmt : Clang_ast_t.stmt -> Clang_ast_t.source_range

val extract_stmt_from_singleton :
  Clang_ast_t.stmt list -> Clang_ast_t.source_range -> string -> Clang_ast_t.stmt

val is_null_stmt : Clang_ast_t.stmt -> bool

val dereference_var_sil : Exp.t * Typ.t -> Location.t -> Sil.instr * Exp.t

val dereference_value_from_result :
  ?strip_pointer:bool -> Clang_ast_t.source_range -> Location.t -> trans_result -> trans_result
(** Given a [trans_result], create a temporary variable with dereferenced value of an expression
    assigned to it *)

val cast_operation :
     ?objc_bridge_cast_kind:Clang_ast_t.obj_c_bridge_cast_kind
  -> Clang_ast_t.cast_kind
  -> Exp.t * Typ.t
  -> Typ.t
  -> Location.t
  -> Sil.instr list * (Exp.t * Typ.t)

val trans_assertion : trans_state -> Location.t -> trans_result

val contains_opaque_value_expr : Clang_ast_t.stmt -> bool

val builtin_trans :
     trans_state
  -> Clang_ast_t.source_range
  -> Location.t
  -> trans_result list
  -> Procname.t
  -> trans_result option

val cxx_method_builtin_trans :
     trans_state
  -> Clang_ast_t.source_range
  -> Location.t
  -> trans_result list
  -> Procname.t
  -> trans_result option

val new_or_alloc_trans :
     trans_state
  -> Location.t
  -> Clang_ast_t.stmt_info
  -> Typ.t
  -> Typ.Name.t option
  -> string
  -> trans_result

val cpp_new_trans :
  IntegerWidths.t -> Location.t -> Typ.t -> Exp.t option -> (Exp.t * Typ.t) list -> trans_result

(** Module for creating cfg nodes and other utility functions related to them. *)
module Nodes : sig
  val is_binary_assign_op : Clang_ast_t.binary_operator_info -> bool

  val create_prune_node :
       Procdesc.t
    -> branch:bool
    -> negate_cond:bool
    -> Exp.t
    -> Sil.instr list
    -> Location.t
    -> Sil.if_kind
    -> Procdesc.Node.t

  val is_true_prune_node : Procdesc.Node.t -> bool
end

(** priority_node is used to enforce some kind of policy for creating nodes in the cfg. Certain
    elements of the AST _must_ create nodes therefore there is no need for them to use
    priority_node. Certain elements instead need or need not to create a node depending of certain
    factors. When an element of the latter kind wants to create a node it must claim priority first
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

  val compute_controls_to_parent :
       trans_state
    -> Location.t
    -> Procdesc.Node.stmt_nodekind
    -> Clang_ast_t.stmt_info
    -> control list
    -> control
  (** Used by translation functions to handle potential cfg nodes. It connects nodes returned by the
      translation of stmt children and deals with creating or not a cfg node depending of owning the
      priority_node. It returns the [control] that should be passed to the parent. *)

  val compute_control_to_parent :
       trans_state
    -> Location.t
    -> Procdesc.Node.stmt_nodekind
    -> Clang_ast_t.stmt_info
    -> control
    -> control
  (** like [compute_controls_to_parent] but for a singleton, so the only possible effect is creating
      a node *)

  val compute_results_to_parent :
       trans_state
    -> Location.t
    -> Procdesc.Node.stmt_nodekind
    -> Clang_ast_t.stmt_info
    -> return:Exp.t * Typ.t
    -> trans_result list
    -> trans_result
  (** convenience wrapper around [compute_controls_to_parent] *)

  val compute_result_to_parent :
       trans_state
    -> Location.t
    -> Procdesc.Node.stmt_nodekind
    -> Clang_ast_t.stmt_info
    -> trans_result
    -> trans_result
  (** convenience function like [compute_results_to_parent] when there is a single [trans_result] to
      consider *)

  val force_sequential :
       Location.t
    -> Procdesc.Node.stmt_nodekind
    -> trans_state
    -> Clang_ast_t.stmt_info
    -> mk_first_opt:(trans_state -> Clang_ast_t.stmt_info -> trans_result option)
    -> mk_second:(trans_state -> Clang_ast_t.stmt_info -> trans_result)
    -> mk_return:(fst:trans_result -> snd:trans_result -> Exp.t * Typ.t)
    -> trans_result

  val force_sequential_with_acc :
       Location.t
    -> Procdesc.Node.stmt_nodekind
    -> trans_state
    -> Clang_ast_t.stmt_info
    -> mk_first:(trans_state -> Clang_ast_t.stmt_info -> trans_result * 'a)
    -> mk_second:('a -> trans_state -> Clang_ast_t.stmt_info -> trans_result)
    -> mk_return:(fst:trans_result -> snd:trans_result -> Exp.t * Typ.t)
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
        { init: Clang_ast_t.stmt
        ; decl_stmt: Clang_ast_t.stmt
        ; condition: Clang_ast_t.stmt
        ; increment: Clang_ast_t.stmt
        ; body: Clang_ast_t.stmt }
    | While of
        {decl_stmt: Clang_ast_t.stmt option; condition: Clang_ast_t.stmt; body: Clang_ast_t.stmt}
    | DoWhile of {condition: Clang_ast_t.stmt; body: Clang_ast_t.stmt}

  val get_cond : loop_kind -> Clang_ast_t.stmt

  val get_body : loop_kind -> Clang_ast_t.stmt
end

(** This module handles the translation of the variable self which is challenging because self is
    used both as a variable in instance method calls and also as a type in class method calls. *)
module Self : sig
  val add_self_parameter_for_super_instance :
       Clang_ast_t.stmt_info
    -> CContext.t
    -> Procname.t
    -> Location.t
    -> Clang_ast_t.obj_c_message_expr_info
    -> trans_result option

  val is_var_self : Pvar.t -> bool -> bool
end

val is_logical_negation_of_int :
  Tenv.t -> Clang_ast_t.expr_info -> Clang_ast_t.unary_operator_info -> bool

val mk_fresh_void_exp_typ : unit -> Exp.t * Typ.t

val mk_fresh_void_id_typ : unit -> Ident.t * Typ.t

val mk_fresh_void_return : unit -> (Ident.t * Typ.t) * (Exp.t * Typ.t)

val last_or_mk_fresh_void_exp_typ : (Exp.t * Typ.t) list -> Exp.t * Typ.t

val should_remove_first_param : trans_state -> Clang_ast_t.stmt -> Typ.name option
(** Return a class name when the first parameter should be removed according to its context, for
    example, when [self] or [[x class]] is given as the first parameter for a class method. *)
