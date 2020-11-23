(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Contains current class and current method to be translated as well as local variables, and the
    cfg, and tenv corresponding to the current file. *)

module StmtMap = ClangPointers.Map

type curr_class = ContextClsDeclPtr of int | ContextNoCls [@@deriving compare]

type str_node_map = (string, Procdesc.Node.t) Caml.Hashtbl.t

type t =
  { translation_unit_context: CFrontend_config.translation_unit_context
  ; tenv: Tenv.t
  ; cfg: Cfg.t
  ; procdesc: Procdesc.t
  ; immediate_curr_class: curr_class
  ; return_param_typ: Typ.t option
  ; outer_context: t option
        (** in case of objc blocks, the context of the method containing the block *)
  ; mutable blocks_static_vars: (Pvar.t * Typ.t) list Procname.Map.t
  ; label_map: str_node_map
  ; vars_to_destroy: Clang_ast_t.decl list StmtMap.t
        (** mapping from a statement to a list of variables, that go out of scope after the end of
            the statement *)
  ; temporary_names: (Clang_ast_t.pointer, Pvar.t * Typ.t) Caml.Hashtbl.t
  ; temporaries_constructor_markers: (Pvar.t * Typ.t) Exp.Map.t
        (** In order to know when to destruct C++ temporaries created in expressions containing
            conditionals (e.g. to hold the object created by [X()] in [b?foo(X()):goo()]), we
            associate "markers" to each one of them, set to true if and only if the temporary has
            been created. This is the map associating each such C++ temporary with its marker
            variable. *) }

val get_curr_class : t -> curr_class

val get_curr_class_typename : Clang_ast_t.stmt_info -> t -> Typ.Name.t

val get_curr_class_decl_ptr : Clang_ast_t.stmt_info -> curr_class -> Clang_ast_t.pointer

val is_objc_method : t -> bool

val is_objc_class_method : t -> bool

val create_context :
     CFrontend_config.translation_unit_context
  -> Tenv.t
  -> Cfg.t
  -> Procdesc.t
  -> curr_class
  -> Typ.t option
  -> t option
  -> Clang_ast_t.decl list StmtMap.t
  -> t

val add_block_static_var : t -> Procname.t -> Pvar.t * Typ.t -> unit

val get_outer_procname : t -> Procname.t
