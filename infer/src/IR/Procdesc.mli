(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {1 Per-procedure CFG} *)

module NodeKey : sig
  type t

  val to_string : t -> string

  val of_frontend_node_key : string -> t
end

(** node of the control flow graph *)
module Node : sig
  (** type of nodes *)
  type t [@@deriving compare]

  (** node id *)
  type id = private int [@@deriving compare]

  val equal_id : id -> id -> bool

  (** kind of statement node *)
  type stmt_nodekind =
    | AssertionFailure
    | BetweenJoinAndExit
    | BinaryConditionalStmtInit
    | BinaryOperatorStmt of string
    | Call of string
    | CallObjCNew
    | ClassCastException
    | ConditionalStmtBranch
    | ConstructorInit
    | CXXDynamicCast
    | CXXNewExpr
    | CXXStdInitializerListExpr
    | CXXTypeidExpr
    | DeclStmt
    | DefineBody
    | Destruction
    | ExceptionHandler
    | ExceptionsSink
    | FallbackNode
    | FinallyBranch
    | GCCAsmStmt
    | GenericSelectionExpr
    | IfStmtBranch
    | InitializeDynamicArrayLength
    | InitListExp
    | MessageCall of string
    | MethodBody
    | MonitorEnter
    | MonitorExit
    | ObjCCPPThrow
    | OutOfBound
    | ReturnStmt
    | Skip of string
    | SwitchStmt
    | ThisNotNull
    | Throw
    | ThrowNPE
    | UnaryOperator

  type prune_node_kind =
    | PruneNodeKind_ExceptionHandler
    | PruneNodeKind_FalseBranch
    | PruneNodeKind_InBound
    | PruneNodeKind_IsInstance
    | PruneNodeKind_MethodBody
    | PruneNodeKind_NotNull
    | PruneNodeKind_TrueBranch

  (** kind of cfg node *)
  type nodekind =
    | Start_node
    | Exit_node
    | Stmt_node of stmt_nodekind
    | Join_node
    | Prune_node of bool * Sil.if_kind * prune_node_kind
        (** (true/false branch, if_kind, comment) *)
    | Skip_node of string
  [@@deriving compare]

  val equal_nodekind : nodekind -> nodekind -> bool

  val exn_handler_kind : nodekind
  (** kind of Stmt_node for an exception handler. *)

  val exn_sink_kind : nodekind
  (** kind of Stmt_node for an exceptions sink. *)

  val throw_kind : nodekind
  (** kind of Stmt_node for a throw instruction. *)

  val append_instrs : t -> Sil.instr list -> unit
  (** Append the instructions to the list of instructions to execute *)

  val d_instrs : highlight:Sil.instr option -> t -> unit
  (** Dump instructions for the node, highlighting the given subinstruction if present *)

  val dummy : Typ.Procname.t -> t
  (** Create a dummy node *)

  val equal : t -> t -> bool
  (** Check if two nodes are equal *)

  val get_description : Pp.env -> t -> string
  (** Return a description of the node *)

  val get_distance_to_exit : t -> int option
  (** Get the distance to the exit node, if it has been computed *)

  val get_exn : t -> t list
  (** Get the exception nodes from the current node *)

  val get_id : t -> id
  (** Get the unique id of the node *)

  val get_instrs : t -> Instrs.not_reversed_t
  (** Get the instructions to be executed *)

  val get_kind : t -> nodekind
  (** Get the kind of the current node *)

  val get_last_loc : t -> Location.t
  (** Get the source location of the last instruction in the node *)

  val find_in_node_or_preds : t -> f:(t -> Sil.instr -> 'a option) -> 'a option
  (** Find in the given node or its predecessors *)

  val get_loc : t -> Location.t
  (** Get the source location of the node *)

  val get_preds : t -> t list
  (** Get the predecessor nodes of the current node *)

  val get_siblings : t -> t Sequence.t
  (** Get siblings of the current node *)

  val get_proc_name : t -> Typ.Procname.t
  (** Get the name of the procedure the node belongs to *)

  val get_succs : t -> t list
  (** Get the successor nodes of the current node *)

  val get_wto_index : t -> int

  val hash : t -> int
  (** Hash function for nodes *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the node *)

  val pp_id : Format.formatter -> id -> unit
  (** Pretty print a node id *)

  val pp_stmt : Format.formatter -> stmt_nodekind -> unit

  val compute_key : t -> NodeKey.t
end

(** Map with node id keys. *)
module IdMap : PrettyPrintable.PPMap with type key = Node.id

(** Hash table with nodes as keys. *)
module NodeHash : Caml.Hashtbl.S with type key = Node.t

(** Map over nodes. *)
module NodeMap : Caml.Map.S with type key = Node.t

(** Set of nodes. *)
module NodeSet : Caml.Set.S with type elt = Node.t

(** procedure descriptions *)

(** proc description *)
type t

val append_locals : t -> ProcAttributes.var_data list -> unit
(** append a list of new local variables to the existing list of local variables *)

val compute_distance_to_exit_node : t -> unit
(** Compute the distance of each node to the exit node, if not computed already *)

val create_node : t -> Location.t -> Node.nodekind -> Sil.instr list -> Node.t
(** Create a new cfg node with the given location, kind, list of instructions,
    and add it to the procdesc. *)

val create_node_from_not_reversed :
  t -> Location.t -> Node.nodekind -> Instrs.not_reversed_t -> Node.t

val did_preanalysis : t -> bool
(** true if we ran the preanalysis on the CFG associated with [t] *)

val fold_instrs : t -> init:'accum -> f:('accum -> Node.t -> Sil.instr -> 'accum) -> 'accum
(** fold over all nodes and their instructions *)

val find_map_instrs : t -> f:(Sil.instr -> 'a option) -> 'a option

val from_proc_attributes : ProcAttributes.t -> t
(** Use [Cfg.create_proc_desc] if you are adding a proc desc to a cfg *)

val get_access : t -> PredSymb.access
(** Return the visibility attribute *)

val get_attributes : t -> ProcAttributes.t
(** Get the attributes of the procedure. *)

val set_attributes : t -> ProcAttributes.t -> unit

val get_captured : t -> (Mangled.t * Typ.t) list
(** Return name and type of block's captured variables *)

val get_exit_node : t -> Node.t

val get_formals : t -> (Mangled.t * Typ.t) list
(** Return name and type of formal parameters *)

val get_pvar_formals : t -> (Pvar.t * Typ.t) list
(** Return pvar and type of formal parameters *)

val get_loc : t -> Location.t
(** Return loc information for the procedure *)

val get_locals : t -> ProcAttributes.var_data list
(** Return name and type and attributes of local variables *)

val get_nodes : t -> Node.t list

val get_nodes_num : t -> int

val get_proc_name : t -> Typ.Procname.t

val get_ret_type : t -> Typ.t
(** Return the return type of the procedure and type string *)

val has_added_return_param : t -> bool

val get_ret_var : t -> Pvar.t

val get_start_node : t -> Node.t

val is_defined : t -> bool
(** Return [true] iff the procedure is defined, and not just declared *)

val is_java_synchronized : t -> bool
(** Return [true] if the procedure signature has the Java synchronized keyword *)

val iter_instrs : (Node.t -> Sil.instr -> unit) -> t -> unit
(** iterate over all nodes and their instructions *)

val replace_instrs : t -> f:(Node.t -> Sil.instr -> Sil.instr) -> bool
(** Map and replace the instructions to be executed.
    Returns true if at least one substitution occured. *)

val iter_nodes : (Node.t -> unit) -> t -> unit
(** iterate over all the nodes of a procedure *)

val fold_nodes : t -> init:'accum -> f:('accum -> Node.t -> 'accum) -> 'accum
(** fold over all the nodes of a procedure *)

val fold_slope_range : Node.t -> Node.t -> init:'accum -> f:('accum -> Node.t -> 'accum) -> 'accum
(** fold between two nodes or until we reach a branching structure *)

val set_succs_exn_only : Node.t -> Node.t list -> unit

val node_set_succs_exn : t -> Node.t -> Node.t list -> Node.t list -> unit
(** Set the successor nodes and exception nodes, and build predecessor links *)

val set_exit_node : t -> Node.t -> unit
(** Set the exit node of the procedure *)

val set_start_node : t -> Node.t -> unit

val signal_did_preanalysis : t -> unit
(** indicate that we have performed preanalysis on the CFG associated with [t] *)

val get_wto : t -> Node.t WeakTopologicalOrder.Partition.t

val is_loop_head : t -> Node.t -> bool

val pp_signature : Format.formatter -> t -> unit

val pp_local : Format.formatter -> ProcAttributes.var_data -> unit

val is_specialized : t -> bool

val is_captured_pvar : t -> Pvar.t -> bool
(** true if pvar is a captured variable of a cpp lambda or obcj block *)

val is_captured_var : t -> Var.t -> bool
(** true if var is a captured variable of a cpp lambda or obcj block *)

val has_modify_in_block_attr : t -> Pvar.t -> bool

val is_connected : t -> (unit, [`Join | `Other]) Result.t
(** checks whether a cfg for the given procdesc is connected or not *)

(** per-procedure CFGs are stored in the SQLite "procedures" table as NULL if the procedure has no
   CFG *)
module SQLite : SqliteUtils.Data with type t = t option

val load : Typ.Procname.t -> t option
