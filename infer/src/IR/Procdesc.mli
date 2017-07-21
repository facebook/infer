(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** node of the control flow graph *)
module Node : sig
  (** type of nodes *)
  type t [@@deriving compare]

  (** node id *)
  type id = private int [@@deriving compare]

  val equal_id : id -> id -> bool

  (** kind of cfg node *)
  type nodekind =
    | Start_node of Typ.Procname.t
    | Exit_node of Typ.Procname.t
    | Stmt_node of string
    | Join_node
    | Prune_node of bool * Sil.if_kind * string  (** (true/false branch, if_kind, comment) *)
    | Skip_node of string
    [@@deriving compare]

  val equal_nodekind : nodekind -> nodekind -> bool

  val exn_handler_kind : nodekind
  (** kind of Stmt_node for an exception handler. *)

  val exn_sink_kind : nodekind
  (** kind of Stmt_node for an exceptions sink. *)

  val throw_kind : nodekind
  (** kind of Stmt_node for a throw instruction. *)

  val add_locals_ret_declaration : t -> ProcAttributes.t -> (Mangled.t * Typ.t) list -> unit
  (** Add declarations for local variables and return variable to the node *)

  val append_instrs : t -> Sil.instr list -> unit
  (** Append the instructions to the list of instructions to execute *)

  val d_instrs : sub_instrs:bool -> Sil.instr option -> t -> unit
  (** Dump extended instructions for the node *)

  val dummy : Typ.Procname.t option -> t
  (** Create a dummy node *)

  val equal : t -> t -> bool
  (** Check if two nodes are equal *)

  val get_callees : t -> Typ.Procname.t list
  (** Get the list of callee procnames from the node *)

  val get_description : Pp.env -> t -> string
  (** Return a description of the node *)

  val get_distance_to_exit : t -> int option
  (** Get the distance to the exit node, if it has been computed *)

  val get_exn : t -> t list
  (** Get the exception nodes from the current node *)

  val get_generated_slope : t -> (t -> t list) -> t list
  (** Get a list of unique nodes until the first branch starting
      from a node with subsequent applications of a generator function *)

  val get_id : t -> id
  (** Get the unique id of the node *)

  val get_instrs : t -> Sil.instr list
  (** Get the instructions to be executed *)

  val get_kind : t -> nodekind
  (** Get the kind of the current node *)

  val get_last_loc : t -> Location.t
  (** Get the source location of the last instruction in the node *)

  val get_loc : t -> Location.t
  (** Get the source location of the node *)

  val get_preds : t -> t list
  (** Get the predecessor nodes of the current node *)

  val get_proc_name : t -> Typ.Procname.t
  (** Get the name of the procedure the node belongs to *)

  val get_sliced_preds : t -> (t -> bool) -> t list
  (** Get the predecessor nodes of a node where the given predicate evaluates to true *)

  val get_sliced_succs : t -> (t -> bool) -> t list
  (** Get the successor nodes of a node where the given predicate evaluates to true *)

  val get_succs : t -> t list
  (** Get the successor nodes of the current node *)

  val hash : t -> int
  (** Hash function for nodes *)

  val pp : Format.formatter -> t -> unit
  (** Pretty print the node *)

  val pp_id : Format.formatter -> id -> unit
  (** Pretty print a node id *)

  val pp_instrs : Pp.env -> sub_instrs:bool -> Sil.instr option -> Format.formatter -> t -> unit
  (** Print extended instructions for the node,
      highlighting the given subinstruction if present *)

  val replace_instrs : t -> Sil.instr list -> unit
  (** Replace the instructions to be executed. *)
end

(** Map with node id keys. *)
module IdMap : Caml.Map.S with type key = Node.id

(** Hash table with nodes as keys. *)
module NodeHash : Caml.Hashtbl.S with type key = Node.t

(** Map over nodes. *)
module NodeMap : Caml.Map.S with type key = Node.t

(** Set of nodes. *)
module NodeSet : Caml.Set.S with type elt = Node.t

(** procedure descriptions *)

(** proc description *)
type t [@@deriving compare]

val append_locals : t -> (Mangled.t * Typ.t) list -> unit
(** append a list of new local variables to the existing list of local variables *)

val compute_distance_to_exit_node : t -> unit
(** Compute the distance of each node to the exit node, if not computed already *)

val create_node : t -> Location.t -> Node.nodekind -> Sil.instr list -> Node.t
(** Create a new cfg node with the given location, kind, list of instructions,
    and add it to the procdesc. *)

val did_preanalysis : t -> bool
(** true if we ran the preanalysis on the CFG associated with [t] *)

val fold_calls : ('a -> Typ.Procname.t * Location.t -> 'a) -> 'a -> t -> 'a
(** fold over the calls from the procedure: (callee, location) pairs *)

val fold_instrs : ('a -> Node.t -> Sil.instr -> 'a) -> 'a -> t -> 'a
(** fold over all nodes and their instructions *)

val fold_nodes : ('a -> Node.t -> 'a) -> 'a -> t -> 'a
(** fold over all nodes *)

val from_proc_attributes : called_from_cfg:bool -> ProcAttributes.t -> t
(** Only call from Cfg. *)

val get_access : t -> PredSymb.access
(** Return the visibility attribute *)

val get_attributes : t -> ProcAttributes.t
(** Get the attributes of the procedure. *)

val get_captured : t -> (Mangled.t * Typ.t) list
(** Return name and type of block's captured variables *)

val get_err_log : t -> Errlog.t

val get_exit_node : t -> Node.t

val get_flags : t -> ProcAttributes.proc_flags
(** Get flags for the proc desc *)

val get_formals : t -> (Mangled.t * Typ.t) list
(** Return name and type of formal parameters *)

val get_loc : t -> Location.t
(** Return loc information for the procedure *)

val get_locals : t -> (Mangled.t * Typ.t) list
(** Return name and type of local variables *)

val get_nodes : t -> Node.t list

val get_proc_name : t -> Typ.Procname.t

val get_ret_type : t -> Typ.t
(** Return the return type of the procedure and type string *)

val get_ret_var : t -> Pvar.t

val get_sliced_slope : t -> (Node.t -> bool) -> Node.t list
(** Get the sliced procedure's nodes up until the first branching *)

val get_slope : t -> Node.t list
(** Get the procedure's nodes up until the first branching *)

val get_start_node : t -> Node.t

val is_defined : t -> bool
(** Return [true] iff the procedure is defined, and not just declared *)

val is_body_empty : t -> bool
(** Return [true] if the body of the procdesc is empty (no instructions) *)

val is_java_synchronized : t -> bool
(** Return [true] if the procedure signature has the Java synchronized keyword *)

val iter_calls : (Typ.Procname.t * Location.t -> unit) -> t -> unit
(** iterate over the calls from the procedure: (callee, location) pairs *)

val iter_instrs : (Node.t -> Sil.instr -> unit) -> t -> unit
(** iterate over all nodes and their instructions *)

val iter_nodes : (Node.t -> unit) -> t -> unit
(** iterate over all the nodes of a procedure *)

val iter_slope : (Node.t -> unit) -> t -> unit
(** iterate over all nodes until we reach a branching structure *)

val iter_slope_calls : (Typ.Procname.t -> unit) -> t -> unit
(** iterate over all calls until we reach a branching structure *)

val iter_slope_range : (Node.t -> unit) -> Node.t -> Node.t -> unit
(** iterate between two nodes or until we reach a branching structure *)

val node_set_succs_exn : t -> Node.t -> Node.t list -> Node.t list -> unit
(** Set the successor nodes and exception nodes, and build predecessor links *)

val set_exit_node : t -> Node.t -> unit
(** Set the exit node of the procedure *)

val set_flag : t -> string -> string -> unit
(** Set a flag for the proc desc *)

val set_start_node : t -> Node.t -> unit

val signal_did_preanalysis : t -> unit
(** indicate that we have performed preanalysis on the CFG assoociated with [t] *)

val is_loop_head : t -> Node.t -> bool

val pp_signature : Format.formatter -> t -> unit
