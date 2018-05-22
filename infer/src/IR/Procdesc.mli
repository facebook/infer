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

  val add_locals_ret_declaration : t -> ProcAttributes.t -> ProcAttributes.var_data list -> unit
  (** Add declarations for local variables and return variable to the node *)

  val append_instrs : t -> Sil.instr list -> unit
  (** Append the instructions to the list of instructions to execute *)

  val d_instrs : sub_instrs:bool -> Sil.instr option -> t -> unit
  (** Dump extended instructions for the node *)

  val dummy : Typ.Procname.t option -> t
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

val append_locals : t -> ProcAttributes.var_data list -> unit
(** append a list of new local variables to the existing list of local variables *)

val compute_distance_to_exit_node : t -> unit
(** Compute the distance of each node to the exit node, if not computed already *)

val create_node : t -> Location.t -> Node.nodekind -> Sil.instr list -> Node.t
(** Create a new cfg node with the given location, kind, list of instructions,
    and add it to the procdesc. *)

val did_preanalysis : t -> bool
(** true if we ran the preanalysis on the CFG associated with [t] *)

val fold_instrs : t -> init:'accum -> f:('accum -> Node.t -> Sil.instr -> 'accum) -> 'accum
(** fold over all nodes and their instructions *)

val from_proc_attributes : ProcAttributes.t -> t
(** Use [Cfg.create_proc_desc] if you are adding a proc desc to a cfg *)

val get_access : t -> PredSymb.access
(** Return the visibility attribute *)

val get_attributes : t -> ProcAttributes.t
(** Get the attributes of the procedure. *)

val get_captured : t -> (Mangled.t * Typ.t) list
(** Return name and type of block's captured variables *)

val get_exit_node : t -> Node.t

val get_formals : t -> (Mangled.t * Typ.t) list
(** Return name and type of formal parameters *)

val get_loc : t -> Location.t
(** Return loc information for the procedure *)

val get_locals : t -> ProcAttributes.var_data list
(** Return name and type and attributes of local variables *)

val get_nodes : t -> Node.t list

val get_nodes_num : t -> int

val get_proc_name : t -> Typ.Procname.t

val get_ret_type : t -> Typ.t
(** Return the return type of the procedure and type string *)

val get_ret_var : t -> Pvar.t

val get_start_node : t -> Node.t

val is_defined : t -> bool
(** Return [true] iff the procedure is defined, and not just declared *)

val is_java_synchronized : t -> bool
(** Return [true] if the procedure signature has the Java synchronized keyword *)

val iter_instrs : (Node.t -> Sil.instr -> unit) -> t -> unit
(** iterate over all nodes and their instructions *)

val iter_nodes : (Node.t -> unit) -> t -> unit
(** iterate over all the nodes of a procedure *)

val fold_slope_range : Node.t -> Node.t -> init:'accum -> f:('accum -> Node.t -> 'accum) -> 'accum
(** fold between two nodes or until we reach a branching structure *)

val set_succs_exn_only : Node.t -> Node.t list -> unit

val node_set_succs_exn : t -> Node.t -> Node.t list -> Node.t list -> unit
(** Set the successor nodes and exception nodes, and build predecessor links *)

val set_exit_node : t -> Node.t -> unit
(** Set the exit node of the procedure *)

val set_start_node : t -> Node.t -> unit

val signal_did_preanalysis : t -> unit
(** indicate that we have performed preanalysis on the CFG assoociated with [t] *)

val is_loop_head : t -> Node.t -> bool

val pp_signature : Format.formatter -> t -> unit

val pp_local : Format.formatter -> ProcAttributes.var_data -> unit

val is_specialized : t -> bool

val is_captured_var : t -> Pvar.t -> bool
(** true if pvar is a captured variable of a cpp lambda or obcj block *)

val has_modify_in_block_attr : t -> Pvar.t -> bool

val specialize_types : t -> Typ.Procname.t -> (Exp.t * Typ.t) list -> t
(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting procdesc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)

val specialize_with_block_args : t -> Typ.Procname.t -> Exp.closure option list -> t
(** Creates a copy of a procedure description given a list of possible closures
  that are passed as arguments to the method. The resulting procdesc is isomorphic but
  a) the block parameters are replaces with the closures
  b) the parameters of the method are extended with parameters for the captured variables
  in the closures *)

val is_connected : t -> (unit, [`Join | `Other]) Result.t
(** checks whether a cfg for the given procdesc is connected or not *)
