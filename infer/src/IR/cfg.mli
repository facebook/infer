(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(**  Control Flow Graph for Interprocedural Analysis *)

(** {2 ADT node and proc_desc} *)

type node
type cfg

(** Load a cfg from a file *)
val load_cfg_from_file: DB.filename -> cfg option

(** Save a cfg into a file, and save a copy of the source files if the boolean is true *)
val store_cfg_to_file: DB.filename -> bool -> cfg -> unit

(** proc description *)
module Procdesc : sig
  (** proc description *)
  type t

  (** Compute the distance of each node to the exit node, if not computed already *)
  val compute_distance_to_exit_node : t -> unit

  (** Create a procdesc *)
  val create : cfg -> ProcAttributes.t -> t

  (** [remove cfg name remove_nodes] remove the procdesc [name]
      from the control flow graph [cfg]. *)
  (** It also removes all the nodes from the procedure from the cfg if remove_nodes is true *)
  val remove: cfg -> Procname.t -> bool -> unit

  (** Find the procdesc given the proc name. Return None if not found. *)
  val find_from_name : cfg -> Procname.t -> t option

  (** Get the attributes of the procedure. *)
  val get_attributes : t -> ProcAttributes.t

  val get_err_log : t -> Errlog.t

  val get_exit_node : t -> node

  (** Get flags for the proc desc *)
  val get_flags : t -> proc_flags

  (** Return name and type of formal parameters *)
  val get_formals : t -> (Mangled.t * Sil.typ) list

  (** Return loc information for the procedure *)
  val get_loc : t -> Location.t

  (** Return name and type of local variables *)
  val get_locals : t -> (Mangled.t * Sil.typ) list

  (** Return name and type of block's captured variables *)
  val get_captured : t -> (Mangled.t * Sil.typ) list

  (** Return the visibility attribute *)
  val get_access : t -> Sil.access

  val get_nodes : t -> node list

  (** Get the procedure's nodes up until the first branching *)
  val get_slope : t -> node list

  (** Get the sliced procedure's nodes up until the first branching *)
  val get_sliced_slope : t -> (node -> bool) -> node list

  val get_proc_name : t -> Procname.t

  (** Return the return type of the procedure and type string *)
  val get_ret_type : t -> Sil.typ

  val get_ret_var : t -> Pvar.t

  val get_start_node : t -> node

  (** Return [true] iff the procedure is defined, and not just declared *)
  val is_defined : t -> bool

  (** iterate over all the nodes of a procedure *)
  val iter_nodes : (node -> unit) -> t -> unit

  (** fold over the calls from the procedure: (callee, location) pairs *)
  val fold_calls : ('a -> Procname.t * Location.t -> 'a) -> 'a -> t -> 'a

  (** iterate over the calls from the procedure: (callee, location) pairs *)
  val iter_calls : (Procname.t * Location.t -> unit) -> t -> unit

  (** iterate over all nodes and their instructions *)
  val iter_instrs : (node -> Sil.instr -> unit) -> t -> unit

  (** fold over all nodes and their instructions *)
  val fold_instrs : ('a -> node -> Sil.instr -> 'a) -> 'a -> t -> 'a

  (** iterate over all nodes until we reach a branching structure *)
  val iter_slope : (node -> unit) -> t -> unit

  (** iterate over all calls until we reach a branching structure *)
  val iter_slope_calls : (Procname.t -> unit) -> t -> unit

  (** iterate between two nodes or until we reach a branching structure *)
  val iter_slope_range : (node -> unit) -> node -> node -> unit

  val set_exit_node : t -> node -> unit

  (** Set a flag for the proc desc *)
  val set_flag : t -> string -> string -> unit

  val set_start_node : t -> node -> unit

  (** append a list of new local variables to the existing list of local variables *)
  val append_locals : t -> (Mangled.t * Sil.typ) list -> unit

end

(** node of the control flow graph *)
module Node : sig
  type t = node (** type of nodes *)

  type id = private int

  (** kind of cfg node *)
  type nodekind =
    | Start_node of Procdesc.t
    | Exit_node of Procdesc.t
    | Stmt_node of string
    | Join_node
    | Prune_node of bool * Sil.if_kind * string (** (true/false branch, if_kind, comment) *)
    | Skip_node of string

  (** kind of Stmt_node for an exception handler. *)
  val exn_handler_kind : nodekind

  (** kind of Stmt_node for an exceptions sink. *)
  val exn_sink_kind : nodekind

  (** kind of Stmt_node for a throw instruction. *)
  val throw_kind : nodekind

  (** Append the instructions and temporaries to the list of instructions to execute *)
  val append_instrs_temps : t -> Sil.instr list -> Ident.t list -> unit

  (** Add the instructions and temporaries at the beginning
      of the list of instructions to execute *)
  val prepend_instrs_temps : t -> Sil.instr list -> Ident.t list -> unit

  (** Add declarations for local variables and return variable to the node *)
  val add_locals_ret_declaration : t -> (Mangled.t * Sil.typ) list -> unit

  (** Compare two nodes *)
  val compare : t -> t -> int

  (** [create cfg loc kind instrs proc_desc temps] create a new cfg node
      with the given location, kind, list of instructions,
      procdesc and list of temporary variables *)
  val create : cfg -> Location.t -> nodekind -> Sil.instr list -> Procdesc.t -> Ident.t list -> t

  (** create a new empty cfg *)
  val create_cfg : unit -> cfg

  (** Dump extended instructions for the node *)
  val d_instrs : sub_instrs: bool -> Sil.instr option -> t -> unit

  (** Create a dummy node *)
  val dummy : unit -> t

  (** Check if two nodes are equal *)
  val equal : t -> t -> bool

  (** Get all the nodes *)
  val get_all_nodes : cfg -> t list

  (** Get the (after/before) dead program variables.
      After/before indicated with the true/false flag. *)
  val get_dead_pvars: t -> bool -> Pvar.t list

  (** Get the distance to the exit node, if it has been computed *)
  val get_distance_to_exit: t -> int option

  (** Return a description of the node *)
  val get_description : printenv -> t -> string

  (** Get the exception nodes from the current node *)
  val get_exn : t -> t list

  (** Get the unique id of the node *)
  val get_id : t -> id

  (** compare node ids *)
  val id_compare : id -> id -> int

  (** Get the source location of the node *)
  val get_loc : t -> Location.t

  (** Get the source location of the last instruction in the node *)
  val get_last_loc : t -> Location.t

  (** Get the kind of the current node *)
  val get_kind : t -> nodekind

  (** Get the predecessor nodes of the current node *)
  val get_preds : t -> t list

  (** Get a list of unique nodes until the first branch starting
      from a node with subsequent applications of a generator function *)
  val get_generated_slope : t -> (t -> t list) -> t list

  (** Get the proc desc associated to the node *)
  val get_proc_desc : t -> Procdesc.t

  (** Get the instructions to be executed *)
  val get_instrs : t -> Sil.instr list

  (** Get the list of callee procnames from the node *)
  val get_callees : t -> Procname.t list

  (** Get the successor nodes of the current node *)
  val get_succs : t -> t list

  (** Get the successor nodes of a node where the given predicate evaluates to true *)
  val get_sliced_succs : t -> (t -> bool) -> t list

  (** Get the predecessor nodes of a node where the given predicate evaluates to true *)
  val get_sliced_preds : t -> (t -> bool) -> t list

  (** Get the temporary variables introduced for the instructions stored in the node *)
  val get_temps: t -> Ident.t list

  (** Hash function for nodes *)
  val hash : t -> int

  (** Comparison for node kind *)
  val kind_compare : nodekind -> nodekind -> int

  (** Pretty print the node *)
  val pp : Format.formatter -> t -> unit

  (** Print extended instructions for the node,
      highlighting the given subinstruction if present *)
  val pp_instrs :
    printenv -> sub_instrs: bool -> Sil.instr option -> Format.formatter -> t -> unit

  (** Replace the instructions to be executed. *)
  val replace_instrs : t -> Sil.instr list -> unit

  (** Set the (after/before) dead program variables.
      After/before indicated with the true/false flag. *)
  val set_dead_pvars : t -> bool -> Pvar.t list -> unit

  (** Set the node kind *)
  val set_kind : t -> nodekind -> unit

  (** Set the source location of the node *)
  val set_loc : t -> Location.t -> unit

  (** Set the proc desc associated to the node *)
  val set_proc_desc : t -> Procdesc.t -> unit

  (** Set the successor nodes and exception nodes, and build predecessor links *)
  val set_succs_exn : cfg -> t -> t list -> t list -> unit

  (** Set the temporary variables *)
  val set_temps : t -> Ident.t list -> unit
(*
  (** Replace the instructions to be executed. *)
  val replace_instrs : t -> Sil.instr list -> unit
*)
end

(** Hash table with nodes as keys. *)
module NodeHash : Hashtbl.S with type key = Node.t

(** Set of nodes. *)
module NodeSet : Set.S with type elt = Node.t

val pp_node_list : Format.formatter -> Node.t list -> unit

(** {2 Functions for manipulating an interprocedural CFG} *)

(** Iterate over all the procdesc's *)
val iter_proc_desc : cfg -> (Procname.t -> Procdesc.t -> unit) -> unit

(** Get all the procedures (defined and declared) *)
val get_all_procs : cfg -> Procdesc.t list

(** Get the procedures whose body is defined in this cfg *)
val get_defined_procs : cfg -> Procdesc.t list

(** get the function names which should be analyzed before the other ones *)
val get_priority_procnames : cfg -> Procname.Set.t

(** set the function names whose address has been taken in this file *)
val set_procname_priority : cfg -> Procname.t -> unit

(** remove the return variable from the prop *)
val remove_ret : Procdesc.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** remove locals and return variable from the prop *)
val remove_locals_ret : Procdesc.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. *)
val remove_locals_formals : Procdesc.t -> Prop.normal Prop.t -> Pvar.t list * Prop.normal Prop.t

(** remove seed vars from a prop *)
val remove_seed_vars : 'a Prop.t -> Prop.normal Prop.t

(** checks whether a cfg is connected or not *)
val check_cfg_connectedness : cfg -> unit

(** Removes seeds variables from a prop corresponding to captured variables in an objc block *)
val remove_seed_captured_vars_block : Mangled.t list -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting procdesc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)
val specialize_types :
  Procdesc.t -> Procname.t -> (Sil.exp * Sil.typ) list -> Procdesc.t
