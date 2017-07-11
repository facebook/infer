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

(**  Control Flow Graph for Interprocedural Analysis *)

(** A control-flow graph *)

type cfg

(** Load a cfg from a file *)

val load_cfg_from_file : DB.filename -> cfg option

(** Save a cfg into a file, and save a copy of the source files if the boolean is true *)

val store_cfg_to_file : source_file:SourceFile.t -> DB.filename -> cfg -> unit

(** {2 Functions for manipulating an interprocedural CFG} *)

(** create a new empty cfg *)

val create_cfg : unit -> cfg

(** Create a new procdesc *)

val create_proc_desc : cfg -> ProcAttributes.t -> Procdesc.t

(** Iterate over all the procdesc's *)

val iter_proc_desc : cfg -> (Typ.Procname.t -> Procdesc.t -> unit) -> unit

(** Find the procdesc given the proc name. Return None if not found. *)

val find_proc_desc_from_name : cfg -> Typ.Procname.t -> Procdesc.t option

(** Get all the procedures (defined and declared) *)

val get_all_procs : cfg -> Procdesc.t list

(** Get the procedures whose body is defined in this cfg *)

val get_defined_procs : cfg -> Procdesc.t list

(** Iterate over all the nodes in the cfg *)

val iter_all_nodes : ?sorted:bool -> (Procdesc.t -> Procdesc.Node.t -> unit) -> cfg -> unit

(** checks whether a cfg is connected or not *)

val check_cfg_connectedness : cfg -> unit

(** Remove the procdesc from the control flow graph. *)

val remove_proc_desc : cfg -> Typ.Procname.t -> unit

(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting procdesc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)

val specialize_types : Procdesc.t -> Typ.Procname.t -> (Exp.t * Typ.t) list -> Procdesc.t

val pp_proc_signatures : Format.formatter -> cfg -> unit
