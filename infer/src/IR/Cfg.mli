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

(** Control Flow Graph for Interprocedural Analysis *)

(** A control-flow graph *)
type t

val load : SourceFile.t -> t option
(** Load the cfgs of the procedures of a source file *)

val store : SourceFile.t -> t -> unit
(** Save a cfg into the database *)

(** {2 Functions for manipulating an interprocedural CFG} *)

val create_cfg : unit -> t
(** create a new empty cfg *)

val create_proc_desc : t -> ProcAttributes.t -> Procdesc.t
(** Create a new procdesc *)

val iter_proc_desc : t -> (Typ.Procname.t -> Procdesc.t -> unit) -> unit
(** Iterate over all the procdesc's *)

val find_proc_desc_from_name : t -> Typ.Procname.t -> Procdesc.t option
(** Find the procdesc given the proc name. Return None if not found. *)

val get_all_procs : t -> Procdesc.t list
(** Get all the procedures (defined and declared) *)

val get_defined_procs : t -> Procdesc.t list
(** Get the procedures whose body is defined in this cfg *)

val iter_all_nodes : ?sorted:bool -> (Procdesc.t -> Procdesc.Node.t -> unit) -> t -> unit
(** Iterate over all the nodes in the cfg *)

val check_cfg_connectedness : t -> unit
(** checks whether a cfg is connected or not *)

val remove_proc_desc : t -> Typ.Procname.t -> unit
(** Remove the procdesc from the control flow graph. *)

val specialize_types : Procdesc.t -> Typ.Procname.t -> (Exp.t * Typ.t) list -> Procdesc.t
(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting procdesc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)

val specialize_with_block_args :
  Procdesc.t -> Typ.Procname.t -> Exp.closure option list -> Procdesc.t
(** Creates a copy of a procedure description given a list of possible closures 
  that are passed as arguments to the method. The resulting procdesc is isomorphic but
  a) the block parameters are replaces with the closures 
  b) the parameters of the method are extended with parameters for the captured variables 
  in the closures *)

val pp_proc_signatures : Format.formatter -> t -> unit

val exists_for_source_file : SourceFile.t -> bool
