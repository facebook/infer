(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Control Flow Graph for Interprocedural Analysis *)

(** A control-flow graph is a collection of all the CFGs for the procedure names in a file *)
type t = Procdesc.t Typ.Procname.Hash.t

val load : SourceFile.t -> t option
(** Load the cfgs of the procedures of a source file *)

val get_all_proc_names : t -> Typ.Procname.t list
(** get all the keys from the hashtable *)

(** {2 Functions for manipulating an interprocedural CFG} *)

val create : unit -> t
(** create a new empty cfg *)

val create_proc_desc : t -> ProcAttributes.t -> Procdesc.t
(** Create a new procdesc and add it to the cfg *)

val iter_all_nodes : sorted:bool -> t -> f:(Procdesc.t -> Procdesc.Node.t -> unit) -> unit
(** Iterate over all the nodes in the cfg *)

val save_attributes : SourceFile.t -> t -> unit
(** Save the .attr files for the procedures in the cfg. *)

val inline_java_synthetic_methods : t -> unit
(** Inline the java synthetic methods in the cfg (in-place) *)

val merge : src:t -> dst:t -> t
(** Best-effort merge of [src] into [dst]. If a procedure is both in [dst] and [src], the one in
   [src] will get overwritten. *)

val pp_proc_signatures : Format.formatter -> t -> unit

module SQLite : SqliteUtils.Data with type t = t
