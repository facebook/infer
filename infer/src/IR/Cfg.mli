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

val get_all_defined_proc_names : t -> Typ.Procname.t list
(** get all the procedure names that are defined in the current file *)

val store : SourceFile.t -> t -> unit
(** Save the individual [Procdesc.t] and [ProcAttributes.t] to the database for the procedures in
   the cfg. *)

(** {2 Functions for manipulating an interprocedural CFG} *)

val create : unit -> t
(** create a new empty cfg *)

val create_proc_desc : t -> ProcAttributes.t -> Procdesc.t
(** Create a new procdesc and add it to the cfg *)

val iter_sorted : t -> f:(Procdesc.t -> unit) -> unit
(** Iterate over all the proc descs in the cfg in ascending order *)

val inline_java_synthetic_methods : t -> unit
(** Inline the java synthetic methods in the cfg (in-place) *)

val pp_proc_signatures : Format.formatter -> t -> unit
