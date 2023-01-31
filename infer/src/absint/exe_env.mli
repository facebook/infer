(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Execution environments are a means to perform I/O operations during analysis time such as
    getting a [tenv] of a function while providing appropriate caching *)

type t

val mk : unit -> t
(** Create a new cache *)

val get_proc_tenv : t -> Procname.t -> Tenv.t
(** return the type environment associated with the procedure *)

val load_java_global_tenv : t -> Tenv.t
(** Load Java type environment (if not done yet), and return it. Useful for accessing type info not
    related to any concrete function. *)

val get_integer_type_widths : t -> Procname.t -> Typ.IntegerWidths.t
(** return the integer type widths associated with the procedure *)

val get_attributes : t -> Procname.t -> ProcAttributes.t option

val get_formal_types : t -> Procname.t -> Typ.t list

val is_no_return : t -> Procname.t -> bool

val get_procs_in_file : t -> Procname.t -> Procname.t list
(** return the list of procedures in the file where the given procedure name was defined *)
