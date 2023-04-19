(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Execution environments are a means to get a function's type environment and integer widths and
    cache those *)

type t

val mk : unit -> t
(** Create a new cache *)

val get_proc_tenv : t -> Procname.t -> Tenv.t
(** return the type environment associated with the procedure *)

val get_source_tenv : t -> SourceFile.t -> Tenv.t option
(** return the type environment associated with the source file *)

val load_java_global_tenv : t -> Tenv.t
(** Load Java type environment (if not done yet), and return it. Useful for accessing type info not
    related to any concrete function. *)

val get_integer_type_widths : t -> Procname.t -> IntegerWidths.t
(** return the integer type widths associated with the procedure *)
