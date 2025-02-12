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

val get_proc_tenv : Procname.t -> Tenv.t
(** return the type environment associated with the procedure *)

val get_source_tenv : SourceFile.t -> Tenv.t option
(** return the type environment associated with the source file *)

val get_integer_type_widths : Procname.t -> IntegerWidths.t
(** return the integer type widths associated with the procedure *)

val clear_caches : unit -> unit

val set_lru_limit : lru_limit:int option -> unit
