(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

val log_out : ('a, Format.formatter, unit) format -> 'a

val log_err : ('a, Format.formatter, unit) format -> 'a

val log_stats : ('a, Format.formatter, unit) format -> 'a

val print_failure_info : string -> unit

val print_tenv : Tenv.t -> unit

val print_tenv_struct_unions : Tenv.t -> unit

val print_procedures : Cfg.cfg -> unit

val print_nodes : Cfg.Node.t list -> unit

val instrs_to_string : Sil.instr list -> string

val field_to_string : Ident.fieldname * Typ.t * Typ.item_annotation -> string
