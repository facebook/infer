(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd


type printf_signature = {
  unique_id: string;
  format_pos: int;
  fixed_pos: int list;
  vararg_pos: int option
}

val add_printf_like_function : printf_signature -> unit

val check_printf_args_ok :
  Tenv.t -> Procdesc.Node.t -> Sil.instr -> Typ.Procname.t -> Procdesc.t -> unit

val callback_printf_args: Callbacks.proc_callback_t
