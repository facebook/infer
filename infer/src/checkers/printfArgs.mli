(*
* Copyright (c) 2013 - Facebook. All rights reserved.
*)


type printf_signature = {
  unique_id: string;
  format_pos: int;
  fixed_pos: int list;
  vararg_pos: int option
}

val add_printf_like_function : printf_signature -> unit


val callback_printf_args: Callbacks.proc_callback_t
