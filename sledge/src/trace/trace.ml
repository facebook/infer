(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Debug trace logging *)

type 'a printf = ('a, Format.formatter, unit) format -> 'a

let ff = Format.err_formatter

let flush = Format.pp_print_newline ff

let margin = 100

let trace_all = ref false

let init ~trace_all:ta =
  Format.set_margin margin ;
  Format.set_max_indent (margin - 1) ;
  Format.pp_set_margin ff margin ;
  Format.pp_set_max_indent ff (margin - 1) ;
  Format.pp_open_vbox ff 0 ;
  Caml.at_exit flush ;
  trace_all := ta

(* selective tracing not yet implemented *)
let enabled _ = !trace_all

let info mod_name fun_name fmt =
  if enabled [fun_name; mod_name] then (
    Format.fprintf ff "@\n@[<2>| " ;
    Format.kfprintf (fun ff -> Format.fprintf ff "@]") ff fmt )
  else Format.ifprintf ff fmt

let incf rev_prefix name fmt =
  if enabled (name :: rev_prefix) then (
    Format.fprintf ff "@\n@[<2>@[( %s: " name ;
    Format.kfprintf (fun ff -> Format.fprintf ff "@]") ff fmt )
  else Format.ifprintf ff fmt

let decf rev_prefix name fmt =
  if enabled (name :: rev_prefix) then (
    Format.fprintf ff "@]@\n@[<2>) %s:@ " name ;
    Format.kfprintf (fun ff -> Format.fprintf ff "@]") ff fmt )
  else Format.ifprintf ff fmt

let call mod_name fun_name k = k (incf [mod_name] fun_name)

let retn mod_name fun_name k result =
  k (decf [mod_name] fun_name) result ;
  result
