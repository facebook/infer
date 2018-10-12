(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Debug trace logging *)

type 'a printf = ('a, Formatter.t, unit) format -> 'a
type pf = {pf: 'a. 'a printf}

let fs = Format.err_formatter
let flush = Format.pp_print_newline fs

type trace_mod_funs =
  {trace_mod: bool option; trace_funs: bool Map.M(String).t}

type config =
  {trace_all: bool; trace_mods_funs: trace_mod_funs Map.M(String).t}

let config : config ref =
  ref {trace_all= false; trace_mods_funs= Map.empty (module String)}

let init ?(margin = 160) ~config:c () =
  Format.set_margin margin ;
  Format.set_max_indent (margin - 1) ;
  Format.pp_set_margin fs margin ;
  Format.pp_set_max_indent fs (margin - 1) ;
  Format.pp_open_vbox fs 0 ;
  Caml.at_exit flush ;
  config := c

let unwrap s =
  let rec index s i =
    if i <= 1 then None
    else if not (Char.equal '_' s.[i]) then index s (i - 1)
    else if not (Char.equal '_' s.[i - 1]) then index s (i - 2)
    else Some (i + 1)
  in
  match index s (String.length s - 2) with
  | Some pos -> String.subo s ~pos
  | None -> s

let enabled mod_name fun_name =
  let {trace_all; trace_mods_funs} = !config in
  match Map.find trace_mods_funs (unwrap mod_name) with
  | Some {trace_mod; trace_funs} -> (
    match Map.find trace_funs fun_name with
    | Some fun_enabled -> fun_enabled
    | None -> (
      match trace_mod with
      | Some mod_enabled -> mod_enabled
      | None -> trace_all ) )
  | None -> trace_all

let info mod_name fun_name fmt =
  if enabled mod_name fun_name then (
    Format.fprintf fs "@\n@[<2>| " ;
    Format.kfprintf (fun fs -> Format.fprintf fs "@]") fs fmt )
  else Format.ifprintf fs fmt

let incf mod_name fun_name fmt =
  if enabled mod_name fun_name then (
    Format.fprintf fs "@\n@[<2>@[<hv 2>( %s:@ " fun_name ;
    Format.kfprintf (fun fs -> Format.fprintf fs "@]") fs fmt )
  else Format.ifprintf fs fmt

let decf mod_name fun_name fmt =
  if enabled mod_name fun_name then (
    Format.fprintf fs "@]@\n@[<2>) %s:@ " fun_name ;
    Format.kfprintf (fun fs -> Format.fprintf fs "@]") fs fmt )
  else Format.ifprintf fs fmt

let call mod_name fun_name k =
  k {pf= (fun fmt -> incf mod_name fun_name fmt)}

let retn mod_name fun_name k result =
  k {pf= (fun fmt -> decf mod_name fun_name fmt)} result ;
  result

let report fmt =
  Format.fprintf fs "@\n@[<2>| " ;
  Format.kfprintf (fun fs -> Format.fprintf fs "@]" ; false) fs fmt

let%test_module _ =
  (module struct let () = init ~margin:70 ~config:!config () end)
