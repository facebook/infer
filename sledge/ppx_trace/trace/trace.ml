(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Char = struct
  include Char

  let is_lowercase = function 'a' .. 'z' | '_' -> true | _ -> false
  let is_uppercase = function 'A' .. 'Z' -> true | _ -> false
end

module String = struct
  include StringLabels

  let is_empty str = length str = 0

  let lsplit2 str ~on =
    match index_opt str on with
    | Some pos ->
        Some
          ( sub str ~pos:0 ~len:pos
          , sub str ~pos:(pos + 1) ~len:(length str - pos - 1) )
    | None -> None

  let subo ?(pos = 0) ?len str =
    let len = match len with Some i -> i | None -> length str - pos in
    sub str ~pos ~len
end

module Map = Map.Make (String)

(** Debug trace logging *)

type ('a, 'b) fmt = ('a, Format.formatter, unit, 'b) format4
type 'a printf = ('a, unit) fmt -> 'a
type pf = {pf: 'a. 'a printf}

let fs = Format.err_formatter
let flush = Format.pp_print_newline fs

type trace_mod_funs = {trace_mod: bool option; trace_funs: bool Map.t}
type trace_mods_funs = trace_mod_funs Map.t

type config =
  {trace_all: bool; trace_mods_funs: trace_mods_funs; colors: bool}

let none = {trace_all= false; trace_mods_funs= Map.empty; colors= false}
let all = {none with trace_all= true}
let config = ref none

let parse s =
  try
    if String.equal s "*" then Ok all
    else
      let default = Map.empty in
      let index_from s i =
        match
          (String.index_from_opt s i '+', String.index_from_opt s i '-')
        with
        | None, o | o, None -> o
        | Some m, Some n -> Some (min m n)
      in
      let rec split s rev_parts i =
        match index_from s (i + 1) with
        | Some j when j = i -> split s rev_parts j
        | Some j ->
            split s (String.sub s ~pos:i ~len:(j - i) :: rev_parts) j
        | _ -> List.rev (String.subo s ~pos:i :: rev_parts)
      in
      let parts = split s [] 0 in
      let trace_mods_funs =
        List.fold_left
          (fun m part ->
            let parse_part part =
              let sign, rest =
                match part.[0] with
                | '-' -> (false, String.subo part ~pos:1)
                | '+' -> (true, String.subo part ~pos:1)
                | _ -> (true, part)
              in
              assert (not (String.is_empty rest)) ;
              assert (Char.is_uppercase rest.[0]) ;
              match String.lsplit2 rest ~on:'.' with
              | Some (mod_name, fun_name) ->
                  assert (Char.is_lowercase fun_name.[0]) ;
                  (mod_name, Some fun_name, sign)
              | None -> (rest, None, sign)
            in
            match parse_part part with
            | mod_name, Some fun_name, enabled ->
                let {trace_mod; trace_funs} =
                  try Map.find mod_name m
                  with Not_found -> {trace_mod= None; trace_funs= default}
                in
                Map.add mod_name
                  { trace_mod
                  ; trace_funs= Map.add fun_name enabled trace_funs }
                  m
            | mod_name, None, enabled ->
                Map.add mod_name
                  {trace_mod= Some enabled; trace_funs= default}
                  m )
          default parts
      in
      Ok {none with trace_mods_funs}
  with Assert_failure _ as exn -> Error exn

let pp_styled style fmt fs =
  Format.pp_open_box fs 2 ;
  if not !config.colors then
    Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
  else (
    ( match style with
    | `Bold -> Format.fprintf fs "@<0>\027[1m"
    | `Cyan -> Format.fprintf fs "@<0>\027[36m"
    | `Magenta -> Format.fprintf fs "@<0>\027[95m" ) ;
    Format.kfprintf
      (fun fs ->
        Format.fprintf fs "@<0>\027[0m" ;
        Format.pp_close_box fs () )
      fs fmt )

let init ?(colors = false) ?(margin = 240) ?config:(c = none) () =
  Format.set_margin margin ;
  Format.set_max_indent (margin - 1) ;
  Format.pp_set_margin fs margin ;
  Format.pp_set_max_indent fs (margin - 1) ;
  Format.pp_open_vbox fs 0 ;
  at_exit flush ;
  config := {c with colors}

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
  let {trace_all; trace_mods_funs; _} = !config in
  match Map.find (unwrap mod_name) trace_mods_funs with
  | {trace_mod; trace_funs} -> (
    try Map.find fun_name trace_funs
    with Not_found -> (
      match trace_mod with
      | Some mod_enabled -> mod_enabled
      | None -> trace_all ) )
  | exception Not_found -> trace_all

let kprintf mod_name fun_name k fmt =
  if enabled mod_name fun_name then Format.kfprintf k fs fmt
  else Format.ifprintf fs fmt

let fprintf mod_name fun_name fs fmt =
  if enabled mod_name fun_name then Format.fprintf fs fmt
  else Format.ifprintf fs fmt

let printf mod_name fun_name fmt = fprintf mod_name fun_name fs fmt

let info mod_name fun_name fmt =
  if enabled mod_name fun_name then (
    Format.fprintf fs "@\n@[<2>| " ;
    Format.kfprintf (fun fs -> Format.fprintf fs "@]") fs fmt )
  else Format.ifprintf fs fmt

let infok mod_name fun_name k =
  k {pf= (fun fmt -> info mod_name fun_name fmt)}

let incf mod_name fun_name fmt =
  if not (enabled mod_name fun_name) then Format.ifprintf fs fmt
  else (
    Format.fprintf fs "@\n@[<2>@[<hv 2>( %s:" fun_name ;
    Format.kfprintf (fun fs -> Format.fprintf fs "@]") fs fmt )

let decf mod_name fun_name fmt =
  if not (enabled mod_name fun_name) then Format.ifprintf fs fmt
  else (
    Format.fprintf fs "@]@\n@[<2>) %s:@ " fun_name ;
    Format.kfprintf (fun fs -> Format.fprintf fs "@]") fs fmt )

let call mod_name fun_name k =
  k {pf= (fun fmt -> incf mod_name fun_name fmt)}

let retn mod_name fun_name k result =
  k {pf= (fun fmt -> decf mod_name fun_name fmt)} result ;
  result

let trace :
       ?call:(pf -> unit)
    -> ?retn:(pf -> 'a -> unit)
    -> ?rais:(pf -> exn -> Printexc.raw_backtrace -> unit)
    -> string
    -> string
    -> (unit -> 'a)
    -> 'a =
 fun ?call ?retn ?rais mod_name fun_name k ->
  let call = Option.value call ~default:(fun {pf} -> pf "") in
  let retn = Option.value retn ~default:(fun {pf} _ -> pf "") in
  let rais =
    Option.value rais ~default:(fun {pf} exc _ ->
        pf "%s" (Printexc.to_string exc) )
  in
  call {pf= (fun fmt -> incf mod_name fun_name fmt)} ;
  match k () with
  | result ->
      retn {pf= (fun fmt -> decf mod_name fun_name fmt)} result ;
      result
  | exception exc ->
      let bt = Printexc.get_raw_backtrace () in
      rais {pf= (fun fmt -> decf mod_name fun_name fmt)} exc bt ;
      Printexc.raise_with_backtrace exc bt

let raisef ?margin exn fmt =
  let fs = Format.str_formatter in
  ( match margin with
  | Some m ->
      Format.pp_set_margin fs m ;
      Format.pp_set_max_indent fs (m - 1)
  | None -> () ) ;
  Format.pp_open_box fs 2 ;
  Format.kfprintf
    (fun fs () ->
      Format.pp_close_box fs () ;
      raise (exn (Format.flush_str_formatter ())) )
    fs fmt

let fail fmt =
  let margin = Format.pp_get_margin fs () in
  raisef ~margin
    (fun msg ->
      Format.fprintf fs "@\n%s@." msg ;
      Failure msg )
    fmt
