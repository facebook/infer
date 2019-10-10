(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

type trace_mods_funs = trace_mod_funs Map.M(String).t

type config =
  {trace_all: bool; trace_mods_funs: trace_mods_funs; colors: bool}

let none =
  { trace_all= false
  ; trace_mods_funs= Map.empty (module String)
  ; colors= false }

let all = {none with trace_all= true}
let config : config ref = ref none

let parse s =
  try
    if String.equal s "*" then Ok all
    else
      let default = Map.empty (module String) in
      let index_from s i =
        Option.merge ~f:min
          (String.index_from s i '+')
          (String.index_from s i '-')
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
        List.fold parts ~init:default ~f:(fun m part ->
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
                  match Map.find m mod_name with
                  | Some c -> c
                  | None -> {trace_mod= None; trace_funs= default}
                in
                Map.set m ~key:mod_name
                  ~data:
                    { trace_mod
                    ; trace_funs=
                        Map.set trace_funs ~key:fun_name ~data:enabled }
            | mod_name, None, enabled ->
                Map.set m ~key:mod_name
                  ~data:{trace_mod= Some enabled; trace_funs= default} )
      in
      Ok {none with trace_mods_funs}
  with Assert_failure _ as exn -> Error exn

let pp_styled style fmt fs =
  Format.pp_open_box fs 2 ;
  if (not !config.colors) || match style with `None -> true | _ -> false
  then Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
  else (
    ( match style with
    | `Bold -> Format.fprintf fs "\027[1m"
    | `Cyan -> Format.fprintf fs "\027[36m"
    | `Magenta -> Format.fprintf fs "\027[95m"
    | _ -> () ) ;
    Format.kfprintf
      (fun fs ->
        Format.fprintf fs "\027[0m" ;
        Format.pp_close_box fs () )
      fs fmt )

let init ?(colors = false) ?(margin = 240) ~config:c () =
  Format.set_margin margin ;
  Format.set_max_indent (margin - 1) ;
  Format.pp_set_margin fs margin ;
  Format.pp_set_max_indent fs (margin - 1) ;
  Format.pp_open_vbox fs 0 ;
  Caml.at_exit flush ;
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

let fail fmt =
  let margin = Format.pp_get_margin fs () in
  raisef ~margin
    (fun msg ->
      Format.fprintf fs "@\n@[<2>| %s@]@." msg ;
      Failure msg )
    fmt

let%test_module _ =
  (module struct let () = init ~margin:70 ~config:!config () end)
