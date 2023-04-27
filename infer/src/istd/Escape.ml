(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** apply a map function for escape sequences *)
let escape_map map_fun s =
  let needs_escape = String.exists ~f:(fun c -> Option.is_some (map_fun c)) s in
  if needs_escape then (
    let len = String.length s in
    let buf = Buffer.create len in
    for i = 0 to len - 1 do
      let c = String.unsafe_get s i in
      match map_fun c with None -> Buffer.add_char buf c | Some s' -> Buffer.add_string buf s'
    done ;
    Buffer.contents buf )
  else (* not escaping anything, so don't waste memory on a copy of the string *)
    s


let escape_xml s =
  let map = function
    | '"' ->
        Some "&quot;"
    | '>' ->
        Some "&gt;"
    | '<' ->
        Some "&lt;"
    | '&' ->
        Some "&amp;"
    | '%' ->
        Some "&#37;"
    | _ ->
        None
  in
  escape_map map s


let escape_url s =
  let map = function
    | '!' ->
        Some "%21"
    | '"' ->
        Some "%22"
    | '#' ->
        Some "%23"
    | '$' ->
        Some "%24"
    | '&' ->
        Some "%26"
    | '\'' ->
        Some "%27"
    | '(' ->
        Some "%28"
    | ')' ->
        Some "%29"
    | '*' ->
        Some "%2A"
    | '+' ->
        Some "%2B"
    | ',' ->
        Some "%2C"
    | '/' ->
        Some "%2F"
    | ':' ->
        Some "%3A"
    | ';' ->
        Some "%3B"
    | '=' ->
        Some "%3D"
    | '?' ->
        Some "%3F"
    | '@' ->
        Some "%40"
    | '[' ->
        Some "%5B"
    | ']' ->
        Some "%5D"
    | _ ->
        None
  in
  escape_map map s


let escape_dotty s =
  let map = function '"' -> Some "\\\"" | '\\' -> Some "\\\\" | _ -> None in
  escape_map map s


let escape_path s =
  let map = function
    | c ->
        if String.equal (Char.escaped c) Filename.dir_sep then Some "_" else None
  in
  escape_map map s


let escape_filename s =
  let encode c = Some (Format.sprintf "%%%X" (Char.to_int c)) in
  let map c =
    match c with
    | '/' ->
        (* This character is forbidden on Windows and Unix *)
        encode c
    | '<' | '>' | ':' | '"' | '\\' | '|' | '?' | '*' | '\n' | '\r' | '\t' ->
        (* All these characters are forbidden on Windows. See
           https://stackoverflow.com/questions/1976007/what-characters-are-forbidden-in-windows-and-linux-directory-names *)
        if String.equal "Unix" Sys.os_type then None else encode c
    | '\127' .. '\255' ->
        (* Probably Unicode characters in filename. In doubt, encode *)
        encode c
    | _ ->
        None
  in
  escape_map map s


let escape_json s = escape_map (function '"' -> Some "\\\"" | '\\' -> Some "\\\\" | _ -> None) s

let escape_double_quotes s = escape_map (function '"' -> Some "\\\"" | _ -> None) s

let escape_in_single_quotes s =
  Printf.sprintf "'%s'" (escape_map (function '\'' -> Some "'\\''" | _ -> None) s)


let escape_shell =
  let no_quote_needed = Str.regexp "^[A-Za-z0-9-_%/:,.]+$" in
  let easy_single_quotable = Str.regexp "^[^']+$" in
  let easy_double_quotable = Str.regexp "^[^$`\\!]+$" in
  function
  | "" ->
      "''"
  | arg ->
      if Str.string_match no_quote_needed arg 0 then arg
      else if Str.string_match easy_single_quotable arg 0 then F.sprintf "'%s'" arg
      else if Str.string_match easy_double_quotable arg 0 then
        escape_double_quotes arg |> F.sprintf "\"%s\""
      else
        (* ends on-going single quote, output single quote inside double quotes, then open a new
           single quote *)
        escape_map (function '\'' -> Some "'\"'\"'" | _ -> None) arg |> F.sprintf "'%s'"
