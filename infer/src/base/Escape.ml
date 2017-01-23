(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Escape a string for use in a CSV or XML file: replace reserved
    characters with escape sequences *)

(** apply a map function for escape sequences *)
let escape_map map_fun s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    match map_fun c with
    | None -> Buffer.add_char buf c
    | Some s' -> Buffer.add_string buf s'
  done;
  Buffer.contents buf

let escape_csv s =
  let map = function
    | '"' -> Some "\"\""
    | c when Char.to_int c > 127 -> Some "?" (* non-ascii character: escape *)
    | _ -> None in
  escape_map map s

let escape_xml s =
  let map = function
    | '"' -> (* on next line to avoid bad indentation *)
        Some "&quot;"
    | '>' -> Some "&gt;"
    | '<' -> Some "&lt;"
    | '&' -> Some "&amp;"
    | '%' -> Some "&#37;"
    | c when Char.to_int c > 127 -> (* non-ascii character: escape *)
        Some ("&#" ^ string_of_int (Char.to_int c) ^ ";")
    | _ -> None in
  escape_map map s

let escape_dotty s =
  let map = function
    | '"' -> Some "\\\""
    | '\\' -> Some "\\\\"
    | _ -> None in
  escape_map map s

let escape_path s =
  let map = function
    | c ->
        if String.equal (Char.escaped c) Filename.dir_sep
        then Some "_"
        else None in
  escape_map map s

(* Python 2 sucks at utf8 so do not write unicode file names to disk
   as Python may need to see them *)
let escape_filename s =
  let map = function
    | c when Char.to_int c > 127 -> Some "?" (* non-ascii character: escape *)
    | _ -> None in
  escape_map map s
