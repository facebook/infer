(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** Location in the original source file *)
type t =
  { line: int  (** The line number. -1 means "do not know" *)
  ; col: int  (** The column number. -1 means "do not know" *)
  ; file: SourceFile.t  (** The name of the source file *) }
[@@deriving compare]

let equal = [%compare.equal: t]

let none file = {line= -1; col= -1; file}

let dummy = none (SourceFile.invalid __FILE__)

let pp_line f loc = F.fprintf f "line %d" loc.line

(** Pretty print a location *)
let pp f (loc : t) =
  pp_line f loc ;
  if loc.col <> -1 then F.fprintf f ", column %d" loc.col


let pp_short f loc =
  F.pp_print_int f loc.line ;
  if loc.col <> -1 then F.fprintf f ":%d" loc.col


let to_string loc = F.asprintf "%a" pp_short loc

(** Pretty print a file-position of a location *)
let pp_file_pos f (loc : t) = F.fprintf f "%a:%a" SourceFile.pp loc.file pp_short loc

let pp_range f (loc_start, loc_end) =
  let pp_end loc_start f loc_end =
    if Int.equal loc_end.line loc_start.line then
      if Int.equal loc_end.col loc_start.col then () else F.fprintf f "-%d" loc_end.col
    else F.fprintf f "-%a" pp_short loc_end
  in
  F.fprintf f "%a%a" pp_file_pos loc_start (pp_end loc_start) loc_end
