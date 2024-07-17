(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** Location in the original source file *)
type t =
  { file: SourceFile.t  (** The name of the source file *)
  ; line: int  (** The line number. -1 means "do not know" *)
  ; col: int  (** The column number. -1 means "do not know" *)
  ; macro_file_opt: SourceFile.t option
        (** If the location is coming from macro expansion, the name of the file macro is defined in *)
  ; macro_line: int  (** If the location is coming from macro expansion, the line number *) }
[@@deriving compare, equal, sexp, hash, normalize]

let get_macro_file_line_opt {macro_file_opt; macro_line} =
  Option.map macro_file_opt ~f:(fun file -> (file, macro_line))


let none file = {line= -1; col= -1; file; macro_file_opt= None; macro_line= -1}

let dummy = none (SourceFile.invalid __FILE__)

let pp_line f loc = F.fprintf f "line %d" loc.line

(** Pretty print a location *)
let pp f (loc : t) =
  pp_line f loc ;
  if loc.col <> -1 then F.fprintf f ", column %d" loc.col


let pp_short f loc =
  F.pp_print_int f loc.line ;
  if loc.col <> -1 then F.fprintf f ":%d" loc.col


(** Pretty print a file-position of a location *)
let pp_file_pos f (loc : t) = F.fprintf f "%a:%a" SourceFile.pp loc.file pp_short loc

let pp_range f (loc_start, loc_end) =
  let pp_end loc_start f loc_end =
    if Int.equal loc_end.line loc_start.line then (
      if not (Int.equal loc_end.col loc_start.col) then F.fprintf f "-%d" loc_end.col )
    else F.fprintf f "-%a" pp_short loc_end
  in
  F.fprintf f "%a%a" pp_file_pos loc_start (pp_end loc_start) loc_end


module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)
