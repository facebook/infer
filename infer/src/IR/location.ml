(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging

(** Location in the original source file *)
type t = {
  line: int; (** The line number. -1 means "do not know" *)
  col: int; (** The column number. -1 means "do not know" *)
  file: DB.source_file; (** The name of the source file *)
  nLOC : int; (** Lines of code in the source file *)
}

let compare loc1 loc2 =
  let n = int_compare loc1.line loc2.line in
  if n <> 0 then n else DB.source_file_compare loc1.file loc2.file

(** Dump a location *)
let d (loc: t) = L.add_print_action (L.PTloc, Obj.repr loc)

(** Dummy location *)
let dummy = {
  line = -1;
  col = -1;
  file = DB.source_file_empty;
  nLOC = -1;
}

let equal loc1 loc2 =
  compare loc1 loc2 = 0

(** Pretty print a location *)
let pp f (loc: t) =
  F.fprintf f "[line %d]" loc.line

let to_string loc =
  let s = (string_of_int loc.line) in
  if (loc.col != -1) then
    s ^":"^(string_of_int loc.col)
  else s
