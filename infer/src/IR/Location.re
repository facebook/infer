/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

module F = Format;

module L = Logging;


/** Location in the original source file */
type t = {
  line: int, /** The line number. -1 means "do not know" */
  col: int, /** The column number. -1 means "do not know" */
  file: SourceFile.t /** The name of the source file */
}
[@@deriving compare];

let equal = [%compare.equal : t];


/** Dump a location */
let d (loc: t) => L.add_print_action (L.PTloc, Obj.repr loc);

let none file => {line: (-1), col: (-1), file};

let dummy = none (SourceFile.invalid __FILE__);

/** Pretty print a location */
let pp f (loc: t) => F.fprintf f "[line %d]" loc.line;

let to_string loc => {
  let s = string_of_int loc.line;
  if (loc.col != (-1)) {
    s ^ ":" ^ string_of_int loc.col
  } else {
    s
  }
};
