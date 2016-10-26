/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;


/** Location in the original source file */
type t = {
  line: int, /** The line number. -1 means "do not know" */
  col: int, /** The column number. -1 means "do not know" */
  file: DB.source_file, /** The name of the source file */
  nLOC: int /** Lines of code in the source file */
};

let compare: t => t => int;


/** Dump a location. */
let d: t => unit;


/** Dummy location */
let dummy: t;

let equal: t => t => bool;


/** Pretty print a location. */
let pp: Format.formatter => t => unit;


/** String representation of a location. */
let to_string: t => string;
