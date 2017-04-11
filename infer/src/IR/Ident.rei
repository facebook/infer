/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Identifiers: program variables and logical variables */

/** Program and logical variables. */
type t [@@deriving compare];


/** Equality for identifiers. */
let equal: t => t => bool;


/** Names used to replace strings. */
type name [@@deriving compare];


/** Equality for names. */
let equal_name: name => name => bool;


/** Kind of identifiers. */
type kind [@@deriving compare];


/** Equality for kind. */
let equal_kind: kind => kind => bool;


/** Set for identifiers. */
module IdentSet: Caml.Set.S with type elt = t;


/** Hash table with ident as key. */
module IdentHash: Caml.Hashtbl.S with type key = t;


/** Map with ident as key. */
module IdentMap: Caml.Map.S with type key = t;

module NameGenerator: {
  type t;

  /** Get the current name generator. */
  let get_current: unit => t;

  /** Reset the name generator. */
  let reset: unit => unit;

  /** Set the current name generator. */
  let set_current: t => unit;
};


/** Convert an identfier list to an identifier set */
let idlist_to_idset: list t => IdentSet.t;

let kprimed: kind;

let knormal: kind;

let kfootprint: kind;


/** hash table with names as keys */
module NameHash: Caml.Hashtbl.S with type key = name;


/** Name used for primed tmp variables */
let name_primed: name;


/** Name used for spec variables */
let name_spec: name;


/** Name used for the return variable */
let name_return: Mangled.t;


/** Convert a string to a name. */
let string_to_name: string => name;


/** Convert a name to a string. */
let name_to_string: name => string;


/** Name of the identifier. */
let get_name: t => name;


/** Create an identifier with default name for the given kind */
let create: kind => int => t;


/** Generate a normal identifier with the given name and stamp. */
let create_normal: name => int => t;


/** Create a "null" identifier for situations where the IR requires an id that will never be read */
let create_none: unit => t;


/** Generate a primed identifier with the given name and stamp. */
let create_primed: name => int => t;


/** Generate a footprint identifier with the given name and stamp. */
let create_footprint: name => int => t;


/** Update the name generator so that the given id's are not generated again */
let update_name_generator: list t => unit;


/** Create a fresh identifier with default name for the given kind. */
let create_fresh: kind => t;


/** Generate a normal identifier whose name encodes a path given as a string. */
let create_path: string => t;


/** Check whether an identifier is primed or not. */
let is_primed: t => bool;


/** Check whether an identifier is normal or not. */
let is_normal: t => bool;


/** Check whether an identifier is footprint or not. */
let is_footprint: t => bool;


/** Check whether an identifier represents a path or not. */
let is_path: t => bool;


/** Check whether an identifier is the special "none" identifier */
let is_none: t => bool;


/** Convert a primed ident into a nonprimed one, keeping the stamp. */
let make_unprimed: t => t;


/** Get the stamp of the identifier */
let get_stamp: t => int;


/** Set the stamp of the identifier */
let set_stamp: t => int => t;


/** {2 Pretty Printing} */

/** Pretty print a name. */
let pp_name: Format.formatter => name => unit;


/** Pretty print a name in latex. */
let pp_name_latex: Latex.style => Format.formatter => name => unit;


/** Pretty print an identifier. */
let pp: Pp.env => Format.formatter => t => unit;


/** Convert an identifier to a string. */
let to_string: t => string;


/** Pretty print a list of identifiers. */
let pp_list: Pp.env => Format.formatter => list t => unit;


/** Pretty print a list of names. */
let pp_name_list: Format.formatter => list name => unit;
