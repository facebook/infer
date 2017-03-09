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


/** Module for Mangled Names */
let module F = Format;

type t = {plain: string, mangled: option string} [@@deriving compare];

let equal = [%compare.equal : t];


/** Convert a string to a mangled name */
let from_string (s: string) => {plain: s, mangled: None};


/** Create a mangled name from a plain and mangled string */
let mangled (plain: string) (mangled: string) => {
  plain,
  mangled: Some (plain ^ "{" ^ mangled ^ "}")
};


/** Convert a mangled name to a string */
let to_string (pn: t) => pn.plain;


/** Convert a full mangled name to a string */
let to_string_full (pn: t) =>
  switch pn.mangled {
  | Some mangled => pn.plain ^ "{" ^ mangled ^ "}"
  | None => pn.plain
  };


/** Get mangled string if given */
let get_mangled pn =>
  switch pn.mangled {
  | Some s => s
  | None => pn.plain
  };


/** Pretty print a mangled name */
let pp f pn => F.fprintf f "%s" (to_string pn);

let module Set = Caml.Set.Make {
  type nonrec t = t;
  let compare = compare;
};

let module Map = Caml.Map.Make {
  type nonrec t = t;
  let compare = compare;
};
