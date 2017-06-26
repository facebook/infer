/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

open! PVariant;

module F = Format;

module L = Logging;

let multilink_file_name = "multilink.txt";

type t = String.Table.t string;

let add multilinks fname => String.Table.set multilinks key::(Filename.basename fname) data::fname;

let create () :t => String.Table.create size::1 ();

/* Cache of multilinks files read from disk */
let multilink_files_cache = String.Table.create size::1 ();

let reset_cache () => String.Table.clear multilink_files_cache;

let read ::dir :option t => {
  let multilink_fname = Filename.concat dir multilink_file_name;
  switch (Utils.read_file multilink_fname) {
  | Error error =>
    L.internal_error "Couldn't read multilink file '%s': %s@." multilink_fname error;
    None
  | Ok lines =>
    let links = create ();
    List.iter
      f::(fun line => String.Table.set links key::(Filename.basename line) data::line) lines;
    String.Table.set multilink_files_cache key::dir data::links;
    Some links
  }
};

/* Write a multilink file in the given directory */
let write multilinks ::dir => {
  let fname = Filename.concat dir multilink_file_name;
  let outc = open_out fname;
  String.Table.iteri f::(fun key::_ data::src => output_string outc (src ^ "\n")) multilinks;
  Out_channel.close outc
};

let lookup ::dir =>
  try (Some (String.Table.find_exn multilink_files_cache dir)) {
  | Not_found => read ::dir
  };

let resolve fname => {
  let fname_s = DB.filename_to_string fname;
  if (Sys.file_exists fname_s == `Yes) {
    fname
  } else {
    let base = Filename.basename fname_s;
    let dir = Filename.dirname fname_s;
    switch (lookup ::dir) {
    | None => fname
    | Some links =>
      try (DB.filename_from_string (String.Table.find_exn links base)) {
      | Not_found => fname
      }
    }
  }
};
