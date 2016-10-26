/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;

let module F = Format;

let module L = Logging;

let multilink_file_name = "multilink.txt";

type t = StringHash.t string;

let add multilinks fname => StringHash.replace multilinks (Filename.basename fname) fname;

let create () :t => StringHash.create 1;

/* Cache of multilinks files read from disk */
let multilink_files_cache = StringHash.create 1;

let reset_cache () => StringHash.reset multilink_files_cache;

let read dir::dir :option t => {
  let multilink_fname = Filename.concat dir multilink_file_name;
  switch (Utils.read_file multilink_fname) {
  | None => None
  | Some lines =>
    let links = create ();
    IList.iter (fun line => StringHash.add links (Filename.basename line) line) lines;
    StringHash.add multilink_files_cache dir links;
    Some links
  }
};

/* Write a multilink file in the given directory */
let write multilinks dir::dir => {
  let fname = Filename.concat dir multilink_file_name;
  let outc = open_out fname;
  StringHash.iter (fun _ src => output_string outc (src ^ "\n")) multilinks;
  close_out outc
};

let lookup dir::dir =>
  try (Some (StringHash.find multilink_files_cache dir)) {
  | Not_found => read dir::dir
  };

let resolve fname => {
  let fname_s = DB.filename_to_string fname;
  if (Sys.file_exists fname_s) {
    fname
  } else {
    let base = Filename.basename fname_s;
    let dir = Filename.dirname fname_s;
    switch (lookup dir::dir) {
    | None => fname
    | Some links =>
      try (DB.filename_from_string (StringHash.find links base)) {
      | Not_found => fname
      }
    }
  }
};
