(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Printf

let list_entry e =
  let t = Unix.localtime e.Zip.mtime in
  printf "%6d  %6d  %c  %04d-%02d-%02d %02d:%02d  %c  %s\n"
    e.Zip.uncompressed_size
    e.Zip.compressed_size
    (match e.Zip.methd with Zip.Stored -> 's' | Zip.Deflated -> 'd')
    (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min
    (if e.Zip.is_directory then 'd' else ' ')
    e.Zip.filename;
  if e.Zip.comment <> "" then
    printf "        %s\n" e.Zip.comment

let list zipfile =
  let ic = Zip.open_in zipfile in
  if Zip.comment ic <> "" then printf "%s\n" (Zip.comment ic);
  List.iter list_entry (Zip.entries ic);
  Zip.close_in ic

let extract_entry ifile e =
  print_string e.Zip.filename; print_newline();
  if e.Zip.is_directory then begin
    try
      Unix.mkdir e.Zip.filename 0o777
    with Unix.Unix_error(Unix.EEXIST, _, _) -> ()
  end else begin
    Zip.copy_entry_to_file ifile e e.Zip.filename
  end

let extract zipfile =
  let ic = Zip.open_in zipfile in
  List.iter (extract_entry ic) (Zip.entries ic);
  Zip.close_in ic

let rec add_entry oc file =
  let s = Unix.stat file in
  match s.Unix.st_kind with
    Unix.S_REG ->
      printf "Adding file %s\n" file; flush stdout;
      Zip.copy_file_to_entry file oc ~mtime:s.Unix.st_mtime file
  | Unix.S_DIR ->
      printf "Adding directory %s\n" file; flush stdout;
      Zip.add_entry "" oc ~mtime:s.Unix.st_mtime
        (if Filename.check_suffix file "/" then file else file ^ "/");
      let d = Unix.opendir file in
      begin try
        while true do
          let e = Unix.readdir d in
          if e <> "." && e <> ".." then add_entry oc (Filename.concat file e)
        done
      with End_of_file -> ()
      end;
      Unix.closedir d
  | _ -> ()  

let create zipfile files =
  let oc = Zip.open_out zipfile in
  Array.iter (add_entry oc) files;
  Zip.close_out oc

let usage() =
  prerr_string
"Usage: 
  minizip t <zipfile>           show contents of <zipfile>
  minizip x <zipfile>           extract files from <zipfile>
  minizip c <zipfile> <file> .. create a <zipfile> with the given files\n";
  exit 2

let _ =
  if Array.length Sys.argv < 3 then usage();
  match Sys.argv.(1) with
    "t" -> list Sys.argv.(2)
  | "x" -> extract Sys.argv.(2)
  | "c" -> create Sys.argv.(2)
                  (Array.sub Sys.argv 3 (Array.length Sys.argv - 3))
  | _ -> usage()
