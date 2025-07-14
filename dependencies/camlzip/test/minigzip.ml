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

let buffer = Bytes.create 4096

let _ =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "-d" then begin
    (* decompress *)
    let ic = Gzip.open_in_chan stdin in
    let rec decompress () =
      let n = Gzip.input ic buffer 0 (Bytes.length buffer) in
      if n = 0 then () else begin output stdout buffer 0 n; decompress() end
    in decompress(); Gzip.dispose ic
  end else begin
    (* compress *)
    let oc = Gzip.open_out_chan stdout in
    let rec compress () =
      let n = input stdin buffer 0 (Bytes.length buffer) in
      if n = 0 then () else begin Gzip.output oc buffer 0 n; Gzip.flush_continue oc; compress() end
    in compress(); Gzip.flush oc
  end
