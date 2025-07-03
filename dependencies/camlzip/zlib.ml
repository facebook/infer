(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License, with     *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

exception Error of string * string

let _ =
  Callback.register_exception "Zlib.Error" (Error("",""))

type stream

type flush_command =
    Z_NO_FLUSH
  | Z_SYNC_FLUSH
  | Z_FULL_FLUSH
  | Z_FINISH

external deflate_init: int -> bool -> stream = "camlzip_deflateInit"
external deflate:
  stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_deflate_bytecode" "camlzip_deflate"
external deflate_string:
  stream -> string -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_deflate_bytecode" "camlzip_deflate"
external deflate_end: stream -> unit = "camlzip_deflateEnd"

external inflate_init: bool -> stream = "camlzip_inflateInit"
external inflate:
  stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_inflate_bytecode" "camlzip_inflate"
external inflate_string:
  stream -> string -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_inflate_bytecode" "camlzip_inflate"
external inflate_end: stream -> unit = "camlzip_inflateEnd"

external update_crc: int32 -> bytes -> int -> int -> int32
                   = "camlzip_update_crc32"
external update_crc_string: int32 -> string -> int -> int -> int32
                   = "camlzip_update_crc32"

let buffer_size = 1024

let compress ?(level = 6) ?(header = true) refill flush =
  let inbuf = Bytes.create buffer_size
  and outbuf = Bytes.create buffer_size in
  let zs = deflate_init level header in
  let rec compr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then compr_finish() else compr 0 incount
    end else begin
      let (_, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf 0 buffer_size Z_NO_FLUSH in
      flush outbuf used_out;
      compr (inpos + used_in) (inavail - used_in)
    end
  and compr_finish () =
    let (finished, _, used_out) =
       deflate zs inbuf 0 0 outbuf 0 buffer_size Z_FINISH in
    flush outbuf used_out;
    if not finished then compr_finish()
  in
    compr 0 0;
    deflate_end zs

let compress_direct  ?(level = 6) ?(header = true) flush =
  let outbuf = Bytes.create buffer_size in
  let zs = deflate_init level header in
  let rec compr inbuf inpos inavail =
    if inavail = 0 then ()
    else begin
      let (_, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf 0 buffer_size Z_NO_FLUSH in
      flush outbuf used_out;
      compr inbuf (inpos + used_in) (inavail - used_in)
    end
  and compr_finish () =
    let (finished, _, used_out) =
      deflate zs (Bytes.unsafe_of_string "") 0 0
                 outbuf 0 buffer_size Z_FINISH in
    flush outbuf used_out;
    if not finished then compr_finish()
    else deflate_end zs
  in
  compr, compr_finish

let uncompress ?(header = true) refill flush =
  let inbuf = Bytes.create buffer_size
  and outbuf = Bytes.create buffer_size in
  let zs = inflate_init header in
  let rec uncompr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then uncompr_finish true else uncompr 0 incount
    end else begin
      let (finished, used_in, used_out) =
        inflate zs inbuf inpos inavail outbuf 0 buffer_size Z_SYNC_FLUSH in
      flush outbuf used_out;
      if not finished then uncompr (inpos + used_in) (inavail - used_in)
    end
  and uncompr_finish first_finish =
    (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
       after the compressed stream in order to complete decompression
       and return finished = true. *)
    let dummy_byte = if first_finish && not header then 1 else 0 in
    let (finished, _, used_out) =
       inflate zs inbuf 0 dummy_byte outbuf 0 buffer_size Z_SYNC_FLUSH in
    flush outbuf used_out;
    if not finished then uncompr_finish false
  in
    uncompr 0 0;
    inflate_end zs
