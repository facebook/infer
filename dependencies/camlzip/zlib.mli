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

val compress:
  ?level: int -> ?header: bool -> 
  (bytes -> int) -> (bytes -> int -> unit) -> unit

val compress_direct:
  ?level: int -> ?header: bool -> (bytes -> int -> unit) ->
  (bytes -> int -> int -> unit) * (unit -> unit)

val uncompress:
  ?header: bool -> (bytes -> int) -> (bytes -> int -> unit) -> unit

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
