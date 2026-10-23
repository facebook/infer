(***************************************************************************)
(* Copyright 1999-2010, Jérôme Vouillon                                    *)
(*                                                                         *)
(*  This library is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU Lesser General Public License as         *)
(*  published by the Free Software Foundation, either version 2 of the     *)
(*  License, or (at your option) any later version.  A special linking     *)
(*  exception to the GNU Lesser General Public License applies to this     *)
(*  library, see the LICENSE file for more information.                    *)
(***************************************************************************)

type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type tf =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t

val length : t -> int
(*
val to_string : t -> string
 *)
val of_string : string -> t

val mmap_of_string : Unix.file_descr -> string -> t

val to_floatarray : tf -> int -> float array

val to_this_floatarray : float array -> tf -> int -> float array

val of_floatarray : float array -> tf
(*
val sub : t -> int -> int -> string
 *)
val blit_from_string : string -> int -> t -> int -> int -> unit
(*
val blit_to_bytes : t -> int -> bytes -> int -> int -> unit
 *)
val prefix : t -> t -> int -> bool

val marshal : 'a -> Marshal.extern_flags list -> t

val unmarshal : t -> int -> 'a

val marshal_to_buffer : t -> int -> 'a -> Marshal.extern_flags list -> int
