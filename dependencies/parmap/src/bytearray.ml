(***************************************************************************)
(* bytearray.ml : functions for efficient marshaling to and from bigarrays *)
(*                                                                         *)
(* Copyright 1999-2011, Jérôme Vouillon                                    *)
(*                                                                         *)
(*  This library is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU Lesser General Public License as         *)
(*  published by the Free Software Foundation, either version 2 of the     *)
(*  License, or (at your option) any later version.  A special linking     *)
(*  exception to the GNU Lesser General Public License applies to this     *)
(*  library, see the LICENSE file for more information.                    *)
(***************************************************************************)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

type tf = (float, float64_elt, c_layout) Array1.t

let length = Bigarray.Array1.dim

let create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

let createf l = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout l

(*
let unsafe_blit_from_string s i a j l =
  for k = 0 to l - 1 do
    a.{j + k} <- s.[i + k]
  done

let unsafe_blit_to_string a i s j l =
  for k = 0 to l - 1 do
    s.[j + k] <- a.{i + k}
  done
*)

external unsafe_blit_from_string : string -> int -> t -> int -> int -> unit
  = "ml_blit_string_to_bigarray" [@@noalloc]
(*
external unsafe_blit_to_bytes : t -> int -> bytes -> int -> int -> unit
  = "ml_blit_bigarray_to_string" "noalloc"
 *)
(*
let unsafe_sub a ofs len =
  let s = Bytes.create len in
  unsafe_blit_to_bytes a ofs s 0 len;
  Bytes.to_string s
 *)
(*
let to_string a =
  let l = length a in
  if l > Sys.max_string_length then invalid_arg "Bytearray.to_string" else
  unsafe_sub a 0 l
 *)
let of_string s =
  let l = String.length s in
  let a = create l in
  unsafe_blit_from_string s 0 a 0 l;
  a

let mmap_of_string fd s =
  let l = String.length s in
  let ba =
    Bigarray.array1_of_genarray @@
    Parmap_compat.map_file fd Bigarray.char Bigarray.c_layout true [|l|] in
  unsafe_blit_from_string s 0 ba 0 l;
  ba
(*
let sub a ofs len =
  if
    ofs < 0 || len < 0 || ofs > length a - len || len > Sys.max_string_length
  then
    invalid_arg "Bytearray.sub"
  else
    unsafe_sub a ofs len
 *)
let rec prefix_rec a i a' i' l =
  l = 0 ||
  (a.{i} = a'.{i'} && prefix_rec a (i + 1) a' (i' + 1) (l - 1))

let prefix a a' i =
  let l = length a in
  let l' = length a' in
  i <= l' - l &&
  prefix_rec a 0 a' i l

let blit_from_string s i a j l =
  if l < 0 || i < 0 || i > String.length s - l
           || j < 0 || j > length a - l
  then invalid_arg "Bytearray.blit_from_string"
  else unsafe_blit_from_string s i a j l
(*
let blit_to_bytes a i s j l =
  if l < 0 || i < 0 || i > length a - l
           || j < 0 || j > Bytes.length s - l
  then invalid_arg "Bytearray.blit_to_bytes"
  else unsafe_blit_to_bytes a i s j l
 *)
external marshal : 'a -> Marshal.extern_flags list -> t
  = "ml_marshal_to_bigarray"

external marshal_to_buffer : t -> int -> 'a -> Marshal.extern_flags list -> int
  = "ml_marshal_to_bigarray_buffer"

external unmarshal : t -> int -> 'a
  = "ml_unmarshal_from_bigarray"

external unsafe_blit_from_floatarray : float array -> int -> tf -> int -> int -> unit
  = "ml_blit_floatarray_to_bigarray" [@@noalloc]

external unsafe_blit_to_floatarray : tf -> int -> float array -> int -> int -> unit
  = "ml_blit_bigarray_to_floatarray" [@@noalloc]

let to_floatarray a l =
  let fa = Array.create_float l in
  unsafe_blit_to_floatarray a 0 fa 0 l;
  fa

let to_this_floatarray fa a l =
  unsafe_blit_to_floatarray a 0 fa 0 l;
  fa

let of_floatarray fa =
  let l = Array.length fa in
  let a = createf l in
  unsafe_blit_from_floatarray fa 0 a 0 l;
  a
