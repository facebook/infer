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

(* Module [Gzip]: reading and writing to/from [gzip] compressed files *)

exception Error of string

let buffer_size = 1024

type in_channel =
  { in_chan: Stdlib.in_channel;
    in_buffer: bytes;
    mutable in_pos: int;
    mutable in_avail: int;
    mutable in_eof: bool;
    in_stream: Zlib.stream;
    mutable in_size: int32;
    mutable in_crc: int32 }

let open_in_chan ic =
  (* Superficial parsing of header *)
  begin try
    let id1 = input_byte ic in
    let id2 = input_byte ic in
    if id1 <> 0x1F || id2 <> 0x8B then
      raise(Error("bad magic number, not a gzip file"));
    let cm = input_byte ic in
    if cm <> 8 then
      raise(Error("unknown compression method"));
    let flags = input_byte ic in
    if flags land 0xE0 <> 0 then
      raise(Error("bad flags, not a gzip file"));
    for i = 1 to 6 do ignore(input_byte ic) done;
    if flags land 0x04 <> 0 then begin
      (* Skip extra data *)
      let len1 = input_byte ic in
      let len2 = input_byte ic in
      for i = 1 to len1 + len2 lsl 8 do ignore(input_byte ic) done
    end;
    if flags land 0x08 <> 0 then begin
      (* Skip original file name *)
      while input_byte ic <> 0 do () done
    end;
    if flags land 0x10 <> 0 then begin
      (* Skip comment *)
      while input_byte ic <> 0 do () done
    end;
    if flags land 0x02 <> 0 then begin
      (* Skip header CRC *)
      ignore(input_byte ic); ignore(input_byte ic)
    end
  with End_of_file ->
    raise(Error("premature end of file, not a gzip file"))
  end;
  { in_chan = ic;
    in_buffer = Bytes.create buffer_size;
    in_pos = 0;
    in_avail = 0;
    in_eof = false;
    in_stream = Zlib.inflate_init false;
    in_size = Int32.zero;
    in_crc = Int32.zero }

let open_in filename =
  let ic = Stdlib.open_in_bin filename in
  try
    open_in_chan ic
  with exn ->
    Stdlib.close_in ic; raise exn

let read_byte iz =
  if iz.in_avail = 0 then begin
    let n = Stdlib.input iz.in_chan iz.in_buffer 0
                             (Bytes.length iz.in_buffer) in
    if n = 0 then raise End_of_file;
    iz.in_pos <- 0;
    iz.in_avail <- n
  end;
  let c = Bytes.get iz.in_buffer iz.in_pos in
  iz.in_pos <- iz.in_pos + 1;
  iz.in_avail <- iz.in_avail - 1;
  Char.code c

let read_int32 iz =
  let b1 = read_byte iz in
  let b2 = read_byte iz in
  let b3 = read_byte iz in
  let b4 = read_byte iz in
  Int32.logor (Int32.of_int b1)
    (Int32.logor (Int32.shift_left (Int32.of_int b2) 8)
      (Int32.logor (Int32.shift_left (Int32.of_int b3) 16)
                   (Int32.shift_left (Int32.of_int b4) 24)))

let rec input iz buf pos len =
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    invalid_arg "Gzip.input";
  if iz.in_eof then 0 else begin
    if iz.in_avail = 0 then begin
      let n = Stdlib.input iz.in_chan iz.in_buffer 0
                               (Bytes.length iz.in_buffer) in
      if n = 0 then raise(Error("truncated file"));
      iz.in_pos <- 0;
      iz.in_avail <- n
    end;
    let (finished, used_in, used_out) =
      try
        Zlib.inflate iz.in_stream iz.in_buffer iz.in_pos iz.in_avail
                                   buf pos len Zlib.Z_SYNC_FLUSH
      with Zlib.Error(_, _) ->
        raise(Error("error during decompression")) in
    iz.in_pos <- iz.in_pos + used_in;
    iz.in_avail <- iz.in_avail - used_in;
    iz.in_crc <- Zlib.update_crc iz.in_crc buf pos used_out;
    iz.in_size <- Int32.add iz.in_size (Int32.of_int used_out);
    if finished then begin
      try
        let crc = read_int32 iz in
        let size = read_int32 iz in
        if iz.in_crc <> crc then
          raise(Error("CRC mismatch, data corrupted"));
        if iz.in_size <> size then
          raise(Error("size mismatch, data corrupted"));
        iz.in_eof <- true;
        used_out
      with End_of_file ->
        raise(Error("truncated file"))
    end
    else if used_out = 0 then
      input iz buf pos len
    else
      used_out
  end

let rec really_input iz buf pos len =
  if len <= 0 then () else begin
    let n = input iz buf pos len in
    if n = 0 then raise End_of_file;
    really_input iz buf (pos + n) (len - n)
  end

let char_buffer = Bytes.create 1

let input_char iz =
  if input iz char_buffer 0 1 = 0
  then raise End_of_file
  else Bytes.get char_buffer 0

let input_byte iz =
  Char.code (input_char iz)

let dispose iz =
  iz.in_eof <- true;
  Zlib.inflate_end iz.in_stream

let close_in iz =
  dispose iz;
  Stdlib.close_in iz.in_chan

type out_channel =
  { out_chan: Stdlib.out_channel;
    out_buffer: bytes;
    mutable out_pos: int;
    mutable out_avail: int;
    out_stream: Zlib.stream;
    mutable out_size: int32;
    mutable out_crc: int32 }

let open_out_chan ?(level = 6) oc =
  if level < 1 || level > 9 then invalid_arg "Gzip.open_out: bad level";
  (* Write minimal header *)
  output_byte oc 0x1F;                  (* ID1 *)
  output_byte oc 0x8B;                  (* ID2 *)
  output_byte oc 8;                     (* compression method *)
  output_byte oc 0;                     (* flags *)
  for i = 1 to 4 do output_byte oc 0 done; (* mtime *)
  output_byte oc 0;                     (* xflags *)
  output_byte oc 0xFF;                  (* OS (unknown) *)
  { out_chan = oc;
    out_buffer = Bytes.create buffer_size;
    out_pos = 0;
    out_avail = buffer_size;
    out_stream = Zlib.deflate_init level false;
    out_size = Int32.zero;
    out_crc = Int32.zero }

let open_out ?(level = 6) filename =
  open_out_chan ~level (Stdlib.open_out_bin filename)

let flush_and_reset_out_buffer oz =
  Stdlib.output oz.out_chan oz.out_buffer 0 oz.out_pos;
  oz.out_pos <- 0;
  oz.out_avail <- Bytes.length oz.out_buffer

let rec output oz buf pos len =
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    invalid_arg "Gzip.output";
  (* If output buffer is full, flush it *)
  if oz.out_avail = 0 then flush_and_reset_out_buffer oz;
  (* Patch request #1428: Zlib disallows zero-length writes *)
  if len > 0 then begin
    let (_, used_in, used_out) =
      try
        Zlib.deflate oz.out_stream buf pos len
                                   oz.out_buffer oz.out_pos oz.out_avail
                                   Zlib.Z_NO_FLUSH
      with Zlib.Error(_, _) ->
        raise (Error("error during compression")) in
    oz.out_pos <- oz.out_pos + used_out;
    oz.out_avail <- oz.out_avail - used_out;
    oz.out_size <- Int32.add oz.out_size (Int32.of_int used_in);
    oz.out_crc <- Zlib.update_crc oz.out_crc buf pos used_in;
    if used_in < len then output oz buf (pos + used_in) (len - used_in)
  end

let output_substring oz buf pos len =
  output oz (Bytes.unsafe_of_string buf) pos len

let output_char oz c =
  Bytes.set char_buffer 0 c;
  output oz char_buffer 0 1

let output_byte oz b =
  output_char oz (Char.unsafe_chr b)

let write_int32 oc n =
  let r = ref n in
  for i = 1 to 4 do
    Stdlib.output_byte oc (Int32.to_int !r);
    r := Int32.shift_right_logical !r 8
  done

let flush_to_out_chan ~flush_command oz =
  let rec do_flush () =
    (* If output buffer is full, flush it *)
    if oz.out_avail = 0 then flush_and_reset_out_buffer oz;
    let (finished, _, used_out) =
      Zlib.deflate oz.out_stream oz.out_buffer 0 0
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 flush_command in
    oz.out_pos <- oz.out_pos + used_out;
    oz.out_avail <- oz.out_avail - used_out;
    (* When we use the Z_FINISH command, we must retry if finished is false. For all other
     * flush commands, we should retry if we have filled the output buffer *)
    let continue = (flush_command = Zlib.Z_FINISH && not finished) || oz.out_avail = 0 in
    if continue then do_flush() in
  do_flush();
  (* Final data flush *)
  if oz.out_pos > 0 then flush_and_reset_out_buffer oz

let flush_continue oz =
  (* Flush everything to the underlying file channel, then flush the channel. *)
  flush_to_out_chan ~flush_command:Zlib.Z_SYNC_FLUSH oz;
  Stdlib.flush oz.out_chan

let flush oz =
  (* Flush everything to the output channel. *)
  flush_to_out_chan ~flush_command:Zlib.Z_FINISH oz;
  (* Write CRC and size *)
  write_int32 oz.out_chan oz.out_crc;
  write_int32 oz.out_chan oz.out_size;
  (* Dispose of stream *)
  Zlib.deflate_end oz.out_stream

let close_out oz =
  flush oz;
  Stdlib.close_out oz.out_chan

