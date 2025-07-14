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

(** Reading and writing to/from [gzip] compressed files

   This module provides functions to read and write compressed data
   to/from files in [gzip] format. *)

(** {1 Reading from compressed files} *)

type in_channel
       (** Abstract type representing a channel opened for reading
           from a compressed file. *)
val open_in: string -> in_channel
       (** Open a compressed file for reading.  The argument is the file
           name. *)
val open_in_chan: Stdlib.in_channel -> in_channel
       (** Open a compressed file for reading.  The argument is a
           regular file channel already opened on the compressed file. *)
val input_char: in_channel -> char
       (** Uncompress one character from the given channel, and return it.
           Raise [End_of_file] if no more compressed data is available. *)
val input_byte: in_channel -> int
       (** Same as [Gzip.input_char], but return the 8-bit integer
           representing the character.
           Raise [End_of_file] if no more compressed data is available. *)
val input: in_channel -> bytes -> int -> int -> int
       (** [input ic buf pos len] uncompresses up to [len] characters
           from the given channel [ic],
           storing them in string [buf], starting at character number [pos].
           It returns the actual number of characters read, between 0 and
           [len] (inclusive).
           A return value of 0 means that the end of file was reached.
           A return value between 0 and [len] exclusive means that
           not all requested [len] characters were read, either because
           no more characters were available at that time, or because
           the implementation found it convenient to do a partial read;
           [input] must be called again to read the remaining characters,
           if desired.  (See also [Gzip.really_input] for reading
           exactly [len] characters.)
           Exception [Invalid_argument "Gzip.input"] is raised if
           [pos] and [len] do not designate a valid substring of [buf]. *)
val really_input: in_channel -> bytes -> int -> int -> unit
       (** [really_input ic buf pos len] uncompresses [len] characters
           from the given channel, storing them in
           string [buf], starting at character number [pos].
           Raise [End_of_file] if fewer than [len] characters can be read.
           Raise [Invalid_argument "Gzip.input"] if
           [pos] and [len] do not designate a valid substring of [buf]. *)
val close_in: in_channel -> unit
       (** Close the given input channel.  If the channel was created with
           [Gzip.open_in_chan], the underlying regular file channel
           (of type [Stdlib.in_channel]) is also closed.
           Do not apply any of the functions above to a closed channel. *)
val dispose: in_channel -> unit
       (** Same as [Gzip.close_in], but does not close the underlying
           regular file channel (of type [Stdlib.in_channel]);
           just dispose of the resources associated with the decompression
           channel.  This can be useful if e.g. the underlying file channel
           is a network socket on which more (uncompressed) data
           is expected. *)

(** {1 Writing to compressed files} *)

type out_channel
       (** Abstract type representing a channel opened for writing
           to a compressed file. *)
val open_out: ?level:int -> string -> out_channel
       (** Open a compressed file for writing.  The argument is the file
           name.  The file is created if it does not exist, or
           truncated to zero length if it exists.
           The optional [level] argument (an integer between 1 and 9)
           indicates the compression level, with 1 being the weakest
           (but fastest) compression and 9 being the strongest
           (but slowest) compression.  The default level is 6
           (medium compression). *)
val open_out_chan: ?level:int -> Stdlib.out_channel -> out_channel
       (** Open a compressed file for writing.  The argument is a
           regular file channel already opened on the compressed file.
           The optional [level] argument sets the compression level
           as documented for [Gzip.open_out]. *)
val output_char: out_channel -> char -> unit
       (** Output one character to the given compressed channel. *)
val output_byte: out_channel -> int -> unit
       (** Same as [Gzip.output_char], but the output character is given
           by its code.  The given integer is taken modulo 256. *)
val output: out_channel -> bytes -> int -> int -> unit
       (** [output oc buf pos len] compresses and writes [len] characters
           from string [buf], starting at offset [pos], and writes the
           compressed data to the channel [oc].
           Raise [Invalid_argument "Gzip.output"] if
           [pos] and [len] do not designate a valid substring of [buf]. *)
val output_substring: out_channel -> string -> int -> int -> unit
       (** Same as [output], but takes a string as argument instead of
           a byte sequence.
          @since 1.06 *)
val close_out: out_channel -> unit
       (** Close the given output channel.  If the channel was created with
           [Gzip.open_out_chan], the underlying regular file channel
           (of type [Stdlib.out_channel]) is also closed.
           Do not apply any of the functions above to a closed channel. *)
val flush: out_channel -> unit
       (** Same as [Gzip.close_out], but do not close the underlying
           regular file channel (of type [Stdlib.out_channel]);
           just flush all pending compressed data and
           dispose of the resources associated with the compression
           channel.  This can be useful if e.g. the underlying file channel
           is a network socket on which more data is to be sent. *)
val flush_continue: out_channel -> unit
       (** Flush all pending compressed data through both the compression
           channel and the underlying regular file channel, but keep both
           channels open to accept further data. *)

(** {1 Error reporting} *)

exception Error of string
       (** Exception raised by the functions above to signal errors during
           compression or decompression, or ill-formed input files. *)
