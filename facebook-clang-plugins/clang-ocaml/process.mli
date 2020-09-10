(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val file_exists : string -> bool

val mktemp : string -> string

val wait : int -> bool

val exec : string array -> in_channel -> out_channel -> out_channel -> bool

val close_in : in_channel -> unit

val close_out : out_channel -> unit

val tee : in_channel -> out_channel list -> unit

val copy : in_channel -> out_channel -> unit

val diff : string -> string -> out_channel -> bool

val gzip : in_channel -> out_channel -> bool

val gunzip : in_channel -> out_channel -> bool

val fork : (out_channel -> bool) -> int * in_channel

val compose :
     (in_channel -> out_channel -> bool)
  -> (in_channel -> out_channel -> bool)
  -> in_channel
  -> out_channel
  -> bool

val diff_on_same_input :
     (in_channel -> out_channel -> bool)
  -> (in_channel -> out_channel -> bool)
  -> in_channel
  -> out_channel
  -> bool
