(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val open_in : string -> in_channel

val open_out : string -> out_channel

val string_starts_with : string -> string -> bool

val string_ends_with : string -> string -> bool

val string_split : char -> string -> string list

val string_join : char -> string list -> string

val list_starts_with : 'a list -> 'a list -> bool

val list_ends_with : 'a list -> 'a list -> bool

val make_cached : ('a -> 'b) -> 'a -> 'b

val line_stream_of_channel : in_channel -> string Stream.t

val stream_concat : 'a Stream.t Stream.t -> 'a Stream.t

val stream_append : 'a Stream.t -> 'a Stream.t -> 'a Stream.t

val stream_map : ('a -> 'b) -> 'a Stream.t -> 'b Stream.t

val stream_filter : ('a -> bool) -> 'a Stream.t -> 'a Stream.t

val stream_fold : ('a -> 'b -> 'b) -> 'b -> 'a Stream.t -> 'b

val stream_to_list : 'a Stream.t -> 'a list

val assert_true : string -> bool -> unit

val assert_false : string -> bool -> unit

val assert_equal : string -> 'a -> 'a -> unit

module DisjointSet : sig
  type 'a t

  val create : unit -> 'a t

  val find : 'a t -> 'a -> 'a

  val union : 'a t -> 'a -> 'a -> unit

  val iter : 'a t -> ('a -> 'a -> unit) -> unit
end

val fix_arg_spec :
  (Arg.key * Arg.spec * Arg.doc) list -> Arg.usage_msg -> (Arg.key * Arg.spec * Arg.doc) list
