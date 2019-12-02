(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module UnixDiff : sig
  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY : sig
    type t = Unchanged | New | Old

    val equal : t -> t -> bool

    val process_raw_directives : string -> t list

    val pp : Format.formatter -> t -> unit
  end
end

module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY : sig
  val parse_directives : UnixDiff.VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY.t list -> int list
end

val parse_unix_diff : string -> int list
(** Given a difference between two files, return the relevant lines in the new file; a line is
    relevant when a change took place in it, or nearby. To generate a valid input for this parser,
    use unix-diff command with the following formatter arguments: diff --unchanged-line-format="U"
    \--old-line-format="O" --new-line-format="N" File1 File2 *)
