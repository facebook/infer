(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module FileRenamings : sig
  type renaming = {current: string; previous: string}

  type t

  val empty : t

  val from_json : string -> t

  val from_json_file : string -> t

  val find_previous : t -> string -> string option

  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY : sig
    val from_renamings : renaming list -> t

    val equal : t -> t -> bool

    val pp : Format.formatter -> t -> unit
  end
end

val do_filter :
     Differential.t
  -> FileRenamings.t
  -> skip_duplicated_types:bool
  -> interesting_paths:SourceFile.t list option
  -> Differential.t

module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY : sig
  val relative_complements :
       compare:('a -> 'a -> int)
    -> ?pred:('a -> bool)
    -> 'a list
    -> 'a list
    -> 'a list * 'a list * 'a list

  val skip_duplicated_types_on_filenames : FileRenamings.t -> Differential.t -> Differential.t

  val interesting_paths_filter :
    SourceFile.t list option -> Jsonbug_t.jsonbug list -> Jsonbug_t.jsonbug list
end
