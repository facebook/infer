(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module FileRenamings :
sig
  type renaming = {
    current: string;
    previous: string;
  }
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

val do_filter : Differential.t -> FileRenamings.t -> skip_duplicated_types:bool -> Differential.t

module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY : sig
  val relative_complements :
    cmp:('a -> 'a -> int) -> ?pred:('a -> bool) -> 'a list -> 'a list -> 'a list * 'a list
  val skip_duplicated_types_on_filenames : FileRenamings.t -> Differential.t -> Differential.t
  val java_anon_class_pattern : Str.regexp
  val value_of_qualifier_tag : Jsonbug_t.tag_value_record list -> string -> string option
  val skip_anonymous_class_renamings : Differential.t -> Differential.t
  val resolve_infer_eradicate_conflict :
    Config.analyzer ->
    (Config.analyzer -> Inferconfig.filters) ->
    Differential.t -> Differential.t
end
