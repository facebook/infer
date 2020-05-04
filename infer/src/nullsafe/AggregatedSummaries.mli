(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Results of proc-level analysis, grouped by Java class they belong to. Useful for class-level
    analysis, when summaries for all methods are calculated and need to be combined together. *)

(** Aggregated information for each user defined (not anonymous) Java class *)
module ClassInfo : sig
  type t

  val get_all_summaries : t -> NullsafeSummary.t list
  (** List of all summaries, user-level and anonymous, combined together *)
end

type t

val make_empty : unit -> t

val register_summary : JavaClassName.t -> NullsafeSummary.t -> t -> t
(** Add information about summary to the class map. Depending on if this an anonymous or
    user-defined class, adds it to corresponding lists. *)

val group_by_user_class : t -> (JavaClassName.t * ClassInfo.t) list
(** List of pairs <user defined class name, info about this class *)
