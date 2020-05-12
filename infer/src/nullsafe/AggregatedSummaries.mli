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

  val get_class_name : t -> JavaClassName.t

  val get_summaries : t -> NullsafeSummary.t list

  val get_nested_classes_info : t -> t list

  val get_nested_anonymous_summaries : t -> NullsafeSummary.t list JavaClassName.Map.t
  (** List of all anonymous class summaries belonging to this class, together with name of anonymous
      class. This is a flattenned list, so we don't care if one anonymous class is nested inside the
      other. *)

  val get_recursive_summaries : t -> (JavaClassName.t * NullsafeSummary.t) list
  (** A flattened list of all summaries, user-level, nested, and anonymous, combined together *)

  val pp : Format.formatter -> t -> unit
end

val aggregate : (JavaClassName.t * NullsafeSummary.t) list -> ClassInfo.t list
(** Given a list of all summaries and their classes, group them by names and aggregate in a list of
    top-level classes. *)
