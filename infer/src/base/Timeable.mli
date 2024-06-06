(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** types of everything we might possibly want to time *)
type t =
  | Checker of Checker.t
  | Preanalysis  (** the "pre-analysis" phase of a procedure, before we run the checkers *)

val to_string : t -> string

val pp : F.formatter -> t -> unit

module Map : Caml.Map.S with type key = t

val mk_map_of_all : init:'a -> 'a Map.t
(** [mk_map_of_all ~init] is a map [timeable -> init] for all [timeable] keys *)
