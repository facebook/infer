(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type checker_without_payload = LoopHoisting

(** This is given to the callback iteration, so that only a subset of them are triggered when
    necessary. *)
type t = private
  | All  (** Request the analyses for all payloads enabled *)
  | One of PayloadId.t  (** Request an analysis for one payload *)
  | CheckerWithoutPayload of checker_without_payload
      (** Request an analysis of a checker that has no payload in DB *)
[@@deriving equal, hash, show]

val all : t

val one : PayloadId.t -> t

val checker_without_payload : checker_without_payload -> t
