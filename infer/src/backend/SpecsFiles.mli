(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* Suppress unused value warning because this method is intended for incremental analysis, which is
   not yet finished *)
val iter : f:(Summary.t -> unit) -> unit
  [@@warning "-32"]
(** Iterates over all summaries from the .specs files *)

val iter_from_config : f:(Summary.t -> unit) -> unit
(** Iterates over all sumaries from the .specs files unless a list of specs files has been passed on
    the command line *)
