(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* Suppress unused value warning until this is used for incremental diff analysis *)
val iter : f:(Summary.t -> unit) -> unit
  [@@warning "-32"]
(** Iterates over all summaries from the .specs files *)

val iter_from_config : f:(Summary.t -> unit) -> unit
(** Iterates over all sumaries from the .specs files unless a list of specs files has been passed on
    the command line *)

(* Suppress unused value warning until this is used for incremental diff analysis *)
val delete : Typ.Procname.t -> unit
  [@@warning "-32"]
(** Delete the .specs file associated with a summary and remove the summary from the caches in
    Summary.ml and ondemand.ml *)
