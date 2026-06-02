(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : files:string list -> unit
(** Capture a list of ToyLang ([.toy]) source files using the tutorial frontend [TutoFrontend]:
    translate each to SIL and store it in the capture database. *)
