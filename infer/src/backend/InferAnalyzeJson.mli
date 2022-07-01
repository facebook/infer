(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Main module for the analysis after the capture phase *)

val parse_tenv : Yojson.Safe.t -> Tenv.t

val parse_cfg : Yojson.Safe.t -> Cfg.t
