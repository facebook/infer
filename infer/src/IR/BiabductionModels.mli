(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val mem : Procname.t -> bool
(** Check if a summary for a given procedure exists in the models directory *)
