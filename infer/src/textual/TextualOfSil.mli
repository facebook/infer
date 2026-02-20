(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val from_java : filename:string -> Tenv.t -> Cfg.t -> unit
(** generate a .sil file with name [filename] containing all the functions in the given cfg *)
