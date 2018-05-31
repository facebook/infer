(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module that define preanalysis to derive nullability annotations *)

open! IStd

val analysis : Cfg.t -> Tenv.t -> unit
(** Analysis the cfg and updates the tenv with nullability annotations *)
