(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
(* Module that define preanalysis to derive nullability annotations *)

open! IStd

(* Analysis the cfg and updates the tenv with nullability annotations *)

val analysis : Cfg.cfg -> Tenv.t -> unit
