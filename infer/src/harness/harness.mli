(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Automatically create a harness method to exercise code under test *)

val create_harness : Cfg.cfg -> Cg.t -> Tenv.t -> unit
(** Generate a harness method for exe_env and add it to the execution environment *)
