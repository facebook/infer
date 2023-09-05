(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let protect ~f x =
  (* Some steps must be done only in 'fork' mode. In fork-exec mode, the process just went through
     Infer's initialization phase, and those steps need not be redone. *)
  if Option.is_none Config.run_as_child then (
    Epilogues.reset () ;
    L.reset_formatters () ) ;
  Database.new_database_connections Primary ;
  (* get different streams of random numbers in each fork, in particular to lessen contention in
     `Filename.mk_temp` *)
  Random.self_init () ;
  f x
