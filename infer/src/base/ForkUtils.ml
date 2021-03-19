(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let protect ~f x =
  if Config.is_child then
    (* Few things need to be done in fork-exec mode, as the process just went through Infer's
       initialization phase: most of the steps below (except opening the db) just got done. *)
    ResultsDatabase.new_database_connection ()
  else (
    Epilogues.reset () ;
    L.reset_formatters () ;
    ResultsDatabase.new_database_connection () ;
    (* get different streams of random numbers in each fork, in particular to lessen contention in
       `Filename.mk_temp` *)
    Random.self_init () ) ;
  f x
