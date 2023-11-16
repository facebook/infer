(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val skip_unwanted :
     string (* Analysis name for logs *)
  -> max_size:int option
  -> ('payload InterproceduralAnalysis.t -> 'arg -> 'summary option)
  -> 'payload InterproceduralAnalysis.t
  -> 'arg
  -> 'summary option
(** Takes a checker and returns a checker that will skip synthethic and too-big functions. *)
