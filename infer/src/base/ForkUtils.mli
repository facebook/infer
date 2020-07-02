(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val protect : f:('a -> 'b) -> 'a -> 'b
(** does the bookkeeping necessary to safely execute an infer function [f] after a call to fork(2) *)
