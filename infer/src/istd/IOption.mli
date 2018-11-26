(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val find_value_exn : 'a option -> 'a
(** Like [Option.value_exn] but raises [Caml.Not_found] when called with [None]. *)

val value_default_f : f:(unit -> 'a) -> 'a option -> 'a
(** Like [Option.value ~default:(f ())] but [f] is called only if [None]. *)
