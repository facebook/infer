(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val expect_1_arg : 'a list -> string -> 'a

val expect_2_args : 'a list -> string -> 'a * 'a

val expect_3_args : 'a list -> string -> 'a * 'a * 'a

val expect_4_args : 'a list -> string -> 'a * 'a * 'a * 'a

val expect_5_args : 'a list -> string -> 'a * 'a * 'a * 'a * 'a

val expect_at_least_1_arg : 'a list -> string -> 'a * 'a list

val expect_at_least_2_args : 'a list -> string -> 'a * 'a * 'a list

val expect_at_least_3_args : 'a list -> string -> 'a * 'a * 'a * 'a list
