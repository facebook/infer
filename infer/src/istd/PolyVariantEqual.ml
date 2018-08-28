(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

let ( = ) (v1 : [> ]) (v2 : [> ]) = Polymorphic_compare.( = ) v1 v2
