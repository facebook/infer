(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = CPP_INSTANCE | OBJC_INSTANCE | CPP_CLASS | OBJC_CLASS | BLOCK | C_FUNCTION
[@@deriving compare, equal]

let to_string = function
  | CPP_INSTANCE ->
      "CPP_INSTANCE"
  | OBJC_INSTANCE ->
      "OBJC_INSTANCE"
  | CPP_CLASS ->
      "CPP_CLASS"
  | OBJC_CLASS ->
      "OBJC_CLASS"
  | BLOCK ->
      "BLOCK"
  | C_FUNCTION ->
      "C_FUNCTION"
