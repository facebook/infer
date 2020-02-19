(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = string [@@deriving compare]

let from_string str = str

let to_string str = str

let pp = String.pp
