(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

let ( = ) (v1 : [> ]) (v2 : [> ]) = Poly.equal v1 v2

let ( <> ) (v1 : [> ]) (v2 : [> ]) = not (v1 = v2)
