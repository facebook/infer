(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  L.die InternalError "!!Rust.capture not yet implented: prog:%s , arg:%s" prog
    (String.concat ~sep:" " args)
