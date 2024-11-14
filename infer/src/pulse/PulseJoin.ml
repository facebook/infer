(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module PathContext = PulsePathContext

let join (_astate1, _path1) (_astate2, _path2) = (AbductiveDomain.empty, PathContext.initial)
