(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain

let exec_load ~id:_ ~e:_ ~typ:_ ~loc:_ (astate : AbductiveDomain.t) = astate

let exec_store ~lhs:_ ~rhs:_ ~typ:_ ~loc:_ (astate : AbductiveDomain.t) = astate
