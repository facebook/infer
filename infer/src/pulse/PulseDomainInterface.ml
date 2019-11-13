(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module AbductiveDomain = PulseAbductiveDomain
(** if you do any mutations of the state in pulse you probably want this module *)

module Stack = AbductiveDomain.Stack
module Memory = AbductiveDomain.Memory

module BaseDomain = PulseBaseDomain
(** use only if you know what you are doing or you risk break bi-abduction *)

module BaseStack = PulseBaseStack
module BaseMemory = PulseBaseMemory
