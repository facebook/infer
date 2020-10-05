(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module ExecutionDomain = PulseExecutionDomain

(** if you do any mutations of the state in pulse you probably want this module *)
module AbductiveDomain = PulseAbductiveDomain

module Stack = AbductiveDomain.Stack
module Memory = AbductiveDomain.Memory
module AddressAttributes = AbductiveDomain.AddressAttributes

(** use only if you know what you are doing or you risk break bi-abduction *)
module BaseDomain = PulseBaseDomain

module BaseStack = PulseBaseStack
module BaseMemory = PulseBaseMemory
module BaseAddressAttributes = PulseBaseAddressAttributes
module LatentIssue = PulseLatentIssue

(** {2 Enforce short form usage} *)

include struct
  [@@@warning "-60"]

  module PulseAbductiveDomain = PulseAbductiveDomain
  [@@deprecated "use the short form AbductiveDomain instead"]
  module PulseBaseDomain = PulseBaseDomain [@@deprecated "use the short form BaseDomain instead"]
  module PulseBaseStack = PulseBaseStack [@@deprecated "use the short form BaseStack instead"]
  module PulseBaseMemory = PulseBaseMemory [@@deprecated "use the short form BaseMemory instead"]
  module PulseBaseAddressAttributes = PulseBaseAddressAttributes
  [@@deprecated "use the short form BaseAddressAttributes instead"]
  module PulseExecutionDomain = PulseExecutionDomain
  [@@deprecated "use the short form ExecutionDomain instead"]
  module PulseLatentIssue = PulseLatentIssue [@@deprecated "use the short form LatentIssue instead"]
end
