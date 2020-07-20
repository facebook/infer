(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module ExecutionDomain = PulseISLExecutionDomain

module AbductiveDomain = PulseISLAbductiveDomain
module PostStatus = AbductiveDomain.PostStatus
(** if you do any mutations of the state in pulseISL you probably want this module *)

module Stack = AbductiveDomain.Stack
module Memory = AbductiveDomain.Memory
module AddressAttributes = AbductiveDomain.AddressAttributes

module BaseDomain = PulseISLBaseDomain
(** use only if you know what you are doing or you risk break bi-abduction *)

module BaseStack = PulseISLBaseStack
module BaseMemory = PulseISLBaseMemory
module BaseAddressAttributes = PulseISLBaseAddressAttributes

(** {2 Enforce short form usage} *)

include struct
  [@@@warning "-60"]

  module PulseISLAbductiveDomain = PulseISLAbductiveDomain
  [@@deprecated "use the short form AbductiveDomain instead"]
  module PulseISLBaseDomain = PulseISLBaseDomain [@@deprecated "use the short form BaseDomain instead"]
  module PulseISLBaseStack = PulseISLBaseStack [@@deprecated "use the short form BaseStack instead"]
  module PulseISLBaseMemory = PulseISLBaseMemory [@@deprecated "use the short form BaseMemory instead"]
  module PulseISLBaseAddressAttributes = PulseISLBaseAddressAttributes
  [@@deprecated "use the short form BaseAddressAttributes instead"]
  module PulseISLExecutionDomain = PulseISLExecutionDomain
  [@@deprecated "use the short form ExecutionDomain instead"]
end
