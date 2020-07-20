(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Basic Pulse.ISL modules that are safe to use in any module *)

module AbstractValue = PulseISLAbstractValue
module Attribute = PulseISLAttribute
module Attributes = PulseISLAttribute.Attributes
module CallEvent = PulseISLCallEvent
module Diagnostic = PulseISLDiagnostic
module Invalidation = PulseISLInvalidation
module PathCondition = PulseISLPathCondition
module SkippedCalls = PulseISLSkippedCalls
module Trace = PulseISLTrace
module ValueHistory = PulseISLValueHistory

(** {2 Enforce short form usage} *)
                    
                   
include struct
  [@@@warning "-60"]

  module PulseISLAbstractValue = PulseISLAbstractValue
  [@@deprecated "use the short form AbstractValue instead"]
  module PulseISLAttribute = PulseISLAttribute [@@deprecated "use the short form Attribute instead"]
  module PulseISLCallEvent = PulseISLCallEvent [@@deprecated "use the short form CallEvent instead"]
  module PulseISLDiagnostic = PulseISLDiagnostic [@@deprecated "use the short form Diagnostic instead"]
  module PulseISLInvalidation = PulseISLInvalidation
  [@@deprecated "use the short form Invalidation instead"]
  module PulseISLPathCondition = PulseISLPathCondition
  [@@deprecated "use the short form PathCondition instead"]
  module PulseISLSkippedCalls = PulseISLSkippedCalls
  [@@deprecated "use the short form SkippedCalls instead"]
  module PulseISLSledge = PulseISLSledge [@@deprecated "use Pudge instead"]
  module PulseISLDummySledge = PulseISLDummySledge [@@deprecated "use Pudge instead"]
  module PulseISLTrace = PulseISLTrace [@@deprecated "use the short form Trace instead"]
  module PulseISLValueHistory = PulseISLValueHistory
   [@@deprecated "use the short form ValueHistory instead"]

end
