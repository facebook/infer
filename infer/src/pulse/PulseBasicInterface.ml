(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Basic Pulse modules that are safe to use in any module *)

module AbstractValue = PulseAbstractValue
module Attribute = PulseAttribute
module Attributes = PulseAttribute.Attributes
module CallEvent = PulseCallEvent
module CItv = PulseCItv
module Diagnostic = PulseDiagnostic
module Invalidation = PulseInvalidation

module PathCondition = ( val if Config.pulse_path_conditions then (module PulsePathCondition)
                             else (module PulseDummyPathCondition) : PulsePathConditionModuleType.S
                       )

module SkippedCalls = PulseSkippedCalls
module Trace = PulseTrace
module ValueHistory = PulseValueHistory

(** {2 Enforce short form usage} *)

include struct
  [@@@warning "-60"]

  module PulseAbstractValue = PulseAbstractValue
  [@@deprecated "use the short form AbstractValue instead"]
  module PulseAttribute = PulseAttribute [@@deprecated "use the short form Attribute instead"]
  module PulseCallEvent = PulseCallEvent [@@deprecated "use the short form CallEvent instead"]
  module PulseCItv = PulseCItv [@@deprecated "use the short form CItv instead"]
  module PulseDiagnostic = PulseDiagnostic [@@deprecated "use the short form Diagnostic instead"]
  module PulseInvalidation = PulseInvalidation
  [@@deprecated "use the short form Invalidation instead"]
  module PulsePathCondition = PulsePathCondition
  [@@deprecated "use the short form PathCondition instead"]
  module PulseSkippedCalls = PulseSkippedCalls
  [@@deprecated "use the short form SkippedCalls instead"]
  module PulseTrace = PulseTrace [@@deprecated "use the short form Trace instead"]
  module PulseValueHistory = PulseValueHistory
  [@@deprecated "use the short form ValueHistory instead"]
end
