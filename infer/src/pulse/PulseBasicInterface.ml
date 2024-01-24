(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Basic Pulse modules that are safe to use in any module *)

module AbstractValue = PulseAbstractValue
module Access = PulseAccess
module AccessSet = PulseAccess.Set
module Attribute = PulseAttribute
module Attributes = PulseAttribute.Attributes
module CallEvent = PulseCallEvent
module Formula = PulseFormula
module Invalidation = PulseInvalidation
module SatUnsat = PulseSatUnsat
module SkippedCalls = PulseSkippedCalls
module TaintConfig = PulseTaintConfig
module TaintItem = PulseTaintItem
module Timestamp = PulseTimestamp
module Trace = PulseTrace
module TransitiveInfo = PulseTransitiveInfo
module ValueHistory = PulseValueHistory
module CellId = ValueHistory.CellId
module ValueOrigin = PulseValueOrigin
include SatUnsat.Types

(** {2 Enforce short form usage} *)

include struct
  [@@@warning "-unused-module"]

  module PulseAbstractValue = PulseAbstractValue
  [@@deprecated "use the short form AbstractValue instead"]
  module PulseAttribute = PulseAttribute [@@deprecated "use the short form Attribute instead"]
  module PulseCallEvent = PulseCallEvent [@@deprecated "use the short form CallEvent instead"]
  module PulseInvalidation = PulseInvalidation
  [@@deprecated "use the short form Invalidation instead"]
  module PulseTimestamp = PulseTimestamp [@@deprecated "use the short form Timestamp instead"]
  module PulseSkippedCalls = PulseSkippedCalls
  [@@deprecated "use the short form SkippedCalls instead"]
  module PulseTaintConfig = PulseTaintConfig [@@deprecated "use the short form TaintConfig instead"]
  module PulseTaintItem = PulseTaintItem [@@deprecated "use the short form TaintItem instead"]
  module PulseTrace = PulseTrace [@@deprecated "use the short form Trace instead"]
  module PulseValueHistory = PulseValueHistory
  [@@deprecated "use the short form ValueHistory instead"]
  module PulseSatUnsat = PulseSatUnsat [@@deprecated "use the short form SatUnsat instead"]
end
