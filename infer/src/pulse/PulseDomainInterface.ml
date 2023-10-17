(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

[@@@warning "-unused-module"]

(** {1 High-level Pulse Operations}

    If you do any mutations of the state in pulse that go beyond what {!PulseOperations} offers you
    may want these modules (but consider adding what you need to [PulseOperations] or to one of the
    models modules if appropriate). In addition to defining short names for safe versions of
    stack/heap/... it ensures only those safe versions are used. Do not use lower-level modules
    directly! *)

(** {2 High-level modules operating on the entire biabductive state} *)

module AbductiveDomain = PulseAbductiveDomain
module Stack = AbductiveDomain.Stack
module Memory = AbductiveDomain.Memory
module AddressAttributes = AbductiveDomain.AddressAttributes
module CanonValue = AbductiveDomain.CanonValue

(** {2 Low-level modules operating on individual elements of the abductive domain} *)

module BaseStack = CanonValue.Stack
module BaseMemory = CanonValue.Memory
module CanonAccess = BaseMemory.Access
module CanonAccessSet = BaseMemory.Access.Set
module BaseAddressAttributes = CanonValue.Attributes
module Decompiler = PulseAbductiveDecompiler
module NonDisjDomain = PulseNonDisjunctiveDomain
module PathContext = PulsePathContext

(** {2 Miscellaneous} *)

module AccessResult = PulseAccessResult
module DecompilerExpr = PulseDecompilerExpr
module Diagnostic = PulseDiagnostic
module ExecutionDomain = PulseExecutionDomain
module LatentIssue = PulseLatentIssue

(** {2 Unsafe modules, use with caution}

    Misusing these modules may miss abducing addresses to the pre-condition or mistakenly manipulate
    non-normalized values. See {!PulseAbductiveDomain} for more information.

    These can safely be used when reading summaries, where all values have already been normalized
    on creation. *)

module BaseDomain = PulseBaseDomain
module UnsafeStack = PulseBaseStack
module UnsafeMemory = PulseBaseMemory
module UnsafeAttributes = PulseBaseAddressAttributes

(** {2 Enforce short form usage} *)

include struct
  [@@@warning "-unused-module"]

  module PulseAbductiveDomain = PulseAbductiveDomain
  [@@deprecated "use the short form AbductiveDomain instead"]
  module PulseAccessResult = PulseAccessResult
  [@@deprecated "use the short form AccessResult instead"]
  module PulseBaseDomain = PulseBaseDomain [@@deprecated "use the short form BaseDomain instead"]
  module PulseBaseStack = PulseBaseStack [@@deprecated "use the short form BaseStack instead"]
  module PulseBaseMemory = PulseBaseMemory [@@deprecated "use the short form BaseMemory instead"]
  module PulseBaseAddressAttributes = PulseBaseAddressAttributes
  [@@deprecated "use the short form BaseAddressAttributes instead"]
  module PulseAbductiveDecompiler = PulseAbductiveDecompiler
  [@@deprecated "use the short form Decompiler instead"]
  module PulseDiagnostic = PulseDiagnostic [@@deprecated "use the short form Diagnostic instead"]
  module PulseExecutionDomain = PulseExecutionDomain
  [@@deprecated "use the short form ExecutionDomain instead"]
  module PulseLatentIssue = PulseLatentIssue [@@deprecated "use the short form LatentIssue instead"]
  module PulsePathContext = PulsePathContext [@@deprecated "use the short form PathContext instead"]
end

include PulseResult.Type
