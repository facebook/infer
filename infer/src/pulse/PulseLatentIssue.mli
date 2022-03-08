(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module Diagnostic = PulseDiagnostic

(** A subset of [PulseDiagnostic] that can be "latent", i.e. there is a potential issue in the code
    but we want to delay reporting until we see the conditions for the bug manifest themselves in
    some calling context. *)

type t =
  | AccessToInvalidAddress of Diagnostic.access_to_invalid_address
  | ErlangError of Diagnostic.erlang_error
  | ReadUninitializedValue of Diagnostic.read_uninitialized_value
[@@deriving compare, equal, yojson_of]

val to_diagnostic : t -> Diagnostic.t

val should_report : AbductiveDomain.summary -> Diagnostic.t -> [> `DelayReport of t | `ReportNow]

val add_call : CallEvent.t * Location.t -> t -> t
