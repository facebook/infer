(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module PathContext = PulsePathContext

val join :
     AbductiveDomain.t * PathContext.t
  -> AbductiveDomain.t * PathContext.t
  -> AbductiveDomain.t * PathContext.t

val join_summaries :
  AbductiveDomain.Summary.t -> AbductiveDomain.Summary.t -> AbductiveDomain.Summary.t
