(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module CallSites : AbstractDomain.FiniteSetS with type elt = CallSite.t

module SinkMap : AbstractDomain.MapS with type key = Procname.t and type value = CallSites.t

include AbstractDomain.MapS with type key = Annot.t and type value = SinkMap.t

val add_call_site : Annot.t -> Procname.t -> CallSite.t -> t -> t
