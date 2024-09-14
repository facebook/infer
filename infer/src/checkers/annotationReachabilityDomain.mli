(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type call_site_info = {call_site: CallSite.t; is_in_loop: bool [@ignore]} [@@deriving compare]

module CallSites : AbstractDomain.FiniteSetS with type elt = call_site_info

module SinkMap : AbstractDomain.MapS with type key = Procname.t and type value = CallSites.t

include AbstractDomain.MapS with type key = Annot.t and type value = SinkMap.t

val add_call_site : Annot.t -> Procname.t -> call_site_info -> t -> t
