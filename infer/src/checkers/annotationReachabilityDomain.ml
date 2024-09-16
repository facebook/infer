(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type call_site_info = {call_site: CallSite.t; is_in_loop: bool [@ignore]} [@@deriving compare]

module CSSet = PrettyPrintable.MakePPSet (struct
  type nonrec t = call_site_info [@@deriving compare]

  let pp fmt t =
    Format.fprintf fmt "%a%s" CallSite.pp t.call_site (if t.is_in_loop then " (inside loop)" else "")
end)

module CallSites = AbstractDomain.FiniteSetOfPPSet (CSSet)
module SinkMap = AbstractDomain.MapOfPPMap (Procname.Map) (CallSites)
include AbstractDomain.Map (Annot) (SinkMap)

let add_call_site annot sink call_site annot_map =
  let sink_map = find_opt annot annot_map |> Option.value ~default:SinkMap.empty in
  let call_sites = SinkMap.find_opt sink sink_map |> Option.value ~default:CallSites.empty in
  let call_sites' = CallSites.add call_site call_sites in
  let sink_map' = SinkMap.add sink call_sites' sink_map in
  add annot sink_map' annot_map
