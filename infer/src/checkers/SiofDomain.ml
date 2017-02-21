(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module VarNames = PrettyPrintable.MakePPSet(String)

module BottomSiofTrace = AbstractDomain.BottomLifted(SiofTrace)

include AbstractDomain.Pair
    (BottomSiofTrace)
    (AbstractDomain.FiniteSet(VarNames))

(** group together procedure-local accesses *)
let normalize ((trace, initialized) as astate) = match trace with
  | BottomSiofTrace.Bottom -> astate
  | BottomSiofTrace.NonBottom trace ->
      let elems = SiofTrace.Sinks.elements (SiofTrace.sinks trace) in
      let (direct, indirect) = IList.partition SiofTrace.is_intraprocedural_access elems in
      match direct with
      | [] | _::[] -> astate
      | access::_ ->
          (* [loc] should be the same for all local accesses: it's the loc of the enclosing
             procdesc. Use the loc of the first access. *)
          let loc = CallSite.loc (SiofTrace.Sink.call_site access) in
          let kind =
            IList.map SiofTrace.Sink.kind direct
            |> List.fold
              ~f:SiofTrace.GlobalsAccesses.union
              ~init:SiofTrace.GlobalsAccesses.empty in
          let trace' =
            SiofTrace.make_access kind loc::indirect
            |> SiofTrace.Sinks.of_list
            |> SiofTrace.update_sinks trace in
          (BottomSiofTrace.NonBottom trace', initialized)
