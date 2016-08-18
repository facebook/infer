(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

(** Create a taint analysis from a trace domain *)
module Make (TraceDomain : Trace.S) = struct

  module TaintDomain = AccessTree.Make (TraceDomain)
  module IdMapDomain = IdAccessPathMapDomain

  module Domain = struct
    type astate =
      {
        access_tree : TaintDomain.astate; (* mapping of access paths to trace sets *)
        id_map : IdMapDomain.astate; (* mapping of id's to access paths for normalization *)
      }

    let initial =
      let access_tree = TaintDomain.initial in
      let id_map = IdMapDomain.initial in
      { access_tree; id_map; }

    let (<=) ~lhs ~rhs =
      if lhs == rhs
      then true
      else
        TaintDomain.(<=) ~lhs:lhs.access_tree ~rhs:rhs.access_tree &&
        IdMapDomain.(<=) ~lhs:lhs.id_map ~rhs:rhs.id_map

    let join astate1 astate2 =
      if astate1 == astate2
      then astate1
      else
        let access_tree = TaintDomain.join astate1.access_tree astate2.access_tree in
        let id_map = IdMapDomain.join astate1.id_map astate2.id_map in
        { access_tree; id_map; }

    let widen ~prev ~next ~num_iters =
      if prev == next
      then prev
      else
        let access_tree =
          TaintDomain.widen ~prev:prev.access_tree ~next:next.access_tree ~num_iters in
        let id_map = IdMapDomain.widen ~prev:prev.id_map ~next:next.id_map ~num_iters in
        { access_tree; id_map; }

    let pp fmt { access_tree; id_map; } =
      F.fprintf fmt "(%a, %a)" TaintDomain.pp access_tree IdMapDomain.pp id_map
  end

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type formal_list = AccessPath.base list
    type extras = formal_list

    let add_source source ret_id ret_typ access_tree =
      let trace = TraceDomain.of_source source in
      let id_ap = AccessPath.Exact (AccessPath.of_id ret_id ret_typ) in
      TaintDomain.add_trace id_ap trace access_tree

    let exec_instr ({ Domain.access_tree; } as astate) proc_data _ instr =
      match instr with
      | Sil.Call (ret_ids, Const (Cfun callee_pname), _, callee_loc, _) ->
          let call_site = CallSite.make callee_pname callee_loc in
          let ret_typ =
            match callee_pname with
            | Procname.Java java_pname ->
                let ret_typ_str = Procname.java_get_return_type java_pname in
                Option.default
                  Typ.Tvoid
                  (Tenv.lookup_java_typ_from_string (proc_data.ProcData.tenv) ret_typ_str)
            | Procname.C _ ->
                Typ.Tvoid (* for tests only, since tests use C-style procnames *)
            | _ ->
                failwith "Unimp: looking up return type for non-Java procedure" in

          let astate_with_source =
            match TraceDomain.Source.get call_site, ret_ids with
            | [(0, source)], [ret_id] ->
                let access_tree' = add_source source ret_id ret_typ access_tree in
                { astate with Domain.access_tree = access_tree'; }
            | [], _ |  _, [] ->
                astate
            | _ ->
                (* this is allowed by SIL, but not currently used in any frontends *)
                failwith "Unimp: handling multiple return ids" in
          astate_with_source
      | Sil.Call _ ->
          failwith "Unimp: non-pname call expressions"
      | Sil.Letderef _ | Set _ ->
          failwith "Unimp: assignment, load, and store"
      | Sil.Prune _ | Remove_temps _ | Nullify _ | Abstract _ | Stackop _ | Declare_locals _ ->
          astate
  end

  module Analyzer = AbstractInterpreter.Make
      (ProcCfg.Normal)
      (Scheduler.ReversePostorder)
      (TransferFunctions)

  let checker { Callbacks.proc_name; proc_desc; tenv; } =
    let formals =
      let attrs = Cfg.Procdesc.get_attributes proc_desc in
      IList.map
        (fun (name, typ) -> AccessPath.base_of_pvar (Pvar.mk name proc_name) typ)
        attrs.formals in
    let proc_data = ProcData.make proc_desc tenv formals in
    ignore (Analyzer.compute_post proc_data)
end
