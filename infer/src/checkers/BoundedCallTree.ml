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

(** find transitive procedure calls for each procedure *)

module ProcnameSet = PrettyPrintable.MakePPSet(struct
    type t = Procname.t
    let compare = Procname.compare
    let pp_element = Procname.pp
  end)

module Domain = AbstractDomain.FiniteSet(ProcnameSet)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = Stacktrace.t

  let exec_instr astate proc_data _ = function
    | Sil.Call (_, Const (Const.Cfun pn), _, _, _) ->
        (** TODO: Match class. *)
        let caller = Cfg.Procdesc.get_proc_name proc_data.ProcData.pdesc in
        let matches_proc frame =
          frame.Stacktrace.method_str = (Procname.get_method caller) in
        let proc_in_trace = IList.exists
            matches_proc
            proc_data.ProcData.extras.Stacktrace.frames in
        if proc_in_trace then Domain.add pn astate else astate
    | Sil.Call _ ->
        (** We currently ignore calls through function pointers in C and
         * other potential special kinds of procedure calls to be added later,
         * e.g. Java reflection. *)
        astate
    | Sil.Letderef _ | Set _ | Prune _ | Declare_locals _
    | Stackop _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

let checker { Callbacks.proc_desc; tenv; } trace =
  ignore(Analyzer.exec_pdesc (ProcData.make proc_desc tenv trace))
