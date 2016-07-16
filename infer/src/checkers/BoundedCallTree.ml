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
  type extras = ProcData.no_extras

  let exec_instr astate _ _ = function
    | Sil.Call (_, Const (Const.Cfun pn), _, _, _) ->
        Domain.add pn astate
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

let checker { Callbacks.proc_desc; tenv; } =
  ignore(Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv))
