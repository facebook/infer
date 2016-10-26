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

module Domain = AbstractDomain.FiniteSet(Pvar.Set)

module Summary = Summary.Make (struct
    type summary = Domain.astate

    let update_payload astate payload =
      { payload with Specs.globals_read = Some astate }

    let read_from_payload payload =
      match payload.Specs.globals_read with
      | Some astate -> astate
      | None -> Domain.initial
  end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = ProcData.no_extras

  let get_globals e =
    Exp.get_vars e |> snd |> IList.filter Pvar.is_global |> Domain.of_list

  let exec_instr astate { ProcData.pdesc; tenv } _ (instr : Sil.instr) = match instr with
    | Load (_, exp, _, _)
    | Store (_, _, exp, _)
    | Prune (exp, _, _, _) ->
        let globals = get_globals exp in
        Domain.union astate globals
    | Call (_, Const (Cfun callee_pname), params, _, _) ->
        let param_globals =
          IList.map fst params
          |> IList.map get_globals
          |> IList.fold_left Domain.union astate in
        let callee_globals =
          Option.default Domain.initial
          @@ Summary.read_summary tenv pdesc callee_pname in
        Domain.union callee_globals param_globals
    | Call (_, _, params, _, _) ->
        IList.map fst params
        |> IList.map get_globals
        |> IList.fold_left Domain.union astate
    | Declare_locals _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Backward(ProcCfg.Exceptional))
    (Scheduler.ReversePostorder)
    (TransferFunctions)

module Interprocedural = Analyzer.Interprocedural (Summary)

let report_siof pname loc bad_globals =
  let pp_desc fmt () =
    let pp_var f v =
      let pp_source f v = match Pvar.get_source_file v with
        | Some source_file when DB.source_file_equal DB.source_file_empty source_file ->
            Format.fprintf f ""
        | None ->
            Format.fprintf f ""
        | Some source_file ->
            Format.fprintf f " from file %s" (DB.source_file_to_string source_file) in
      Format.fprintf f "%s%a" (Pvar.get_simplified_name v) pp_source v in
    let pp_set f s = pp_seq pp_var f (Pvar.Set.elements s) in
    Format.fprintf fmt
      "This global variable initializer accesses the following globals in another translation \
       unit: %a"
      pp_set bad_globals in
  let description = pp_to_string pp_desc () in
  let exn = Exceptions.Checkers
      ("STATIC_INITIALIZATION_ORDER_FIASCO", Localise.verbatim_desc description) in
  Reporting.log_error pname ~loc exn


let siof_check pdesc = function
  | Some post ->
      let attrs = Cfg.Procdesc.get_attributes pdesc in
      let is_orig_file f = match attrs.ProcAttributes.translation_unit with
        | Some orig_file ->
            let orig_path = DB.source_file_to_abs_path orig_file in
            string_equal orig_path @@ DB.source_file_to_abs_path f
        | None -> false in
      let is_foreign v = Option.map_default
          (fun f -> not @@ is_orig_file f) false (Pvar.get_source_file v) in
      let foreign_globals = Domain.filter is_foreign post in
      if not (Domain.is_empty foreign_globals) then
        report_siof (Cfg.Procdesc.get_proc_name pdesc) attrs.ProcAttributes.loc foreign_globals;
  | None -> ()

let checker callback =
  let pdesc = callback.Callbacks.proc_desc in
  let post = Interprocedural.checker callback ProcData.empty_extras in
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  match pname with
  | Procname.C c when Procname.is_globals_initializer c ->
      siof_check pdesc post
  | _ -> ()
