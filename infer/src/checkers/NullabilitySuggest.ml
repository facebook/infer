(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

module F = Format
module L = Logging

module Domain =
  AbstractDomain.FiniteSet(struct
    type t = AccessPath.Raw.t * Location.t

    (* Ignore location while comparing access paths.
     * They are there just for error reporting purpose, and are pretty much irrelevant
     * from the analysis' perspective *)
    let compare (ap0, _) (ap1, _) = [%compare: AccessPath.Raw.t] ap0 ap1

    let pp fmt (ap, loc) = F.fprintf fmt "(%a, %a)" AccessPath.Raw.pp ap Location.pp loc
  end)

(* Right now this is just a placeholder. We'll change it to something useful when necessary *)
module Summary = Summary.Make(struct
    type payload = unit

    let update_payload _ summary = summary
    let read_payload _ = None
  end)

type extras = unit

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type nonrec extras = extras

  let is_null = HilExp.is_null_literal

  let is_access_nullable ap proc_data =
    match AccessPath.Raw.get_field_and_annotation ap proc_data.ProcData.tenv with
    | Some (_, annot_item) ->
        Annotations.ia_is_nullable annot_item
    | _ -> false

  let exec_instr (astate : Domain.astate) proc_data _ (instr : HilInstr.t) =
    match instr with
    | Assume _ ->
        (* For now we just assume that conditionals does not have any implications on nullability *)
        astate
    | Call _ ->
        (* For now we just assume the callee always return non-null *)
        astate
    | Assign (lhs, rhs, loc) ->
        if is_null rhs && not (is_access_nullable lhs proc_data) then
          Domain.add (lhs, loc) astate
        else
          astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (LowerHil.Make (TransferFunctions))
module Interprocedural = AbstractInterpreter.Interprocedural (Summary)

let checker callback =
  let report nullable_aps (proc_data : extras ProcData.t) =
    let report_access_path (ap, loc) =
      let pname = Procdesc.get_proc_name proc_data.pdesc in
      let issue_kind = Localise.to_issue_id Localise.field_should_be_nullable in
      match AccessPath.Raw.get_field_and_annotation ap proc_data.tenv with
      | Some (field_name, _) ->
          let message =
            F.asprintf "Field %s should be annotated with @Nullable"
              (Fieldname.to_string field_name) in
          let exn = Exceptions.Checkers (issue_kind, Localise.verbatim_desc message) in
          Reporting.log_warning pname ~loc exn
      | _ -> ()
    in
    Domain.iter report_access_path nullable_aps
  in
  let compute_post (proc_data : extras ProcData.t) =
    (* Assume all fields are not null in the beginning *)
    let initial = Domain.empty, IdAccessPathMapDomain.empty in
    match Analyzer.compute_post proc_data ~initial ~debug:false with
    | Some (post, _) ->
        report post proc_data;
        Some ()
    | None ->
        failwithf
          "Analyzer failed to compute post for %a"
          Typ.Procname.pp (Procdesc.get_proc_name proc_data.pdesc)
  in
  Interprocedural.compute_and_store_post ~compute_post ~make_extras:(fun _ -> ()) callback
