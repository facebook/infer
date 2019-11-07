(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module DomainData = struct
  type self_pointer_kind = SELF | WEAK_SELF [@@deriving compare]

  let is_self kind = match kind with SELF -> true | WEAK_SELF -> false

  let is_weak_self kind = match kind with SELF -> false | WEAK_SELF -> true

  let pp_self_pointer_kind fmt kind =
    let s = match kind with SELF -> "SELF" | WEAK_SELF -> "WEAK_SELF" in
    F.fprintf fmt "%s" s


  type t = {pvar: Pvar.t; typ: Typ.t [@compare.ignore]; loc: Location.t; kind: self_pointer_kind}
  [@@deriving compare]

  let pp fmt {pvar; typ; loc; kind} =
    F.fprintf fmt "%a:%a, at %a (%a)" (Pvar.pp Pp.text) pvar (Typ.pp Pp.text) typ Location.pp loc
      pp_self_pointer_kind kind
end

module TransferFunctions = struct
  module Domain = AbstractDomain.FiniteSet (DomainData)
  module CFG = ProcCfg.Normal

  type extras = unit

  let pp_session_name _node fmt = F.pp_print_string fmt "SelfCapturedInBlock"

  let is_captured_strong_self attributes pvar =
    let pvar_name = Pvar.get_name pvar in
    Pvar.is_self pvar
    && List.exists
         ~f:(fun (captured, typ) -> Mangled.equal captured pvar_name && Typ.is_strong_pointer typ)
         attributes.ProcAttributes.captured


  let is_captured_weak_self attributes pvar =
    List.exists
      ~f:(fun (captured, typ) ->
        Mangled.equal captured (Pvar.get_name pvar)
        && String.is_substring ~substring:"self" (String.lowercase (Mangled.to_string captured))
        && Typ.is_weak_pointer typ )
      attributes.ProcAttributes.captured


  let exec_instr (astate : Domain.t) {ProcData.summary} _cfg_node (instr : Sil.instr) =
    let attributes = Summary.get_attributes summary in
    match instr with
    | Load {e= Lvar pvar; loc; typ} ->
        if is_captured_strong_self attributes pvar then
          Domain.add {pvar; typ; loc; kind= SELF} astate
        else if is_captured_weak_self attributes pvar then
          Domain.add {pvar; typ; loc; kind= WEAK_SELF} astate
        else astate
    | _ ->
        astate
end

let make_trace_elements domain =
  let trace_elems =
    TransferFunctions.Domain.fold
      (fun {pvar; loc} trace_elems ->
        let trace_elem_desc = F.asprintf "Using %a" (Pvar.pp Pp.text) pvar in
        let trace_elem = Errlog.make_trace_element 0 loc trace_elem_desc [] in
        trace_elem :: trace_elems )
      domain []
  in
  List.sort trace_elems ~compare:(fun {Errlog.lt_loc= loc1} {Errlog.lt_loc= loc2} ->
      Location.compare loc1 loc2 )


let report_issues summary domain =
  let weakSelf_opt =
    TransferFunctions.Domain.filter (fun {kind} -> DomainData.is_weak_self kind) domain
    |> TransferFunctions.Domain.choose_opt
  in
  let self_opt =
    TransferFunctions.Domain.filter (fun {kind} -> DomainData.is_self kind) domain
    |> TransferFunctions.Domain.choose_opt
  in
  match (weakSelf_opt, self_opt) with
  | Some {pvar= weakSelf; loc= weakLoc}, Some {pvar= self; loc= selfLoc} ->
      let message =
        F.asprintf
          "This block uses both %a (%a) and %a (%a). This could lead to retain cycles or \
           unexpected behavior."
          (Pvar.pp Pp.text) weakSelf Location.pp weakLoc (Pvar.pp Pp.text) self Location.pp selfLoc
      in
      let ltr = make_trace_elements domain in
      Reporting.log_error summary ~ltr ~loc:selfLoc IssueType.mixed_self_weakself message
  | _ ->
      ()


module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let checker {Callbacks.exe_env; summary} =
  let initial = TransferFunctions.Domain.empty in
  let procname = Summary.get_proc_name summary in
  let tenv = Exe_env.get_tenv exe_env procname in
  let proc_data = ProcData.make summary tenv () in
  ( if Typ.Procname.is_objc_block procname then
    match Analyzer.compute_post proc_data ~initial with
    | Some domain ->
        report_issues summary domain
    | None ->
        () ) ;
  summary
