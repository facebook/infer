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

module UseDefChain = struct
  type astate =
    | NullDef of (Location.t * AccessPath.Raw.t)
    | DependsOn of (Location.t * AccessPath.Raw.t)

  (* We explicitly write out the comparator here instead of relying on
   * [@@deriving compare], as the alternative may not always guarantee the
   * desirable order. *)
  let compare_astate lhs rhs = match lhs, rhs with
    | NullDef lorig, NullDef rorig ->
        [%compare : Location.t * AccessPath.Raw.t] rorig lorig
    | DependsOn ldep, DependsOn rdep ->
        [%compare : Location.t * AccessPath.Raw.t] rdep ldep
    | NullDef _, DependsOn _ ->
        1
    | DependsOn _, NullDef _ ->
        -1

  let (<=) ~lhs ~rhs =
    compare_astate lhs rhs <= 0

  (* Keep only one chain in join/widen as we are going to report only one
   * trace to the user eventually. *)
  let join lhs rhs =
    if (<=) ~lhs ~rhs then rhs else lhs

  let widen ~prev ~next ~num_iters:_ =
    join prev next

  let pp fmt = function
    | NullDef (loc, ap) ->
        F.fprintf fmt "NullDef(%a, %a)" Location.pp loc AccessPath.Raw.pp ap
    | DependsOn (loc, ap) ->
        F.fprintf fmt "DependsOn(%a, %a)" Location.pp loc AccessPath.Raw.pp ap
end
module Domain = AbstractDomain.Map (AccessPath.Raw) (UseDefChain)

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

  let is_access_nullable ap proc_data =
    match AccessPath.Raw.get_field_and_annotation ap proc_data.ProcData.tenv with
    | Some (_, annot_item) ->
        Annotations.ia_is_nullable annot_item
    | _ -> false

  let nullable_usedef_chain_of exp lhs astate loc =
    match exp with
    | HilExp.Constant (Cint n) when IntLit.isnull n ->
        Some (UseDefChain.NullDef (loc, lhs))
    | HilExp.AccessPath ap ->
        if Domain.mem ap astate then
          Some (UseDefChain.DependsOn (loc, ap))
        else
          None
    | _ -> None

  let exec_instr (astate : Domain.astate) proc_data _ (instr : HilInstr.t) =
    match instr with
    | Assume _ ->
        (* For now we just assume that conditionals does not have any implications on nullability *)
        astate
    | Call _ ->
        (* For now we just assume the callee always return non-null *)
        astate
    | Assign (lhs, rhs, loc) ->
        if not (is_access_nullable lhs proc_data) then
          match nullable_usedef_chain_of rhs lhs astate loc with
          | Some udchain ->
              Domain.add lhs udchain astate
          | None ->
              astate
        else
          astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (LowerHil.Make (TransferFunctions))
module Interprocedural = AbstractInterpreter.Interprocedural (Summary)

let make_error_trace astate ap ud =
  let name_of ap =
    match AccessPath.Raw.get_last_access ap with
    | Some (AccessPath.FieldAccess field_name) ->
        "Field " ^ (Fieldname.to_flat_string field_name)
    | Some (AccessPath.ArrayAccess _) ->
        "Some array element"
    | None ->
        "Variable"
  in
  let open UseDefChain in
  let rec error_trace_impl depth ap = function
    | NullDef (loc, src) ->
        let msg = F.sprintf "%s is assigned null here" (name_of src) in
        let ltr = [Errlog.make_trace_element depth loc msg []] in
        Some (loc, ltr)
    | DependsOn (loc, dep) ->
        try
          let ud' = Domain.find dep astate in
          let msg = F.sprintf "%s could be assigned here" (name_of ap) in
          let trace_elem = Errlog.make_trace_element depth loc msg [] in
          Option.map (error_trace_impl (depth+1) dep ud') ~f:(
            fun (_, trace) -> loc, trace_elem :: trace
          )
        with
          Not_found -> None
  in
  error_trace_impl 0 ap ud

let checker callback =
  let report astate (proc_data : extras ProcData.t) =
    let report_access_path ap udchain =
      let pname = Procdesc.get_proc_name proc_data.pdesc in
      let issue_kind = Localise.to_issue_id Localise.field_should_be_nullable in
      match AccessPath.Raw.get_field_and_annotation ap proc_data.tenv with
      | Some (field_name, _) ->
          let message =
            F.asprintf "Field %s should be annotated with @Nullable"
              (Fieldname.to_string field_name) in
          let exn = Exceptions.Checkers (issue_kind, Localise.verbatim_desc message) in
          begin
            match make_error_trace astate ap udchain with
            | Some (loc, ltr) ->
                Reporting.log_warning pname ~loc ~ltr exn
            | None ->
                Reporting.log_warning pname exn
          end
      | _ -> ()
    in
    Domain.iter report_access_path astate
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
