(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

module UseDefChain = struct
  type t =
    | DependsOn of (Location.t * AccessPath.t)
    | NullDefCompare of (Location.t * AccessPath.t)
    | NullDefAssign of (Location.t * AccessPath.t)
  [@@deriving compare]

  let ( <= ) ~lhs ~rhs = compare lhs rhs <= 0

  (* Keep only one chain in join/widen as we are going to report only one
   * trace to the user eventually. *)
  let join lhs rhs = if ( <= ) ~lhs ~rhs then rhs else lhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | NullDefAssign (loc, ap) ->
        F.fprintf fmt "NullDefAssign(%a, %a)" Location.pp loc AccessPath.pp ap
    | NullDefCompare (loc, ap) ->
        F.fprintf fmt "NullDefCompare(%a, %a)" Location.pp loc AccessPath.pp ap
    | DependsOn (loc, ap) ->
        F.fprintf fmt "DependsOn(%a, %a)" Location.pp loc AccessPath.pp ap


  module Set = Caml.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module Domain = AbstractDomain.Map (AccessPath) (UseDefChain)

type extras = ProcData.no_extras

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type nonrec extras = extras

  let is_access_nullable ap proc_data =
    match AccessPath.get_field_and_annotation ap proc_data.ProcData.tenv with
    | Some (_, annot_item) ->
        Annotations.ia_is_nullable annot_item
    | _ ->
        false


  let rec nullable_usedef_chain_of exp lhs astate loc =
    match exp with
    | HilExp.Cast (_, e) ->
        nullable_usedef_chain_of e lhs astate loc
    | HilExp.Constant (Cint n) when IntLit.isnull n ->
        Some (UseDefChain.NullDefAssign (loc, lhs))
    | HilExp.AccessExpression access_expr -> (
      try
        let ap = HilExp.AccessExpression.to_access_path access_expr in
        match Domain.find ap astate with
        | UseDefChain.NullDefCompare _ ->
            (* Stop NullDefCompare from propagating here because we want to prevent
                 * the checker from suggesting @Nullable on y in the following case:
                 * if (x == null) ... else { y = x; } *)
            None
        | _ ->
            Some (UseDefChain.DependsOn (loc, ap))
      with Caml.Not_found -> None )
    | _ ->
        None


  let rec extract_null_compare_expr = function
    | HilExp.Cast (_, e) ->
        extract_null_compare_expr e
    | HilExp.BinaryOperator ((Eq | Ne), HilExp.AccessExpression access_expr, exp)
    | HilExp.BinaryOperator ((Eq | Ne), exp, HilExp.AccessExpression access_expr) ->
        Option.some_if (HilExp.is_null_literal exp)
          (HilExp.AccessExpression.to_access_path access_expr)
    | _ ->
        None


  let exec_instr (astate : Domain.t) proc_data _ (instr : HilInstr.t) =
    match instr with
    | Assume (expr, _, _, loc) -> (
      match extract_null_compare_expr expr with
      | Some ap when not (is_access_nullable ap proc_data) ->
          let udchain = UseDefChain.NullDefCompare (loc, ap) in
          Domain.add ap udchain astate
      | _ ->
          astate )
    | Call _ ->
        (* For now we just assume the callee always return non-null *)
        astate
    | Assign (lhs_access_expr, rhs, loc) ->
        let lhs = HilExp.AccessExpression.to_access_path lhs_access_expr in
        if not (is_access_nullable lhs proc_data) then
          match nullable_usedef_chain_of rhs lhs astate loc with
          | Some udchain ->
              Domain.add lhs udchain astate
          | None ->
              astate
        else astate
    | ExitScope _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "nullability suggest"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Exceptional))

let make_error_trace astate ap ud =
  let name_of ap =
    match AccessPath.get_last_access ap with
    | Some (AccessPath.FieldAccess field_name) ->
        "Field " ^ Typ.Fieldname.to_flat_string field_name
    | Some (AccessPath.ArrayAccess _) ->
        "Some array element"
    | None ->
        "Variable"
  in
  let open UseDefChain in
  let rec error_trace_impl seen depth ap = function
    | NullDefAssign (loc, src) ->
        let msg = F.sprintf "%s is assigned null here" (name_of src) in
        let ltr = [Errlog.make_trace_element depth loc msg []] in
        Some (loc, ltr)
    | NullDefCompare (loc, src) ->
        let msg = F.sprintf "%s is compared to null here" (name_of src) in
        let ltr = [Errlog.make_trace_element depth loc msg []] in
        Some (loc, ltr)
    | DependsOn (loc, dep) -> (
      match Domain.find dep astate with
      | exception Caml.Not_found ->
          None
      | ud' when Set.mem ud' seen ->
          None
      | ud' ->
          let msg = F.sprintf "%s could be assigned here" (name_of ap) in
          let trace_elem = Errlog.make_trace_element depth loc msg [] in
          let seen' = Set.add ud' seen in
          Option.map
            (error_trace_impl seen' (depth + 1) dep ud')
            ~f:(fun (_, trace) -> (loc, trace_elem :: trace)) )
  in
  error_trace_impl Set.empty 0 ap ud


let pretty_field_name proc_data field_name =
  match Procdesc.get_proc_name proc_data.ProcData.pdesc with
  | Typ.Procname.Java jproc_name ->
      let proc_class_name = Typ.Procname.Java.get_class_name jproc_name in
      let field_class_name = Typ.Fieldname.Java.get_class field_name in
      if String.equal proc_class_name field_class_name then Typ.Fieldname.to_flat_string field_name
      else Typ.Fieldname.to_simplified_string field_name
  | _ ->
      (* This format is subject to change once this checker gets to run on C/Cpp/ObjC *)
      Typ.Fieldname.to_string field_name


(* Checks if a field name stems from a class outside the domain of what is analyzed by Infer *)
let is_outside_codebase proc_name field_name =
  match proc_name with
  | Typ.Procname.Java _ ->
      Typ.Name.Java.is_external_classname (Typ.Fieldname.Java.get_class field_name)
  | _ ->
      false


let checker {Callbacks.summary; proc_desc; tenv} =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let annotation = Localise.nullable_annotation_name proc_name in
  let report astate (proc_data : extras ProcData.t) =
    let report_access_path ap udchain =
      match AccessPath.get_field_and_annotation ap proc_data.tenv with
      | Some (field_name, _) when is_outside_codebase proc_name field_name ->
          (* Skip reporting when the field is outside the analyzed codebase *)
          ()
      | Some (field_name, _) when Typ.Fieldname.Java.is_captured_parameter field_name ->
          (* Skip reporting when field comes from generated code *)
          ()
      | Some (field_name, _) ->
          let message =
            F.asprintf "Field %a should be annotated with %a" MF.pp_monospaced
              (pretty_field_name proc_data field_name)
              MF.pp_monospaced annotation
          in
          let loc, ltr =
            match make_error_trace astate ap udchain with
            | Some (loc, ltr) ->
                (loc, Some ltr)
            | None ->
                (Procdesc.get_loc proc_desc, None)
          in
          Reporting.log_warning summary ~loc ?ltr IssueType.nullsafe_field_not_nullable message
      | _ ->
          ()
    in
    Domain.iter report_access_path astate
  in
  let proc_name = Procdesc.get_proc_name proc_desc in
  if AndroidFramework.is_destroy_method proc_name then
    (* Skip the fields nullified in Fragment onDestroy and onDestroyView *)
    summary
  else
    (* Assume all fields are not null in the beginning *)
    let initial = Domain.empty in
    let proc_data = ProcData.make_default proc_desc tenv in
    ( match Analyzer.compute_post proc_data ~initial with
    | Some post ->
        report post proc_data
    | None ->
        L.internal_error "Analyzer failed to compute post for %a@." Typ.Procname.pp proc_name ) ;
    summary
