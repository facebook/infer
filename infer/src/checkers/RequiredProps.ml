(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Domain = LithoDomain

(* VarProp is only for props that have a varArg parameter like
     @Prop(varArg = "var_prop") whereas Prop is for everything except. *)
type required_prop = Prop of string | VarProp of {prop: string; var_prop: string}

let get_required_props typename tenv =
  let is_required annot_list =
    List.exists
      ~f:(fun ({Annot.parameters} as annot) ->
        Annotations.annot_ends_with annot Annotations.prop
        && (* Don't count as required if it's @Prop(optional = true) *)
        not
          (List.exists
             ~f:(fun Annot.{name; value} ->
               match (name, value) with Some "optional", Annot.Bool true -> true | _ -> false )
             parameters ) )
      annot_list
  in
  let get_var_args annot_list =
    List.fold ~init:None
      ~f:(fun acc ({Annot.parameters} as annot) ->
        if Annotations.annot_ends_with annot Annotations.prop then
          (* Pick up the parameter for varArg if it has the form
             @Prop(varArg = myProp). *)
          List.fold ~init:acc
            ~f:(fun acc Annot.{name; value} ->
              match (name, value) with
              | Some "varArg", Annot.Str str_value ->
                  Some str_value
              | _ ->
                  acc )
            parameters
        else acc )
      annot_list
  in
  match Tenv.lookup tenv typename with
  | Some {fields} ->
      List.filter_map
        ~f:(fun {Struct.name= fieldname; annot} ->
          if is_required annot then
            let prop = Fieldname.get_field_name fieldname in
            let var_prop_opt = get_var_args annot in
            Some
              (Option.value_map var_prop_opt ~default:(Prop prop) ~f:(fun var_prop ->
                   VarProp {var_prop; prop} ) )
          else None )
        fields
  | None ->
      []


let report_missing_required_prop proc_desc err_log prop parent_typename ~create_loc call_chain =
  let message =
    let prop_string =
      match prop with
      | Prop prop ->
          F.asprintf "`@Prop %s`" prop
      | VarProp {var_prop; prop} ->
          F.asprintf "Either `@Prop %s` or `@Prop(varArg = %s)`" prop var_prop
    in
    F.asprintf
      "%a is required for component `%s`, but is not set before the call to `build()`. Either set \
       the missing `@Prop` or make `@Prop(optional = true)`."
      MarkupFormatter.pp_bold prop_string (Typ.Name.name parent_typename)
  in
  let make_single_trace loc message = Errlog.make_trace_element 0 loc message [] in
  let create_message = F.asprintf "calls %s.create(...)" (Typ.Name.name parent_typename) in
  let ltr =
    make_single_trace create_loc message
    :: make_single_trace create_loc create_message
    :: List.map call_chain ~f:(fun Domain.MethodCallPrefix.{procname; location} ->
           let call_msg =
             F.asprintf "calls %a" (Procname.pp_simplified_string ~withclass:false) procname
           in
           Errlog.make_trace_element 0 location call_msg [] )
  in
  Reporting.log_issue proc_desc err_log ~loc:create_loc ~ltr LithoRequiredProps
    IssueType.missing_required_prop message


let has_prop prop_set prop =
  let check prop =
    String.Set.mem prop_set prop
    || (* @Prop(resType = ...) myProp can also be set via myProp(), myPropAttr(), myPropDip(), myPropPx(), myPropRes() or myPropSp().
          Our annotation parameter parsing is too primitive to identify resType, so just assume
          that all @Prop's can be set any of these 6 ways. *)
    String.Set.exists prop_set ~f:(fun el ->
        String.chop_prefix el ~prefix:prop
        |> Option.exists ~f:(fun suffix -> String.Set.mem LithoDomain.suffixes suffix) )
  in
  match prop with
  | Prop prop ->
      check prop
  | VarProp {var_prop; prop} ->
      (* @Prop(varArg = myProp) List <?> myPropList can also be set
         via myPropList() or myProp().*)
      check var_prop || check prop


(** return true if this function is part of the Litho framework code rather than client code *)
let is_litho_function = function
  | Procname.Java java_procname -> (
    match Procname.Java.get_package java_procname with
    | Some "com.facebook.litho" ->
        true
    | _ ->
        false )
  | _ ->
      false


let is_builder procname tenv =
  match procname with
  | Procname.Java java_procname ->
      let class_name = Procname.Java.get_class_type_name java_procname in
      Domain.is_component_or_section_builder class_name tenv
  | _ ->
      false


let is_component_or_section procname tenv =
  match procname with
  | Procname.Java java_procname ->
      let class_name = Procname.Java.get_class_type_name java_procname in
      PatternMatch.is_subtype_of_str tenv class_name "com.facebook.litho.Component"
      || PatternMatch.is_subtype_of_str tenv class_name "com.facebook.litho.sections.Section"
  | _ ->
      false


let is_build_method procname tenv =
  match Procname.get_method procname with "build" -> is_builder procname tenv | _ -> false


let is_create_method procname tenv =
  match Procname.get_method procname with
  | "create" ->
      is_component_or_section procname tenv
  | _ ->
      false


let get_component_create_typ_opt procname tenv =
  match procname with
  | Procname.Java java_pname when is_create_method procname tenv ->
      Some (Procname.Java.get_class_type_name java_pname)
  | _ ->
      None


let satisfies_heuristic ~callee_pname ~callee_summary_opt tenv =
  (* If the method is build() or create() itself or doesn't contain a build() in
     its summary, we want to track it in the domain. *)
  let build_exists_in_callees =
    Option.exists callee_summary_opt ~f:LithoDomain.Mem.contains_build
  in
  is_build_method callee_pname tenv
  || is_create_method callee_pname tenv
  ||
  match callee_pname with
  | Procname.Java java_callee_procname ->
      not (Procname.Java.is_static java_callee_procname || build_exists_in_callees)
  | _ ->
      not build_exists_in_callees


let should_report pname tenv = not (is_litho_function pname || is_build_method pname tenv)

let report {InterproceduralAnalysis.proc_desc; tenv; err_log} astate =
  let check_on_string_set parent_typename create_loc call_chain prop_set =
    let required_props = get_required_props parent_typename tenv in
    List.iter required_props ~f:(fun required_prop ->
        if not (has_prop prop_set required_prop) then
          report_missing_required_prop proc_desc err_log required_prop parent_typename ~create_loc
            call_chain )
  in
  Domain.check_required_props ~check_on_string_set astate


type analysis_data =
  { interproc: LithoDomain.summary InterproceduralAnalysis.t
  ; get_proc_summary_and_formals: Procname.t -> (Domain.summary * (Pvar.t * Typ.t) list) option }

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = LithoDomain

  type nonrec analysis_data = analysis_data

  let apply_callee_summary summary_opt callsite ~caller_pname ~callee_pname ret_id_typ formals
      actuals astate =
    Option.value_map summary_opt ~default:astate ~f:(fun callee_summary ->
        Domain.subst ~callsite ~formals ~actuals ~ret_id_typ ~caller_pname ~callee_pname
          ~caller:astate ~callee:callee_summary )


  let assume_null caller_pname x astate =
    let access_path =
      Domain.LocalAccessPath.make (HilExp.AccessExpression.to_access_path x) caller_pname
    in
    Domain.assume_null access_path astate


  let exec_instr astate {interproc= {proc_desc; tenv}; get_proc_summary_and_formals} _ _
      (instr : HilInstr.t) : Domain.t =
    let caller_pname = Procdesc.get_proc_name proc_desc in
    match instr with
    | Call
        ( return_base
        , Direct callee_pname
        , (HilExp.AccessExpression receiver_ae :: _ as actuals)
        , _
        , location ) ->
        let callee_summary_and_formals_opt = get_proc_summary_and_formals callee_pname in
        let callee_summary_opt = Option.map callee_summary_and_formals_opt ~f:fst in
        let receiver =
          Domain.LocalAccessPath.make_from_access_expression receiver_ae caller_pname
        in
        if
          (is_builder callee_pname tenv || is_create_method callee_pname tenv)
          (* track callee in order to report respective errors *)
          && satisfies_heuristic ~callee_pname ~callee_summary_opt tenv
        then
          let return_access_path = Domain.LocalAccessPath.make (return_base, []) caller_pname in
          match get_component_create_typ_opt callee_pname tenv with
          | Some create_typ ->
              Domain.call_create return_access_path create_typ location astate
          | None ->
              if is_build_method callee_pname tenv then
                Domain.call_build_method ~ret:return_access_path ~receiver astate
              else if is_builder callee_pname tenv then
                let callee_prefixes =
                  Domain.MethodCallPrefix.make_with_prefixes callee_pname location
                in
                List.fold ~init:astate callee_prefixes ~f:(fun acc callee_prefix ->
                    Domain.call_builder ~ret:return_access_path ~receiver callee_prefix acc )
              else astate
        else
          (* treat it like a normal call *)
          Option.value_map callee_summary_and_formals_opt ~default:astate ~f:(fun (_, formals) ->
              apply_callee_summary callee_summary_opt location ~caller_pname ~callee_pname
                return_base formals actuals astate )
    | Call (ret_id_typ, Direct callee_pname, actuals, _, location) ->
        let callee_summary_and_formals_opt = get_proc_summary_and_formals callee_pname in
        let callee_summary_opt = Option.map callee_summary_and_formals_opt ~f:fst in
        Option.value_map callee_summary_and_formals_opt ~default:astate ~f:(fun (_, formals) ->
            apply_callee_summary callee_summary_opt location ~caller_pname ~callee_pname ret_id_typ
              formals actuals astate )
    | Assign (lhs_ae, rhs, _) ->
        let astate =
          match rhs with
          | HilExp.AccessExpression rhs_ae ->
              (* creating an alias for the rhs binding; assume all reads will now occur through the
                 alias. this helps us keep track of chains in cases like tmp = getFoo(); x = tmp;
                 tmp.getBar() *)
              let lhs_access_path =
                Domain.LocalAccessPath.make
                  (HilExp.AccessExpression.to_access_path lhs_ae)
                  caller_pname
              in
              let rhs_access_path =
                Domain.LocalAccessPath.make
                  (HilExp.AccessExpression.to_access_path rhs_ae)
                  caller_pname
              in
              Domain.assign ~lhs:lhs_access_path ~rhs:rhs_access_path astate
          | _ ->
              astate
        in
        if HilExp.AccessExpression.is_return_var lhs_ae then Domain.call_return astate else astate
    | Assume (BinaryOperator (Eq, AccessExpression x, null), _, _, _)
      when HilExp.is_null_literal null ->
        assume_null caller_pname x astate
    | Assume (BinaryOperator (Eq, null, AccessExpression x), _, _, _)
      when HilExp.is_null_literal null ->
        assume_null caller_pname x astate
    | Assume (UnaryOperator (LNot, BinaryOperator (Ne, AccessExpression x, null), _), _, _, _)
      when HilExp.is_null_literal null ->
        assume_null caller_pname x astate
    | Assume (UnaryOperator (LNot, BinaryOperator (Ne, null, AccessExpression x), _), _, _, _)
      when HilExp.is_null_literal null ->
        assume_null caller_pname x astate
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "litho required props"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions)

let init_analysis_data ({InterproceduralAnalysis.analyze_dependency} as interproc) =
  let get_proc_summary_and_formals callee_pname =
    let open IOption.Let_syntax in
    let* callee_summary = analyze_dependency callee_pname |> AnalysisResult.to_option in
    let+ callee_attrs = Attributes.load callee_pname in
    (callee_summary, ProcAttributes.get_pvar_formals callee_attrs)
  in
  {interproc; get_proc_summary_and_formals}


let checker ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let ret_typ = Procdesc.get_ret_type proc_desc in
  let ret_path =
    let ret_var = Procdesc.get_ret_var proc_desc in
    Domain.LocalAccessPath.make_from_pvar ret_var ret_typ proc_name
  in
  let initial = Domain.init tenv proc_name (Procdesc.get_pvar_formals proc_desc) ret_path in
  Analyzer.compute_post (init_analysis_data analysis_data) ~initial proc_desc
  |> Option.map ~f:(fun post ->
         let is_void_func = Typ.is_void ret_typ in
         let post = Domain.get_summary ~is_void_func post in
         if should_report proc_name tenv then report analysis_data post else post )
