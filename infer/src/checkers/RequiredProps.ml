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
      ~f:(fun (({Annot.parameters} as annot), _) ->
        Annotations.annot_ends_with annot Annotations.prop
        && (* Don't count as required if it's @Prop(optional = true) *)
        not
          (List.exists
             ~f:(fun Annot.{name; value} ->
               Option.value_map name ~default:false ~f:(fun name -> String.equal "optional" name)
               && String.equal value "true" )
             parameters) )
      annot_list
  in
  let get_var_args annot_list =
    List.fold ~init:None
      ~f:(fun acc (({Annot.parameters} as annot), _) ->
        if Annotations.annot_ends_with annot Annotations.prop then
          (* Pick up the parameter for varArg if it has the form
             @Prop(varArg = myProp). *)
          List.fold ~init:acc
            ~f:(fun acc Annot.{name; value} ->
              if Option.value_map name ~default:false ~f:(fun name -> String.equal "varArg" name)
              then Some value
              else acc )
            parameters
        else acc )
      annot_list
  in
  match Tenv.lookup tenv typename with
  | Some {fields} ->
      List.filter_map
        ~f:(fun (fieldname, _, annot) ->
          if is_required annot then
            let prop = Typ.Fieldname.Java.get_field fieldname in
            let var_prop_opt = get_var_args annot in
            Some
              (Option.value_map var_prop_opt ~default:(Prop prop) ~f:(fun var_prop ->
                   VarProp {var_prop; prop} ))
          else None )
        fields
  | None ->
      []


let report_missing_required_prop summary prop parent_typename loc call_chain =
  let message =
    let prop_string =
      match prop with
      | Prop prop ->
          F.asprintf "@Prop %s" prop
      | VarProp {var_prop; prop} ->
          F.asprintf "Either @Prop %s or @Prop(varArg = %s)" prop var_prop
    in
    F.asprintf "%s is required for component %s, but is not set before the call to build()"
      prop_string (Typ.Name.name parent_typename)
  in
  let ltr =
    Errlog.make_trace_element 0 loc message []
    :: List.map call_chain ~f:(fun Domain.MethodCall.{procname; location} ->
           let call_msg = F.asprintf "calls %a" Typ.Procname.pp procname in
           Errlog.make_trace_element 0 location call_msg [] )
  in
  Reporting.log_error summary ~loc ~ltr IssueType.missing_required_prop message


(* walk backward through [call_chain] and return the first type T <: Component that is not part of
     the Litho framework (i.e., is client code) *)
let find_client_component_type call_chain =
  List.find_map
    ~f:(fun Domain.MethodCall.{procname} ->
      match procname with
      | Typ.Procname.Java java_pname ->
          Typ.Name.Java.get_outer_class (Typ.Procname.Java.get_class_type_name java_pname)
      | _ ->
          None )
    call_chain


let suffixes = String.Set.of_list ["Attr"; "Dip"; "Px"; "Res"; "Sp"]

let has_prop prop_set prop =
  let check prop =
    String.Set.mem prop_set prop
    || (* @Prop(resType = ...) myProp can also be set via myProp(), myPropAttr(), myPropDip(), myPropPx(), myPropRes() or myPropSp().
          Our annotation parameter parsing is too primitive to identify resType, so just assume
          that all @Prop's can be set any of these 6 ways. *)
    String.Set.exists prop_set ~f:(fun el ->
        String.chop_prefix el ~prefix:prop
        |> Option.exists ~f:(fun suffix -> String.Set.mem suffixes suffix) )
  in
  match prop with
  | Prop prop ->
      check prop
  | VarProp {var_prop; prop} ->
      (* @Prop(varArg = myProp) List <?> myPropList can also be set
         via myPropList() or myProp().*)
      check var_prop || check prop


module LithoContext = struct
  type t = Domain.t

  let check_callee ~callee_pname ~tenv _ =
    LithoFramework.is_component_builder callee_pname tenv
    || LithoFramework.is_component_create_method callee_pname tenv


  let satisfies_heuristic ~callee_pname ~callee_summary_opt tenv =
    (* If the method is build() or create() itself or doesn't contain a build() in
       its summary, we want to track it in the domain. *)
    ( LithoFramework.is_component_build_method callee_pname tenv
    || LithoFramework.is_component_create_method callee_pname tenv )
    ||
    (* check if build()/create() exists in callees *)
    let build_exists_in_callees =
      Option.value_map callee_summary_opt ~default:[] ~f:Domain.bindings
      |> List.exists ~f:(fun (_, call_set) ->
             LithoDomain.CallSet.exists
               (fun LithoDomain.MethodCall.{procname} ->
                 LithoFramework.is_component_build_method procname tenv
                 || LithoFramework.is_component_create_method procname tenv )
               call_set )
    in
    match callee_pname with
    | Typ.Procname.Java java_callee_procname ->
        not (Typ.Procname.Java.is_static java_callee_procname || build_exists_in_callees)
    | _ ->
        not build_exists_in_callees


  let field = Payloads.Fields.litho_required_props

  let should_report proc_desc tenv =
    let pname = Procdesc.get_proc_name proc_desc in
    (not (LithoFramework.is_function pname))
    && (not (LithoFramework.is_component_build_method pname tenv))
    && Procdesc.get_access proc_desc <> PredSymb.Private


  let report astate tenv summary =
    let check_required_prop_chain _ call_chain =
      let rev_chain = List.rev call_chain in
      match rev_chain with
      | Domain.MethodCall.{procname} :: _
        when LithoFramework.is_component_build_method procname tenv -> (
        (* Here, we'll have a type name like MyComponent$Builder in hand. Truncate the $Builder
           part from the typename, then look at the fields of MyComponent to figure out which
           ones are annotated with @Prop *)
        match find_client_component_type call_chain with
        | Some parent_typename ->
            let required_props = get_required_props parent_typename tenv in
            let prop_set =
              List.map
                ~f:(fun Domain.MethodCall.{procname} -> Typ.Procname.get_method procname)
                call_chain
              |> String.Set.of_list
            in
            List.iter
              ~f:(fun required_prop ->
                if not (has_prop prop_set required_prop) then
                  report_missing_required_prop summary required_prop parent_typename
                    (Summary.get_loc summary) call_chain )
              required_props
        | _ ->
            () )
      | _ ->
          ()
    in
    Domain.iter_call_chains ~f:check_required_prop_chain astate


  let session_name = "litho required props"
end

module Analyzer = LithoFramework.MakeAnalyzer (LithoContext)

let checker callback = Analyzer.checker callback
