(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseDomainInterface
open PulseOperationResult.Import

module Config : sig
  val fieldname_must_be_monitored : Fieldname.t -> bool

  val procname_must_be_monitored : Tenv.t -> Procname.t -> bool

  type context =
    { initial_caller_class_extends: string list
    ; initial_caller_class_does_not_extend: string list
    ; description: string
    ; tag: string }

  val find_matching_context : Tenv.t -> Procname.t -> context option
end = struct
  type procname_to_monitor_spec =
    { class_names: string list option [@yojson.option]
    ; method_names: string list option [@yojson.option]
    ; class_name_regex: string option [@yojson.option] }
  [@@deriving of_yojson]

  type procname_to_monitor =
    | ClassAndMethodNames of {class_names: string list; method_names: string list}
    | ClassNameRegex of {class_name_regex: Str.regexp}

  let procname_to_monitor_of_yojson json =
    match procname_to_monitor_spec_of_yojson json with
    | {class_names= Some class_names; method_names= Some method_names} ->
        ClassAndMethodNames {class_names; method_names}
    | {class_name_regex= Some class_name_regex} ->
        let class_name_regex = Str.regexp class_name_regex in
        ClassNameRegex {class_name_regex}
    | _ ->
        L.die UserError "parsing of transitive-access config has failed:@\n %a" Yojson.Safe.pp json


  type context =
    { initial_caller_class_extends: string list
    ; initial_caller_class_does_not_extend: string list
    ; description: string
    ; tag: string }
  [@@deriving of_yojson]

  type t =
    { fieldnames_to_monitor: string list
    ; procnames_to_monitor: procname_to_monitor list
    ; contexts: context list }
  [@@deriving of_yojson]

  let get, set =
    let current = ref (None : t option) in
    ((fun () -> !current), fun config -> current := Some config)


  let fieldname_must_be_monitored fieldname =
    match get () with
    | None ->
        false
    | Some {fieldnames_to_monitor} ->
        List.exists fieldnames_to_monitor ~f:(String.equal (Fieldname.get_field_name fieldname))


  let procname_must_be_monitored tenv procname =
    let class_name = Procname.get_class_type_name procname in
    let method_name = Procname.get_method procname in
    let match_class_name names =
      Option.exists class_name ~f:(fun class_name ->
          PatternMatch.supertype_exists tenv
            (fun class_name _ ->
              let class_name_string = Typ.Name.name class_name in
              List.exists names ~f:(fun typ -> String.is_substring ~substring:typ class_name_string)
              )
            class_name )
    in
    let regexp_match regexp name =
      match Str.search_forward regexp name 0 with _ -> true | exception Caml.Not_found -> false
    in
    let match_class_name_regex regexp =
      Option.exists class_name ~f:(fun class_name ->
          PatternMatch.supertype_exists tenv
            (fun class_name _ -> regexp_match regexp (Typ.Name.name class_name))
            class_name )
    in
    match get () with
    | None ->
        false
    | Some {procnames_to_monitor} ->
        List.exists procnames_to_monitor ~f:(function
          | ClassAndMethodNames {class_names; method_names} ->
              match_class_name class_names && List.mem ~equal:String.equal method_names method_name
          | ClassNameRegex {class_name_regex} ->
              match_class_name_regex class_name_regex )


  let is_matching_context tenv procname
      {initial_caller_class_extends; initial_caller_class_does_not_extend} =
    match Procname.get_class_type_name procname with
    | Some type_name ->
        let parents =
          Tenv.fold_supers tenv type_name ~init:String.Set.empty ~f:(fun parent _ acc ->
              String.Set.add acc (Typ.Name.to_string parent) )
        in
        List.exists initial_caller_class_extends ~f:(String.Set.mem parents)
        && List.for_all initial_caller_class_does_not_extend ~f:(fun type_str ->
               not (String.Set.mem parents type_str) )
    | None ->
        false


  let find_matching_context tenv procname =
    let open IOption.Let_syntax in
    let* {contexts} = get () in
    List.find contexts ~f:(is_matching_context tenv procname)


  let () =
    match Config.pulse_transitive_access_config with
    | None ->
        ()
    | Some filepath -> (
      match Utils.read_safe_json_file filepath with
      | Ok (`List []) ->
          L.die ExternalError "The content of transitive-access JSON config is empty@."
      | Ok json -> (
        try t_of_yojson json |> set
        with _ ->
          L.die ExternalError "Could not read or parse transitive-access JSON config in %s@."
            filepath )
      | Error msg ->
          L.die ExternalError "Could not read or parse transitive-access JSON config in %s:@\n%s@."
            filepath msg )
end

let record_load rhs_exp location astates =
  match rhs_exp with
  | Exp.Lfield (_, fieldname, _) when Config.fieldname_must_be_monitored fieldname ->
      List.map astates ~f:(function
        | ContinueProgram astate ->
            ContinueProgram (AbductiveDomain.record_transitive_access location astate)
        | execstate ->
            execstate )
  | _ ->
      astates


let record_call tenv procname location astate =
  if Option.exists procname ~f:(Config.procname_must_be_monitored tenv) then
    AbductiveDomain.record_transitive_access location astate
  else astate


let report_errors tenv proc_desc err_log summary =
  let procname = Procdesc.get_proc_name proc_desc in
  match Config.find_matching_context tenv procname with
  | Some {tag; description} ->
      List.iter summary.PulseSummary.pre_post_list ~f:(function
        | ContinueProgram astate ->
            PulseTrace.Set.iter
              (fun call_trace ->
                PulseReport.report ~is_suppressed:false ~latent:false tenv proc_desc err_log
                  (Diagnostic.TransitiveAccess {tag; description; call_trace}) )
              (AbductiveDomain.Summary.get_transitive_accesses astate)
        | _ ->
            () )
  | None ->
      ()
