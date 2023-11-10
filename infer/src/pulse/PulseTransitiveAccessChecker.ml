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

  val is_initial_caller : Tenv.t -> Procname.t -> bool
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


  type t =
    { fieldnames_to_monitor: string list
    ; procnames_to_monitor: procname_to_monitor list
    ; initial_caller_class_extends: string
    ; initial_caller_class_does_not_extend: string list }
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


  let is_initial_caller tenv procname =
    (let open IOption.Let_syntax in
     let* {initial_caller_class_extends; initial_caller_class_does_not_extend} = get () in
     let+ type_name = Procname.get_class_type_name procname in
     let parents =
       Tenv.fold_supers tenv type_name ~init:String.Set.empty ~f:(fun name _ set ->
           String.Set.add set (Typ.Name.to_string name) )
     in
     String.Set.mem parents initial_caller_class_extends
     && List.for_all initial_caller_class_does_not_extend ~f:(fun str ->
            not (String.Set.mem parents str) ) )
    |> Option.value ~default:false


  let () =
    match Config.pulse_transitive_access_config with
    | [] ->
        ()
    | [filepath] when Filename.check_suffix filepath "json" -> (
      match Utils.read_safe_json_file filepath with
      | Ok (`List []) ->
          L.die ExternalError "The content of transitive-access JSON config is empty@."
      | Ok json ->
          t_of_yojson json |> set
      | Error msg ->
          L.die ExternalError "Could not read or parse transitive-access JSON config in %s:@\n%s@."
            filepath msg )
    | [filepath] ->
        L.die UserError "%s is not a valid path to .json file" filepath
    | l ->
        L.die UserError "%d transitive-access files where give while at most 1 is expected"
          (List.length l)
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
  if Config.is_initial_caller tenv procname then
    List.iter summary ~f:(function
      | ContinueProgram astate ->
          AbductiveDomain.Summary.get_transitive_accesses astate
          |> List.iter ~f:(fun call_trace ->
                 PulseReport.report ~is_suppressed:false ~latent:false tenv proc_desc err_log
                   (Diagnostic.TransitiveAccess {call_trace}) )
      | _ ->
          () )
