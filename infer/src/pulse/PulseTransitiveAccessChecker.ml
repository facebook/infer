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
  val must_be_monitored : Fieldname.t -> bool

  val is_initial_caller : Tenv.t -> Procname.t -> bool
end = struct
  type t =
    { fieldnames_to_monitor: string list
    ; initial_caller_class_extends: string
    ; initial_caller_class_does_not_extend: string list }

  module Json = struct
    let fieldnames_to_monitor_name = "fieldnames_to_monitor"

    let initial_caller_class_extends_name = "initial_caller_class_extends"

    let initial_caller_class_does_not_extend_name = "initial_caller_class_does_not_extend"

    let parse json =
      let lookup field assoc =
        match List.find assoc ~f:(fun (key, _) -> String.equal field key) with
        | None ->
            L.die UserError
              "parsing error on transitive-access config file. The field %s is missing: %a@\n" field
              Yojson.Basic.pp json
        | Some (_, value) ->
            value
      in
      let to_string = function
        | `String s ->
            s
        | value ->
            L.die UserError
              "parsing error on transitive-access config file. The value %a should be a string: %a@\n"
              Yojson.Basic.pp value Yojson.Basic.pp json
      in
      let to_list = function
        | `List l ->
            l
        | value ->
            L.die UserError
              "parsing error on transitive-access config file. The value %a should be a list: %a@\n"
              Yojson.Basic.pp value Yojson.Basic.pp json
      in
      let lookup_string field assoc = lookup field assoc |> to_string in
      let lookup_string_list field assoc = lookup field assoc |> to_list |> List.map ~f:to_string in
      match json with
      | `Assoc assoc ->
          { fieldnames_to_monitor= lookup_string_list fieldnames_to_monitor_name assoc
          ; initial_caller_class_extends= lookup_string initial_caller_class_extends_name assoc
          ; initial_caller_class_does_not_extend=
              lookup_string_list initial_caller_class_does_not_extend_name assoc }
      | _ ->
          L.die UserError "parsing error on transitive-access config file: %a@\n" Yojson.Basic.pp
            json
  end

  let get, set =
    let current = ref (None : t option) in
    ((fun () -> !current), fun config -> current := Some config)


  let must_be_monitored fieldname =
    match get () with
    | None ->
        false
    | Some {fieldnames_to_monitor} ->
        List.exists fieldnames_to_monitor ~f:(String.equal (Fieldname.get_field_name fieldname))


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
      match Utils.read_json_file filepath with
      | Ok (`List []) ->
          L.die ExternalError "The content of transitive-access JSON config is empty@."
      | Ok json ->
          Json.parse json |> set
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
  | Exp.Lfield (_, fieldname, _) when Config.must_be_monitored fieldname ->
      List.map astates ~f:(function
        | ContinueProgram astate ->
            ContinueProgram (AbductiveDomain.record_transitive_access location astate)
        | execstate ->
            execstate )
  | _ ->
      astates


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
