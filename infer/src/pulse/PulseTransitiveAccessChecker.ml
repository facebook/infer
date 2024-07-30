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

let pulse_transitive_access_verbose = Config.pulse_transitive_access_verbose

module Config : sig
  val fieldname_must_be_monitored : Fieldname.t -> bool

  val procname_must_be_monitored : Tenv.t -> Procname.t -> bool

  val procname_must_be_skipped : Tenv.t -> Procname.t -> bool

  type context_metadata = {description: string; tag: string}

  val find_matching_context : Tenv.t -> Procname.t -> context_metadata option
end = struct
  type regexp_type = Str.regexp

  let regexp_type_of_yojson json = Str.regexp (string_of_yojson json)

  type procname_match_spec =
    { class_names: string list option [@yojson.option]
    ; method_names: string list option [@yojson.option]
    ; class_name_regex: regexp_type option [@yojson.option]
    ; method_name_regex: regexp_type option [@yojson.option]
    ; annotations: string list option [@yojson.option] }
  [@@deriving of_yojson]

  type context =
    { initial_caller_class_extends: string list option [@yojson.option]
    ; initial_caller_class_does_not_extend: string list option [@yojson.option]
    ; final_class_only: bool [@yojson.default false]
    ; annotations: string list option [@yojson.option]
    ; description: string
    ; tag: string }
  [@@deriving of_yojson]

  type t =
    { fieldnames_to_monitor: string list
    ; procnames_to_skip: procname_match_spec list option [@yojson.option]
    ; procnames_to_monitor: procname_match_spec list
    ; contexts: context list }
  [@@deriving of_yojson]

  let empty =
    {fieldnames_to_monitor= []; procnames_to_monitor= []; procnames_to_skip= None; contexts= []}


  let get, set =
    let current = ref (None : t option) in
    ((fun () -> !current), fun config -> current := Some config)


  let fieldname_must_be_monitored fieldname =
    match get () with
    | None ->
        false
    | Some {fieldnames_to_monitor} ->
        List.exists fieldnames_to_monitor ~f:(String.equal (Fieldname.get_field_name fieldname))


  let procname_has_annotation procname annotations =
    let match_one_annotation anno =
      let has_annot ia = Annotations.ia_ends_with ia anno in
      Annotations.pname_has_return_annot procname has_annot
    in
    List.exists annotations ~f:match_one_annotation


  let procname_is_matched specs tenv procname =
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
    let match_method_name_regex regexp = regexp_match regexp method_name in
    let check_one_procname_spec spec =
      match spec with
      | { class_names= None
        ; method_names= None
        ; class_name_regex= None
        ; method_name_regex= None
        ; annotations= None } ->
          false
      | {class_names; method_names; class_name_regex; method_name_regex; annotations} ->
          let map_or_true = Option.value_map ~default:true in
          map_or_true class_names ~f:match_class_name
          && map_or_true method_names ~f:(function mn ->
                 List.mem ~equal:String.equal mn method_name )
          && map_or_true class_name_regex ~f:match_class_name_regex
          && map_or_true method_name_regex ~f:match_method_name_regex
          && map_or_true annotations ~f:(procname_has_annotation procname)
    in
    List.exists specs ~f:check_one_procname_spec


  let procname_must_be_monitored tenv procname =
    match get () with
    | None ->
        false
    | Some {procnames_to_monitor} ->
        procname_is_matched procnames_to_monitor tenv procname


  let procname_must_be_skipped tenv procname =
    match get () with
    | None ->
        false
    | Some {procnames_to_skip} ->
        Option.exists procnames_to_skip ~f:(fun procnames_to_skip ->
            procname_is_matched procnames_to_skip tenv procname )


  let is_matching_context tenv procname context =
    let check_final_status tenv type_name final_class_only =
      let is_final () =
        Tenv.lookup tenv type_name
        |> Option.exists ~f:(fun {Struct.annots} -> Annot.Item.is_final annots)
      in
      pulse_transitive_access_verbose || (not final_class_only) || is_final ()
    in
    let has_parents tenv type_name =
      let parents =
        Tenv.fold_supers tenv type_name ~init:String.Set.empty ~f:(fun parent _ acc ->
            String.Set.add acc (Typ.Name.name parent) )
      in
      fun classes -> List.exists classes ~f:(String.Set.mem parents)
    in
    let check_extends tenv procname final_class_only initial_caller_class_extends =
      match Procname.get_class_type_name procname with
      | Some type_name ->
          check_final_status tenv type_name final_class_only
          && has_parents tenv type_name initial_caller_class_extends
      | None ->
          false
    in
    let check_not_extends tenv procname final_class_only initial_caller_class_does_not_extend =
      match Procname.get_class_type_name procname with
      | Some type_name ->
          check_final_status tenv type_name final_class_only
          && ( pulse_transitive_access_verbose
             || not (has_parents tenv type_name initial_caller_class_does_not_extend) )
      | None ->
          false
    in
    match context with
    | { initial_caller_class_extends= None
      ; initial_caller_class_does_not_extend= None
      ; annotations= None } ->
        false
    | { initial_caller_class_extends
      ; initial_caller_class_does_not_extend
      ; final_class_only
      ; annotations } ->
        let map_or_true = Option.value_map ~default:true in
        map_or_true initial_caller_class_extends ~f:(check_extends tenv procname final_class_only)
        && map_or_true initial_caller_class_does_not_extend
             ~f:(check_not_extends tenv procname final_class_only)
        && map_or_true annotations ~f:(procname_has_annotation procname)


  type context_metadata = {description: string; tag: string}

  let find_matching_context tenv procname =
    let open IOption.Let_syntax in
    let* {contexts} = get () in
    match List.find contexts ~f:(is_matching_context tenv procname) with
    | Some {tag; description} ->
        Some {tag; description}
    | None ->
        None


  let () =
    match Config.pulse_transitive_access_config with
    | [] ->
        ()
    | config_files ->
        let rev_config =
          List.fold config_files ~init:empty ~f:(fun merged_config config_file ->
              let new_config =
                match Utils.read_json_file config_file with
                | Ok (`List []) ->
                    L.die ExternalError "The content of transitive-access JSON config is empty@."
                | Ok json -> (
                  try t_of_yojson json
                  with _ ->
                    L.die ExternalError
                      "Could not read or parse transitive-access JSON config in %s@." config_file )
                | Error msg ->
                    L.die ExternalError
                      "Could not read or parse transitive-access JSON config in %s:@\n%s@."
                      config_file msg
              in
              { fieldnames_to_monitor=
                  List.rev_append new_config.fieldnames_to_monitor
                    merged_config.fieldnames_to_monitor
              ; procnames_to_monitor=
                  List.rev_append new_config.procnames_to_monitor merged_config.procnames_to_monitor
              ; procnames_to_skip=
                  Option.merge new_config.procnames_to_skip merged_config.procnames_to_skip
                    ~f:List.rev_append
              ; contexts= List.rev_append new_config.contexts merged_config.contexts } )
        in
        { fieldnames_to_monitor= List.rev rev_config.fieldnames_to_monitor
        ; procnames_to_monitor= List.rev rev_config.procnames_to_monitor
        ; procnames_to_skip= Option.map ~f:List.rev rev_config.procnames_to_skip
        ; contexts= List.rev rev_config.contexts }
        |> set
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


let should_skip_call procname = Config.procname_must_be_skipped procname

let report_errors ({InterproceduralAnalysis.tenv; proc_desc} as analysis_data)
    {PulseSummary.pre_post_list; non_disj} =
  let procname = Procdesc.get_proc_name proc_desc in
  match Config.find_matching_context tenv procname with
  | Some {tag; description} ->
      let nothing_reported = ref true in
      let report transitive_callees transitive_missed_captures call_trace =
        nothing_reported := false ;
        PulseReport.report analysis_data ~is_suppressed:false ~latent:false
          (TransitiveAccess
             {tag; description; call_trace; transitive_callees; transitive_missed_captures} )
      in
      List.iter pre_post_list ~f:(function
        | ContinueProgram astate ->
            let {PulseTransitiveInfo.accesses; callees; missed_captures} =
              AbductiveDomain.Summary.get_transitive_info astate
            in
            PulseTrace.Set.iter (report callees missed_captures) accesses
        | _ ->
            () ) ;
      NonDisjDomain.Summary.get_transitive_info_if_not_top non_disj
      |> Option.iter ~f:(fun {PulseTransitiveInfo.accesses; callees; missed_captures} ->
             PulseTrace.Set.iter (report callees missed_captures) accesses ;
             if !nothing_reported && pulse_transitive_access_verbose then
               let call_trace : PulseTrace.t =
                 Immediate {location= Location.dummy; history= PulseValueHistory.epoch}
               in
               let transitive_callees = callees in
               let transitive_missed_captures = missed_captures in
               PulseReport.report analysis_data ~is_suppressed:false ~latent:false
                 (TransitiveAccess
                    { tag= "NO ACCESS FOUND"
                    ; description= ""
                    ; call_trace
                    ; transitive_callees
                    ; transitive_missed_captures } ) )
  | None ->
      ()
