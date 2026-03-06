(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type stats =
  { mutable files_total: int
  ; mutable files_captured: int
  ; mutable procs_total: int
  ; mutable procs_captured: int }

let stats = {files_total= 0; files_captured= 0; procs_total= 0; procs_captured= 0}

let init_module_state llair_program lang =
  stats.procs_total <- Llair.FuncName.Map.length llair_program.Llair.functions ;
  Llair2Textual.init_module_state llair_program lang


module Error = struct
  type errors =
    {verification: TextualVerification.error list; transformation: Textual.transform_error list}

  type t = errors Textual.SourceFile.Map.t

  let log level =
    let level = if Config.keep_going then L.InternalError else level in
    match (level : L.error) with
    | InternalError ->
        L.internal_error
    | ExternalError ->
        L.external_error
    | UserError ->
        L.user_error


  let format_error (t : t) =
    Textual.SourceFile.Map.iter
      (fun source_file {verification; transformation} ->
        List.iter verification
          ~f:
            (log L.InternalError "%a@\n" (TextualVerification.pp_error_with_sourcefile source_file)) ;
        List.iter transformation
          ~f:(log L.InternalError "%a@\n" (Textual.pp_transform_error source_file)) )
      t


  let empty_error = {verification= []; transformation= []}

  let no_errors : t = Textual.SourceFile.Map.empty

  let add_errors t ~f sourcefile =
    Textual.SourceFile.Map.update sourcefile
      (fun errors_opt ->
        let error = match errors_opt with Some errors -> errors | None -> empty_error in
        Some (f error) )
      t


  let add_verification_errors (t : t) sourcefile list =
    add_errors t ~f:(fun error -> {error with verification= list @ error.verification}) sourcefile


  let add_transformation_errors (t : t) sourcefile list =
    add_errors t
      ~f:(fun error -> {error with transformation= list @ error.transformation})
      sourcefile
end

let textual_version = ref 0

let dump_textual_file source_file module_ =
  let suffix = if Config.frontend_tests then "test.sil" else "sil" in
  let filename =
    match !textual_version with
    | 0 ->
        Format.asprintf "%s.%s" source_file suffix
    | _ ->
        Format.asprintf "%s.v%d.%s" source_file !textual_version suffix
  in
  TextualSil.dump_module ~filename ~show_location:true module_ ;
  incr textual_version


let should_dump_textual () = Config.debug_mode || Config.dump_textual || Config.frontend_tests

let language_of_source_file source_file =
  if String.is_suffix source_file ~suffix:".c" then Textual.Lang.C
  else if String.is_suffix source_file ~suffix:".swift" then Textual.Lang.Swift
  else L.die UserError "Currently the llvm frontend is only enabled for C and Swift programs@."


let capture_llair source_file module_state =
  stats.files_total <- stats.files_total + 1 ;
  let open IResult.Let_syntax in
  let lang = language_of_source_file source_file in
  let result =
    let error_state = Error.no_errors in
    let textual = Llair2Textual.translate ~source_file ~module_state in
    if should_dump_textual () then dump_textual_file source_file textual ;
    let textual_source_file = Textual.SourceFile.create source_file in
    let* verified_textual, error_state =
      let f = Error.add_verification_errors error_state textual_source_file in
      match TextualVerification.verify_keep_going textual with
      | Ok (textual, errors) ->
          Ok (textual, f errors)
      | Error errors ->
          Error (f errors)
    in
    if Config.debug_mode then dump_textual_file source_file textual ;
    let* (transformed_textual, decls), error_state =
      let f = Error.add_transformation_errors error_state textual_source_file in
      match TextualTransform.run lang verified_textual with
      | Ok result ->
          Ok (result, error_state)
      | Error errors ->
          Error (f errors)
    in
    let* (cfg, tenv), error_state =
      let f = Error.add_transformation_errors error_state textual_source_file in
      match TextualSil.module_to_sil lang transformed_textual decls with
      | Ok (cfg, tenv) ->
          Ok ((cfg, tenv), error_state)
      | Error errors ->
          Error (f errors)
    in
    if Config.debug_mode then dump_textual_file source_file textual ;
    let sil = {TextualParser.TextualFile.sourcefile= textual_source_file; cfg; tenv} in
    let use_global_tenv = if Textual.Lang.is_swift lang then true else false in
    TextualParser.TextualFile.capture ~use_global_tenv sil ;
    ( if use_global_tenv then
        let global_tenv =
          Tenv.Global.load ()
          |> Option.value_or_thunk ~default:(fun () ->
                 let tenv = Tenv.create () in
                 Tenv.Global.set (Some tenv) ;
                 tenv )
        in
        Tenv.merge ~src:tenv ~dst:global_tenv ) ;
    stats.files_captured <- stats.files_captured + 1 ;
    Ok error_state
  in
  match result with
  | Ok warnings ->
      Error.format_error warnings
  | Error err ->
      Error.format_error err


let dump_llair_text llair_program source_file =
  let output_file = source_file ^ ".llair.text" in
  Utils.with_file_out output_file ~f:(fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Llair.Program.pp fmt llair_program ;
      Format.pp_print_flush fmt () )


let dump_llair llair_program source_file =
  let output_file = source_file ^ ".llair" in
  Utils.with_file_out output_file ~f:(fun outc -> Marshal.to_channel outc llair_program [])


let log_stats () =
  let count_defined_procs =
    let query_str = "SELECT count(*) FROM procedures WHERE cfg IS NOT NULL" in
    let db = Database.get_database CaptureDatabase in
    let stmt = Sqlite3.prepare db query_str in
    let log = "Counting defined procs" in
    fun () ->
      SqliteUtils.result_option db ~log stmt ~finalize:true ~read_row:(fun stmt ->
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT i ->
              Int64.to_int i |> Option.value_exn
          | _ ->
              L.die InternalError "Expected integer result from query %s" query_str )
  in
  stats.procs_captured <- count_defined_procs () |> Option.value_exn ;
  StatsLogging.log_many
    [ LogEntry.mk_count ~label:"capture.files_total" ~value:stats.files_total
    ; LogEntry.mk_count ~label:"capture.files_captured" ~value:stats.files_captured
    ; LogEntry.mk_count ~label:"capture.procs_total" ~value:stats.procs_total
    ; LogEntry.mk_count ~label:"capture.procs_captured" ~value:stats.procs_captured ] ;
  stats.files_captured <- 0 ;
  stats.procs_captured <- 0 ;
  stats.files_total <- 0 ;
  stats.procs_total <- 0


let capture ~sources llvm_bitcode_in =
  let lang = language_of_source_file (List.hd_exn sources) in
  let llvm_program = In_channel.input_all llvm_bitcode_in in
  let llair_program = LlvmSledgeFrontend.translate llvm_program in
  let module_state = init_module_state llair_program lang in
  List.iter sources ~f:(fun source_file ->
      textual_version := 0 ;
      if Config.dump_llair then dump_llair llair_program source_file ;
      if Config.dump_llair_text then dump_llair_text llair_program source_file ;
      capture_llair source_file module_state ) ;
  log_stats ()


(** shadows above definition to provide a different interface to module users *)
let capture_llair ~source_file ~llair_file =
  Utils.with_file_in llair_file ~f:(fun llair_in ->
      let llair_program : Llair.program = Marshal.from_channel llair_in in
      let lang = language_of_source_file source_file in
      let module_state = Llair2Textual.init_module_state llair_program lang in
      capture_llair source_file module_state ) ;
  log_stats ()
