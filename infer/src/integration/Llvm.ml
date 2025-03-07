(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let run_swift_frontend source_file llvm_bitcode = LlvmFrontend.capture source_file llvm_bitcode

let run_swift_frontend source_file llvm_bitcode =
  PerfEvent.(
    log (fun logger ->
        PerfEvent.log_begin_event logger ~categories:["frontend"] ~name:"swiftc frontend" () ) ) ;
  run_swift_frontend source_file llvm_bitcode ;
  PerfEvent.(log (fun logger -> PerfEvent.log_end_event logger ()))


let run_swiftc swiftc_cmd read =
  let exit_with_error exit_code =
    L.external_error "Error: the following swiftc command did not run successfully:@\n  %s@."
      swiftc_cmd ;
    L.exit exit_code
  in
  match Utils.with_process_in swiftc_cmd read with
  | res, Ok () ->
      res
  | _, Error (`Exit_non_zero n) ->
      (* exit with the same error code as swiftc in case of compilation failure *)
      exit_with_error n
  | _ ->
      exit_with_error 1


let run_swiftc swiftc_cmd frontend =
  PerfEvent.(
    log (fun logger -> PerfEvent.log_begin_event logger ~categories:["frontend"] ~name:"swiftc" ()) ) ;
  let result = run_swiftc swiftc_cmd frontend in
  PerfEvent.(log (fun logger -> PerfEvent.log_end_event logger ())) ;
  result


let llvm_capture command args =
  let swiftc_cmd = command :: "-emit-bc" :: "-o" :: "-" :: args in
  let source_path =
    let swiftc_cmd =
      List.filter swiftc_cmd ~f:(fun arg -> not (String.is_prefix ~prefix:"-" arg))
    in
    List.last_exn swiftc_cmd
  in
  L.debug Capture Quiet "@\n*** Beginning capture of file %s ***@\n" source_path ;
  let swiftc_cmd = String.concat ~sep:" " swiftc_cmd in
  run_swiftc swiftc_cmd (fun chan_in -> run_swift_frontend source_path chan_in)


let capture ~command ~args =
  L.debug Capture Quiet "processed swiftc command is %s, args are %a@\n" command
    (Pp.comma_seq F.pp_print_string) args ;
  llvm_capture command args


let capture_llair ~source_file ~llair_file =
  Utils.with_file_in llair_file ~f:(fun llair_in ->
      let llair_program : Llair.program = Marshal.from_channel llair_in in
      L.progress "did not crash yet!@\n" ;
      LlvmFrontend.capture_llair source_file llair_program )
