(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type compiler = Clang | Swiftc

let run_llvm_frontend ~sources llvm_bitcode_in =
  PerfEvent.(
    log (fun logger ->
        PerfEvent.log_begin_event logger ~categories:["frontend"] ~name:"llvm frontend" () ) ) ;
  LlvmFrontend.capture ~sources llvm_bitcode_in ;
  PerfEvent.(log (fun logger -> PerfEvent.log_end_event logger ()))


let run_cmd cmd read =
  let exit_with_error exit_code =
    L.external_error "Error: the following command did not run successfully:@\n  %s@." cmd ;
    L.exit exit_code
  in
  L.debug Capture Quiet "Running command '%s'@\n" cmd ;
  match Utils.with_process_in cmd read with
  | res, Ok () ->
      res
  | _, Error (`Exit_non_zero n) ->
      (* exit with the same error code as the compiler in case of compilation failure *)
      exit_with_error n
  | _ ->
      exit_with_error 1


let run_cmd cmd frontend =
  PerfEvent.(
    log (fun logger -> PerfEvent.log_begin_event logger ~categories:["frontend"] ~name:"llvm" ()) ) ;
  let result = run_cmd cmd frontend in
  PerfEvent.(log (fun logger -> PerfEvent.log_end_event logger ())) ;
  result


let llvm_capture ~compiler command args =
  let cmd =
    match compiler with
    | Swiftc ->
        (command :: args) @ ["-emit-bc"; "-o"; "-"]
    | Clang ->
        (command :: args) @ ["-emit-llvm"; "-o"; "-"]
  in
  let source_path =
    let cmd = List.filter cmd ~f:(fun arg -> not (String.is_prefix ~prefix:"-" arg)) in
    List.last_exn cmd
  in
  L.debug Capture Quiet "@\n*** Beginning capture of file %s ***@\n" source_path ;
  let cmd = String.concat ~sep:" " cmd in
  run_cmd cmd (fun chan_in -> run_llvm_frontend ~sources:[source_path] chan_in)


let capture compiler ~command ~args =
  L.debug Capture Quiet "processed command is %s, args are %a@\n" command
    (Pp.comma_seq F.pp_print_string) args ;
  llvm_capture ~compiler command args ;
  Tenv.Global.load () |> Option.iter ~f:(Tenv.Global.store ~normalize:true)


let direct_bitcode_capture ~sources ~bitcode =
  Utils.with_file_in bitcode ~f:(fun llvm_bitcode -> LlvmFrontend.capture ~sources llvm_bitcode) ;
  Tenv.Global.load () |> Option.iter ~f:(Tenv.Global.store ~normalize:true)
