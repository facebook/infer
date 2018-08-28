(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let compilation_db = lazy (CompilationDatabase.from_json_files !Config.clang_compilation_dbs)

(** Given proc_attributes try to produce proc_attributes' where proc_attributes'.is_defined = true
    It may trigger capture of extra files to do so and when it does, it waits for
    frontend to finish before returning *)
let try_capture (attributes : ProcAttributes.t) : ProcAttributes.t option =
  let (lazy cdb) = compilation_db in
  ( if Option.is_none (Attributes.load_defined attributes.proc_name) then
    let decl_file = attributes.loc.file in
    let definition_file_opt = SourceFile.of_header decl_file in
    let try_compile definition_file =
      (* Use the cfg as a proxy to find out whether definition_file was already captured.  If it
           was, there is no point in trying to capture it again.  Treat existance of the cfg as a
           barrier - if it exists it means that all attributes files have been created - write logic
           is defined in Cfg.store *)
      if not (SourceFiles.is_captured decl_file) then (
        L.(debug Capture Verbose) "Started capture of %a...@\n" SourceFile.pp definition_file ;
        Timeout.suspend_existing_timeout ~keep_symop_total:true ;
        protect
          ~f:(fun () -> CaptureCompilationDatabase.capture_file_in_database cdb definition_file)
          ~finally:Timeout.resume_previous_timeout ;
        if Config.debug_mode && Option.is_none (Attributes.load_defined attributes.proc_name) then
          (* peek at the results to know if capture succeeded, but only in debug mode *)
          L.(debug Capture Verbose)
            "Captured file %a to get procedure %a but it wasn't found there@\n" SourceFile.pp
            definition_file Typ.Procname.pp attributes.proc_name )
      else
        L.(debug Capture Verbose)
          "Wanted to capture file %a to get procedure %a but file was already captured@\n"
          SourceFile.pp definition_file Typ.Procname.pp attributes.proc_name
    in
    match definition_file_opt with
    | None ->
        L.(debug Capture Medium)
          "Couldn't find source file for %a (declared in %a)@\n" Typ.Procname.pp
          attributes.proc_name SourceFile.pp decl_file
    | Some file ->
        try_compile file ) ;
  (* It's important to call load_defined_attributes again in all cases to make sure we try
     reading from disk again no matter which condition happened. If previous call to
     load_defined_attributes is None, it may mean couple of things:
     - proc_name hasn't been captured yet, so it needs to get captured (most likely scenario)
     - there was a race and proc_name got captured by the time we checked whether
       cfg_filename exists. In this case it's important to refetch attributes from disk because
       contents may have changed (attributes file for proc_name may be there now)

     Caveat: it's possible that procedure will be captured in some other unrelated file
             later - infer may ignore it then. *)
  Attributes.load_defined attributes.proc_name
