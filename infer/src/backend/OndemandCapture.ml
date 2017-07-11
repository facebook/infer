(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging

let compilation_db = (lazy (CompilationDatabase.from_json_files !Config.clang_compilation_dbs))

(** Given proc_attributes try to produce proc_attributes' where proc_attributes'.is_defined = true
    It may trigger capture of extra files to do so and when it does, it waits for
    frontend to finish before returning *)
let try_capture (attributes: ProcAttributes.t) : ProcAttributes.t option =
  let lazy cdb = compilation_db in
  ( if Option.is_none
         (AttributesTable.load_defined_attributes ~cache_none:false attributes.proc_name)
    then
      let decl_file = attributes.loc.file in
      let definition_file_opt = SourceFile.of_header decl_file in
      let try_compile definition_file =
        let source_dir = DB.source_dir_from_source_file definition_file in
        (* Use cfg_filename as a proxy to find out whether definition_file was already captured.
         If it was, there is no point in trying to capture it again.
         Treat existance of cfg_filename as a barrier - if it exists it means that
         all attributes files have been created - write logic is defined in
         Cfg.store_cfg_to_file *)
        let cfg_filename = DB.source_dir_get_internal_file source_dir ".cfg" in
        if not (DB.file_exists cfg_filename) then (
          L.(debug Capture Verbose) "Started capture of %a...@\n" SourceFile.pp definition_file ;
          Timeout.suspend_existing_timeout ~keep_symop_total:true ;
          protect
            ~f:(fun () -> CaptureCompilationDatabase.capture_file_in_database cdb definition_file)
            ~finally:Timeout.resume_previous_timeout ;
          if Config.debug_mode
             && Option.is_none
                  (AttributesTable.load_defined_attributes ~cache_none:false attributes.proc_name)
          then
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
      | None
       -> L.(debug Capture Medium)
            "Couldn't find source file for %a (declared in %a)@\n" Typ.Procname.pp
            attributes.proc_name SourceFile.pp decl_file
      | Some file
       -> try_compile file ) ;
  (* It's important to call load_defined_attributes again in all cases to make sure we try
     reading from disk again no matter which condition happened. If previous call to
     load_defined_attributes is None, it may mean couple of things:
     - proc_name hasn't been captured yet, so it needs to get captured (most likely scenario)
     - there was a race and proc_name got captured by the time we checked whether
       cfg_filename exists. In this case it's important to refetch attributes from disk because
       contents may have changed (attributes file for proc_name may be there now)
     - proc_name can't be captured (there is no definition we know of). In that case
       result will stay None. At this point we know(?) we won't be able to find definition
       for it ever so we can cache None.
       Caveat: it's possible that procedure will be captured in some other unrelated file
               later - infer may ignore it then.
     It also relies on retry mechanism in deserialization code to deal with half-written
     attributes files *)
  AttributesTable.load_defined_attributes ~cache_none:true attributes.proc_name
