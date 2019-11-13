(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** sledge-buck integration *)

module Process = Shexp_process
open Process.Infix

(* directory relative to buck root containing llvm binaries *)
let llvm_bin = lazy (Option.value (Config.find "llvm-bin-dir") ~default:"")

(* buck build mode to specify an LTO build *)
let mode = lazy (Config.find_exn "buck-build-mode")

(* resolve relative path wrt root *)
let make_absolute root path =
  if Filename.is_relative path then Filename.concat root path else path

(* initial working directory *)
let cwd = Unix.getcwd ()

(* query buck for root *)
let buck_root =
  let open Process in
  lazy (String.strip (eval (run "buck" ["root"] |- read_all)))

(* use buck root for working directory *)
let context () =
  let open Process in
  Context.create ~cwd:(Path (Lazy.force buck_root)) ()

(* invoke the LTO build for the target *)
let buck_build ~context target =
  let open Process in
  eval ~context
    (run "buck"
       [ "build"; "@mode/" ^ Lazy.force mode; "-c"; "sledge.build=True"
       ; target ])

(* split a fully-qualified buck target into file and rule *)
let parse_target target =
  try Scanf.sscanf target "//%s@:%s" (fun file rule -> (file, rule))
  with Scanf.Scan_failure _ | End_of_file ->
    fail "could not parse target: %s" target ()

(* compute the filename of the linker.argsfile for the target *)
let argsfile ~context target =
  let file, rule = parse_target target in
  Printf.sprintf "%s/buck-out/%s/bin/%s/%s#binary/linker.argsfile"
    Process.(eval ~context cwd_logical)
    (Lazy.force mode) file rule

(* add a file to the list of modules, if it exists *)
let add_module ?archive_name ~context file rev_modules =
  let open Process in
  if eval ~context (file_exists file) then file :: rev_modules
  else (
    ( match archive_name with
    | None -> warn "%s doesn't exist" file ()
    | Some archive_name ->
        warn "%s doesn't exist in archive: %s" file
          (Filename.basename archive_name)
          () ) ;
    rev_modules )

(* use llvm-ar to get toc of an archive *)
let expand_thin_archive ~context archive_name rev_modules =
  let open Process in
  eval ~context
    ( run (Lazy.force llvm_bin ^ "llvm-ar") ["t"; archive_name]
    |- fold_lines ~init:rev_modules ~f:(fun rev_modules line ->
           return (add_module ~archive_name ~context line rev_modules) ) )

(* Use llvm-ar to check if the archive contains any bitcode files; if it
   does, fail for now as it doesn't seem to happen. *)
let expand_arch_archive ~context archive_name =
  let number_of_bitcode_files =
    let open Process in
    eval ~context
      ( run (Lazy.force llvm_bin ^ "llvm-ar") ["t"; archive_name]
      |- fold_lines ~init:0 ~f:(fun acc name ->
             return
               (let _, is_bc =
                  eval ~context
                    (* Need to run_exit_status, because otherwise llvm-ar
                       gets -8 signal; which is strange. Repeating the
                       command in bash doesn't signal and this works. *)
                    ( run_exit_status
                        (Lazy.force llvm_bin ^ "llvm-ar")
                        ["p"; archive_name; name]
                    |+ run "head" ["-c2"]
                    |+ read_all )
                in
                if String.equal is_bc "BC" then (
                  warn "found bc file %s in %s" name archive_name () ;
                  acc + 1 )
                else acc ) ) )
  in
  number_of_bitcode_files = 0
  || fail "found %d bitcode files in archive %s" number_of_bitcode_files
       archive_name ()

(* find bitcode module(s) in a linker arg *)
let parse_linker_arg ~context rev_modules arg =
  if String.is_suffix arg ~suffix:".o" then
    add_module ~context arg rev_modules
  else if String.is_suffix arg ~suffix:".a" then
    let thin_archive =
      String.strip Process.(eval (run "head" ["-1"; arg] |- read_all))
    in
    if String.equal thin_archive "!<thin>" then
      expand_thin_archive ~context arg rev_modules
    else if String.equal thin_archive "!<arch>" then (
      assert (expand_arch_archive ~context arg) ;
      rev_modules )
    else fail "unknown type of archive file %s" thin_archive ()
  else rev_modules

(* build target and find constituent bitcode modules *)
let bitcode_files_of ~target =
  let target =
    if
      List.exists (Config.find_list "buck-target-patterns")
        ~f:(fun substring -> String.is_substring target ~substring)
    then target ^ "_sledge"
    else target
  in
  let context = context () in
  buck_build ~context target ;
  let modules =
    In_channel.with_file
      (argsfile ~context target)
      ~f:(In_channel.fold_lines ~init:[] ~f:(parse_linker_arg ~context))
    |> List.rev
  in
  List.map ~f:(make_absolute (Lazy.force buck_root)) modules

(* link and optimize the modules *)
let llvm_link_opt ~fuzzer ~bitcode_output modules =
  let context = context () in
  let modules = if fuzzer then "-" :: modules else modules in
  let open Process in
  eval ~context
    ( ( if fuzzer then
        echo ~n:() (Option.value_exn (Model.read "/lib_fuzzer_main.bc"))
      else return () )
    |- run (Lazy.force llvm_bin ^ "llvm-link") ("-o=-" :: modules)
    |- run
         (Lazy.force llvm_bin ^ "opt")
         [ "-o=" ^ bitcode_output; "-internalize"
         ; "-internalize-public-api-list="
           ^ String.concat ~sep:"," (Config.find_list "entry-points")
         ; "-globaldce"; "-globalopt"; "-mergefunc"; "-constmerge"
         ; "-argpromotion"; "-ipsccp"; "-mem2reg"; "-dce"; "-globaldce"
         ; "-deadargelim"; "-global-merge-on-const"
         ; "-global-merge-ignore-single-use=false"
         ; "-global-merge-group-by-use=false"
           (* global-merge-max-offset is set to 0 by default. If a global
              variable has larger allocation size than the max-offset, it is
              not merged, therefore the global-merge pass is a noop. We set
              it to something big, so that it merges as much as possible. *)
         ; "-global-merge-max-offset=1000000"; "-global-merge" ] )

(** command line interface *)

open Command.Let_syntax

let ( |*> ) a' f' = a' |> Command.Param.apply f'
let ( |**> ) = Command.Param.map2 ~f:(fun a f b -> f b a)

let abs_path_arg =
  Command.Param.(Arg_type.map string ~f:(make_absolute cwd))

let main ~(command : unit Command.basic_command) ~analyze =
  let target_flag = Command.Param.(anon ("<target>" %: string)) in
  let bitcode_inputs =
    let%map_open target = target_flag
    and modules =
      flag "modules" (optional string)
        ~doc:
          "<file> write list of bitcode files to <file>, or to standard \
           output if <file> is `-`"
    in
    let bitcode_files = bitcode_files_of ~target in
    ( match modules with
    | Some "-" ->
        Format.printf "%a"
          (List.pp " " Format.pp_print_string)
          bitcode_files
    | Some file -> Out_channel.write_lines file bitcode_files
    | None -> () ) ;
    bitcode_files
  in
  let bitcode_cmd =
    let summary = "report bitcode files in buck target" in
    let readme () =
      "Build a buck target and report the included bitcode files."
    in
    let param = bitcode_inputs >>| fun _ () -> () in
    command ~summary ~readme param
  in
  let analyze_cmd =
    let summary = "analyze buck target" in
    let readme () =
      "Analyze code in a buck target. This is a convenience wrapper for \
       the sequence `sledge buck bitcode`; `sledge llvm translate`; \
       `sledge analyze`."
    in
    let param = bitcode_inputs |*> analyze in
    command ~summary ~readme param
  in
  let link_cmd =
    let summary = "link buck target to LLVM bitcode" in
    let readme () =
      "Link code in a buck target to a single LLVM bitcode module. This \
       also internalizes all symbols except `main` and removes dead code."
    in
    let link =
      let%map_open bitcode_output =
        flag "bitcode-output" (required abs_path_arg)
          ~doc:"<file> write linked bitcode to <file>"
      and fuzzer =
        flag "fuzzer" no_arg ~doc:"add a harness for libFuzzer targets"
      in
      fun () -> llvm_link_opt ~fuzzer ~bitcode_output
    in
    let param = bitcode_inputs |**> link in
    command ~summary ~readme param
  in
  let summary = "integration with Buck" in
  let readme () =
    "Code can be provided by a buck build target, such as \
     //fully/qualified/build:target. The mechanism used to integrate with \
     buck uses the arguments passed to the linker, so the target must \
     specify a binary that will be linked, not for instance a library \
     archive. Sledge passes the --config sledge.build=True flag to buck, \
     which can be used to configure buck targets for sledge."
  in
  Command.group ~summary ~readme ~preserve_subcommand_order:()
    [("analyze", analyze_cmd); ("bitcode", bitcode_cmd); ("link", link_cmd)]
