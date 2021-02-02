(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** SLEdge command line interface *)

module Command = Core.Command
open Command.Let_syntax

type 'a param = 'a Command.Param.t

(* reverse application in the Command.Param applicative *)
let ( |*> ) : 'a param -> ('a -> 'b) param -> 'b param =
 fun x f -> x |> Command.Param.apply f

(* function composition in the Command.Param applicative *)
let ( >*> ) : ('a -> 'b) param -> ('b -> 'c) param -> ('a -> 'c) param =
 fun f' g' -> Command.Param.both f' g' >>| fun (f, g) -> f >> g

(* define a command, with trace flag, and with action wrapped in reporting *)
let command ~summary ?readme param =
  let trace =
    let%map_open config =
      flag "trace" ~doc:"<spec> enable debug tracing"
        (optional_with_default Trace.none
           (Arg_type.create (fun s -> Trace.parse s |> Result.get_ok)))
    and colors = flag "colors" no_arg ~doc:"enable printing in colors"
    and margin =
      flag "margin" ~doc:"<cols> wrap debug tracing at <cols> columns"
        (optional int)
    and report =
      flag "report" (optional string)
        ~doc:
          "<file> write report sexp to <file>, or to standard error if \
           \"-\""
    and append_report =
      flag "append-report" no_arg ~doc:"append to report file"
    in
    Trace.init ~colors ?margin ~config () ;
    Option.iter ~f:(Report.init ~append:append_report) report
  in
  Llair.Loc.root := Some (Core.Filename.realpath (Sys.getcwd ())) ;
  let flush main () = Fun.protect main ~finally:Trace.flush in
  let report main () =
    try main () |> Report.status
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let rec status_of_exn = function
        | Invariant.Violation (exn, _, _) | Replay (exn, _) ->
            status_of_exn exn
        | Frontend.Invalid_llvm msg -> Report.InvalidInput msg
        | Unimplemented msg -> Report.Unimplemented msg
        | Assert_failure _ as exn ->
            Report.InternalError (Sexp.to_string_hum (sexp_of_exn exn))
        | Failure msg -> Report.InternalError msg
        | exn -> Report.UnknownError (Printexc.to_string exn)
      in
      Report.status (status_of_exn exn) ;
      Printexc.raise_with_backtrace exn bt
  in
  Command.basic ~summary ?readme (trace *> param >>| flush >>| report)

let marshal program file =
  Out_channel.with_file file ~f:(fun oc -> Marshal.to_channel oc program [])

let unmarshal file () =
  In_channel.with_file
    ~f:(fun ic -> (Marshal.from_channel ic : Llair.program))
    file

let entry_points = Config.find_list "entry-points"

let used_globals pgm entry_points preanalyze : Domain_intf.used_globals =
  if preanalyze then
    let module Opts = struct
      let bound = 1
      let function_summaries = true
      let entry_points = entry_points
      let globals = Domain_intf.Declared Llair.Global.Set.empty
    end in
    let module Analysis = Control.Make (Opts) (Domain_used_globals) in
    let summary_table = Analysis.compute_summaries pgm in
    Per_function
      (Llair.Function.Map.map summary_table ~f:Llair.Global.Set.union_list)
  else
    Declared
      (Llair.Global.Set.of_iter
         (Iter.map ~f:(fun g -> g.name) (IArray.to_iter pgm.globals)))

let analyze =
  let%map_open bound =
    flag "bound"
      (optional_with_default 1 int)
      ~doc:"<int> stop execution exploration at depth <int>"
  and function_summaries =
    flag "function-summaries" no_arg
      ~doc:"use function summaries (in development)"
  and preanalyze_globals =
    flag "preanalyze-globals" no_arg
      ~doc:"pre-analyze global variables used by each function"
  and domain =
    flag "domain"
      (optional_with_default `sh
         (Arg_type.of_alist_exn
            [ ("sh", `sh)
            ; ("globals", `globals)
            ; ("itv", `itv)
            ; ("unit", `unit) ]))
      ~doc:
        "<string> select abstract domain; must be one of \"sh\" (default, \
         symbolic heap domain), \"globals\" (used-globals domain), or \
         \"unit\" (unit domain)"
  and no_simplify_states =
    flag "no-simplify-states" no_arg
      ~doc:"do not simplify states during symbolic execution"
  and stats =
    flag "stats" no_arg ~doc:"output performance statistics to stderr"
  and dump_query =
    flag "dump-query" (optional int)
      ~doc:"<int> dump solver query <int> and halt"
  in
  fun program () ->
    Timer.enabled := stats ;
    let pgm = program () in
    let globals = used_globals pgm entry_points preanalyze_globals in
    let module Opts = struct
      let bound = bound
      let function_summaries = function_summaries
      let entry_points = entry_points
      let globals = globals
    end in
    let dom : (module Domain_intf.Dom) =
      match domain with
      | `sh -> (module Domain_relation.Make (Domain_sh))
      | `globals -> (module Domain_used_globals)
      | `itv -> (module Domain_itv)
      | `unit -> (module Domain_unit)
    in
    let module Dom = (val dom) in
    let module Analysis = Control.Make (Opts) (Dom) in
    Domain_sh.simplify_states := not no_simplify_states ;
    Option.iter dump_query ~f:(fun n -> Solver.dump_query := n) ;
    Analysis.exec_pgm pgm ;
    Report.coverage pgm ;
    Report.safe_or_unsafe ()

let analyze_cmd =
  let summary = "analyze LLAIR code" in
  let readme () =
    "The <input> file must be binary LLAIR, such as produced by `sledge \
     translate`."
  in
  let param =
    Command.Param.(anon ("<input>" %: string) >>| unmarshal |*> analyze)
  in
  command ~summary ~readme param

let disassemble =
  let%map_open llair_output =
    flag "llair-output" (optional string)
      ~doc:
        "<file> write generated textual LLAIR to <file>, or to standard \
         output if omitted"
  in
  fun program () ->
    let pgm = program () in
    ( match llair_output with
    | None -> Format.printf "%a@." Llair.Program.pp pgm
    | Some file ->
        Out_channel.with_file file ~f:(fun oc ->
            let fs = Format.formatter_of_out_channel oc in
            Format.fprintf fs "%a@." Llair.Program.pp pgm ) ) ;
    Report.Ok

let disassemble_cmd =
  let summary = "print LLAIR code in textual form" in
  let readme () =
    "The <input> file must be LLAIR code, as produced by `sledge llvm \
     translate`."
  in
  let param =
    Command.Param.(anon ("<input>" %: string) >>| unmarshal |*> disassemble)
  in
  command ~summary ~readme param

let translate =
  let%map_open output =
    flag "output" (optional string)
      ~doc:"<file> write generated binary LLAIR to <file>"
  and no_models =
    flag "no-models" no_arg
      ~doc:"do not add models for C/C++ runtime and standard libraries"
  and fuzzer =
    flag "fuzzer" no_arg ~doc:"add a harness for libFuzzer targets"
  and no_internalize =
    flag "no-internalize" no_arg
      ~doc:
        "do not internalize all functions except the entry points \
         specified in the config file"
  in
  fun bitcode_inputs () ->
    let program =
      Frontend.translate ~models:(not no_models) ~fuzzer
        ~internalize:(not no_internalize) bitcode_inputs
    in
    Option.iter ~f:(marshal program) output ;
    program

let llvm_grp =
  let translate_inputs =
    let expand_argsfile input =
      if Char.equal input.[0] '@' then
        In_channel.with_file ~f:In_channel.input_lines (String.drop 1 input)
      else [input]
    in
    let open Command.Param in
    let input_arg = Arg_type.map string ~f:expand_argsfile in
    anon
      (map_anons ~f:List.concat
         (non_empty_sequence_as_list ("<input>" %: input_arg)))
    |*> translate
  in
  let translate_cmd =
    let summary = "translate LLVM bitcode to LLAIR" in
    let readme () =
      "Translate one or more LLVM bitcode files to LLAIR. Each <input> \
       filename may be either: an LLVM bitcode file, in binary (.bc) or \
       textual (.ll) form; or of the form @<argsfile>, where <argsfile> \
       names a file containing one <input> per line."
    in
    let param =
      translate_inputs >*> Command.Param.return (fun _ -> Report.Ok)
    in
    command ~summary ~readme param
  in
  let disassemble_cmd =
    let summary =
      "translate LLVM bitcode to LLAIR and print in textual form"
    in
    let readme () = "The <input> file must be LLVM bitcode." in
    let param = translate_inputs |*> disassemble in
    command ~summary ~readme param
  in
  let analyze_cmd =
    let summary = "analyze LLVM bitcode" in
    let readme () =
      "Analyze code in one or more LLVM bitcode files. This is a \
       convenience wrapper for the sequence `sledge llvm translate`; \
       `sledge analyze`."
    in
    let param = translate_inputs |*> analyze in
    command ~summary ~readme param
  in
  let summary = "integration with LLVM" in
  let readme () =
    "Code can be provided by one or more LLVM bitcode files."
  in
  Command.group ~summary ~readme ~preserve_subcommand_order:()
    [ ("analyze", analyze_cmd)
    ; ("translate", translate_cmd)
    ; ("disassemble", disassemble_cmd) ]

let smt_cmd =
  let summary = "process SMT-LIB benchmarks" in
  let readme () =
    "The <input> file is interpreted as an SMT-LIB 2 benchmark."
  in
  let param =
    let%map_open input = anon ("<input>" %: string) in
    fun () -> Smtlib.process input
  in
  command ~summary ~readme param

let summary = "SLEdge static analyzer"

let readme () =
  "The [-trace <spec>] argument of each subcommand enables debug tracing \
   according to <spec>, which is a sequence of module and function names \
   separated by + or -. For example, M-M.f enables all tracing in the M \
   module except the M.f function. The <spec> value * enables all debug \
   tracing."

;;
Printexc.record_backtrace Version.debug
;;
Stdlib.Sys.catch_break true

;;
Command.run ~version:Version.version ~build_info:Version.build_info
  (Command.group ~summary ~readme ~preserve_subcommand_order:()
     [ ("buck", Sledge_buck.main ~command ~analyze:(translate >*> analyze))
     ; ("llvm", llvm_grp)
     ; ("analyze", analyze_cmd)
     ; ("disassemble", disassemble_cmd)
     ; ("smt", smt_cmd) ])
