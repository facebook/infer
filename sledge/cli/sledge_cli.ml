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

;;
register_sexp_of_exn (Dbg.Parse_failure "") (function
  | Dbg.Parse_failure msg -> Sexplib0.Sexp.Atom msg
  | _ -> assert false )

;;
register_sexp_of_exn (Goal.Sparse_trace.Failed_lookup "") (function
  | Goal.Sparse_trace.Failed_lookup msg -> Sexplib0.Sexp.Atom msg
  | _ -> assert false )

(* define a command, with trace flag, and with action wrapped in
   reporting *)
let command ~summary ?readme param =
  let trace =
    let%map_open config =
      flag "trace" ~doc:"<spec> enable debug tracing"
        (optional_with_default Dbg.none (Arg_type.create Dbg.parse))
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
    Dbg.init ~colors ?margin ~config () ;
    Option.iter ~f:(Report.init ~append:append_report) report
  in
  Llair.Loc.root := Some (Unix.realpath (Sys.getcwd ())) ;
  let flush main () = Fun.protect main ~finally:Dbg.flush in
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
        | Stop.Stop -> Report.safe_or_unsafe ()
        | Stop.Reached_goal {steps} -> Report.Reached_goal {steps}
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
    ~f:(fun ic : Llair.program -> Marshal.from_channel ic)
    file

let entry_points = Config.find_list "entry-points"

let used_globals pgm entry_points preanalyze =
  let module UG = Domain_used_globals in
  if preanalyze then
    let module Config = struct
      let loop_bound = 1
      let switch_bound = 0
      let function_summaries = true
      let entry_points = entry_points
      let globals = UG.Declared Llair.Global.Set.empty
    end in
    let module Analysis = Control.Make (Config) (UG) (Control.PriorityQueue)
    in
    let summary_table = Analysis.compute_summaries pgm in
    UG.Per_function
      (Llair.Function.Map.map summary_table ~f:Llair.Global.Set.union_list)
  else
    UG.Declared
      (Llair.Global.Set.of_iter
         (Iter.map ~f:(fun g -> g.name) (IArray.to_iter pgm.globals)) )

let analyze =
  let%map_open loop_bound =
    flag "loop-bound" ~aliases:["bound"]
      (optional_with_default 1 int)
      ~doc:
        "<int> limit execution exploration to <int> loop iterations, a \
         negative bound is never hit and leads to unbounded exploration"
  and switch_bound =
    flag "switch-bound" ~aliases:["yield"]
      (optional_with_default (-1) int)
      ~doc:
        "<int> limit execution exploration to <int> context switches, a \
         negative bound is never hit and leads to unbounded exploration"
  and cct_schedule_points =
    flag "cct-schedule-points" no_arg
      ~doc:"context switch only at cct_point calls"
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
            ; ("unit", `unit) ] ) )
      ~doc:
        "<string> select abstract domain; must be one of \"sh\" (default, \
         symbolic heap domain), \"globals\" (used-globals domain), or \
         \"unit\" (unit domain)"
  and sample = flag "sample" no_arg ~doc:" randomly sample execution paths"
  and seed =
    flag "seed" (optional int)
      ~doc:"<int> specify random number generator seed"
  and normalize_states =
    flag "normalize-states" no_arg
      ~doc:"normalize states during symbolic execution"
  and no_simplify_states =
    flag "no-simplify-states" no_arg
      ~doc:"do not simplify states during symbolic execution"
  and stats =
    flag "stats" no_arg ~doc:"output performance statistics to stderr"
  and dump_query =
    flag "dump-query" (optional int)
      ~doc:"<int> dump solver query <int> and halt"
  and dump_simplify =
    flag "dump-simplify" (optional int)
      ~doc:"<int> dump simplify query <int> and halt"
  and goal_trace =
    flag "goal-trace"
      (optional (Arg_type.create Goal.Sparse_trace.parse_exn))
      ~doc:
        "<string> specify a trace to try to explore, given as a \
         \"+\"-delimited sequence of function names appearing in the input \
         LLVM bitcode.  If provided, analysis prioritizes trace progress. \
         When an execution is found that visits each function in order, \
         terminate if \"Stop.on_reached_goal\" is being traced."
  in
  fun program () ->
    Timer.enabled := stats ;
    let pgm = program () in
    let globals = used_globals pgm entry_points preanalyze_globals in
    let module Config = struct
      let loop_bound = if loop_bound < 0 then Int.max_int else loop_bound

      let switch_bound =
        if switch_bound < 0 then Int.max_int else switch_bound

      let function_summaries = function_summaries
      let entry_points = entry_points
      let globals = globals
    end in
    let dom : (module Domain_intf.Domain) =
      match domain with
      | `sh -> (module Domain_relation.Make (Domain_sh))
      | `globals -> (module Domain_used_globals)
      | `itv -> (module Domain_itv)
      | `unit -> (module Domain_unit)
    in
    let module Domain = (val dom) in
    let queue : (module Control.Queue) =
      if sample then (module Control.RandomQueue)
      else (module Control.PriorityQueue)
    in
    let module Queue = (val queue) in
    (match seed with None -> Random.self_init () | Some n -> Random.init n) ;
    Llair.cct_schedule_points := cct_schedule_points ;
    Sh.do_normalize := normalize_states ;
    Domain_sh.simplify_states := not no_simplify_states ;
    Option.iter dump_query ~f:(fun n -> Solver.dump_query := n) ;
    Option.iter dump_simplify ~f:(fun n -> Sh.dump_simplify := n) ;
    at_exit (fun () -> Report.coverage pgm) ;
    ( match goal_trace with
    | None ->
        let module Analysis = Control.Make (Config) (Domain) (Queue) in
        Analysis.exec_pgm pgm
    | Some mk_goal ->
        let module Analysis =
          Control.MakeDirected (Config) (Domain) (Queue) (Goal.Sparse_trace)
        in
        let goal = mk_goal pgm in
        Analysis.exec_pgm pgm goal ) ;
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
  let%map_open dump_bitcode =
    flag "dump-bitcode" (optional string)
      ~doc:"<file> write transformed LLVM bitcode to <file>"
  and output =
    flag "output" (optional string)
      ~doc:"<file> write generated binary LLAIR to <file>"
  and opt_level, size_level =
    choose_one
      [ flag "O0" (no_arg_some (0, 0)) ~doc:" optimization level 0"
      ; flag "O1" (no_arg_some (1, 0)) ~doc:" optimization level 1"
      ; flag "O2" (no_arg_some (2, 0)) ~doc:" optimization level 2"
      ; flag "O3" (no_arg_some (3, 0)) ~doc:" optimization level 3"
      ; flag "Os"
          (no_arg_some (2, 1))
          ~doc:" like -O2 with extra optimizations for size"
      ; flag "Oz"
          (no_arg_some (2, 2))
          ~doc:" like -Os but reduces code size further (default)" ]
      ~if_nothing_chosen:(Default_to (2, 2))
  and no_internalize =
    flag "no-internalize" no_arg
      ~doc:
        "do not internalize all functions except the entry points \
         specified in the config file"
  in
  fun bitcode_input () ->
    let program =
      Frontend.translate ~internalize:(not no_internalize) ~opt_level
        ~size_level bitcode_input ?dump_bitcode
    in
    Option.iter ~f:(marshal program) output ;
    program

let llvm_grp =
  let translate_input =
    Command.Param.(anon ("<input>" %: string) |*> translate)
  in
  let translate_cmd =
    let summary = "translate LLVM bitcode to LLAIR" in
    let readme () =
      "Translate LLVM bitcode to LLAIR. The <input> file must contain LLVM \
       bitcode in either binary (.bc) or textual (.ll) form."
    in
    let param =
      translate_input >*> Command.Param.return (fun _ -> Report.Ok)
    in
    command ~summary ~readme param
  in
  let disassemble_cmd =
    let summary =
      "translate LLVM bitcode to LLAIR and print in textual form"
    in
    let readme () = "The <input> file must be LLVM bitcode." in
    let param = translate_input |*> disassemble in
    command ~summary ~readme param
  in
  let analyze_cmd =
    let summary = "analyze LLVM bitcode" in
    let readme () =
      "Analyze LLVM bitcode. This is a convenience wrapper for the \
       sequence `sledge llvm translate`; `sledge analyze`."
    in
    let param = translate_input |*> analyze in
    command ~summary ~readme param
  in
  let summary = "integration with LLVM" in
  Command.group ~summary ~preserve_subcommand_order:()
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
Memtrace.trace_if_requested ()
;;
if Version.debug then Printexc.record_backtrace true
;;
Stdlib.Sys.catch_break true

;;
Command.run ~version:Version.version ~build_info:Version.build_info
  (Command.group ~summary ~readme ~preserve_subcommand_order:()
     [ ("buck", Sledge_buck.main ~command)
     ; ("llvm", llvm_grp)
     ; ("analyze", analyze_cmd)
     ; ("disassemble", disassemble_cmd)
     ; ("smt", smt_cmd) ] )
