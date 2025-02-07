(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Functional statistics *)

let solver_steps = ref 0
let step_solver () = Int.incr solver_steps
let steps = ref 0
let hit_insts = Llair.IP.Tbl.create ()
let hit_terms = Llair.Block.Tbl.create ()

let step_inst ip =
  Llair.IP.Tbl.incr hit_insts ip ;
  Int.incr steps

let step_term b =
  Llair.Block.Tbl.incr hit_terms b ;
  Int.incr steps

let bound = ref (-1)
let hit_loop_bound n = bound := n
let switches = ref (-1)
let hit_switch_bound n = switches := n

(** Issue reporting *)

let alarm_count = ref 0

let alarm alrm ~dp_witness =
  Int.incr alarm_count ;
  Format.printf "@\n@[<v 2>%a@]@." Alarm.pp alrm ;
  Format.printf "@\n@[<v 2> %t@ %a@]@." dp_witness Alarm.pp_trace alrm ;
  Stop.on_alarm alrm

let unknown_call call =
  [%Dbg.printf
    "@\n@[<v 2>%a Unknown function call %a@;<1 2>@[%a@]@]@."
      (fun fs call -> Llair.Loc.pp fs (Llair.Term.loc call))
      call
      (fun fs (call : Llair.Term.t) ->
        match call with
        | Call {callee} -> Llair.Term.pp_callee fs callee
        | _ -> () )
      call Llair.Term.pp call] ;
  Stop.on_unknown_call ()

let reached_goal ~dp_goal ~dp_valid ~dp_witness =
  Format.printf "@\n%t@\n@[<v 2>%t@ %t@]@." dp_valid dp_witness dp_goal ;
  Stop.on_reached_goal !steps ()

let unreachable_goal ~dp_path =
  [%Dbg.printf "%t" dp_path] ;
  Stop.on_unreachable_goal ~dp_path

let unimplemented feature fn =
  let open Llair in
  [%Dbg.printf
    "@\n@[<v 2>%s unimplemented in %a@]@." feature FuncName.pp fn.name] ;
  Stop.on_unimplemented feature fn

(** Status reporting *)

type status =
  | Safe of {bound: int; switches: int}
  | Unsafe of {alarms: int; bound: int; switches: int}
  | Reached_goal of {steps: int}
  | Unreachable_goal
  | Ok
  | Unsound
  | Incomplete
  | InvalidInput of string
  | Unimplemented of string
  | InternalError of string
  | Timeout
  | Memout
  | Abort
  | Assert of string
  | UnknownError of string
[@@deriving compare, equal, sexp]

let pp_status ppf stat =
  let pf fmt = Format.fprintf ppf fmt in
  match stat with
  | Safe {bound= -1; switches= -1} -> pf "Safe"
  | Safe {bound= -1; switches} -> pf "Safe (%i,_)" switches
  | Safe {bound; switches= -1} -> pf "Safe (_,%i)" bound
  | Safe {bound; switches} -> pf "Safe (%i,%i)" switches bound
  | Unsafe {alarms; bound= -1; switches= -1} -> pf "Unsafe: %i" alarms
  | Unsafe {alarms; bound= -1; switches} ->
      pf "Unsafe: %i (%i,_)" alarms switches
  | Unsafe {alarms; bound; switches= -1} ->
      pf "Unsafe: %i (_,%i)" alarms bound
  | Unsafe {alarms; bound; switches} ->
      pf "Unsafe: %i (%i,%i)" alarms switches bound
  | Reached_goal {steps} -> pf "Reached: %i" steps
  | Unreachable_goal -> pf "Unreachable Goal"
  | Ok -> pf "Ok"
  | Unsound -> pf "Unsound"
  | Incomplete -> pf "Incomplete"
  | InvalidInput msg -> pf "Invalid input: %s" msg
  | Unimplemented msg -> pf "Unimpemented: %s" msg
  | InternalError msg -> pf "Internal error: %s" msg
  | Timeout -> pf "Timeout"
  | Memout -> pf "Memout"
  | Abort -> pf "Abort"
  | Assert msg -> pf "Assert: %s" msg
  | UnknownError msg -> pf "Unknown error: %s" msg

let pp_status_coarse ppf stat =
  let pf fmt = Format.fprintf ppf fmt in
  match stat with
  | InvalidInput _ -> pf "Invalid input"
  | Abort | Unimplemented _ | UnknownError _ -> pf "Error"
  | _ -> pp_status ppf stat

let safe_or_unsafe () =
  if !alarm_count = 0 then Safe {bound= !bound; switches= !switches}
  else Unsafe {alarms= !alarm_count; bound= !bound; switches= !switches}

type gc_stats = {allocated: float; promoted: float; peak_size: float}
[@@deriving sexp]

type times =
  {etime: float; utime: float; stime: float; cutime: float; cstime: float}
[@@deriving sexp]

type coverage = {steps: int; hit: int; fraction: float; solver_steps: int}
[@@deriving compare, equal, sexp]

type entry =
  | ProcessTimes of times
  | GcStats of gc_stats
  | Status of status
  | Coverage of coverage
[@@deriving sexp]

let process_times () =
  let {Unix.tms_utime; tms_stime; tms_cutime; tms_cstime} = Unix.times () in
  let etime =
    try Mtime.Span.to_s (Mtime_clock.elapsed ()) with Sys_error _ -> 0.
  in
  ProcessTimes
    { etime
    ; utime= tms_utime
    ; stime= tms_stime
    ; cutime= tms_cutime
    ; cstime= tms_cstime }

let gc_stats () =
  let words_to_MB n = n *. float (Sys.word_size / 8) /. (1024. *. 1024.) in
  let ctrl = Gc.get () in
  let stat = Gc.quick_stat () in
  let allocated =
    words_to_MB (stat.minor_words +. stat.major_words -. stat.promoted_words)
  in
  let promoted = words_to_MB stat.promoted_words in
  let peak_size =
    words_to_MB (float (ctrl.minor_heap_size + stat.top_heap_words))
  in
  GcStats {allocated; promoted; peak_size}

type t = {name: string; entry: entry} [@@deriving sexp]

let chan = ref None
let name = ref ""

let output entry =
  Option.iter !chan ~f:(fun chan ->
      Sexp.output chan (sexp_of_t {name= !name; entry}) ;
      Out_channel.output_char chan '\n' )

let init ?append filename =
  (chan :=
     match filename with
     | "" -> None
     | "-" -> Some Out_channel.stderr
     | _ ->
         let flags =
           (match append with Some true -> [] | _ -> [Open_trunc])
           @ [Open_wronly; Open_creat; Open_binary]
         in
         let perm = 0o666 in
         Some (Out_channel.open_gen flags perm filename) ) ;
  name :=
    Option.value
      (Filename.chop_suffix_opt ~suffix:".sexp" filename)
      ~default:filename ;
  at_exit (fun () ->
      output (process_times ()) ;
      output (gc_stats ()) ;
      Option.iter ~f:Out_channel.close_noerr !chan )

let coverage (pgm : Llair.program) =
  let size =
    Llair.FuncName.Map.fold pgm.functions 0 ~f:(fun ~key:_ ~data:func n ->
        Llair.Func.fold_cfg func n ~f:(fun blk n ->
            n + IArray.length blk.cmnd + 1 ) )
  in
  let hit =
    Llair.IP.Tbl.length hit_insts + Llair.Block.Tbl.length hit_terms
  in
  let fraction = Float.(of_int hit /. of_int size) in
  output
    (Coverage {steps= !steps; hit; fraction; solver_steps= !solver_steps})

let status s = output (Status s)
