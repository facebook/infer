(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

let unknown_call call =
  [%Trace.kprintf
    Stop.on_unknown_call
      "@\n@[<v 2>%a Unknown function call %a@;<1 2>@[%a@]@]@."
      (fun fs call -> Llair.Loc.pp fs (Llair.Term.loc call))
      call
      (fun fs (call : Llair.Term.t) ->
        match call with Call {callee} -> Llair.Exp.pp fs callee | _ -> () )
      call Llair.Term.pp call]

let invalid_access_count = ref 0

let invalid_access fmt_thunk pp access loc =
  Int.incr invalid_access_count ;
  let rep fs =
    Format.fprintf fs "%a Invalid memory access@;<1 2>@[%a@]" Llair.Loc.pp
      (loc access) pp access
  in
  Format.printf "@\n@[<v 2>%t@]@." rep ;
  [%Trace.printf "@\n@[<v 2>%t@;<1 2>@[{ %t@ }@]@]@." rep fmt_thunk] ;
  Stop.on_invalid_access ()

let invalid_access_inst fmt_thunk inst =
  invalid_access fmt_thunk Llair.Inst.pp inst Llair.Inst.loc

let invalid_access_term fmt_thunk term =
  invalid_access fmt_thunk Llair.Term.pp term Llair.Term.loc

(** Functional statistics *)

let steps = ref 0
let step () = Int.incr steps

(** Status reporting *)

type status =
  | Safe of {steps: int}
  | Unsafe of {alarms: int; steps: int}
  | Ok
  | Unsound
  | Incomplete
  | InvalidInput of string
  | Unimplemented of string
  | InternalError of string
  | Timeout
  | Memout
  | Crash of string
  | UnknownError of string
[@@deriving compare, equal, sexp]

let pp_status ppf stat =
  let pf fmt = Format.fprintf ppf fmt in
  match stat with
  | Safe {steps} -> pf "Safe (%i)" steps
  | Unsafe {alarms; steps} -> pf "Unsafe: %i (%i)" alarms steps
  | Ok -> pf "Ok"
  | Unsound -> pf "Unsound"
  | Incomplete -> pf "Incomplete"
  | InvalidInput msg -> pf "Invalid input: %s" msg
  | Unimplemented msg -> pf "Unimpemented: %s" msg
  | InternalError msg -> pf "Internal error: %s" msg
  | Timeout -> pf "Timeout"
  | Memout -> pf "Memout"
  | Crash msg -> pf "Crash: %s" msg
  | UnknownError msg -> pf "Unknown error: %s" msg

let safe_or_unsafe () =
  if !invalid_access_count = 0 then Safe {steps= !steps}
  else Unsafe {alarms= !invalid_access_count; steps= !steps}

type gc_stats = {allocated: float; promoted: float; peak_size: float}
[@@deriving sexp]

type times =
  {etime: float; utime: float; stime: float; cutime: float; cstime: float}
[@@deriving sexp]

type entry =
  | ProcessTimes of times
  | GcStats of gc_stats
  | Status of status
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
  let words_to_MB n = n /. float (Sys.word_size / 8) /. (1024. *. 1024.) in
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
      Out_channel.output_lines chan
        [Sexp.to_string (sexp_of_t {name= !name; entry})] )

let init ?append filename =
  (chan :=
     match filename with
     | "" -> None
     | "-" -> Some Out_channel.stderr
     | _ -> Some (Out_channel.create ?append filename)) ;
  name :=
    Option.value
      (Filename.chop_suffix_opt ~suffix:".sexp" filename)
      ~default:filename ;
  at_exit (fun () ->
      output (process_times ()) ;
      output (gc_stats ()) ;
      Option.iter ~f:Out_channel.close_no_err !chan )

let status s = output (Status s)
