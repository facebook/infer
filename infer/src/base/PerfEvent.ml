(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module Json = struct
  (** do not break lines *)
  let pp_seq pp f l = Pp.seq ~sep:"," pp f l

  let pp_string f s = F.fprintf f "\"%s\"" (Escape.escape_json s)

  let pp_field pp_value f field_name field_value =
    Format.fprintf f "%a:%a" pp_string field_name pp_value field_value


  (** only what we need for now *)
  type t = [`Int of int | `String of string | `Assoc of (string * t) list | `List of t list]

  let rec pp_assoc_field f (key, value) = pp_field pp f key value

  and pp f = function
    | `Int i ->
        F.pp_print_int f i
    | `String s ->
        pp_string f s
    | `Assoc fields ->
        F.fprintf f "{%a}" (pp_seq pp_assoc_field) fields
    | `List items ->
        F.fprintf f "[%a]" (pp_seq pp) items
end

module JsonFragment = struct
  type t = AssocBegin | AssocEnd | ListBegin | ListItemSeparator | ListEnd

  let to_string = function
    | AssocBegin ->
        "AssocBegin"
    | AssocEnd ->
        "AssocEnd"
    | ListBegin ->
        "ListBegin"
    | ListItemSeparator ->
        "ListItemSeparator"
    | ListEnd ->
        "ListEnd"


  type pp_state = Outside | InAssocFirst | InAssocMiddle | InList

  let string_of_state = function
    | Outside ->
        "Outside"
    | InAssocFirst ->
        "InAssocFirst"
    | InAssocMiddle ->
        "InAssocMiddle"
    | InList ->
        "InList"


  (** for some limited (not thread-safe) form of safety, and to know when we need to print
      separators *)
  let pp_state = ref [Outside]

  let pp f json_fragment =
    match (json_fragment, !pp_state) with
    | AssocBegin, ((Outside | InList) :: _ as state) ->
        pp_state := InAssocFirst :: state
    | AssocEnd, (InAssocFirst | InAssocMiddle) :: state' ->
        F.pp_print_string f "}" ;
        pp_state := state'
    | ListBegin, ((Outside | InList) :: _ as state) ->
        F.pp_print_string f "[" ;
        pp_state := InList :: state
    | ListItemSeparator, InList :: _ ->
        F.pp_print_string f ","
    | ListEnd, InList :: state0 ->
        F.pp_print_string f "]" ;
        pp_state := state0
    | _ ->
        L.die InternalError "Unexpected json fragment \"%s\" in state [%a]"
          (to_string json_fragment)
          (Pp.seq (Pp.of_string ~f:string_of_state))
          !pp_state


  let pp_assoc_field pp_value f key value =
    match !pp_state with
    | InAssocFirst :: state0 ->
        F.pp_print_string f "{" ;
        Json.pp_field pp_value f key value ;
        pp_state := InAssocMiddle :: state0
    | InAssocMiddle :: _ ->
        F.pp_print_string f "," ;
        Json.pp_field pp_value f key value
    | _ ->
        L.die InternalError "Unexpected assoc field \"%t\" in state [%a]"
          (fun f -> Json.pp_field pp_value f key value)
          (Pp.seq (Pp.of_string ~f:string_of_state))
          !pp_state
end

type event_type = Begin | Complete | End | Instant

(* TODO(2018): add [Thread] when we have multicore OCaml :) *)
type scope = Global | Process

(* initialised at the start of the program *)
let t0 = Mtime_clock.now ()

let pp_arguments_field f arguments =
  match arguments with
  | [] ->
      ()
  | _ :: _ ->
      JsonFragment.pp_assoc_field Json.pp f "args" (`Assoc arguments)


let pp_categories_field f categories =
  match categories with
  | [] ->
      ()
  | _ :: _ ->
      let pp_categories f categories =
        Format.fprintf f "\"%a\"" (Json.pp_seq F.pp_print_string) categories
      in
      JsonFragment.pp_assoc_field pp_categories f "cat" categories


let pp_duration_field f duration =
  JsonFragment.pp_assoc_field Json.pp f "dur" (`Int (IMtime.span_to_us_int duration))


let pp_event_type_field f event_type =
  let pp_event_type f event_type =
    match event_type with
    | Begin ->
        Json.pp_string f "B"
    | Complete ->
        Json.pp_string f "X"
    | End ->
        Json.pp_string f "E"
    | Instant ->
        Json.pp_string f "i"
  in
  JsonFragment.pp_assoc_field pp_event_type f "ph" event_type


let pp_process_id_field f pid = JsonFragment.pp_assoc_field Pid.pp f "pid" pid

let pp_name_field f name = JsonFragment.pp_assoc_field Json.pp f "name" (`String name)

let pp_scope_field f scope =
  let pp_scope f = function Global -> Json.pp_string f "g" | Process -> Json.pp_string f "p" in
  JsonFragment.pp_assoc_field pp_scope f "s" scope


let pp_timestamp_field f ts_opt =
  let ts = match ts_opt with None -> Mtime_clock.elapsed () | Some t -> Mtime.span t0 t in
  JsonFragment.pp_assoc_field Json.pp f "ts" (`Int (IMtime.span_to_us_int ts))


let log_begin_event f ?timestamp ?categories ?arguments ~name () =
  JsonFragment.pp f AssocBegin ;
  pp_event_type_field f Begin ;
  pp_name_field f name ;
  pp_timestamp_field f timestamp ;
  pp_process_id_field f (ProcessPoolState.get_pid ()) ;
  Option.iter categories ~f:(pp_categories_field f) ;
  Option.iter arguments ~f:(pp_arguments_field f) ;
  JsonFragment.pp f AssocEnd


let log_end_event f ?timestamp ?arguments () =
  JsonFragment.pp f AssocBegin ;
  pp_event_type_field f End ;
  pp_timestamp_field f timestamp ;
  Option.iter arguments ~f:(pp_arguments_field f) ;
  pp_process_id_field f (ProcessPoolState.get_pid ()) ;
  JsonFragment.pp f AssocEnd


let log_complete_event f ~timestamp ?duration ?categories ?arguments ~name () =
  JsonFragment.pp f AssocBegin ;
  pp_event_type_field f Complete ;
  pp_name_field f name ;
  pp_timestamp_field f (Some timestamp) ;
  pp_process_id_field f (ProcessPoolState.get_pid ()) ;
  Option.iter duration ~f:(pp_duration_field f) ;
  Option.iter categories ~f:(pp_categories_field f) ;
  Option.iter arguments ~f:(pp_arguments_field f) ;
  JsonFragment.pp f AssocEnd


let log_instant_event f ?timestamp ~name scope =
  JsonFragment.pp f AssocBegin ;
  pp_event_type_field f Instant ;
  pp_name_field f name ;
  pp_timestamp_field f timestamp ;
  pp_process_id_field f (ProcessPoolState.get_pid ()) ;
  pp_scope_field f scope ;
  JsonFragment.pp f AssocEnd


type logger = F.formatter

let register_gc_stats logger =
  let alarms = ref [] in
  let gc_alarm = Gc.Expert.Alarm.create (fun () -> alarms := Mtime_clock.now () :: !alarms) in
  Epilogues.register ~description:"recording gc alarms" ~f:(fun () ->
      Gc.Expert.Alarm.delete gc_alarm ;
      List.iter !alarms ~f:(fun timestamp ->
          log_instant_event logger ~timestamp ~name:"gc_major" Process ;
          JsonFragment.pp logger ListItemSeparator ;
          F.fprintf logger "%!" ) )


let logger =
  lazy
    (let log_file =
       (* if invoked in a sub-dir (e.g., in Buck integrations), log inside the original log file *)
       ResultsDirEntryName.get_path ~results_dir:Config.toplevel_results_dir PerfEvents
     in
     let is_toplevel_process =
       Config.is_originator && not (Option.is_some !ProcessPoolState.in_child)
     in
     ( if is_toplevel_process then
         let preexisting_logfile = ISys.file_exists log_file in
         if preexisting_logfile then Unix.unlink log_file ) ;
     let out_channel = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 log_file in
     let logger = F.formatter_of_out_channel out_channel in
     register_gc_stats logger ;
     ( if is_toplevel_process then (
         JsonFragment.pp logger ListBegin ;
         F.fprintf logger "%!" ;
         Epilogues.register_late ~description:"closing perf trace json" ~f:(fun () ->
             log_instant_event logger ~name:"end" Global ;
             JsonFragment.pp logger ListEnd ;
             F.fprintf logger "@." ;
             Out_channel.close out_channel ) )
       else
         (* assume the trace file is here and is ready to accept list elements *)
         JsonFragment.(pp_state := InList :: !pp_state) ) ;
     logger )


(* export logging functions that output a list element at a time and flushes so that multiple
   processes can write to the same file and not garble each other's output. This should mostly work
   as appending to a file is atomic as long as the write is not too big. *)

let log_begin_event f ?timestamp ?categories ?arguments ~name () =
  log_begin_event f ?timestamp ?categories ?arguments ~name () ;
  JsonFragment.pp f ListItemSeparator ;
  F.fprintf f "%!"


let log_end_event f ?timestamp ?arguments () =
  log_end_event f ?timestamp ?arguments () ;
  JsonFragment.pp f ListItemSeparator ;
  F.fprintf f "%!"


let log_complete_event f ~timestamp ?duration ?categories ?arguments ~name () =
  log_complete_event f ~timestamp ?duration ?categories ?arguments ~name () ;
  JsonFragment.pp f ListItemSeparator ;
  F.fprintf f "%!"


let log_instant_event f ?timestamp ~name scope =
  log_instant_event f ?timestamp ~name scope ;
  JsonFragment.pp f ListItemSeparator ;
  F.fprintf f "%!"


let log =
  if Config.trace_events then fun f_log ->
    let logger = Lazy.force logger in
    f_log logger
  else fun _ -> ()


let init () = if Config.trace_events then ignore (Lazy.force logger)
