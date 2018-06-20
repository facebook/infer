(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** arbitrary *)
let progress_bar_total_size_default = 60

(** state of a multi-line task bar *)
type multiline_info =
  { jobs: int  (** number of jobs running in parallel *)
  ; statuses: string Array.t
        (** array of size [jobs] with a description of what the process is doing *)
  ; start_times: Mtime.t Array.t  (** array of size [jobs] of start times for each process *)
  ; mutable tasks_done: int
  ; mutable tasks_total: int }

type t =
  | MultiLine of multiline_info  (** interactive *)
  | NonInteractive  (** display terse progress, to use when output is redirected *)
  | Dummy  (** ignore everything *)

(** print [c] [n] times *)
let rec pp_n c oc n =
  if n > 0 then (
    Out_channel.output_char oc c ;
    pp_n c oc (n - 1) )


let progress_bar_total_size =
  lazy
    ( if Unix.(isatty stdin) then
        let term_width, _ = ANSITerminal.size () in
        min progress_bar_total_size_default term_width
    else progress_bar_total_size_default )


let draw_progress_bar ~total ~don =
  let lazy progress_bar_total_size = progress_bar_total_size in
  let bar_done_size = don * progress_bar_total_size / total in
  let tasks_total_string = Int.to_string total in
  let bar_tasks_num_size = String.length tasks_total_string in
  Printf.eprintf "%*d/%s [%a%a] %d%%\n" bar_tasks_num_size don tasks_total_string (pp_n '#')
    bar_done_size (pp_n '.')
    (progress_bar_total_size - bar_done_size)
    (don * 100 / total)


let draw_job_status ~draw_time t ~status ~t0 =
  ANSITerminal.(prerr_string [Bold; magenta]) "⊢ " ;
  ( if draw_time then
      let time_running = Mtime.span t0 t |> Mtime.Span.to_s in
      Printf.eprintf "[%4.1fs] " time_running ) ;
  Out_channel.output_string stderr status ;
  ANSITerminal.erase Eol ;
  Out_channel.output_string stderr "\n"


let refresh_multiline task_bar =
  ANSITerminal.move_bol () ;
  let should_draw_progress_bar = task_bar.tasks_total > 0 && task_bar.tasks_done >= 0 in
  if should_draw_progress_bar then
    draw_progress_bar ~total:task_bar.tasks_total ~don:task_bar.tasks_done ;
  let t = Mtime_clock.now () in
  let draw_time =
    (* When there is only 1 job we are careful not to spawn processes needlessly, thus there is no
       one to refresh the task bar while the analysis is running and the time displayed will always
       be 0. Avoid confusion by not displaying the time in that case. *)
    task_bar.jobs > 1
  in
  Array.iter2_exn task_bar.statuses task_bar.start_times ~f:(fun status t0 ->
      draw_job_status ~draw_time t ~status ~t0 ) ;
  let lines_printed =
    let progress_bar = if should_draw_progress_bar then 1 else 0 in
    task_bar.jobs + progress_bar
  in
  ANSITerminal.move_cursor 0 (-lines_printed) ;
  ()


let refresh = function MultiLine t -> refresh_multiline t | NonInteractive | Dummy -> ()

let create_multiline ~jobs =
  if Unix.(isatty stdin) && Unix.(isatty stderr) then (
    let t0 = Mtime_clock.now () in
    let task_bar =
      { jobs
      ; statuses= Array.create ~len:jobs "idle"
      ; start_times= Array.create ~len:jobs t0
      ; tasks_done= 0
      ; tasks_total= 0 }
    in
    ANSITerminal.erase Below ; MultiLine task_bar )
  else NonInteractive


let create_dummy () = Dummy

let update_status_multiline task_bar ~slot:job t0 status =
  (task_bar.statuses).(job) <- status ;
  (task_bar.start_times).(job) <- t0 ;
  ()


let update_status task_bar ~slot t0 status =
  match task_bar with
  | MultiLine t ->
      update_status_multiline t ~slot t0 status
  | NonInteractive | Dummy ->
      ()


let set_tasks_total task_bar n =
  match task_bar with
  | MultiLine multiline ->
      multiline.tasks_total <- n
  | NonInteractive | Dummy ->
      ()


let tasks_done_add task_bar n =
  match task_bar with
  | MultiLine multiline ->
      multiline.tasks_done <- multiline.tasks_done + n
  | NonInteractive ->
      L.progress "#%!"
  | Dummy ->
      ()


let tasks_done_reset task_bar =
  match task_bar with
  | MultiLine multiline ->
      multiline.tasks_done <- 0
  | NonInteractive | Dummy ->
      ()


let finish = function
  | MultiLine _ ->
      (* leave the progress bar displayed *)
      Out_channel.output_string stderr "\n" ;
      ANSITerminal.erase Below ;
      Out_channel.flush stderr
  | NonInteractive | Dummy ->
      ()


let is_interactive = function MultiLine _ -> true | NonInteractive | Dummy -> false
