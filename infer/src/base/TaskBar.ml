(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** {2 arbitrary constants} *)

(** max size for the top bar of the multiline task bar *)
let top_bar_size_default = 100

(** do not attempt to draw the top bar of the multiline task bar unless it can be at least this big *)
let min_acceptable_progress_bar = 10

(** infer rulez *)
let job_prefix = SpecialChars.right_tack ^ " "

(** How often we allow refreshing the task bar *)
let refresh_timeout =
  (* 25 Hz *)
  Mtime.Span.(40 * ms)


(** {2 Task bar} *)

(** state of a multi-line task bar *)
type multiline_info =
  { jobs: int  (** number of jobs running in parallel *)
  ; jobs_statuses: string Array.t
        (** array of size [jobs] with a description of what the process is doing *)
  ; jobs_start_times: Mtime.t Array.t  (** array of size [jobs] of start times for each process *)
  ; heap_words: int option Array.t  (** array of size [jobs] of heap words for each process *)
  ; start_time: Mtime_clock.counter  (** time since the creation of the task bar *)
  ; mutable tasks_done: int
  ; mutable tasks_total: int
  ; mutable last_refresh_time: Mtime_clock.counter }

type t =
  | MultiLine of multiline_info  (** interactive *)
  | NonInteractive  (** display terse progress, to use when output is redirected *)
  | Quiet  (** ignore everything *)

(** print [c] [n] times *)
let rec pp_n c fmt n =
  if n > 0 then (
    F.pp_print_char fmt c ;
    pp_n c fmt (n - 1) )


let move_bol = "\r"

let move_cursor_down n = Printf.sprintf "\027[%iB" n

let move_cursor_up n = Printf.sprintf "\027[%iA" n

let erase_eol = "\027[0K"

let draw_top_bar fmt ~term_width ~total ~finished ~elapsed =
  let tasks_total_string = Int.to_string total in
  let bar_tasks_num_size = String.length tasks_total_string in
  let elapsed_string = F.asprintf "%a" Mtime.Span.pp elapsed in
  (* format string for the full top bar, assuming there is enough room, and number of characters
     taken by the portion of the top bar that is not the progress bar itself *)
  let top_bar_fmt, size_around_progress_bar =
    (* add pairs of a partial format string and its expected size *)
    let ( ++ ) (f1, l1) (f2, l2) = (f1 ^^ f2, l1 + l2) in
    let ( +++ ) (f1, l1) f2 = (f1 ^^ f2, l1 + (string_of_format f2 |> String.length)) in
    ("%*d", bar_tasks_num_size (* finished *))
    +++ "/"
    ++ ("%s", bar_tasks_num_size (* total *))
    +++ " [" ++ ("%a%a", 0 (* progress bar *)) +++ "] "
    ++ ("%d%%", 3 (* "xxx%", even though sometimes it's just "x%" *))
    +++ " "
    ++ ( "%s"
       , max (String.length elapsed_string) 9
         (* leave some room for elapsed_string to avoid flicker. 9 characters is "XXhXXmXXs" so it
            gives some reasonable margin. *) )
  in
  let top_bar_size = min term_width top_bar_size_default in
  let progress_bar_size = top_bar_size - size_around_progress_bar in
  ( if progress_bar_size < min_acceptable_progress_bar then
      let s = Printf.sprintf "%d/%s %s" finished tasks_total_string elapsed_string in
      F.fprintf fmt "%s" (String.prefix s term_width)
    else
      let bar_done_size = finished * progress_bar_size / total in
      F.fprintf fmt top_bar_fmt bar_tasks_num_size finished tasks_total_string (pp_n '#')
        bar_done_size (pp_n '.')
        (progress_bar_size - bar_done_size)
        (finished * 100 / total)
        elapsed_string ) ;
  F.fprintf fmt "%s\n" erase_eol


let print_heap_words =
  let word_size_byte = Word_size.(num_bits word_size) / 8 in
  let hundred_k = 1024 * 100 in
  fun fmt -> function
    | None ->
        F.fprintf fmt "[     ]"
    | Some heap_words ->
        let bytes = heap_words * word_size_byte in
        if bytes < hundred_k then F.fprintf fmt "[%4.1fK]" Float.(of_int bytes / 1024.0)
        else
          let kilo_bytes = bytes / 1024 in
          if kilo_bytes < hundred_k then F.fprintf fmt "[%4.1fM]" Float.(of_int kilo_bytes / 1024.0)
          else
            let mega_bytes = kilo_bytes / 1024 in
            if mega_bytes < hundred_k then
              F.fprintf fmt "[%4.1fG]" Float.(of_int mega_bytes / 1024.0)
            else
              let giga_bytes = mega_bytes / 1024 in
              F.fprintf fmt "[%4dG]" giga_bytes


let draw_job_status fmt ~term_width ~draw_time t ~status ~t0 ~heap_words =
  let length = ref 0 in
  let job_prefix_size = String.length job_prefix in
  if term_width > job_prefix_size then (
    F.fprintf fmt "%s" (ANSITerminal.(sprintf [Bold; magenta]) "%s" job_prefix) ;
    length := !length + job_prefix_size ) ;
  let time_width = 4 + (* actually drawing the time *) 4 (* "[s] " *) in
  if draw_time && term_width > time_width + job_prefix_size then (
    let time_running = Mtime.span t0 t |> IMtime.span_to_s_float in
    F.fprintf fmt "[%4.1fs]" time_running ;
    (let heap_words_width = 7 in
     if term_width > heap_words_width + time_width + job_prefix_size then (
       print_heap_words fmt heap_words ;
       length := !length + heap_words_width ) ) ;
    F.fprintf fmt " " ;
    length := !length + time_width ) ;
  F.fprintf fmt "%s%s\n" (String.prefix status (term_width - !length)) erase_eol


let refresh_multiline task_bar =
  let should_draw_progress_bar = task_bar.tasks_total > 0 && task_bar.tasks_done >= 0 in
  let term_width, _ = ANSITerminal.size () in
  F.pp_print_string F.err_formatter move_bol ;
  if should_draw_progress_bar then
    draw_top_bar F.err_formatter ~term_width ~total:task_bar.tasks_total
      ~finished:task_bar.tasks_done
      ~elapsed:(Mtime_clock.count task_bar.start_time) ;
  let draw_time =
    (* When there is only 1 job we are careful not to spawn processes needlessly, thus there is no
       one to refresh the task bar while the analysis is running and the time displayed will always
       be 0. Avoid confusion by not displaying the time in that case. *)
    task_bar.jobs > 1
  in
  let now = Mtime_clock.now () in
  for i = 0 to task_bar.jobs - 1 do
    let status = task_bar.jobs_statuses.(i) in
    let t0 = task_bar.jobs_start_times.(i) in
    let heap_words = task_bar.heap_words.(i) in
    draw_job_status F.err_formatter ~term_width ~draw_time now ~status ~t0 ~heap_words
  done ;
  let lines_printed =
    let progress_bar = if should_draw_progress_bar then 1 else 0 in
    task_bar.jobs + progress_bar
  in
  F.eprintf "%s%!" (move_cursor_up lines_printed) ;
  ()


let refresh_multiline ?(force = false) task_bar =
  let elapsed = Mtime_clock.count task_bar.last_refresh_time in
  if force || Mtime.Span.compare elapsed refresh_timeout > 0 then (
    refresh_multiline task_bar ;
    task_bar.last_refresh_time <- Mtime_clock.counter () )


let refresh = function MultiLine t -> refresh_multiline t | NonInteractive | Quiet -> ()

let create ~jobs =
  match Config.progress_bar with
  | `Quiet ->
      Quiet
  | `Plain ->
      NonInteractive
  | `MultiLine ->
      let t0 = Mtime_clock.now () in
      let task_bar =
        { jobs
        ; jobs_statuses= Array.create ~len:jobs "idle"
        ; jobs_start_times= Array.create ~len:jobs t0
        ; heap_words= Array.create ~len:jobs None
        ; start_time= Mtime_clock.counter ()
        ; tasks_done= 0
        ; tasks_total= 0
        ; last_refresh_time= Mtime_clock.counter () }
      in
      ANSITerminal.erase Below ;
      MultiLine task_bar


let update_status_multiline task_bar ~slot:job t0 ?heap_words status =
  task_bar.jobs_statuses.(job) <- status ;
  task_bar.jobs_start_times.(job) <- t0 ;
  if Option.is_some heap_words then task_bar.heap_words.(job) <- heap_words ;
  ()


let update_status task_bar ~slot t0 ?heap_words status =
  match task_bar with
  | MultiLine t ->
      update_status_multiline t ~slot t0 ?heap_words status
  | NonInteractive | Quiet ->
      ()


let update_heap_words task_bar ~slot heap_words =
  match task_bar with
  | MultiLine t ->
      t.heap_words.(slot) <- Some heap_words
  | NonInteractive | Quiet ->
      ()


let set_tasks_total task_bar n =
  match task_bar with
  | MultiLine multiline ->
      multiline.tasks_total <- n
  | NonInteractive | Quiet ->
      ()


let set_remaining_tasks task_bar n =
  match task_bar with
  | MultiLine multiline ->
      multiline.tasks_done <- multiline.tasks_total - n
  | NonInteractive | Quiet ->
      ()


let tasks_done_reset task_bar =
  match task_bar with
  | MultiLine multiline ->
      multiline.tasks_done <- 0
  | NonInteractive | Quiet ->
      ()


let finish = function
  | MultiLine taskbar ->
      (* Force the final refresh to display the latest progress *)
      refresh_multiline ~force:true taskbar ;
      (* leave the progress bar displayed *)
      F.eprintf "%s%!" (move_cursor_down 1) ;
      ANSITerminal.erase Below ;
      Out_channel.flush stderr
  | NonInteractive | Quiet ->
      ()
