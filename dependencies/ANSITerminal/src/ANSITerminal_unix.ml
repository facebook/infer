(* File: ANSITerminal_unix.ml
   Allow colors, cursor movements, erasing,... under Unix shells.
   *********************************************************************

   Copyright 2004 by Troestler Christophe
   Christophe.Troestler(at)umons.ac.be

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 3 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(** See the file(s) ctlseqs.* (unix; in Debian package xspecs)

    CSI = "\027[" (ESC [)

    man console_codes
*)

(* man tty(4) *)

open Printf
open Scanf
include ANSITerminal_common

let isatty = ref Unix.isatty

let is_out_channel_atty ch = !isatty(Unix.descr_of_out_channel ch)

(* Cursor *)

let set_cursor x y =
  if is_out_channel_atty stdout then (
    if x <= 0 then (if y > 0 then printf "\027[%id%!" y)
    else (* x > 0 *) if y <= 0 then printf "\027[%iG%!" x
    else printf "\027[%i;%iH%!" y x
  )

let move_cursor x y =
  if is_out_channel_atty stdout then (
    if x > 0 then printf "\027[%iC%!" x
    else if x < 0 then printf "\027[%iD%!" (-x);
    if y > 0 then printf "\027[%iB%!" y
    else if y < 0 then printf "\027[%iA%!" (-y)
  )

let save_cursor () =
  if is_out_channel_atty stdout then printf "\027[s%!"
let restore_cursor () =
  if is_out_channel_atty stdout then printf "\027[u%!"
let move_bol () = print_string "\r"; flush stdout


(* Inpired by http://www.ohse.de/uwe/software/resize.c.html and
   http://qemacs.sourcearchive.com/documentation/0.3.1.cvs.20050713-5/tty_8c-source.html *)
let send_and_read_response fdin query fmt f =
  let alarm = ref false in
  let set_alarm (_:int) = alarm := true in
  let old_alarm = Sys.signal Sys.sigalrm (Sys.Signal_handle set_alarm) in
  let tty = Unix.tcgetattr fdin in
  Unix.tcsetattr fdin Unix.TCSANOW { tty with
    Unix.c_ignbrk = false; c_brkint = false; c_parmrk = false;
    c_istrip = false; c_inlcr = false; c_igncr = false; c_icrnl = false;
    c_ixon = false;  c_opost = true;
    c_csize = 8;  c_parenb = false;  c_icanon = false; c_isig = false;
    c_echo = false; c_echonl = false;
    c_vmin = 1; c_vtime = 0 };
  let restore() =
    ignore(Unix.alarm 0);
    Unix.tcsetattr fdin Unix.TCSANOW tty;
    Sys.set_signal Sys.sigalrm old_alarm in
  let buf = Bytes.make 127 '\000' in
  (* FIXME: make it more robust so that it ignores previous key pressed. *)
  let rec get_answer pos =
    let l = Unix.read fdin buf pos 1 in
    let buf = Bytes.unsafe_to_string buf in (* local use only *)
    try sscanf buf fmt f (* bail out as soon as enough info is present *)
    with Scan_failure _ ->
      if !alarm || pos = 126 then failwith "ANSITerminal.input_answer"
      else if buf.[pos] = '\000' then get_answer pos
      else get_answer (pos + l) in
  try
    ignore(Unix.write fdin query 0 (Bytes.length query));
    ignore(Unix.alarm 1);
    let r = get_answer 0 in
    restore();
    r
  with e ->
    restore();
    raise e

(* Query Cursor Position	<ESC>[6n *)
(* Report Cursor Position	<ESC>[{ROW};{COLUMN}R *)
let pos_cursor_query = Bytes.of_string "\027[6n"
let pos_cursor () =
  if is_out_channel_atty stdout then (
    try
      send_and_read_response Unix.stdin pos_cursor_query
        "\027[%d;%dR" (fun y x -> (x,y))
    with _ -> failwith "ANSITerminal.pos_cursor"
  )
  else failwith "ANSITerminal.pos_cursor: not a TTY"

(* See also the output of 'resize -s x y' (e.g. in an Emacs shell). *)
let resize width height =
  if is_out_channel_atty stdout then (
    if width <= 0 then invalid_arg "ANSITerminal.resize: width <= 0";
    if height <= 0 then invalid_arg "ANSITerminal.resize: height <= 0";
    printf "\027[8;%i;%it%!" height width
  )

(* FIXME: what about the following recipe:
   If you run
     echo -e "\e[18t"
   then xterm will respond with a line of the form
     ESC [ 8 ; height ; width t
   It generates this line as if it were typed input, so it can then be
   read by your program on stdin. *)
external size_ : Unix.file_descr -> int * int = "ANSITerminal_term_size"

let size () =
  if !isatty Unix.stdin then (
    size_ Unix.stdin
  )
  else failwith "ANSITerminal.size: not a TTY"

(* Erasing *)

let erase loc =
  if is_out_channel_atty stdout then (
    print_string (match loc with
                  | Eol -> "\027[K"
                  | Above -> "\027[1J"
                  | Below ->"\027[0J"
                  | Screen -> "\027[2J");
    flush stdout
  )

(* Scrolling *)

let scroll lines =
  if is_out_channel_atty stdout then (
    if lines > 0 then printf "\027[%iS%!" lines
    else if lines < 0 then printf "\027[%iT%!" (- lines)
  )

let style_to_string = function
  | Reset -> "0"
  | Bold -> "1"
  | Underlined -> "4"
  | Blink -> "5"
  | Inverse -> "7"
  | Hidden -> "8"
  | Foreground Black -> "30"
  | Foreground Red -> "31"
  | Foreground Green -> "32"
  | Foreground Yellow -> "33"
  | Foreground Blue -> "34"
  | Foreground Magenta -> "35"
  | Foreground Cyan -> "36"
  | Foreground White -> "37"
  | Foreground Default -> "39"
  | Background Black -> "40"
  | Background Red -> "41"
  | Background Green -> "42"
  | Background Yellow -> "43"
  | Background Blue -> "44"
  | Background Magenta -> "45"
  | Background Cyan -> "46"
  | Background White -> "47"
  | Background Default -> "49"


let print_with pr ~tty style txt =
  if tty then (
    pr "\027[";
    pr (String.concat ";" (List.map style_to_string style));
    pr "m";
  );
  pr txt;
  if tty && !autoreset then pr "\027[0m"

let print_string style txt =
  print_with print_string style txt ~tty:(is_out_channel_atty stdout)

let prerr_string style txt =
  print_with prerr_string style txt ~tty:(is_out_channel_atty stderr)

let printf style = ksprintf (print_string style)

let eprintf style = ksprintf (prerr_string style)


let to_string style txt =
  let s = "\027["
          ^ String.concat ";" (List.map style_to_string style)
          ^ "m"
          ^ txt in
  if !autoreset then s ^ "\027[0m" else s

let sprintf style = ksprintf (to_string style)
