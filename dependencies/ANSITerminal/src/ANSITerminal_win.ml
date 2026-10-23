(* File: ANSITerminal_win.ml

   Copyright 2010 by Vincent Hugot
   vincent.hugot@gmail.com
   www.vincent-hugot.com

   Copyright 2010 by Troestler Christophe
   Christophe.Troestler@umons.ac.be

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 3 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

open Printf
include ANSITerminal_common

exception Error of string * string
let () = Callback.register_exception "ANSITerminal.Error" (Error("",""))

let isatty = ref Unix.isatty

let is_out_channel_atty ch = !isatty(Unix.descr_of_out_channel ch)


type rgb = R|G|B

let rgb_of_color = function
  | Red -> [R]
  | Green -> [G]
  | Blue -> [B]
  | Default -> []
  | White -> [R;G;B]
  | Cyan -> [B;G]
  | Magenta -> [B;R]
  | Yellow -> [R;G]
  | Black -> []

(* calls to SetConsoleTextAttribute replace one another, so
   foreground, background and bold must be set in the same action *)
type color_state = {
  fore : rgb list;
  back : rgb list;
  bold : bool ; (* could intensify background too, but Unix does not
                   support that so scrapped. *)
}

let empty = { fore = [R;G;B]; back = [] ; bold = false }

let state_of_styles sty =
  List.fold_left (fun sta style ->
    match style with
    | Reset -> empty (* could stop there, but does not,
                       for exact compat with ansi *)
    | Bold -> {sta with bold = true }
    | Inverse ->
        (* simulated inverse... not exact compat *)
        let oba = sta.back and ofo = sta.fore in
        {sta with fore = oba; back = ofo }
    | Foreground c -> {sta with fore = rgb_of_color c }
    | Background c -> {sta with back = rgb_of_color c }
    | _  -> sta
  ) empty sty

let int_of_state st =
  (* Quoth wincon.h
     #define FOREGROUND_BLUE 1
     #define FOREGROUND_GREEN  2
     #define FOREGROUND_RED  4
     #define FOREGROUND_INTENSITY  8
     #define BACKGROUND_BLUE 16
     #define BACKGROUND_GREEN  32
     #define BACKGROUND_RED  64
     #define BACKGROUND_INTENSITY  128 *)
  let fo = function R -> 4  | G -> 2  | B -> 1
  and ba = function R -> 64 | G -> 32 | B -> 16
  and sum mode rgb = List.fold_left (lor) 0 (List.map mode rgb) in
  sum fo st.fore lor sum ba st.back lor (if st.bold then 8 else 0)
  (*
    let win_set_style code = printf "<%d>" code
    let win_unset_style () = printf "<unset>"
  *)

external win_set_style : out_channel -> int -> unit = "ANSITerminal_set_style"
external win_unset_style : out_channel -> int -> unit
  = "ANSITerminal_unset_style"
(* [win_unset_style] is the same as [win_set_style] except for the
   error message. *)
external win_get_style : out_channel -> int = "ANSITerminal_get_style"

let channel_styles = Hashtbl.create 8

let set_style ch styles =
  let prev_sty = win_get_style ch in
  Hashtbl.add channel_styles ch prev_sty;
  let st = int_of_state (state_of_styles styles) in
  flush ch;
  win_set_style ch st;
  flush ch

let unset_style ch =
  flush ch;
  try win_unset_style ch (Hashtbl.find channel_styles ch);
      Hashtbl.remove channel_styles ch
  with Not_found -> ()


let print ch styles txt =
  let tty = is_out_channel_atty ch in
  if tty then set_style ch styles;
  output_string ch txt;
  flush ch;
  if tty && !autoreset then unset_style ch

let print_string = print stdout
let prerr_string = print stderr

let printf style = kprintf (print_string style)

let eprintf style = ksprintf (prerr_string style)

let sprintf _style = sprintf

external set_cursor_ : int -> int -> unit = "ANSITerminal_SetCursorPosition"
external pos_cursor : unit -> int * int = "ANSITerminal_pos"
external scroll : int -> unit = "ANSITerminal_Scroll"
external size : unit -> int * int = "ANSITerminal_size"
external resize_ : int -> int -> unit = "ANSITerminal_resize"

let set_cursor x y =
  if is_out_channel_atty stdout then (
    let x0, y0 = pos_cursor() in
    let x = if x <= 0 then x0 else x
    and y = if y <= 0 then y0 else y in
    set_cursor_ x y (* FIXME: (x,y) outside the console?? *)
  )

let move_cursor dx dy =
  if is_out_channel_atty stdout then (
    let x0, y0 = pos_cursor() in
    let x = x0 + dx and y = y0 + dy in
    let x = if x <= 0 then 1 else x
    and y = if y <= 0 then 1 else y in
    set_cursor_ x y (* FIXME: (x,y) outside the console?? *)
  )

let move_bol () =
  if is_out_channel_atty stdout then (
    let _, y0 = pos_cursor() in
    set_cursor_ 1 y0
  )

let saved_x = ref 0
let saved_y = ref 0

let save_cursor () =
  if is_out_channel_atty stdout then (
    let x,y = pos_cursor() in
    saved_x := x;
    saved_y := y
  )

let restore_cursor () =
  if is_out_channel_atty stdout then set_cursor_ !saved_x !saved_y


let resize x y =
  if is_out_channel_atty stdout then (
    (* The specified width and height cannot be less than the width and
       height of the console screen buffer's window. *)
    let xmin, ymin = size() in
    let x = if x <= xmin then xmin else x
    and y = if y <= ymin then ymin else y in
    resize_ x y
  )


external fill : out_channel -> char -> n:int -> x:int -> y:int -> int
  = "ANSITerminal_FillConsoleOutputCharacter"
(* Writes the character to the console screen buffer [n] times,
   beginning at the coordinates [(x,y)].  Returns the number of chars
   actually written. *)

let erase loc =
  if is_out_channel_atty stdout then (
    let w, h = size() in
    match loc with
    | Eol ->
       let x, y = pos_cursor() in
       ignore(fill stdout ' ' ~n:(w - x + 1) ~x ~y)
    | Above ->
       let x, y = pos_cursor() in
       ignore(fill stdout ' ' ~n:((y - 1) * w + x) ~x:1 ~y:1)
    | Below ->
       let x, y = pos_cursor() in
       ignore(fill stdout ' ' ~n:(w - x + 1 + (h - y) * w) ~x ~y)
    | Screen ->
       ignore(fill stdout ' ' ~n:(w * h) ~x:1 ~y:1)
  )


;;
(* Local Variables: *)
(* compile-command: "make ANSITerminal_win.cmo" *)
(* End: *)
