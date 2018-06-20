(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Core

[@@@warning "-32"]

(* Compare police: generic compare mostly disabled. *)
let compare = No_polymorphic_compare.compare

let equal = No_polymorphic_compare.equal

let ( = ) = No_polymorphic_compare.( = )

let failwith _ : [`use_Logging_die_instead] = assert false

let failwithf _ : [`use_Logging_die_instead] = assert false

let invalid_arg _ : [`use_Logging_die_instead] = assert false

let invalid_argf _ : [`use_Logging_die_instead] = assert false

let exit = `In_general_prefer_using_Logging_exit_over_Pervasives_exit

[@@@warning "+32"]

module ANSITerminal : module type of ANSITerminal = struct
  include ANSITerminal

  (* from ANSITerminal_unix.ml but using stderr instead of stdout *)
  (* Cursor *)

  let set_cursor x y =
    if Unix.(isatty stderr) then
      if x <= 0 then ( if y > 0 then Printf.eprintf "\027[%id%!" y )
      else if (* x > 0 *) y <= 0 then Printf.eprintf "\027[%iG%!" x
      else Printf.eprintf "\027[%i;%iH%!" y x


  let move_cursor x y =
    if Unix.(isatty stderr) then (
      if x > 0 then Printf.eprintf "\027[%iC%!" x
      else if x < 0 then Printf.eprintf "\027[%iD%!" (-x) ;
      if y > 0 then Printf.eprintf "\027[%iB%!" y
      else if y < 0 then Printf.eprintf "\027[%iA%!" (-y) )


  let save_cursor () = if Unix.(isatty stderr) then Printf.eprintf "\027[s%!"

  let restore_cursor () = if Unix.(isatty stderr) then Printf.eprintf "\027[u%!"

  let move_bol () =
    Out_channel.output_string stderr "\r" ;
    Out_channel.flush stderr


  (* Erasing *)

  let erase loc =
    if Unix.(isatty stderr) then (
      Out_channel.output_string stderr
        ( match loc with
        | Eol ->
            "\027[K"
        | Above ->
            "\027[1J"
        | Below ->
            "\027[0J"
        | Screen ->
            "\027[2J" ) ;
      Out_channel.flush stderr )


  (* Scrolling *)

  let scroll lines =
    if Unix.(isatty stderr) then
      if lines > 0 then Printf.eprintf "\027[%iS%!" lines
      else if lines < 0 then Printf.eprintf "\027[%iT%!" (-lines)


  (* /from ANSITerminal_unix.ml but using stderr instead of stdout *)
  (* more careful about when the channel is connected to a tty *)
  let print_string =
    if Unix.(isatty stdout) then print_string else fun _ -> Pervasives.print_string


  let prerr_string =
    if Unix.(isatty stderr) then prerr_string else fun _ -> Pervasives.prerr_string


  let printf styles fmt = Format.ksprintf (fun s -> print_string styles s) fmt

  let eprintf styles fmt = Format.ksprintf (fun s -> prerr_string styles s) fmt

  let sprintf = if Unix.(isatty stderr) then sprintf else fun _ -> Printf.sprintf
end
