(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** log messages at different levels of verbosity *)

module F = Format

(* =============== START of module MyErr =============== *)
(** type of printable elements *)
type print_type =
  | PTatom
  | PTdecrease_indent
  | PTexp
  | PTexp_list
  | PThpred
  | PTincrease_indent
  | PTinstr
  | PTinstr_list
  | PTjprop_list
  | PTjprop_short
  | PTloc
  | PTnode_instrs
  | PToff
  | PToff_list
  | PTpath
  | PTprop
  | PTproplist
  | PTprop_list_with_typ
  | PTprop_with_typ
  | PTpvar
  | PTspec
  | PTstr
  | PTstr_color
  | PTstrln
  | PTstrln_color
  | PTpathset
  | PTpi
  | PTsexp
  | PTsexp_list
  | PTsigma
  | PTtexp_full
  | PTsub
  | PTtyp_full
  | PTtyp_list
  | PTwarning
  | PTerror
  | PTinfo

(** delayable print action *)
type print_action =
  print_type * Obj.t (** data to be printed *)

let delayed_actions = ref []

(** hook for the current printer of delayed print actions *)
let printer_hook = ref (Obj.magic ())

(** Current formatter for the out stream *)
let current_out_formatter = ref F.std_formatter

(** Current formatter for the err stream *)
let current_err_formatter = ref F.err_formatter

(** Get the current err formatter *)
let get_err_formatter () = !current_err_formatter

(** Set the current out formatter *)
let set_out_formatter fmt =
  current_out_formatter := fmt

(** Set the current err formatter *)
let set_err_formatter fmt =
  current_err_formatter := fmt

(** Flush the current streams *)
let flush_streams () =
  if !Config.developer_mode then
    begin
      F.fprintf !current_out_formatter "@?";
      F.fprintf !current_err_formatter "@?"
    end

(** extend the current print log *)
let add_print_action pact =
  if !Config.write_html then delayed_actions := pact :: !delayed_actions
  else if not !Config.test then !printer_hook !current_out_formatter pact

(** reset the delayed print actions *)
let reset_delayed_prints () =
  delayed_actions := []

(** return the delayed print actions *)
let get_delayed_prints () =
  !delayed_actions

(** set the delayed print actions *)
let set_delayed_prints new_delayed_actions =
  delayed_actions := new_delayed_actions

let do_print fmt fmt_string =
  F.fprintf fmt fmt_string

let do_print_in_developer_mode fmt fmt_string =
  if !Config.developer_mode then
    F.fprintf fmt fmt_string
  else
    F.ifprintf fmt fmt_string

(** print to the current out stream (note: only prints in developer mode) *)
let out fmt_string =
  do_print_in_developer_mode !current_out_formatter fmt_string

(** print to the current err stream (note: only prints in developer mode) *)
let err fmt_string =
  do_print_in_developer_mode !current_err_formatter fmt_string

(** print immediately to standard error *)
let stderr fmt_string =
  do_print F.err_formatter fmt_string

(** print immediately to standard output *)
let stdout fmt_string =
  do_print F.std_formatter fmt_string

(** Type of location in ml source: __POS__ *)
type ml_loc = string * int * int * int

(** Convert a ml location to a string *)
let ml_loc_to_string (file, lnum, cnum, enum) =
  Printf.sprintf "%s:%d:%d-%d:" file lnum cnum enum

(** Pretty print a location of ml source *)
let pp_ml_loc fmt ml_loc =
  F.fprintf fmt "%s" (ml_loc_to_string ml_loc)

let pp_ml_loc_opt fmt ml_loc_opt =
  if !Config.developer_mode then match ml_loc_opt with
    | None -> ()
    | Some ml_loc -> F.fprintf fmt "(%a)" pp_ml_loc ml_loc

let assert_false ((file, lnum, cnum, _) as ml_loc) =
  Printf.eprintf "\nASSERT FALSE %s\nCALL STACK\n%s\n%!"
    (ml_loc_to_string ml_loc)
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 1000));
  raise (Assert_failure (file, lnum, cnum))

(** print a warning with information of the position in the ml source where it oririnated.
    use as: warning_position "description" (try assert false with Assert_failure x -> x); *)
let warning_position (s: string) (ml_loc: ml_loc) =
  err "WARNING: %s in %a@." s pp_ml_loc_opt (Some ml_loc)

(** dump a string *)
let d_str (s: string) = add_print_action (PTstr, Obj.repr s)

(** dump a string with the given color *)
let d_str_color (c: color) (s: string) = add_print_action (PTstr_color, Obj.repr (s, c))

(** dump an error string *)
let d_error (s: string) = add_print_action (PTerror, Obj.repr s)

(** dump a warning string *)
let d_warning (s: string) = add_print_action (PTwarning, Obj.repr s)

(** dump an info string *)
let d_info (s: string) = add_print_action (PTinfo, Obj.repr s)

(** dump a string plus newline *)
let d_strln (s: string) = add_print_action (PTstrln, Obj.repr s)

(** dump a string plus newline with the given color *)
let d_strln_color (c: color) (s: string) = add_print_action (PTstrln_color, Obj.repr (s, c))

(** dump a newline *)
let d_ln () = add_print_action (PTstrln, Obj.repr "")

(** dump an indentation *)
let d_indent indent =
  let s = ref "" in
  for _ = 1 to indent do s := "  " ^ !s done;
  if indent <> 0 then add_print_action (PTstr, Obj.repr !s)

(** dump command to increase the indentation level *)
let d_increase_indent (indent: int) =
  add_print_action (PTincrease_indent, Obj.repr indent)

(** dump command to decrease the indentation level *)
let d_decrease_indent (indent: int) =
  add_print_action (PTdecrease_indent, Obj.repr indent)

let log_progress_simple text =
  if !Config.show_progress_bar then
    F.fprintf Format.err_formatter "%s@?" text

let log_progress_file () =
  log_progress_simple "F"

let log_progress_procedure () =
  log_progress_simple "."

let log_progress_timeout_event failure_kind =
  if !Config.developer_mode then
    begin
      match failure_kind with
      | SymOp.FKtimeout ->
          log_progress_simple "T"
      | SymOp.FKsymops_timeout _ ->
          log_progress_simple "S"
      | SymOp.FKrecursion_timeout _ ->
          log_progress_simple "R"
      | SymOp.FKcrash _ ->
          log_progress_simple "C"
    end
