(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type exception_details =
  { msg: string
  ; position: Logging.ocaml_pos
  ; source_range: Clang_ast_t.source_range
  ; ast_node: string option }

exception Unimplemented of exception_details

exception IncorrectAssumption of exception_details

exception Invalid_declaration

let unimplemented position source_range ?ast_node fmt =
  F.kasprintf (fun msg -> raise (Unimplemented {msg; position; source_range; ast_node})) fmt


let incorrect_assumption position source_range ?ast_node fmt =
  F.kasprintf (fun msg -> raise (IncorrectAssumption {msg; position; source_range; ast_node})) fmt


let protect ~f ~recover ~pp_context =
  let log_and_recover ~print fmt =
    recover () ;
    (if print then L.internal_error else L.(debug Capture Quiet)) ("%a@\n" ^^ fmt) pp_context ()
  in
  try f () with
  (* Always keep going in case of known limitations of the frontend, crash otherwise (by not
     catching the exception) unless `--keep-going` was passed. Print errors we should fix
     (t21762295) to the console. *)
  | Unimplemented e ->
      log_and_recover ~print:false "Unimplemented feature:@\n  %s@\n" e.msg
  | IncorrectAssumption e ->
      (* FIXME(t21762295): we do not expect this to happen but it does *)
      log_and_recover ~print:true "Known incorrect assumption in the frontend: %s@\n" e.msg
  | exn ->
      let trace = Backtrace.get () in
      IExn.reraise_if exn ~f:(fun () ->
          L.internal_error "%a: %a@\n%!" pp_context () Exn.pp exn ;
          not Config.keep_going ) ;
      log_and_recover ~print:true "Frontend error: %a@\nBacktrace:@\n%s" Exn.pp exn
        (Backtrace.to_string trace)
