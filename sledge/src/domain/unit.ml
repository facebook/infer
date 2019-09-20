(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** "Unit" abstract domain *)

type t = unit [@@deriving equal, sexp_of]

let pp fs () = Format.pp_print_string fs "()"
let report_fmt_thunk () fs = pp fs ()
let init _ = ()
let join () () = ()
let is_false _ = false
let exec_assume () _ = Some ()
let exec_kill () _ = ()
let exec_move () _ _ = ()
let exec_inst () _ = Ok ()
let exec_intrinsic ~skip_throw:_ _ _ _ _ : (t, unit) result option = None

type from_call = unit [@@deriving compare, equal, sexp]

let call ~summaries:_ _ _ _ _ _ _ = ((), ())
let recursion_beyond_bound = `skip
let post _ _ () = ()
let retn _ _ _ _ = ()
let dnf () = [()]

let resolve_callee lookup ptr () =
  match Var.of_exp ptr with
  | Some callee_name -> (lookup callee_name, ())
  | None -> ([], ())

type summary = unit

let pp_summary fs () = Format.pp_print_string fs "()"
let create_summary ~locals:_ ~formals:_ _ = ((), ())
let apply_summary _ _ = Some ()
