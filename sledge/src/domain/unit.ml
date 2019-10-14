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
let join () () = Some ()
let is_false _ = false
let exec_assume () _ = Some ()
let exec_kill () _ = ()
let exec_move () _ = ()
let exec_inst () _ = Some ()
let exec_intrinsic ~skip_throw:_ _ _ _ _ : t option option = None

type from_call = unit [@@deriving compare, equal, sexp]

let call ~summaries:_ ~globals:_ ~actuals:_ ~areturn:_ ~formals:_ ~freturn:_
    ~locals:_ _ =
  ((), ())

let recursion_beyond_bound = `skip
let post _ _ () = ()
let retn _ _ _ _ = ()
let dnf () = [()]

let resolve_callee lookup ptr _ =
  match Reg.of_exp ptr with
  | Some callee -> (lookup (Reg.name callee), ())
  | None -> ([], ())

type summary = unit

let pp_summary fs () = Format.pp_print_string fs "()"
let create_summary ~locals:_ ~formals:_ _ = ((), ())
let apply_summary _ _ = Some ()
