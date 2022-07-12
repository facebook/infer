(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** "Unit" abstract domain *)

module T = struct
  type t = unit [@@deriving compare, equal, sexp]
end

include T
module Set = Set.Make (T)

let pp fs () = Format.pp_print_string fs "()"
let init _ = ()
let join () () = ()
let joinN _ = ()
let resolve_int _ _ _ = []
let exec_assume _ () _ = Some ()
let exec_kill _ _ () = ()
let exec_move _ _ () = ()
let exec_inst _ _ () = Ok ()

type from_call = unit [@@deriving compare, equal, sexp]

let call ~summaries:_ _ ?child:_ ~globals:_ ~actuals:_ ~areturn:_ ~formals:_
    ~freturn:_ ~locals:_ _ =
  ((), ())

let recursion_beyond_bound = `skip
let post _ _ _ () = ()
let retn _ _ _ _ _ = ()

type term_code = unit [@@deriving compare, sexp_of]

let term _ _ _ _ = ()
let move_term_code _ _ () () = ()
let dnf () = Iter.singleton ()
let resolve_callee _ _ _ _ = []

type summary = unit

let pp_summary fs () = Format.pp_print_string fs "()"
let create_summary _ ~locals:_ ~formals:_ _ = ((), ())
let apply_summary _ _ = Some ()
