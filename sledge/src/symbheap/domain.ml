(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

type t = State_domain.t * State_domain.t

let embed q = (q, q)
let project (_, curr) = curr

let pp fs (entry, current) =
  Format.fprintf fs "@[<v 1> entry: %a@;current: %a@]" State_domain.pp entry
    State_domain.pp current

let init globals = embed (State_domain.init globals)

let join (entry_a, current_a) (entry_b, current_b) =
  assert (State_domain.equal entry_b entry_a) ;
  (entry_a, State_domain.join current_a current_b)

let is_false (_, curr) = State_domain.is_false curr

let exec_assume (entry, current) cnd =
  match State_domain.exec_assume current cnd with
  | Some current -> Some (entry, current)
  | None -> None

let exec_inst (entry, current) inst =
  match State_domain.exec_inst current inst with
  | Ok current -> Ok (entry, current)
  | Error e -> Error e

let exec_return (entry, current) formal actual =
  (entry, State_domain.exec_return current formal actual)

let exec_intrinsic ~skip_throw (entry, current) result intrinsic actuals =
  match
    State_domain.exec_intrinsic ~skip_throw current result intrinsic actuals
  with
  | None -> None
  | Some (Ok current) -> Some (Ok (entry, current))
  | Some (Error e) -> Some (Error e)

type from_call =
  {state_from_call: State_domain.from_call; caller_entry: State_domain.t}
[@@deriving sexp_of]

let jump actuals formals ?temps (entry, current) =
  let current, _ = State_domain.jump actuals formals ?temps current in
  (entry, current)

let call ~summaries actuals formals locals globals_vec (_, current) =
  let globals =
    Var.Set.of_vector
      (Vector.map globals_vec ~f:(fun (g : Global.t) -> g.var))
  in
  ([%Trace.call fun {pf} ->
     pf
       "@[<v>@[actuals: (@[%a@])@ formals: (@[%a@])@]@ locals: {@[%a@]}@ \
        globals: {@[%a@]}@ current: %a@]"
       (List.pp ",@ " Exp.pp) (List.rev actuals) (List.pp ",@ " Var.pp)
       (List.rev formals) Var.Set.pp locals Var.Set.pp globals
       State_domain.pp current]
  ;
  let caller_current, state_from_call =
    State_domain.call ~summaries actuals formals locals globals current
  in
  ( (caller_current, caller_current)
  , {state_from_call; caller_entry= current} ))
  |>
  [%Trace.retn fun {pf} (reln, _) -> pf "@,%a" pp reln]

let post locals {caller_entry} (_, current) =
  [%Trace.call fun {pf} -> pf ""]
  ;
  (caller_entry, State_domain.post locals current)
  |>
  [%Trace.retn fun {pf} -> pf "@,%a" pp]

let retn formals {caller_entry; state_from_call} (_, current) =
  [%Trace.call fun {pf} -> pf "@,%a" State_domain.pp current]
  ;
  (caller_entry, State_domain.retn formals state_from_call current)
  |>
  [%Trace.retn fun {pf} -> pf "@,%a" pp]

let dnf (entry, current) =
  List.map ~f:(fun c -> (entry, c)) (State_domain.dnf current)

let resolve_callee f e (_, current) =
  State_domain.resolve_callee f e current
