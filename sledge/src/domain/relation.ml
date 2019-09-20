(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Relational abstract domain, elements of which are interpreted as Hoare
    triples over a base state domain *)

module type State_domain_sig = sig
  include Domain_sig.Dom

  val create_summary :
       locals:Var.Set.t
    -> formals:Var.Set.t
    -> entry:t
    -> current:t
    -> summary * t
end

module Make (State_domain : State_domain_sig) = struct
  type t = State_domain.t * State_domain.t [@@deriving sexp_of, equal]

  let embed b = (b, b)

  let pp fs (entry, curr) =
    Format.fprintf fs "@[<v 1> entry: %a@;current:%a@]" State_domain.pp
      entry State_domain.pp curr

  let report_fmt_thunk (_, curr) fs = State_domain.pp fs curr
  let init globals = embed (State_domain.init globals)

  let join (entry_a, current_a) (entry_b, current_b) =
    assert (State_domain.equal entry_b entry_a) ;
    (entry_a, State_domain.join current_a current_b)

  let is_false (_, curr) = State_domain.is_false curr

  let exec_assume (entry, current) cnd =
    match State_domain.exec_assume current cnd with
    | Some current -> Some (entry, current)
    | None -> None

  let exec_kill (entry, current) reg =
    (entry, State_domain.exec_kill current reg)

  let exec_move (entry, current) formal actual =
    (entry, State_domain.exec_move current formal actual)

  let exec_inst (entry, current) inst =
    match State_domain.exec_inst current inst with
    | Ok current -> Ok (entry, current)
    | Error e -> Error e

  let exec_intrinsic ~skip_throw (entry, current) areturn intrinsic actuals
      =
    match
      State_domain.exec_intrinsic ~skip_throw current areturn intrinsic
        actuals
    with
    | None -> None
    | Some (Ok current) -> Some (Ok (entry, current))
    | Some (Error e) -> Some (Error e)

  type from_call =
    {state_from_call: State_domain.from_call; caller_entry: State_domain.t}
  [@@deriving sexp_of]

  let recursion_beyond_bound = State_domain.recursion_beyond_bound

  let call ~summaries actuals areturn formals locals globals_vec
      (entry, current) =
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
      State_domain.call ~summaries actuals areturn formals locals
        globals_vec current
    in
    ( (caller_current, caller_current)
    , {state_from_call; caller_entry= entry} ))
    |>
    [%Trace.retn fun {pf} (reln, _) -> pf "@,%a" pp reln]

  let post locals {state_from_call; caller_entry} (_, current) =
    [%Trace.call fun {pf} -> pf "locals: %a" Var.Set.pp locals]
    ;
    (caller_entry, State_domain.post locals state_from_call current)
    |>
    [%Trace.retn fun {pf} -> pf "@,%a" pp]

  let retn formals freturn {caller_entry; state_from_call} (_, current) =
    [%Trace.call fun {pf} -> pf "@,%a" State_domain.pp current]
    ;
    (caller_entry, State_domain.retn formals freturn state_from_call current)
    |>
    [%Trace.retn fun {pf} -> pf "@,%a" pp]

  let dnf (entry, current) =
    List.map ~f:(fun c -> (entry, c)) (State_domain.dnf current)

  let resolve_callee f e (entry, current) =
    let callees, current = State_domain.resolve_callee f e current in
    (callees, (entry, current))

  type summary = State_domain.summary

  let pp_summary = State_domain.pp_summary

  let create_summary ~locals ~formals (entry, current) =
    let fs, current =
      State_domain.create_summary ~locals ~formals ~entry ~current
    in
    (fs, (entry, current))

  let apply_summary rel summ =
    let entry, current = rel in
    Option.map
      ~f:(fun c -> (entry, c))
      (State_domain.apply_summary current summ)
end
