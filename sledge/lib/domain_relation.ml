(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Relational abstract domain, elements of which are interpreted as Hoare
    triples over a base state domain *)

module type State_domain_sig = sig
  include Domain_intf.Dom

  val create_summary :
       locals:Reg.Set.t
    -> formals:Reg.Set.t
    -> entry:t
    -> current:t
    -> summary * t
end

module Make (State_domain : State_domain_sig) = struct
  type t = State_domain.t * State_domain.t [@@deriving sexp_of, equal]

  let embed b = (b, b)

  let pp_entry fs entry =
    [%Trace.fprintf fs "entry: %a@ current: " State_domain.pp entry]

  let pp fs (entry, curr) =
    Format.fprintf fs "@[%a%a@]" pp_entry entry State_domain.pp curr

  let report_fmt_thunk (_, curr) fs = State_domain.pp fs curr
  let init globals = embed (State_domain.init globals)

  let join (entry_a, current_a) (entry_b, current_b) =
    if State_domain.equal entry_a entry_b then
      let+ next = State_domain.join current_a current_b in
      (entry_a, next)
    else None

  let is_false (_, curr) = State_domain.is_false curr

  let exec_assume (entry, current) cnd =
    let+ next = State_domain.exec_assume current cnd in
    (entry, next)

  let exec_kill (entry, current) reg =
    (entry, State_domain.exec_kill current reg)

  let exec_move (entry, current) reg_exps =
    (entry, State_domain.exec_move current reg_exps)

  let exec_inst (entry, current) inst =
    let+ next = State_domain.exec_inst current inst in
    (entry, next)

  let exec_intrinsic ~skip_throw (entry, current) areturn intrinsic actuals
      =
    let+ next_opt =
      State_domain.exec_intrinsic ~skip_throw current areturn intrinsic
        actuals
    in
    let+ next = next_opt in
    (entry, next)

  type from_call =
    {state_from_call: State_domain.from_call; caller_entry: State_domain.t}
  [@@deriving sexp_of]

  let recursion_beyond_bound = State_domain.recursion_beyond_bound

  let call ~summaries ~globals ~actuals ~areturn ~formals ~freturn ~locals
      (entry, current) =
    [%Trace.call fun {pf} ->
      pf
        "@[<v>@[actuals: (@[%a@])@ formals: (@[%a@])@]@ locals: {@[%a@]}@ \
         globals: {@[%a@]}@ current: %a@]"
        (List.pp ",@ " Exp.pp) (List.rev actuals) (List.pp ",@ " Reg.pp)
        (List.rev formals) Reg.Set.pp locals Reg.Set.pp globals
        State_domain.pp current]
    ;
    let caller_current, state_from_call =
      State_domain.call ~summaries ~globals ~actuals ~areturn ~formals
        ~freturn ~locals current
    in
    ( (caller_current, caller_current)
    , {state_from_call; caller_entry= entry} )
    |>
    [%Trace.retn fun {pf} (reln, _) -> pf "@,%a" pp reln]

  let post locals {state_from_call; caller_entry} (_, current) =
    [%Trace.call fun {pf} -> pf "locals: %a" Reg.Set.pp locals]
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
    let callees, next = State_domain.resolve_callee f e current in
    (callees, (entry, next))

  type summary = State_domain.summary

  let pp_summary = State_domain.pp_summary

  let create_summary ~locals ~formals (entry, current) =
    let fs, next =
      State_domain.create_summary ~locals ~formals ~entry ~current
    in
    (fs, (entry, next))

  let apply_summary (entry, current) summ =
    let+ next = State_domain.apply_summary current summ in
    (entry, next)
end
