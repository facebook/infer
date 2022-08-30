(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Relational abstract domain, elements of which are interpreted as Hoare
    triples over a base state domain *)

open Domain_intf

module type State_domain_sig = sig
  include Domain

  val create_summary :
       ThreadID.t
    -> locals:Llair.Reg.Set.t
    -> formals:Llair.Reg.t iarray
    -> entry:t
    -> current:t
    -> summary * t
end

module Make (State_domain : State_domain_sig) = struct
  module T = struct
    type t = State_domain.t * State_domain.t
    [@@deriving compare, equal, sexp_of]
  end

  include T
  module Set = Set.Make (T)

  let embed b = (b, b)

  let pp_entry fs entry =
    [%Dbg.fprintf fs "entry: %a@ current: " State_domain.pp entry]

  let pp fs (entry, curr) =
    Format.fprintf fs "@[%a%a@]" pp_entry entry State_domain.pp curr

  let init globals = embed (State_domain.init globals)

  let join (entry_a, current_a) (entry_b, current_b) =
    ( State_domain.join entry_a entry_b
    , State_domain.join current_a current_b )

  let joinN rs =
    let entrys, currents =
      Set.fold rs (State_domain.Set.empty, State_domain.Set.empty)
        ~f:(fun (entry, current) (entrys, currents) ->
          ( State_domain.Set.add entry entrys
          , State_domain.Set.add current currents ) )
    in
    (State_domain.joinN entrys, State_domain.joinN currents)

  let is_unsat (_, current) = State_domain.is_unsat current
  let resolve_int tid (_, current) = State_domain.resolve_int tid current

  let exec_assume tid (entry, current) cnd =
    let+ next = State_domain.exec_assume tid current cnd in
    (entry, next)

  let exec_kill tid reg (entry, current) =
    (entry, State_domain.exec_kill tid reg current)

  let exec_move tid reg_exps (entry, current) =
    (entry, State_domain.exec_move tid reg_exps current)

  let exec_inst tid inst (entry, current) =
    let open Or_alarm.Import in
    let+ next = State_domain.exec_inst tid inst current in
    (entry, next)

  type from_call =
    {state_from_call: State_domain.from_call; caller_entry: State_domain.t}
  [@@deriving sexp_of]

  let recursion_beyond_bound = State_domain.recursion_beyond_bound

  let call ~summaries tid ?child ~globals ~actuals ~areturn ~formals
      ~freturn ~locals (entry, current) =
    [%Dbg.call fun {pf} ->
      pf
        "@ @[<v>@[actuals: (@[%a@])@ formals: (@[%a@])@]@ locals: \
         {@[%a@]}@ globals: {@[%a@]}@ current: %a@]"
        (IArray.pp ",@ " Llair.Exp.pp)
        actuals
        (IArray.pp ",@ " Llair.Reg.pp)
        formals Llair.Reg.Set.pp locals Llair.Global.Set.pp globals
        State_domain.pp current]
    ;
    let caller_current, state_from_call =
      State_domain.call tid ?child ~summaries ~globals ~actuals ~areturn
        ~formals ~freturn ~locals current
    in
    ( (caller_current, caller_current)
    , {state_from_call; caller_entry= entry} )
    |>
    [%Dbg.retn fun {pf} (reln, _) -> pf "@,%a" pp reln]

  let post tid locals {state_from_call; caller_entry} (_, current) =
    [%Dbg.call fun {pf} -> pf "@ locals: %a" Llair.Reg.Set.pp locals]
    ;
    (caller_entry, State_domain.post tid locals state_from_call current)
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp]

  let retn tid formals freturn {caller_entry; state_from_call} (_, current)
      =
    [%Dbg.call fun {pf} -> pf "@ %a" State_domain.pp current]
    ;
    ( caller_entry
    , State_domain.retn tid formals freturn state_from_call current )
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp]

  type term_code = State_domain.term_code [@@deriving compare, sexp_of]

  let term tid formals freturn (_, current) =
    State_domain.term tid formals freturn current

  let move_term_code tid reg code (entry, current) =
    (entry, State_domain.move_term_code tid reg code current)

  let dnf (entry, current) =
    Iter.map ~f:(fun c -> (entry, c)) (State_domain.dnf current)

  let resolve_callee f tid e (_, current) =
    State_domain.resolve_callee f tid e current

  type summary = State_domain.summary

  let pp_summary = State_domain.pp_summary

  let create_summary tid ~locals ~formals (entry, current) =
    let fs, next =
      State_domain.create_summary tid ~locals ~formals ~entry ~current
    in
    (fs, (entry, next))

  let apply_summary (entry, current) summ =
    let+ next = State_domain.apply_summary current summ in
    (entry, next)
end
[@@inlined]
