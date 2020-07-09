(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Var = Ses.Term.Var

module Term = struct
  include Ses.Term

  let ite = conditional
end

module Formula = struct
  include Ses.Term

  let inject b = b
  let project e = Some e

  let of_exp e =
    let b = Term.of_exp e in
    match project b with Some p -> p | None -> dq Term.zero b
end

module Context = struct
  include Ses.Equality

  let and_formula = and_term
  let normalizef = normalize

  module Subst = struct
    include Subst

    let substf = subst
  end

  (* Replay debugging *)

  type call =
    | Normalize of t * Term.t
    | And_formula of Var.Set.t * Formula.t * t
    | And_ of Var.Set.t * t * t
    | OrN of Var.Set.t * t list
    | Rename of t * Var.Subst.t
    | Apply_subst of Var.Set.t * Subst.t * t
    | Solve_for_vars of Var.Set.t list * t
  [@@deriving sexp]

  let replay c =
    match call_of_sexp (Sexp.of_string c) with
    | Normalize (r, e) -> normalize r e |> ignore
    | And_formula (us, e, r) -> and_formula us e r |> ignore
    | And_ (us, r, s) -> and_ us r s |> ignore
    | OrN (us, rs) -> orN us rs |> ignore
    | Rename (r, s) -> rename r s |> ignore
    | Apply_subst (us, s, r) -> apply_subst us s r |> ignore
    | Solve_for_vars (vss, r) -> solve_for_vars vss r |> ignore

  (* Debug wrappers *)

  let report ~name ~elapsed ~aggregate ~count =
    Format.eprintf "%15s time: %12.3f ms  %12.3f ms  %12d calls@." name
      elapsed aggregate count

  let dump_threshold = ref 1000.

  let wrap tmr f call =
    let f () =
      Timer.start tmr ;
      let r = f () in
      Timer.stop_report tmr (fun ~name ~elapsed ~aggregate ~count ->
          report ~name ~elapsed ~aggregate ~count ;
          if Float.(elapsed > !dump_threshold) then (
            dump_threshold := 2. *. !dump_threshold ;
            Format.eprintf "@\n%a@\n@." Sexp.pp_hum (sexp_of_call (call ()))
            ) ) ;
      r
    in
    if not [%debug] then f ()
    else
      try f () with exn -> raise_s ([%sexp_of: exn * call] (exn, call ()))

  let normalize_tmr = Timer.create "normalize" ~at_exit:report
  let and_formula_tmr = Timer.create "and_formula" ~at_exit:report
  let and_tmr = Timer.create "and_" ~at_exit:report
  let orN_tmr = Timer.create "orN" ~at_exit:report
  let rename_tmr = Timer.create "rename" ~at_exit:report
  let apply_subst_tmr = Timer.create "apply_subst" ~at_exit:report
  let solve_for_vars_tmr = Timer.create "solve_for_vars" ~at_exit:report

  let normalize r e =
    wrap normalize_tmr (fun () -> normalize r e) (fun () -> Normalize (r, e))

  let and_formula us e r =
    wrap and_formula_tmr
      (fun () -> and_formula us e r)
      (fun () -> And_formula (us, e, r))

  let and_ us r s =
    wrap and_tmr (fun () -> and_ us r s) (fun () -> And_ (us, r, s))

  let orN us rs = wrap orN_tmr (fun () -> orN us rs) (fun () -> OrN (us, rs))

  let rename r s =
    wrap rename_tmr (fun () -> rename r s) (fun () -> Rename (r, s))

  let apply_subst us s r =
    wrap apply_subst_tmr
      (fun () -> apply_subst us s r)
      (fun () -> Apply_subst (us, s, r))

  let solve_for_vars vss r =
    wrap solve_for_vars_tmr
      (fun () -> solve_for_vars vss r)
      (fun () -> Solve_for_vars (vss, r))
end
