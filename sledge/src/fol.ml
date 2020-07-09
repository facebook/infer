(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Var = struct
  include Ses.Term.Var

  (** Variable renaming substitutions *)
  module Subst = struct
    type nonrec t = t Map.t [@@deriving compare, equal, sexp_of]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    let t_of_sexp = Map.t_of_sexp t_of_sexp

    let invariant s =
      let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
      let domain, range =
        Map.fold s ~init:(Set.empty, Set.empty)
          ~f:(fun ~key ~data (domain, range) ->
            (* substs are injective *)
            assert (not (Set.mem range data)) ;
            (Set.add domain key, Set.add range data) )
      in
      assert (Set.disjoint domain range)

    let pp = Map.pp pp pp
    let empty = Map.empty
    let is_empty = Map.is_empty

    let freshen vs ~wrt =
      let dom = Set.inter wrt vs in
      ( if Set.is_empty dom then
        ({sub= empty; dom= Set.empty; rng= Set.empty}, wrt)
      else
        let wrt = Set.union wrt vs in
        let sub, rng, wrt =
          Set.fold dom ~init:(empty, Set.empty, wrt)
            ~f:(fun (sub, rng, wrt) x ->
              let x', wrt = fresh (name x) ~wrt in
              let sub = Map.add_exn sub ~key:x ~data:x' in
              let rng = Set.add rng x' in
              (sub, rng, wrt) )
        in
        ({sub; dom; rng}, wrt) )
      |> check (fun ({sub; _}, _) -> invariant sub)

    let fold sub ~init ~f =
      Map.fold sub ~init ~f:(fun ~key ~data s -> f key data s)

    let domain sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key ~data:_ domain ->
          Set.add domain key )

    let range sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key:_ ~data range ->
          Set.add range data )

    let invert sub =
      Map.fold sub ~init:empty ~f:(fun ~key ~data sub' ->
          Map.add_exn sub' ~key:data ~data:key )
      |> check invariant

    let restrict sub vs =
      Map.fold sub ~init:{sub; dom= Set.empty; rng= Set.empty}
        ~f:(fun ~key ~data z ->
          if Set.mem vs key then
            {z with dom= Set.add z.dom key; rng= Set.add z.rng data}
          else (
            assert (
              (* all substs are injective, so the current mapping is the
                 only one that can cause [data] to be in [rng] *)
              (not (Set.mem (range (Map.remove sub key)) data))
              || violates invariant sub ) ;
            {z with sub= Map.remove z.sub key} ) )
      |> check (fun {sub; dom; rng} ->
             assert (Set.equal dom (domain sub)) ;
             assert (Set.equal rng (range sub)) )

    let apply sub v = Map.find sub v |> Option.value ~default:v
  end
end

module Term = struct
  include (
    Ses.Term : module type of Ses.Term with module Var := Ses.Term.Var )

  let ite = conditional
  let rename s e = rename (Var.Subst.apply s) e
end

module Formula = struct
  include Term

  let inject b = b
  let project e = Some e
  let tt = true_
  let ff = false_
  let cond ~cnd ~pos ~neg = conditional ~cnd ~thn:pos ~els:neg
end

module Context = struct
  include Ses.Equality

  let and_formula = and_term
  let normalizef = normalize
  let rename x sub = rename x (Var.Subst.apply sub)

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

(*
 * Convert from Llair
 *)

module Term_of_Llair = struct
  let exp = Ses.Term.of_exp
end

module Formula_of_Llair = struct
  let exp = Term_of_Llair.exp
end

module Var_of_Llair = struct
  let reg = Ses.Var.of_reg
  let regs = Ses.Var.Set.of_regs
end
