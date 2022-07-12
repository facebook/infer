(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

open Fol

[@@@warning "+missing-record-field-pattern"]

let do_normalize = ref false

(** enable stronger unsat checking during normalization *)
let strong_unsat = false

module Sh = struct
  type seg =
    {loc: Term.t; bas: Term.t; len: Term.t; siz: Term.t; cnt: Term.t}
  [@@deriving compare, equal, sexp]

  module Seg = struct
    type t = seg [@@deriving compare, equal, sexp]
  end

  module Segs : sig
    type t [@@deriving compare, equal, sexp]

    val empty : t
    val of_ : seg -> t
    val remove : seg -> t -> t
    val union : t -> t -> t option
    val is_empty : t -> bool
    val map : t -> f:(seg -> seg) -> t option
    val filter : t -> f:(seg -> bool) -> t
    val fold : t -> 's -> f:(seg -> 's -> 's) -> 's
    val to_iter : t -> seg iter
    val to_list : t -> seg list
  end = struct
    include Set.Make (Seg)
    include Provide_of_sexp (Seg)

    let add x s =
      let s' = add x s in
      if s' == s then None else Some s'

    let union s t = if disjoint s t then Some (union s t) else None

    let map s ~f =
      let s, ys =
        fold s (s, []) ~f:(fun x (s, ys) ->
            let y = f x in
            if y == x then (s, ys) else (remove x s, y :: ys) )
      in
      List.fold ys (Some s) ~f:(fun y -> function
        | Some s -> add y s | None -> None )
  end

  module T0 = struct
    type compare [@@deriving compare, equal, sexp]

    type starjunction =
      { heap: Segs.t  (** star-conjunction of segment atomic formulas *)
      ; djns: disjunction list  (** star-conjunction of disjunctions *)
      ; pure: Formula.t  (** pure boolean constraints *)
      ; ctx: Context.t [@ignore]
            (** first-order logical context induced by rest of formula *) }

    and disjunction = (t, compare) Set.t

    and t = starjunction * Var.Context.t [@@deriving compare, equal, sexp]

    (** Accessors *)

    let qf (q, _) = q
    let vx (_, vx) = vx
    let us (_, vx) = Var.Context.voc vx
    let xs (_, vx) = Var.Context.xs vx
    let ctx (q, _) = q.ctx
    let pure (q, _) = q.pure
    let heap (q, _) = q.heap
    let djns (q, _) = q.djns
  end

  module T = struct
    include Comparer.Counterfeit (T0)
    include T0
  end

  module Set = struct
    include Set.Make_from_Comparer (T)
    include Provide_of_sexp (T)
  end

  include T

  (** Replay debugging *)

  type call = Freshen_xs of t * Var.Set.t | Simplify of t
  [@@deriving sexp]

  (** Basic values *)

  let emp =
    ( {ctx= Context.empty; pure= Formula.tt; heap= Segs.empty; djns= []}
    , Var.Context.empty )

  let false_ us =
    ( { ctx= Context.unsat
      ; pure= Formula.ff
      ; heap= Segs.empty
      ; djns= [Set.empty] }
    , Var.Context.of_vars us )

  (** Traversals *)

  let map_seg ~f h =
    let loc = f h.loc in
    let bas = f h.bas in
    let len = f h.len in
    let siz = f h.siz in
    let cnt = f h.cnt in
    if
      loc == h.loc
      && bas == h.bas
      && len == h.len
      && siz == h.siz
      && cnt == h.cnt
    then h
    else {loc; bas; len; siz; cnt}

  let fold_terms_seg {loc; bas; len; siz; cnt} s ~f =
    f loc (f bas (f len (f siz (f cnt s))))

  let fold_vars_seg seg s ~f =
    fold_terms_seg ~f:(Iter.fold ~f << Term.vars) seg s

  let fold_vars_stem ?ignore_ctx ?ignore_pure {ctx; pure; heap; djns= _} s
      ~f =
    let unless flag f s = if Option.is_some flag then s else f s in
    Segs.fold ~f:(fold_vars_seg ~f) heap s
    |> unless ignore_pure (Iter.fold ~f (Formula.vars pure))
    |> unless ignore_ctx (Iter.fold ~f (Context.vars ctx))

  let fold_vars ?ignore_ctx ?ignore_pure fold_vars q s ~f =
    fold_vars_stem ?ignore_ctx ?ignore_pure ~f q s
    |> List.fold ~f:(Set.fold ~f:fold_vars) q.djns

  (** Free variables *)

  let fv_seg seg = fold_vars_seg ~f:Var.Set.add seg Var.Set.empty

  let fv ?ignore_ctx ?ignore_pure xq =
    let rec fv_union ((q, _) as xq) s =
      Var.Set.diff
        (fold_vars ?ignore_ctx ?ignore_pure fv_union ~f:Var.Set.add q s)
        (T.xs xq)
    in
    fv_union xq Var.Set.empty

  (** Pretty-printing *)

  let rec var_strength_ xs m ((q, _) as xq) =
    let add v m =
      Var.Map.update v m ~f:(function
        | None -> Some `Anonymous
        | Some `Anonymous -> Some `Existential
        | o -> o )
    in
    let xs = Var.Set.union xs (T.xs xq) in
    let m_stem =
      fold_vars_stem ~ignore_ctx:() q m ~f:(fun var m ->
          if not (Var.Set.mem var xs) then
            Var.Map.add ~key:var ~data:`Universal m
          else add var m )
    in
    let m =
      List.fold q.djns m_stem ~f:(fun djn m ->
          let ms =
            Set.fold
              ~f:(fun dj ms -> snd (var_strength_ xs m dj) :: ms)
              djn []
          in
          List.reduce ms ~f:(fun m1 m2 ->
              Var.Map.union m1 m2 ~f:(fun _ s1 s2 ->
                  match (s1, s2) with
                  | `Anonymous, `Anonymous -> Some `Anonymous
                  | `Universal, _ | _, `Universal -> Some `Universal
                  | `Existential, _ | _, `Existential -> Some `Existential ) )
          |> Option.value ~default:m )
    in
    (m_stem, m)

  let var_strength ?(xs = Var.Set.empty) q =
    let m =
      Var.Set.fold xs Var.Map.empty ~f:(fun x ->
          Var.Map.add ~key:x ~data:`Existential )
    in
    var_strength_ xs m q

  let pp_chunk x fs (siz, cnt) =
    Format.fprintf fs "@[<2>@<1>⟨%a,%a@<1>⟩@]" (Term.ppx x) siz (Term.ppx x)
      cnt

  let pp_seg x fs {loc; bas; len; siz; cnt} =
    let term_pp = Term.ppx x in
    Format.fprintf fs "@[<2>%a@ @[@[-[%a)->@]@ %a@]@]" term_pp loc
      (fun fs (bas, len) ->
        if (not (Term.equal loc bas)) || not (Term.equal len siz) then
          Format.fprintf fs " %a, %a " term_pp bas term_pp len )
      (bas, len) (pp_chunk x) (siz, cnt)

  let pp_seg_norm ctx fs seg =
    let x _ = None in
    pp_seg x fs (map_seg seg ~f:(Context.normalize ctx))

  let pp_block x fs segs =
    let is_full_alloc segs =
      match segs with
      | {loc; bas; len; _} :: _ -> (
          Term.equal loc bas
          &&
          match Term.get_z len with
          | Some z -> (
            match
              List.fold segs (Some Z.zero) ~f:(fun seg len ->
                  match (len, Term.get_z seg.siz) with
                  | Some len, Some siz -> Some (Z.add len siz)
                  | _ -> None )
            with
            | Some blk_len -> Z.equal z blk_len
            | _ -> false )
          | _ -> false )
      | [] -> false
    in
    let term_pp = Term.ppx x in
    let pp_chunks =
      List.pp "@,^" (fun fs seg -> pp_chunk x fs (seg.siz, seg.cnt))
    in
    match segs with
    | {loc; bas; len; _} :: _ ->
        Format.fprintf fs "@[<2>%a@ @[@[-[%t)->@]@ @[%a@]@]@]" term_pp loc
          (fun fs ->
            if not (is_full_alloc segs) then
              Format.fprintf fs " %a, %a " term_pp bas term_pp len )
          pp_chunks segs
    | [] -> ()

  let pp_heap x ?pre ctx fs heap =
    let bas_off = Term.split_const in
    let cmp s1 s2 =
      [%compare: Term.t * (Term.t * Q.t)]
        ( Context.normalize ctx s1.bas
        , bas_off (Context.normalize ctx s1.loc) )
        ( Context.normalize ctx s2.bas
        , bas_off (Context.normalize ctx s2.loc) )
    in
    let eq s1 s2 =
      Term.equal s1.bas s2.bas
      && Term.equal s1.len s2.len
      && Context.implies ctx (Formula.eq (Term.add s1.loc s1.siz) s2.loc)
    in
    let heap = List.map ~f:(map_seg ~f:(Context.normalize ctx)) heap in
    let blocks = List.group_succ ~eq (List.sort ~cmp heap) in
    List.pp ?pre "@ * " (pp_block x) fs blocks

  let pp_us ?vs fs us =
    match vs with
    | None ->
        if not (Var.Set.is_empty us) then
          [%Dbg.fprintf fs "@<2>∀ @[%a@] .@ " Var.Set.pp us]
    | Some vs ->
        if not (Var.Set.equal vs us) then
          [%Dbg.fprintf fs "@<2>∀ @[%a@] .@ " Var.Set.pp_diff (vs, us)]

  let rec pp_ ?var_strength ?vs ancestor_xs parent_ctx fs
      (({ctx; pure; heap; djns}, _) as xq) =
    Format.pp_open_hvbox fs 0 ;
    let x v =
      Option.bind ~f:(fun (_, m) -> Var.Map.find v m) var_strength
    in
    pp_us ?vs fs (T.us xq) ;
    ( match djns with
    | [djn] when Set.is_empty djn && Option.is_some var_strength ->
        Format.fprintf fs "false"
    | _ ->
        let vs = Option.value vs ~default:Var.Set.empty in
        let xs_d_vs, xs_i_vs =
          Var.Set.diff_inter
            (Var.Set.filter (T.xs xq) ~f:(fun v ->
                 Poly.(x v <> Some `Anonymous) ) )
            vs
        in
        if not (Var.Set.is_empty xs_i_vs) then (
          Format.fprintf fs "@<3>∃↑ @[%a@] ." (Var.Set.ppx x) xs_i_vs ;
          if not (Var.Set.is_empty xs_d_vs) then Format.fprintf fs "@ " ) ;
        if not (Var.Set.is_empty xs_d_vs) then
          Format.fprintf fs "@<2>∃ @[%a@] .@ " (Var.Set.ppx x) xs_d_vs ;
        let first =
          if Option.is_some var_strength then
            Context.ppx_diff x fs parent_ctx pure ctx
          else (
            Format.fprintf fs "@[  %a@ @<2>∧ %a@]" Context.pp ctx Formula.pp
              pure ;
            false )
        in
        if Segs.is_empty heap then
          Format.fprintf fs
            ( if first then if List.is_empty djns then "  emp" else ""
            else "@ @<5>∧ emp" )
        else
          pp_heap x
            ~pre:(if first then "  " else "@ @<2>∧ ")
            (if Option.is_some var_strength then ctx else T.ctx emp)
            fs (Segs.to_list heap) ;
        let first = first && Segs.is_empty heap in
        List.pp
          ~pre:(if first then "  " else "@ * ")
          "@ * "
          (pp_djn ?var_strength
             (Var.Set.union vs (Var.Set.union (T.us xq) (T.xs xq)))
             (Var.Set.union ancestor_xs (T.xs xq))
             (if Option.is_some var_strength then ctx else Context.empty) )
          fs djns ) ;
    Format.pp_close_box fs ()

  and pp_djn ?var_strength vs xs ctx fs djn =
    if Set.is_empty djn then Format.fprintf fs "false"
    else
      Format.fprintf fs "@[<hv>( %a@ )@]"
        (List.pp "@ @<2>∨ " (fun fs sjn ->
             let var_strength =
               let+ var_strength_stem, _ = var_strength in
               var_strength_ xs var_strength_stem sjn
             in
             Format.fprintf fs "@[<hv 1>(%a)@]"
               (pp_ ?var_strength ~vs (Var.Set.union xs (T.xs sjn)) ctx)
               sjn ) )
        (Set.to_list djn)

  let pp_us fs us = pp_us fs us

  let pp_diff_eq ?us ?(xs = Var.Set.empty) ctx fs q =
    pp_ ~var_strength:(var_strength ~xs q) ?vs:us xs ctx fs q

  let pp fs q = pp_diff_eq Context.empty fs q

  let pp_djn fs d =
    pp_djn ?var_strength:None Var.Set.empty Var.Set.empty Context.empty fs d

  let pp_raw fs q =
    pp_ ?var_strength:None ?vs:None Var.Set.empty Context.empty fs q

  (** Invariants *)

  let rec invariant ((q, _) as xq) =
    let@ () = Invariant.invariant [%here] xq [%sexp_of: t] in
    let {ctx; pure; heap; djns} = q in
    try
      assert (
        Var.Set.disjoint (T.us xq) (T.xs xq)
        || fail "inter: @[%a@]@\nq: @[%a@]" Var.Set.pp
             (Var.Set.inter (T.us xq) (T.xs xq))
             pp xq () ) ;
      assert (
        Var.Set.subset (fv xq) ~of_:(T.us xq)
        || fail "unbound but free: %a" Var.Set.pp
             (Var.Set.diff (fv xq) (T.us xq))
             () ) ;
      Context.invariant ctx ;
      match djns with
      | [djn] when Set.is_empty djn ->
          assert (Context.is_unsat ctx) ;
          assert (Formula.equal Formula.ff pure) ;
          assert (Segs.is_empty heap)
      | _ ->
          assert (not (Context.is_unsat ctx)) ;
          assert (not (Formula.equal Formula.ff pure)) ;
          assert (not (List.exists djns ~f:Set.is_empty)) ;
          List.iter djns ~f:(fun djn ->
              assert (Set.cardinal djn > 1) ;
              Set.iter djn ~f:(fun sjn ->
                  assert (
                    Var.Set.subset (T.us sjn)
                      ~of_:(Var.Set.union (T.us xq) (T.xs xq)) ) ;
                  invariant sjn ) )
    with exc ->
      let bt = Printexc.get_raw_backtrace () in
      [%Dbg.info " %a" pp_raw xq] ;
      Printexc.raise_with_backtrace exc bt

  (** Query *)

  (** syntactically empty: empty heap and no pure constraints *)
  let is_emp (q, _) =
    Context.is_empty q.ctx
    && Formula.equal Formula.tt q.pure
    && Segs.is_empty q.heap
    && List.is_empty q.djns

  (** (incomplete syntactic) test that all satisfying heaps are empty *)
  let rec is_empty (q, _) =
    Segs.is_empty q.heap && List.for_all ~f:(Set.for_all ~f:is_empty) q.djns

  (** (incomplete syntactic) test for inconsistency *)
  let is_false (q, _) =
    match q.djns with [djn] -> Set.is_empty djn | _ -> false

  (** Quantification and Vocabulary *)

  let exists_fresh xs ((q, vx) as xq) =
    [%Dbg.call fun {pf} ->
      pf "@ {@[%a@]}@ %a" Var.Set.pp xs pp xq ;
      assert (
        Var.Set.disjoint xs (T.us xq)
        || fail "Sh.exists_fresh xs ∩ q.us: %a" Var.Set.pp
             (Var.Set.inter xs (T.us xq))
             () )]
    ;
    ( if Var.Set.is_empty xs then xq
    else
      (q, Var.Context.with_xs (Var.Set.union (T.xs xq) xs) vx)
      |> check invariant )
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp]

  let exists xs ((q, _) as xq) =
    [%Dbg.call fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp xs pp xq]
    ;
    assert (
      Var.Set.subset xs ~of_:(T.us xq)
      || fail "Sh.exists xs - q.us: %a" Var.Set.pp
           (Var.Set.diff xs (T.us xq))
           () ) ;
    ( if Var.Set.is_empty xs then xq
    else
      ( q
      , Var.Context.with_xs
          (Var.Set.union (T.xs xq) xs)
          (Var.Context.of_vars (Var.Set.diff (T.us xq) xs)) )
      |> check invariant )
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp]

  (** remove quantification on variables disjoint from vocabulary *)
  let elim_exists xs ((q, _) as xq) =
    assert (Var.Set.disjoint xs (T.us xq)) ;
    ( q
    , Var.Context.with_xs
        (Var.Set.diff (T.xs xq) xs)
        (Var.Context.of_vars (Var.Set.union (T.us xq) xs)) )

  (** freshen existentials, preserving vocabulary *)
  let rec freshen_xs xq ~wrt =
    try
      [%Dbg.call fun {pf} ->
        pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp xq ;
        assert (Var.Set.subset (T.us xq) ~of_:wrt)]
      ;
      let Var.Subst.{sub; dom; rng}, _ = Var.Subst.freshen (T.xs xq) ~wrt in
      ( if Var.Subst.is_empty sub then xq
      else
        let ((q', _) as xq') = apply_subst sub xq in
        let xs = Var.Set.union (Var.Set.diff (T.xs xq') dom) rng in
        if xq' == xq && xs == T.xs xq then xq
        else
          let us = Var.Set.diff (T.us xq') xs in
          (q', Var.Context.with_xs xs (Var.Context.of_vars us)) )
      |>
      [%Dbg.retn fun {pf} xq' ->
        pf "%a@ %a" Var.Subst.pp sub pp xq' ;
        assert (Var.Set.equal (T.us xq') (T.us xq)) ;
        assert (Var.Set.disjoint (T.xs xq') (Var.Subst.domain sub)) ;
        assert (Var.Set.disjoint (T.xs xq') (Var.Set.inter (T.xs xq) wrt)) ;
        invariant xq']
    with exc ->
      let bt = Printexc.get_raw_backtrace () in
      Format.eprintf "@\n%a@." Sexp.pp_hum
        (sexp_of_call (Freshen_xs (xq, wrt))) ;
      Printexc.raise_with_backtrace exc bt

  and extend_us us xq =
    let us = Var.Set.union us (T.us xq) in
    ( if us == T.us xq then xq
    else
      let ((q', _) as xq') = freshen_xs xq ~wrt:us in
      (q', Var.Context.with_xs (T.xs xq') (Var.Context.of_vars us)) )
    |> check invariant

  and freshen xq ~wrt =
    [%Dbg.call fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp xq]
    ;
    let xsub, _ =
      Var.Subst.freshen (T.us xq) ~wrt:(Var.Set.union wrt (T.xs xq))
    in
    let xq' = extend_us wrt (rename_ xsub xq) in
    (xq', xsub.sub)
    |>
    [%Dbg.retn fun {pf} (xq', _) ->
      pf "%a" pp xq' ;
      invariant xq' ;
      assert (Var.Set.subset wrt ~of_:(T.us xq')) ;
      assert (Var.Set.disjoint wrt (fv xq'))]

  and bind_exists xq ~wrt =
    [%Dbg.call fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp xq]
    ;
    let ((q', _) as xq') =
      if Var.Set.is_empty wrt then xq
      else freshen_xs xq ~wrt:(Var.Set.union (T.us xq) wrt)
    in
    ( T.xs xq'
    , (q', Var.Context.of_vars (Var.Set.union (T.us xq') (T.xs xq'))) )
    |>
    [%Dbg.retn fun {pf} (_, q') -> pf "%a" pp q']

  (** Transformation *)

  and star xq1 xq2 =
    [%dbg]
      ~call:(fun {pf} -> pf "@ (%a)@ (%a)" pp xq1 pp xq2)
      ~retn:(fun {pf} xq ->
        pf "%a" pp xq ;
        invariant xq ;
        assert (
          Var.Set.equal (T.us xq) (Var.Set.union (T.us xq1) (T.us xq2)) ) )
    @@ fun () ->
    if is_false xq1 || is_false xq2 then
      false_ (Var.Set.union (T.us xq1) (T.us xq2))
    else if is_emp xq1 then extend_us (T.us xq1) xq2
    else if is_emp xq2 then extend_us (T.us xq2) xq1
    else
      let us = Var.Set.union (T.us xq1) (T.us xq2) in
      let xq1 = freshen_xs xq1 ~wrt:(Var.Set.union us (T.xs xq2)) in
      let xq2 = freshen_xs xq2 ~wrt:(Var.Set.union us (T.xs xq1)) in
      let {ctx= c1; pure= p1; heap= h1; djns= d1}, _ = xq1 in
      let {ctx= c2; pure= p2; heap= h2; djns= d2}, _ = xq2 in
      assert (Var.Set.equal us (Var.Set.union (T.us xq1) (T.us xq2))) ;
      match Segs.union h1 h2 with
      | None -> false_ us
      | Some heap ->
          let ctx, vx =
            Var.Fresh.gen
              (Var.Context.of_vars
                 (Var.Set.union us (Var.Set.union (T.xs xq1) (T.xs xq2))) )
              (Context.union c1 c2)
          in
          let xs = Var.Context.xs vx in
          if Context.is_unsat ctx then false_ us
          else
            let pure = Formula.and_ p1 p2 in
            if Formula.equal Formula.ff pure then false_ us
            else
              exists_fresh xs
                ( {ctx; pure; heap; djns= List.append d1 d2}
                , Var.Context.with_xs
                    (Var.Set.union (T.xs xq1) (T.xs xq2))
                    (Var.Context.of_vars us) )

  and map ~f_sjn ~f_ctx ~f_trm ~f_fml ((q, vx) as xq) =
    let {ctx; pure; heap; djns} = q in
    let pure = f_fml pure in
    if Formula.equal Formula.ff pure then false_ (T.us xq)
    else
      let xs, ctx = f_ctx ctx in
      if Context.is_unsat ctx then false_ (T.us xq)
      else
        match Segs.map heap ~f:(map_seg ~f:f_trm) with
        | None -> false_ (T.us xq)
        | Some heap ->
            let djns, hoisted =
              List.partition_map_endo djns ~f:(fun djn ->
                  let djn' =
                    Set.filter_map djn ~f:(fun sjn ->
                        let sjn' = f_sjn sjn in
                        if is_false sjn' then None else Some sjn' )
                  in
                  match Set.classify djn' with
                  | One dj -> Right dj
                  | _ -> Left djn' )
            in
            if
              ctx == q.ctx
              && pure == q.pure
              && heap == q.heap
              && djns == q.djns
              && Var.Set.is_empty xs
            then xq
            else if List.exists ~f:Set.is_empty djns then false_ (T.us xq)
            else
              exists_fresh xs
                (List.fold ~f:star hoisted ({ctx; pure; heap; djns}, vx))

  (** primitive application of a substitution, ignores us and xs, may
      violate invariant *)
  and apply_subst sub q =
    [%dbg]
      ~call:(fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp q)
      ~retn:(fun {pf} -> pf "%a" pp)
    @@ fun () ->
    map q ~f_sjn:(rename sub)
      ~f_ctx:(fun r -> (Var.Set.empty, Context.rename r sub))
      ~f_trm:(Term.rename sub) ~f_fml:(Formula.rename sub)
    |> check (fun q' ->
           assert (Var.Set.disjoint (fv q') (Var.Subst.domain sub)) )

  and rename_ Var.Subst.{sub; dom; rng} xq =
    [%Dbg.call fun {pf} ->
      pf "@ @[%a@]@ %a" Var.Subst.pp sub pp xq ;
      assert (Var.Set.subset dom ~of_:(T.us xq))]
    ;
    let xq = extend_us rng xq in
    ( if Var.Subst.is_empty sub then xq
    else
      let ((q, _) as xq) = apply_subst sub xq in
      ( q
      , Var.Context.with_xs (T.xs xq)
          (Var.Context.of_vars (Var.Set.diff (T.us xq) dom)) ) )
    |>
    [%Dbg.retn fun {pf} xq' ->
      pf "%a" pp xq' ;
      invariant xq' ;
      assert (Var.Set.disjoint (T.us xq') (Var.Subst.domain sub))]

  and rename sub xq =
    [%Dbg.call fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp xq]
    ;
    rename_ (Var.Subst.restrict_dom sub (T.us xq)) xq
    |>
    [%Dbg.retn fun {pf} xq' ->
      pf "%a" pp xq' ;
      invariant xq' ;
      assert (Var.Set.disjoint (T.us xq') (Var.Subst.domain sub))]

  let starN = function
    | [] -> emp
    | [q] -> q
    | q :: qs -> List.fold ~f:star qs q

  let or_ xq1 xq2 =
    [%Dbg.call fun {pf} -> pf "@ (%a)@ (%a)" pp_raw xq1 pp_raw xq2]
    ;
    ( match (xq1, xq2) with
    | _ when is_false xq1 -> extend_us (T.us xq1) xq2
    | _ when is_false xq2 -> extend_us (T.us xq2) xq1
    | ({djns= []; _}, _), (({ctx= _; pure; heap; djns= [djn]} as d), _)
      when Var.Set.is_empty (T.xs xq2)
           && Formula.(equal tt pure)
           && Segs.is_empty heap ->
        ( {d with djns= [Set.add xq1 djn]}
        , Var.Context.of_vars (Var.Set.union (T.us xq1) (T.us xq2)) )
    | (({ctx= _; pure; heap; djns= [djn]} as d), _), ({djns= []; _}, _)
      when Var.Set.is_empty (T.xs xq1)
           && Formula.(equal tt pure)
           && Segs.is_empty heap ->
        ( {d with djns= [Set.add xq2 djn]}
        , Var.Context.of_vars (Var.Set.union (T.us xq1) (T.us xq2)) )
    | _ when equal xq1 xq2 -> xq1
    | _ ->
        ( { ctx= Context.empty
          ; pure= Formula.tt
          ; heap= Segs.empty
          ; djns= [Set.add xq1 (Set.of_ xq2)] }
        , Var.Context.of_vars (Var.Set.union (T.us xq1) (T.us xq2)) ) )
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp_raw xq ;
      invariant xq ;
      assert (Var.Set.equal (T.us xq) (Var.Set.union (T.us xq1) (T.us xq2)))]

  let orN djn =
    match Set.pop djn with
    | None -> false_ Var.Set.empty
    | Some (q, qs) -> Set.fold ~f:or_ qs q

  (** Construct *)

  (** conjoin an FOL context assuming vocabulary is compatible *)
  let and_ctx_ ctx ((q, vx) as xq) =
    assert (Var.Set.subset (Context.fv ctx) ~of_:(T.us xq)) ;
    let ctx, vx' =
      Var.Fresh.gen
        (Var.Context.of_vars (Var.Set.union (T.us xq) (T.xs xq)))
        (Context.union q.ctx ctx)
    in
    let xs = Var.Context.xs vx' in
    if Context.is_unsat ctx then false_ (T.us xq)
    else exists_fresh xs ({q with ctx}, vx)

  let and_ctx ctx xq =
    [%Dbg.call fun {pf} -> pf "@ %a@ %a" Context.pp ctx pp xq]
    ;
    ( if is_false xq then xq
    else and_ctx_ ctx (extend_us (Context.fv ctx) xq) )
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp xq ;
      invariant xq]

  let pure (p : Formula.t) =
    [%Dbg.call fun {pf} -> pf "@ %a" Formula.pp p]
    ;
    let us = Formula.fv p in
    Iter.fold
      (Var.Fresh.gen_ (Var.Context.of_vars us) (Context.dnf p))
      (false_ Var.Set.empty)
      ~f:(fun ((pure, ctx), vx) xq ->
        let xs = Var.Context.xs vx in
        if Context.is_unsat ctx || Formula.equal Formula.ff pure then
          extend_us us xq
        else
          or_ xq
            (exists_fresh xs
               ({(qf emp) with ctx; pure}, Var.Context.of_vars us) ) )
    |>
    [%Dbg.retn fun {pf} q ->
      pf "%a" pp q ;
      invariant q]

  let and_ b ((q, _) as xq) =
    [%dbg]
      ~call:(fun {pf} -> pf "@ (%a)@ (%a)" Formula.pp b pp xq)
      ~retn:(fun {pf} -> pf "%a" pp)
    @@ fun () ->
    let p = pure (Formula.map_terms ~f:(Context.normalize q.ctx) b) in
    if is_emp p then xq
    else
      let xs, xq = bind_exists xq ~wrt:(Formula.fv b) in
      exists xs (star p xq)

  let andN bs q = and_ (Formula.andN bs) q

  let and_subst subst q =
    [%Dbg.call fun {pf} -> pf "@ %a@ %a" Context.Subst.pp subst pp q]
    ;
    Context.Subst.fold_eqs ~f:and_ subst q
    |>
    [%Dbg.retn fun {pf} q ->
      pf "%a" pp q ;
      invariant q]

  let seg pt =
    let us = fv_seg pt in
    if Term.equal Term.zero pt.loc then false_ us
    else
      ({(qf emp) with heap= Segs.of_ pt}, Var.Context.of_vars us)
      |> check invariant

  (** Update *)

  let rem_seg seg (q, vx) =
    ({q with heap= Segs.remove seg q.heap}, vx) |> check invariant

  let filter_heap ~f (q, vx) =
    ({q with heap= Segs.filter q.heap ~f}, vx) |> check invariant

  (** Disjunctive-Normal Form *)

  let fold_dnf ~conj ~disj sjn (xs, conjuncts) disjuncts =
    let rec add_disjunct pending_splits sjn (xs, conjuncts) disjuncts =
      let ys, sjn = bind_exists sjn ~wrt:xs in
      let xs = Var.Set.union ys xs in
      let djns = T.djns sjn in
      let sjn = ({(qf sjn) with djns= []}, vx sjn) in
      split_case
        (Iter.append (Iter.of_list djns) pending_splits)
        (xs, conj sjn conjuncts)
        disjuncts
    and split_case pending_splits (xs, conjuncts) disjuncts =
      match Iter.pop pending_splits with
      | Some (split, pending_splits) ->
          Set.fold split disjuncts ~f:(fun sjn disjuncts ->
              add_disjunct pending_splits sjn (xs, conjuncts) disjuncts )
      | None -> disj (xs, conjuncts) disjuncts
    in
    add_disjunct Iter.empty sjn (xs, conjuncts) disjuncts

  let dnf q =
    [%Dbg.call fun {pf} -> pf "@ %a" pp q]
    ;
    let conj sjn conjuncts = sjn :: conjuncts in
    let disj (xs, conjuncts) disjuncts =
      Set.add (exists xs (starN conjuncts)) disjuncts
    in
    fold_dnf ~conj ~disj q (Var.Set.empty, []) Set.empty
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp_djn]

  let iter_dnf q f = Set.iter (dnf q) ~f

  (** Logical query *)

  (** rename existentially quantified variables to avoid shadowing, and
      reduce quantifier scopes by sinking them as low as possible into
      disjunctions *)
  let rec freshen_nested_xs us ((q, vx) as xq) =
    [%Dbg.call fun {pf} -> pf "@ %a" pp xq]
    ;
    (* trim xs to those that appear in stem or >1 disjunction and sink
       rest *)
    let xs_sink, _ =
      let fv_stem =
        fv ({q with djns= []}, Var.Context.with_xs Var.Set.empty vx)
      in
      let xs_sink, xs_djns =
        (Var.Set.diff (T.xs xq) fv_stem, Var.Set.empty)
      in
      List.fold q.djns (xs_sink, xs_djns) ~f:(fun djn (xs_sink, xs_djns) ->
          Set.fold djn (xs_sink, xs_djns) ~f:(fun djt (xs_sink, xs_djns) ->
              let fv_djt = fv djt in
              let dont_sink = Var.Set.inter xs_djns fv_djt in
              let xs_sink = Var.Set.diff xs_sink dont_sink in
              let xs_djns = Var.Set.union xs_djns fv_djt in
              (xs_sink, xs_djns) ) )
    in
    let xs = Var.Set.diff (T.xs xq) xs_sink in
    let us = Var.Set.union us (T.us xq) in
    let djns, hoisted, xs_below =
      List.fold_partition_map q.djns Var.Set.empty ~f:(fun djn xs_below ->
          let dj, xs_below =
            Set.fold_map djn xs_below ~f:(fun dj xs_below ->
                (* freshen xs that shadow ancestor us *)
                let us = Var.Set.union us (T.us dj) in
                let dj = freshen_xs dj ~wrt:us in
                (* quantify xs not in stem and freshen disjunct *)
                let dj =
                  freshen_nested_xs us
                    (exists (Var.Set.inter xs_sink (T.us dj)) dj)
                in
                let xs_below = Var.Set.union xs_below (T.xs dj) in
                (dj, xs_below) )
          in
          match Set.classify dj with
          | One q -> (Right q, xs_below)
          | _ -> (Left dj, xs_below) )
    in
    (* rename xs to miss all xs in subformulas *)
    freshen_xs
      (List.fold ~f:star hoisted ({q with djns}, Var.Context.with_xs xs vx))
      ~wrt:(Var.Set.union (T.us xq) xs_below)
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp q' ;
      invariant q']

  (** first-order approximation of heap constraints *)
  let rec pure_approx (q, _) =
    Formula.andN
      ( [ q.pure
        ; Formula.distinct
            (Array.of_list
               (Segs.fold q.heap [Term.zero] ~f:(fun seg locs ->
                    seg.loc :: locs ) ) ) ]
      |> List.fold q.djns ~f:(fun djn p ->
             Formula.orN
               (Set.fold ~f:(fun dj ps -> pure_approx dj :: ps) djn [])
             :: p ) )

  let pure_approx q =
    [%Dbg.call fun {pf} -> pf "@ %a" pp q]
    ;
    pure_approx (freshen_nested_xs Var.Set.empty q)
    |>
    [%Dbg.retn fun {pf} -> pf "%a" Formula.pp]

  (** enumerate a DNF-expansion of a symbolic heap's first-order constraints
      conjoined with a first-order approximation of the heap constraints
      until a branch that is not unsatisfiable is found *)
  let is_unsat_dnf xq =
    let exception NotUnsat in
    let conj sjn (wrt, ctx, fml) =
      let wrt = Var.Set.union wrt (T.xs sjn) in
      let ctx, vx =
        Var.Fresh.gen (Var.Context.of_vars wrt)
          (Context.union ctx (T.ctx sjn))
      in
      let zs = Var.Context.xs vx in
      let wrt = Var.Set.union wrt zs in
      let fml = Formula.and_ fml (T.pure sjn) in
      let fml =
        Segs.fold (T.heap sjn) fml ~f:(fun seg ->
            Formula.and_ (Formula.dq0 seg.loc) )
      in
      (wrt, ctx, fml)
    in
    let disj (_, (_, ctx, fml)) () =
      if not (Context.is_unsat ctx || Context.refutes ctx fml) then
        raise_notrace NotUnsat
    in
    try
      fold_dnf ~conj ~disj xq
        (Var.Set.empty, (T.us xq, T.ctx emp, T.pure emp))
        () ;
      true
    with NotUnsat -> false

  let is_unsat ((q, _) as xq) =
    if strong_unsat then is_unsat_dnf xq
    else Context.refutes q.ctx (pure_approx xq)

  (** Simplify *)

  let rec norm_ s xq =
    [%Dbg.call fun {pf} -> pf "@ @[%a@]@ %a" Context.Subst.pp s pp_raw xq]
    ;
    let f_sjn = norm_ s in
    let f_ctx x =
      let x', vx =
        Var.Fresh.gen
          (Var.Context.of_vars (Var.Set.union (T.us xq) (T.xs xq)))
          (Context.apply_subst s x)
      in
      (Var.Context.xs vx, x')
    in
    let f_trm = Context.Subst.subst s in
    let f_fml = Formula.map_terms ~f:(Context.Subst.subst s) in
    map xq ~f_sjn ~f_ctx ~f_trm ~f_fml
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp_raw q' ;
      invariant q']

  let norm s xq =
    [%Dbg.call fun {pf} ->
      pf "@ @[%a@]@ %a" Context.Subst.pp s pp_raw xq ;
      assert (
        let unbound = Var.Set.diff (Context.Subst.fv s) (T.us xq) in
        Var.Set.is_empty unbound
        || fail "unbound subst vars: %a" Var.Set.pp unbound () )]
    ;
    (if Context.Subst.is_empty s then xq else norm_ s xq)
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp_raw q' ;
      invariant q']

  let rec normalize_ ((q, _) as xq) =
    [%Dbg.call fun {pf} -> pf "@ %a" pp_raw xq]
    ;
    let q', vx =
      map xq ~f_sjn:normalize_
        ~f_ctx:(fun x -> (Var.Set.empty, x))
        ~f_trm:(Context.normalize q.ctx)
        ~f_fml:(Formula.map_terms ~f:(Context.normalize q.ctx))
    in
    let pure = Context.fold_eqs ~f:Formula.and_ q.ctx q'.pure in
    ({q' with pure}, vx)
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp_raw q' ;
      invariant q']

  let normalize q =
    [%Dbg.call fun {pf} -> pf "@ %a" pp_raw q]
    ;
    (if !do_normalize then normalize_ q else q)
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp_raw q' ;
      invariant q']

  let rec propagate_context_ ancestor_vs ancestor_ctx ((q, _) as xq) =
    [%Dbg.call fun {pf} -> pf "@ (%a)@ %a" Context.pp ancestor_ctx pp xq]
    ;
    (* extend vocabulary with variables in scope above *)
    let xq =
      ( q
      , Var.Context.with_xs (T.xs xq)
          (Var.Context.of_vars (Var.Set.union ancestor_vs (T.us xq))) )
    in
    (* strengthen context with that from above *)
    let xq = and_ctx_ ancestor_ctx xq in
    ( if is_false xq then false_ (T.us xq)
    else
      (* decompose formula *)
      let xs, ((q, vx) as xq) = bind_exists xq ~wrt:Var.Set.empty in
      let stem, djns = (({q with djns= T.djns emp}, vx), q.djns) in
      (* propagate over disjunctions *)
      let xq' =
        List.fold djns stem ~f:(fun djn xq' ->
            let djn, dj_ctxs =
              Set.fold_map djn [] ~f:(fun dj dj_ctxs ->
                  let dj = propagate_context_ (T.us xq) q.ctx dj in
                  (dj, (T.xs dj, T.ctx dj) :: dj_ctxs) )
            in
            let djn_ctx, vx =
              Var.Fresh.gen
                (Var.Context.of_vars (T.us xq))
                (Context.interN dj_ctxs)
            in
            let new_xs = Var.Context.xs vx in
            (* hoist xs appearing in disjunction's context *)
            let djn_xs = Var.Set.diff (Context.fv djn_ctx) (T.us xq') in
            let djn = Set.map ~f:(elim_exists djn_xs) djn in
            let ctx_djn = and_ctx_ djn_ctx (orN djn) in
            assert (is_false ctx_djn || Var.Set.subset new_xs ~of_:djn_xs) ;
            star (exists djn_xs ctx_djn) xq' )
      in
      (* requantify existentials *)
      let xq' = exists xs xq' in
      (* strengthening contexts can reveal inconsistency *)
      if is_false xq' then false_ (T.us xq') else xq' )
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp q' ;
      invariant q']

  let propagate_context ancestor_vs ancestor_ctx q =
    [%Dbg.call fun {pf} -> pf "@ (%a)@ %a" Context.pp ancestor_ctx pp q]
    ;
    propagate_context_ ancestor_vs ancestor_ctx q
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp q' ;
      invariant q']

  let pp_vss fs vss =
    Format.fprintf fs "[@[%a@]]"
      (List.pp ";@ " (fun fs vs ->
           Format.fprintf fs "{@[%a@]}" Var.Set.pp vs ) )
      vss

  let remove_absent_xs ks ((q, vx) as xq) =
    [%dbg]
      ~call:(fun {pf} -> pf "@ %a%a" Var.Set.pp_xs ks pp xq)
      ~retn:(fun {pf} -> pf "%a" pp)
    @@ fun () ->
    let ks = Var.Set.inter ks (T.xs xq) in
    if Var.Set.is_empty ks then xq
    else
      let xs = Var.Set.diff (T.xs xq) ks in
      let djns, hoisted =
        let rec trim_ks ks djns =
          List.partition_map_endo djns ~f:(fun djn ->
              let djn =
                Set.map djn ~f:(fun sjn ->
                    let us = Var.Set.diff (T.us sjn) ks in
                    let djns, hoisted = trim_ks ks (T.djns sjn) in
                    if us == T.us sjn && djns == T.djns sjn then sjn
                    else
                      List.fold ~f:star hoisted
                        ( {(qf sjn) with djns}
                        , Var.Context.with_xs (T.xs sjn)
                            (Var.Context.of_vars us) ) )
              in
              match Set.classify djn with
              | One dj -> Right dj
              | _ -> Left djn )
        in
        trim_ks ks q.djns
      in
      List.fold ~f:star hoisted ({q with djns}, Var.Context.with_xs xs vx)

  let rec simplify_ us ancestor_xs rev_xss survived ancestor_subst xq =
    [%Dbg.call fun {pf} ->
      pf "@ %a@ %a@ %a" pp_vss (List.rev rev_xss) Context.Subst.pp
        ancestor_subst pp_raw xq]
    ;
    assert (not (is_false xq)) ;
    let xq0 = xq in
    (* bind existentials *)
    let xs, xq = bind_exists ~wrt:Var.Set.empty xq in
    let rev_xss = xs :: rev_xss in
    (* try to solve equations in ctx for variables in xss *)
    let stem_subst =
      Context.solve_for_vars (us :: List.rev rev_xss) (T.ctx xq)
    in
    let subst = Context.Subst.compose ancestor_subst stem_subst in
    ( if Context.Subst.is_empty subst then normalize xq0
    else
      (* normalize context wrt solutions *)
      let union_xss = Var.Set.union xs ancestor_xs in
      let wrt = Var.Set.union us union_xss in
      let (ctx, removed), vx =
        Var.Fresh.gen (Var.Context.of_vars wrt)
          (Context.apply_and_elim union_xss subst (T.ctx xq))
      in
      let fresh = Var.Context.xs vx in
      let xq = extend_us (Var.Set.union fresh ancestor_xs) xq in
      let xq = ({(qf xq) with ctx}, T.vx xq) in
      (* normalize stem wrt its context *)
      let stem = normalize ({(qf xq) with djns= T.djns emp}, snd xq) in
      (* normalize stem wrt both ancestor and current solutions *)
      let stem =
        (* opt: ctx already normalized so just preserve it *)
        let stem' =
          norm subst ({(qf stem) with ctx= T.ctx emp}, snd stem)
        in
        ({(qf stem') with ctx= T.ctx xq}, snd stem')
      in
      if strong_unsat && is_unsat stem then false_ (T.us stem)
      else
        (* recursively simplify subformulas *)
        let survived =
          Var.Set.union survived (fv (elim_exists (T.xs stem) stem))
        in
        let djns, hoisted =
          List.rev_partition_map (T.djns xq) ~f:(fun djn ->
              let djn =
                Set.map
                  ~f:(simplify_ us union_xss rev_xss survived subst)
                  djn
              in
              match Set.classify djn with
              | One sjn -> Right sjn
              | _ -> Left (orN djn) )
        in
        let q = List.fold ~f:star hoisted (List.fold ~f:star djns stem) in
        if is_false q then false_ (T.us xq)
        else
          let removed = Var.Set.diff removed survived in
          let removed =
            if Var.Set.is_empty removed then Var.Set.empty
            else Var.Set.diff removed (fv (elim_exists (T.xs xq) q))
          in
          (* removed may not contain all variables stem_subst has solutions
             for, so the equations in [∃ removed. stem_subst] that are not
             universally valid need to be re-conjoined since they have
             alredy been normalized out *)
          let keep, removed, _ =
            Context.Subst.partition_valid removed stem_subst
          in
          let q = and_subst keep q in
          (* (re)quantify existentials *)
          let q = exists (Var.Set.union fresh xs) q in
          (* remove the eliminated variables from xs and subformulas' us *)
          remove_absent_xs removed q )
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a@ %a" Context.Subst.pp stem_subst pp_raw q' ;
      invariant q']

  let count_simplify = ref (-1)

  let simplify xq =
    [%Dbg.call fun {pf} -> pf " %i@ %a" !count_simplify pp_raw xq]
    ;
    ( if is_false xq then false_ (T.us xq)
    else
      let q = freshen_nested_xs Var.Set.empty xq in
      let q = propagate_context Var.Set.empty Context.empty q in
      if is_false q then false_ (T.us xq)
      else
        simplify_ (T.us xq) Var.Set.empty [] Var.Set.empty
          Context.Subst.empty q )
    |>
    [%Dbg.retn fun {pf} q' ->
      pf "%a" pp_raw q' ;
      invariant q']

  (*
   * Replay debugging
   *)

  let replay c =
    match call_of_sexp (Sexp.of_string c) with
    | Freshen_xs (q, wrt) -> freshen_xs q ~wrt |> ignore
    | Simplify q -> simplify q |> ignore

  let dump_simplify = ref (-1)

  let simplify q =
    Int.incr count_simplify ;
    if !count_simplify = !dump_simplify then
      fail "%a" Sexp.pp_hum (sexp_of_call (Simplify q)) ()
    else
      try simplify q
      with exc ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf "@\n%a@." Sexp.pp_hum (sexp_of_call (Simplify q)) ;
        Printexc.raise_with_backtrace exc bt
end
