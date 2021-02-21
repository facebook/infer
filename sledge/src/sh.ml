(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

open Fol

(** enable stronger unsat checking during normalization *)
let strong_unsat = false

[@@@warning "+9"]

type seg = {loc: Term.t; bas: Term.t; len: Term.t; siz: Term.t; cnt: Term.t}
[@@deriving compare, equal, sexp]

type starjunction =
  { us: Var.Set.t
  ; xs: Var.Set.t
  ; ctx: Context.t [@ignore]
  ; pure: Formula.t
  ; heap: seg list
  ; djns: disjunction list }
[@@deriving compare, equal, sexp]

and disjunction = starjunction list

type t = starjunction [@@deriving compare, equal, sexp]

(** Basic values *)

let emp =
  { us= Var.Set.empty
  ; xs= Var.Set.empty
  ; ctx= Context.empty
  ; pure= Formula.tt
  ; heap= []
  ; djns= [] }

let false_ us =
  {emp with us; ctx= Context.unsat; pure= Formula.ff; djns= [[]]}

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

let fold_vars_stem ?ignore_ctx ?ignore_pure
    {us= _; xs= _; ctx; pure; heap; djns= _} s ~f =
  let unless flag f s = if Option.is_some flag then s else f s in
  List.fold ~f:(fold_vars_seg ~f) heap s
  |> unless ignore_pure (Iter.fold ~f (Formula.vars pure))
  |> unless ignore_ctx (Iter.fold ~f (Context.vars ctx))

let fold_vars ?ignore_ctx ?ignore_pure fold_vars q s ~f =
  fold_vars_stem ?ignore_ctx ?ignore_pure ~f q s
  |> List.fold ~f:(List.fold ~f:fold_vars) q.djns

(** Pretty-printing *)

let rec var_strength_ xs m q =
  let add v m =
    Var.Map.update v m ~f:(function
      | None -> Some `Anonymous
      | Some `Anonymous -> Some `Existential
      | o -> o )
  in
  let xs = Var.Set.union xs q.xs in
  let m_stem =
    fold_vars_stem ~ignore_ctx:() q m ~f:(fun var m ->
        if not (Var.Set.mem var xs) then
          Var.Map.add ~key:var ~data:`Universal m
        else add var m )
  in
  let m =
    List.fold q.djns m_stem ~f:(fun djn m ->
        let ms = List.map ~f:(fun dj -> snd (var_strength_ xs m dj)) djn in
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

let pp_chunk x fs (siz, cnt) = Term.ppx x fs (Term.sized ~siz ~seq:cnt)

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
      (Context.normalize ctx s1.bas, bas_off (Context.normalize ctx s1.loc))
      (Context.normalize ctx s2.bas, bas_off (Context.normalize ctx s2.loc))
  in
  let eq s1 s2 =
    Term.equal s1.bas s2.bas
    && Term.equal s1.len s2.len
    && Context.implies ctx (Formula.eq (Term.add s1.loc s1.siz) s2.loc)
  in
  let heap = List.map heap ~f:(map_seg ~f:(Context.normalize ctx)) in
  let blocks = List.group_succ ~eq (List.sort ~cmp heap) in
  List.pp ?pre "@ * " (pp_block x) fs blocks

let pp_us ?vs fs us =
  match vs with
  | None ->
      if not (Var.Set.is_empty us) then
        [%Trace.fprintf fs "@<2>∀ @[%a@] .@ " Var.Set.pp us]
  | Some vs ->
      if not (Var.Set.equal vs us) then
        [%Trace.fprintf fs "@<2>∀ @[%a@] .@ " Var.Set.pp_diff (vs, us)]

let rec pp_ ?var_strength ?vs ancestor_xs parent_ctx fs
    {us; xs; ctx; pure; heap; djns} =
  Format.pp_open_hvbox fs 0 ;
  let x v = Option.bind ~f:(fun (_, m) -> Var.Map.find v m) var_strength in
  pp_us ?vs fs us ;
  ( match djns with
  | [[]] when Option.is_some var_strength -> Format.fprintf fs "false"
  | _ ->
      let vs = Option.value vs ~default:Var.Set.empty in
      let xs_d_vs, xs_i_vs =
        Var.Set.diff_inter
          (Var.Set.filter xs ~f:(fun v -> Poly.(x v <> Some `Anonymous)))
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
      if List.is_empty heap then
        Format.fprintf fs
          ( if first then if List.is_empty djns then "  emp" else ""
          else "@ @<5>∧ emp" )
      else
        pp_heap x
          ~pre:(if first then "  " else "@ @<2>∧ ")
          (if Option.is_some var_strength then ctx else emp.ctx)
          fs heap ;
      let first = first && List.is_empty heap in
      List.pp
        ~pre:(if first then "  " else "@ * ")
        "@ * "
        (pp_djn ?var_strength
           (Var.Set.union vs (Var.Set.union us xs))
           (Var.Set.union ancestor_xs xs)
           ctx)
        fs djns ) ;
  Format.pp_close_box fs ()

and pp_djn ?var_strength vs xs ctx fs = function
  | [] -> Format.fprintf fs "false"
  | djn ->
      Format.fprintf fs "@[<hv>( %a@ )@]"
        (List.pp "@ @<2>∨ " (fun fs sjn ->
             let var_strength =
               let+ var_strength_stem, _ = var_strength in
               var_strength_ xs var_strength_stem sjn
             in
             Format.fprintf fs "@[<hv 1>(%a)@]"
               (pp_ ?var_strength ~vs (Var.Set.union xs sjn.xs) ctx)
               sjn ))
        djn

let pp_us fs us = pp_us fs us

let pp_diff_eq ?us ?(xs = Var.Set.empty) ctx fs q =
  pp_ ~var_strength:(var_strength ~xs q) ?vs:us xs ctx fs q

let pp fs q = pp_diff_eq Context.empty fs q

let pp_djn fs d =
  pp_djn ?var_strength:None Var.Set.empty Var.Set.empty Context.empty fs d

let pp_raw fs q =
  pp_ ?var_strength:None ?vs:None Var.Set.empty Context.empty fs q

let fv_seg seg = fold_vars_seg ~f:Var.Set.add seg Var.Set.empty

let fv ?ignore_ctx ?ignore_pure q =
  let rec fv_union q s =
    Var.Set.diff
      (fold_vars ?ignore_ctx ?ignore_pure fv_union ~f:Var.Set.add q s)
      q.xs
  in
  fv_union q Var.Set.empty

let rec invariant q =
  let@ () = Invariant.invariant [%here] q [%sexp_of: t] in
  let {us; xs; ctx; pure; heap; djns} = q in
  try
    assert (
      Var.Set.disjoint us xs
      || fail "inter: @[%a@]@\nq: @[%a@]" Var.Set.pp (Var.Set.inter us xs)
           pp q () ) ;
    assert (
      Var.Set.subset (fv q) ~of_:us
      || fail "unbound but free: %a" Var.Set.pp (Var.Set.diff (fv q) us) ()
    ) ;
    Context.invariant ctx ;
    ( match djns with
    | [[]] ->
        assert (Context.is_unsat ctx) ;
        assert (Formula.equal Formula.ff pure) ;
        assert (List.is_empty heap)
    | _ ->
        assert (not (Context.is_unsat ctx)) ;
        assert (not (Formula.equal Formula.ff pure)) ;
        assert (not (List.exists djns ~f:List.is_empty)) ) ;
    List.iter djns ~f:(fun djn ->
        List.iter djn ~f:(fun sjn ->
            assert (Var.Set.subset sjn.us ~of_:(Var.Set.union us xs)) ;
            invariant sjn ) )
  with exc ->
    let bt = Printexc.get_raw_backtrace () in
    [%Trace.info "%a" pp_raw q] ;
    Printexc.raise_with_backtrace exc bt

(** Query *)

(** syntactically empty: empty heap and no pure constraints *)
let is_emp q =
  Context.is_empty q.ctx
  && Formula.equal Formula.tt q.pure
  && List.is_empty q.heap
  && List.is_empty q.djns

(** (incomplete syntactic) test that all satisfying heaps are empty *)
let rec is_empty q =
  List.is_empty q.heap && List.for_all ~f:(List.for_all ~f:is_empty) q.djns

(** syntactically inconsistent *)
let is_false q = match q.djns with [[]] -> true | _ -> false

(** Quantification and Vocabulary *)

let exists_fresh xs q =
  [%Trace.call fun {pf} ->
    pf "@ {@[%a@]}@ %a" Var.Set.pp xs pp q ;
    assert (
      Var.Set.disjoint xs q.us
      || fail "Sh.exists_fresh xs ∩ q.us: %a" Var.Set.pp
           (Var.Set.inter xs q.us) () )]
  ;
  ( if Var.Set.is_empty xs then q
  else {q with xs= Var.Set.union q.xs xs} |> check invariant )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let exists xs q =
  [%Trace.call fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp xs pp q]
  ;
  assert (
    Var.Set.subset xs ~of_:q.us
    || fail "Sh.exists xs - q.us: %a" Var.Set.pp (Var.Set.diff xs q.us) ()
  ) ;
  ( if Var.Set.is_empty xs then q
  else
    {q with us= Var.Set.diff q.us xs; xs= Var.Set.union q.xs xs}
    |> check invariant )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

(** remove quantification on variables disjoint from vocabulary *)
let elim_exists xs q =
  assert (Var.Set.disjoint xs q.us) ;
  {q with us= Var.Set.union q.us xs; xs= Var.Set.diff q.xs xs}

let map ~f_sjn ~f_ctx ~f_trm ~f_fml ({us; xs= _; ctx; pure; heap; djns} as q)
    =
  let pure = f_fml pure in
  if Formula.equal Formula.ff pure then false_ us
  else
    let xs, ctx = f_ctx ctx in
    if Context.is_unsat ctx then false_ us
    else
      let djns = List.map_endo djns ~f:(List.map_endo ~f:f_sjn) in
      if List.exists ~f:List.is_empty djns then false_ us
      else
        let heap = List.map_endo heap ~f:(map_seg ~f:f_trm) in
        if
          ctx == q.ctx
          && pure == q.pure
          && heap == q.heap
          && djns == q.djns
          && Var.Set.is_empty xs
        then q
        else exists_fresh xs {q with ctx; pure; heap; djns}

(** primitive application of a substitution, ignores us and xs, may violate
    invariant *)
let rec apply_subst sub q =
  map q ~f_sjn:(rename sub)
    ~f_ctx:(fun r -> (Var.Set.empty, Context.rename r sub))
    ~f_trm:(Term.rename sub) ~f_fml:(Formula.rename sub)
  |> check (fun q' ->
         assert (Var.Set.disjoint (fv q') (Var.Subst.domain sub)) )

and rename_ Var.Subst.{sub; dom; rng} q =
  [%Trace.call fun {pf} ->
    pf "@ @[%a@]@ %a" Var.Subst.pp sub pp q ;
    assert (Var.Set.subset dom ~of_:q.us)]
  ;
  let q = extend_us rng q in
  ( if Var.Subst.is_empty sub then q
  else {(apply_subst sub q) with us= Var.Set.diff q.us dom} )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Var.Set.disjoint q'.us (Var.Subst.domain sub))]

and rename sub q =
  [%Trace.call fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp q]
  ;
  rename_ (Var.Subst.restrict_dom sub q.us) q
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Var.Set.disjoint q'.us (Var.Subst.domain sub))]

(** freshen existentials, preserving vocabulary *)
and freshen_xs q ~wrt =
  [%Trace.call fun {pf} ->
    pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp q ;
    assert (Var.Set.subset q.us ~of_:wrt)]
  ;
  let Var.Subst.{sub; dom; rng}, _ = Var.Subst.freshen q.xs ~wrt in
  ( if Var.Subst.is_empty sub then q
  else
    let xs = Var.Set.union (Var.Set.diff q.xs dom) rng in
    let q' = apply_subst sub q in
    if xs == q.xs && q' == q then q else {q' with xs} )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a@ %a" Var.Subst.pp sub pp q' ;
    assert (Var.Set.equal q'.us q.us) ;
    assert (Var.Set.disjoint q'.xs (Var.Subst.domain sub)) ;
    assert (Var.Set.disjoint q'.xs (Var.Set.inter q.xs wrt)) ;
    invariant q']

and extend_us us q =
  let us = Var.Set.union us q.us in
  (if us == q.us then q else {(freshen_xs q ~wrt:us) with us})
  |> check invariant

let freshen q ~wrt =
  [%Trace.call fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp q]
  ;
  let xsub, _ = Var.Subst.freshen q.us ~wrt:(Var.Set.union wrt q.xs) in
  let q' = extend_us wrt (rename_ xsub q) in
  (q', xsub.sub)
  |>
  [%Trace.retn fun {pf} (q', _) ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Var.Set.subset wrt ~of_:q'.us) ;
    assert (Var.Set.disjoint wrt (fv q'))]

let bind_exists q ~wrt =
  [%Trace.call fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp q]
  ;
  let q' =
    if Var.Set.is_empty wrt then q
    else freshen_xs q ~wrt:(Var.Set.union q.us wrt)
  in
  (q'.xs, {q' with us= Var.Set.union q'.us q'.xs; xs= Var.Set.empty})
  |>
  [%Trace.retn fun {pf} (_, q') -> pf "%a" pp q']

(** Construct *)

(** conjoin an FOL context assuming vocabulary is compatible *)
let and_ctx_ ctx q =
  assert (Var.Set.subset (Context.fv ctx) ~of_:q.us) ;
  let xs, ctx = Context.union (Var.Set.union q.us q.xs) q.ctx ctx in
  if Context.is_unsat ctx then false_ q.us else exists_fresh xs {q with ctx}

let and_ctx ctx q =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Context.pp ctx pp q]
  ;
  (if is_false q then q else and_ctx_ ctx (extend_us (Context.fv ctx) q))
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp q ;
    invariant q]

let star q1 q2 =
  [%trace]
    ~call:(fun {pf} -> pf "@ (%a)@ (%a)" pp q1 pp q2)
    ~retn:(fun {pf} q ->
      pf "%a" pp q ;
      invariant q ;
      assert (Var.Set.equal q.us (Var.Set.union q1.us q2.us)) )
  @@ fun () ->
  if is_false q1 || is_false q2 then false_ (Var.Set.union q1.us q2.us)
  else if is_emp q1 then extend_us q1.us q2
  else if is_emp q2 then extend_us q2.us q1
  else
    let us = Var.Set.union q1.us q2.us in
    let q1 = freshen_xs q1 ~wrt:(Var.Set.union us q2.xs) in
    let q2 = freshen_xs q2 ~wrt:(Var.Set.union us q1.xs) in
    let {us= us1; xs= xs1; ctx= c1; pure= p1; heap= h1; djns= d1} = q1 in
    let {us= us2; xs= xs2; ctx= c2; pure= p2; heap= h2; djns= d2} = q2 in
    assert (Var.Set.equal us (Var.Set.union us1 us2)) ;
    let xs, ctx =
      Context.union (Var.Set.union us (Var.Set.union xs1 xs2)) c1 c2
    in
    if Context.is_unsat ctx then false_ us
    else
      let pure = Formula.and_ p1 p2 in
      if Formula.equal Formula.ff pure then false_ us
      else
        exists_fresh xs
          { us
          ; xs= Var.Set.union xs1 xs2
          ; ctx
          ; pure
          ; heap= List.append h1 h2
          ; djns= List.append d1 d2 }

let starN = function
  | [] -> emp
  | [q] -> q
  | q :: qs -> List.fold ~f:star qs q

let or_ q1 q2 =
  [%Trace.call fun {pf} -> pf "@ (%a)@ (%a)" pp_raw q1 pp_raw q2]
  ;
  ( match (q1, q2) with
  | _ when is_false q1 -> extend_us q1.us q2
  | _ when is_false q2 -> extend_us q2.us q1
  | ( ({djns= []; _} as q)
    , ({us= _; xs; ctx= _; pure; heap= []; djns= [djn]} as d) )
    when Var.Set.is_empty xs && Formula.(equal tt pure) ->
      {d with us= Var.Set.union q.us d.us; djns= [q :: djn]}
  | ( ({us= _; xs; ctx= _; pure; heap= []; djns= [djn]} as d)
    , ({djns= []; _} as q) )
    when Var.Set.is_empty xs && Formula.(equal tt pure) ->
      {d with us= Var.Set.union q.us d.us; djns= [q :: djn]}
  | _ ->
      { us= Var.Set.union q1.us q2.us
      ; xs= Var.Set.empty
      ; ctx= Context.empty
      ; pure= Formula.tt
      ; heap= []
      ; djns= [[q1; q2]] } )
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp_raw q ;
    invariant q ;
    assert (Var.Set.equal q.us (Var.Set.union q1.us q2.us))]

let orN = function
  | [] -> false_ Var.Set.empty
  | [q] -> q
  | q :: qs -> List.fold ~f:or_ qs q

let pure (p : Formula.t) =
  [%Trace.call fun {pf} -> pf "@ %a" Formula.pp p]
  ;
  Iter.fold (Context.dnf p) (false_ Var.Set.empty)
    ~f:(fun (xs, pure, ctx) q ->
      let us = Formula.fv pure in
      if Context.is_unsat ctx || Formula.equal Formula.ff pure then
        extend_us us q
      else or_ q (exists_fresh xs {emp with us; ctx; pure}) )
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp q ;
    invariant q]

let and_ b q =
  [%trace]
    ~call:(fun {pf} -> pf "@ (%a)@ (%a)" Formula.pp b pp q)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  let xs, q = bind_exists q ~wrt:(Formula.fv b) in
  let b = Formula.map_terms ~f:(Context.normalize q.ctx) b in
  exists xs (star (pure b) q)

let and_subst subst q =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Context.Subst.pp subst pp q]
  ;
  Context.Subst.fold_eqs ~f:and_ subst q
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp q ;
    invariant q]

let subst sub q =
  [%Trace.call fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp q]
  ;
  let dom, eqs =
    Var.Subst.fold sub (Var.Set.empty, Formula.tt)
      ~f:(fun var trm (dom, eqs) ->
        ( Var.Set.add var dom
        , Formula.and_ (Formula.eq (Term.var var) (Term.var trm)) eqs ) )
  in
  exists dom (and_ eqs q)
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Var.Set.disjoint q'.us (Var.Subst.domain sub))]

let seg pt =
  let us = fv_seg pt in
  if Term.equal Term.zero pt.loc then false_ us
  else {emp with us; heap= [pt]} |> check invariant

(** Update *)

let rem_seg seg q =
  {q with heap= List.remove_one_exn ~eq:( == ) seg q.heap}
  |> check invariant

let filter_heap ~f q =
  {q with heap= List.filter q.heap ~f} |> check invariant

(** Disjunctive-Normal Form *)

let fold_dnf ~conj ~disj sjn (xs, conjuncts) disjuncts =
  let rec add_disjunct pending_splits sjn (xs, conjuncts) disjuncts =
    let ys, sjn = bind_exists sjn ~wrt:xs in
    let xs = Var.Set.union ys xs in
    let djns = sjn.djns in
    let sjn = {sjn with djns= []} in
    split_case
      (Iter.append (Iter.of_list djns) pending_splits)
      (xs, conj sjn conjuncts)
      disjuncts
  and split_case pending_splits (xs, conjuncts) disjuncts =
    match Iter.pop pending_splits with
    | Some (split, pending_splits) ->
        List.fold split disjuncts ~f:(fun sjn disjuncts ->
            add_disjunct pending_splits sjn (xs, conjuncts) disjuncts )
    | None -> disj (xs, conjuncts) disjuncts
  in
  add_disjunct Iter.empty sjn (xs, conjuncts) disjuncts

let dnf q =
  [%Trace.call fun {pf} -> pf "@ %a" pp q]
  ;
  let conj sjn conjuncts = sjn :: conjuncts in
  let disj (xs, conjuncts) disjuncts =
    exists xs (starN conjuncts) :: disjuncts
  in
  fold_dnf ~conj ~disj q (Var.Set.empty, []) []
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp_djn]

(** Logical query *)

(** first-order approximation of heap constraints *)
let rec pure_approx q =
  Formula.andN
    ( [q.pure]
    |> List.fold q.heap ~f:(fun seg p -> Formula.dq0 seg.loc :: p)
    |> List.fold q.djns ~f:(fun djn p ->
           Formula.orN (List.map djn ~f:pure_approx) :: p ) )

let pure_approx q =
  [%Trace.call fun {pf} -> pf "@ %a" pp q]
  ;
  pure_approx q
  |>
  [%Trace.retn fun {pf} -> pf "%a" Formula.pp]

(** enumerate a DNF-expansion of a symbolic heap's first-order constraints
    conjoined with a first-order approximation of the heap constraints until
    a branch that is not unsatisfiable is found *)
let is_unsat_dnf q =
  let exception NotUnsat in
  let conj sjn (wrt, ctx, fml) =
    let wrt = Var.Set.union wrt sjn.xs in
    let zs, ctx = Context.union wrt ctx sjn.ctx in
    let wrt = Var.Set.union wrt zs in
    let fml = Formula.and_ fml sjn.pure in
    let fml =
      List.fold sjn.heap fml ~f:(fun seg ->
          Formula.and_ (Formula.dq0 seg.loc) )
    in
    (wrt, ctx, fml)
  in
  let disj (_, (_, ctx, fml)) () =
    if not (Context.is_unsat ctx || Context.refutes ctx fml) then
      raise_notrace NotUnsat
  in
  try
    fold_dnf ~conj ~disj q (Var.Set.empty, (q.us, emp.ctx, emp.pure)) () ;
    true
  with NotUnsat -> false

let is_unsat q =
  if strong_unsat then is_unsat_dnf q
  else Context.refutes q.ctx (pure_approx q)

(** Simplify *)

let rec norm_ s q =
  [%Trace.call fun {pf} -> pf "@ @[%a@]@ %a" Context.Subst.pp s pp_raw q]
  ;
  map q ~f_sjn:(norm_ s)
    ~f_ctx:(Context.apply_subst (Var.Set.union q.us q.xs) s)
    ~f_trm:(Context.Subst.subst s)
    ~f_fml:(Formula.map_terms ~f:(Context.Subst.subst s))
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp_raw q' ;
    invariant q']

let norm s q =
  [%Trace.call fun {pf} -> pf "@ @[%a@]@ %a" Context.Subst.pp s pp_raw q]
  ;
  (if Context.Subst.is_empty s then q else norm_ s q)
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp_raw q' ;
    invariant q']

(** rename existentially quantified variables to avoid shadowing, and reduce
    quantifier scopes by sinking them as low as possible into disjunctions *)
let rec freshen_nested_xs us q =
  [%Trace.call fun {pf} -> pf "@ %a" pp q]
  ;
  (* trim xs to those that appear in the stem and sink the rest *)
  let fv_stem = fv {q with xs= Var.Set.empty; djns= []} in
  let xs_sink, xs = Var.Set.diff_inter q.xs fv_stem in
  let us = Var.Set.union us q.us in
  let djns, xs_below =
    List.fold_map q.djns Var.Set.empty ~f:(fun djn xs_below ->
        List.fold_map djn xs_below ~f:(fun dj xs_below ->
            (* freshen xs that shadow ancestor us *)
            let us = Var.Set.union us dj.us in
            let dj = freshen_xs dj ~wrt:us in
            (* quantify xs not in stem and freshen disjunct *)
            let dj =
              freshen_nested_xs us (exists (Var.Set.inter xs_sink dj.us) dj)
            in
            let xs_below = Var.Set.union xs_below dj.xs in
            (dj, xs_below) ) )
  in
  (* rename xs to miss all xs in subformulas *)
  freshen_xs {q with xs; djns} ~wrt:(Var.Set.union q.us xs_below)
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q']

let rec propagate_context_ ancestor_vs ancestor_ctx q =
  [%Trace.call fun {pf} -> pf "@ (%a)@ %a" Context.pp ancestor_ctx pp q]
  ;
  (* extend vocabulary with variables in scope above *)
  let ancestor_vs = Var.Set.union ancestor_vs (Var.Set.union q.us q.xs) in
  (* decompose formula *)
  let xs, stem, djns =
    (q.xs, {q with us= ancestor_vs; xs= emp.xs; djns= emp.djns}, q.djns)
  in
  (* strengthen context with that from above *)
  let ancestor_stem = and_ctx_ ancestor_ctx stem in
  let ancestor_ctx = ancestor_stem.ctx in
  let q' =
    List.fold djns ancestor_stem ~f:(fun djn q' ->
        let dj_ctxs, djn =
          List.rev_map_split djn ~f:(fun dj ->
              let dj = propagate_context_ ancestor_vs ancestor_ctx dj in
              (dj.ctx, dj) )
        in
        let new_xs, djn_ctx = Context.interN ancestor_vs dj_ctxs in
        (* hoist xs appearing in disjunction's context *)
        let djn_xs = Var.Set.diff (Context.fv djn_ctx) q'.us in
        let djn = List.map ~f:(elim_exists djn_xs) djn in
        let ctx_djn = and_ctx_ djn_ctx (orN djn) in
        assert (is_false ctx_djn || Var.Set.subset new_xs ~of_:djn_xs) ;
        star (exists djn_xs ctx_djn) q' )
  in
  (* requantify existentials *)
  let q' = exists xs q' in
  (* strengthening contexts can reveal inconsistency *)
  (if is_false q' then false_ q'.us else q')
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q']

let propagate_context ancestor_vs ancestor_ctx q =
  [%Trace.call fun {pf} -> pf "@ (%a)@ %a" Context.pp ancestor_ctx pp q]
  ;
  propagate_context_ ancestor_vs ancestor_ctx q
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q']

let pp_vss fs vss =
  Format.fprintf fs "[@[%a@]]"
    (List.pp ";@ " (fun fs vs -> Format.fprintf fs "{@[%a@]}" Var.Set.pp vs))
    vss

let remove_absent_xs ks q =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a%a" Var.Set.pp_xs ks pp q)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  let ks = Var.Set.inter ks q.xs in
  if Var.Set.is_empty ks then q
  else
    let xs = Var.Set.diff q.xs ks in
    let djns =
      let rec trim_ks ks djns =
        List.map_endo djns ~f:(fun djn ->
            List.map_endo djn ~f:(fun sjn ->
                let us = Var.Set.diff sjn.us ks in
                let djns = trim_ks ks sjn.djns in
                if us == sjn.us && djns == sjn.djns then sjn
                else {sjn with us; djns} ) )
      in
      trim_ks ks q.djns
    in
    {q with xs; djns}

let rec simplify_ us rev_xss survived ancestor_subst q =
  [%Trace.call fun {pf} ->
    pf "@ %a@ %a@ %a" pp_vss (List.rev rev_xss) Context.Subst.pp
      ancestor_subst pp_raw q]
  ;
  assert (not (is_false q)) ;
  let q0 = q in
  (* bind existentials *)
  let xs, q = bind_exists ~wrt:emp.us q in
  let rev_xss = xs :: rev_xss in
  (* try to solve equations in ctx for variables in xss *)
  let stem_subst = Context.solve_for_vars (us :: List.rev rev_xss) q.ctx in
  let subst = Context.Subst.compose ancestor_subst stem_subst in
  ( if Context.Subst.is_empty subst then q0
  else
    (* normalize context wrt solutions *)
    let union_xss = Var.Set.union_list rev_xss in
    let wrt = Var.Set.union us union_xss in
    let fresh, ctx, removed =
      Context.apply_and_elim ~wrt union_xss subst q.ctx
    in
    let q = {(extend_us fresh q) with ctx} in
    (* normalize stem wrt both ancestor and current solutions *)
    let stem =
      (* opt: ctx already normalized so just preserve it *)
      {(norm subst {q with djns= emp.djns; ctx= emp.ctx}) with ctx= q.ctx}
    in
    if strong_unsat && is_unsat stem then false_ stem.us
    else
      (* recursively simplify subformulas *)
      let survived =
        Var.Set.union survived (fv (elim_exists stem.xs stem))
      in
      let q =
        starN
          ( stem
          :: List.map q.djns ~f:(fun djn ->
                 orN (List.map ~f:(simplify_ us rev_xss survived subst) djn) )
          )
      in
      if is_false q then false_ q.us
      else
        let removed = Var.Set.diff removed survived in
        let removed =
          if Var.Set.is_empty removed then Var.Set.empty
          else (
            (* opt: removed already disjoint from ctx, so ignore_ctx *)
            assert (Var.Set.disjoint removed (Context.fv q.ctx)) ;
            Var.Set.diff removed (fv ~ignore_ctx:() (elim_exists q.xs q)) )
        in
        (* removed may not contain all variables stem_subst has solutions
           for, so the equations in [∃ removed. stem_subst] that are not
           universally valid need to be re-conjoined since they have alredy
           been normalized out *)
        let keep, removed, _ =
          Context.Subst.partition_valid removed stem_subst
        in
        let q = and_subst keep q in
        (* (re)quantify existentials *)
        let q = exists (Var.Set.union fresh xs) q in
        (* remove the eliminated variables from xs and subformulas' us *)
        remove_absent_xs removed q )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a@ %a" Context.Subst.pp stem_subst pp_raw q' ;
    invariant q']

let simplify q =
  [%Trace.call fun {pf} -> pf "@ %a" pp_raw q]
  ;
  ( if is_false q then false_ q.us
  else
    let q = freshen_nested_xs Var.Set.empty q in
    let q = propagate_context Var.Set.empty Context.empty q in
    if is_false q then false_ q.us
    else
      let q = simplify_ q.us [] Var.Set.empty Context.Subst.empty q in
      q )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp_raw q' ;
    invariant q']
