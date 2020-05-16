(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

[@@@warning "+9"]

type seg = {loc: Term.t; bas: Term.t; len: Term.t; siz: Term.t; arr: Term.t}
[@@deriving compare, equal, sexp]

type starjunction =
  { us: Var.Set.t
  ; xs: Var.Set.t
  ; cong: Equality.t
  ; pure: Term.t list
  ; heap: seg list
  ; djns: disjunction list }
[@@deriving compare, equal, sexp]

and disjunction = starjunction list

type t = starjunction [@@deriving compare, equal, sexp]

(** Basic values *)

let emp =
  { us= Var.Set.empty
  ; xs= Var.Set.empty
  ; cong= Equality.true_
  ; pure= []
  ; heap= []
  ; djns= [] }

let false_ us = {emp with us; djns= [[]]}

(** Traversals *)

let map_seg ~f h =
  let loc = f h.loc in
  let bas = f h.bas in
  let len = f h.len in
  let siz = f h.siz in
  let arr = f h.arr in
  if
    loc == h.loc && bas == h.bas && len == h.len && siz == h.siz
    && arr == h.arr
  then h
  else {loc; bas; len; siz; arr}

let map ~f_sjn ~f_cong ~f_trm ({us; xs= _; cong; pure; heap; djns} as q) =
  let exception Unsat in
  try
    let cong = f_cong cong in
    let pure =
      List.filter_map_endo pure ~f:(fun e ->
          let e' = f_trm e in
          if Term.is_false e' then raise Unsat
          else if Term.is_true e' then None
          else Some e' )
    in
    let heap = List.map_endo heap ~f:(map_seg ~f:f_trm) in
    let djns = List.map_endo djns ~f:(List.map_endo ~f:f_sjn) in
    if cong == q.cong && pure == q.pure && heap == q.heap && djns == q.djns
    then q
    else {q with cong; pure; heap; djns}
  with Unsat -> false_ us

let fold_terms_seg {loc; bas; len; siz; arr} ~init ~f =
  let f b s = f s b in
  f loc (f bas (f len (f siz (f arr init))))

let fold_vars_seg seg ~init ~f =
  fold_terms_seg seg ~init ~f:(fun init -> Term.fold_vars ~f ~init)

let fold_vars_stem ?ignore_cong {us= _; xs= _; cong; pure; heap; djns= _}
    ~init ~f =
  List.fold ~init heap ~f:(fun init -> fold_vars_seg ~f ~init)
  |> fun init ->
  List.fold ~init pure ~f:(fun init -> Term.fold_vars ~f ~init)
  |> fun init ->
  if Option.is_some ignore_cong then init
  else
    Equality.fold_terms ~init cong ~f:(fun init -> Term.fold_vars ~f ~init)

let fold_vars ?ignore_cong fold_vars q ~init ~f =
  fold_vars_stem ?ignore_cong ~init ~f q
  |> fun init ->
  List.fold ~init q.djns ~f:(fun init -> List.fold ~init ~f:fold_vars)

(** Pretty-printing *)

let rec var_strength_ xs m q =
  let add m v =
    match Var.Map.find m v with
    | None -> Var.Map.set m ~key:v ~data:`Anonymous
    | Some `Anonymous -> Var.Map.set m ~key:v ~data:`Existential
    | Some _ -> m
  in
  let xs = Var.Set.union xs q.xs in
  let m_stem =
    fold_vars_stem ~ignore_cong:() q ~init:m ~f:(fun m var ->
        if not (Var.Set.mem xs var) then
          Var.Map.set m ~key:var ~data:`Universal
        else add m var )
  in
  let m =
    List.fold ~init:m_stem q.djns ~f:(fun m djn ->
        let ms = List.map ~f:(fun dj -> snd (var_strength_ xs m dj)) djn in
        List.reduce_balanced ms ~f:(fun m1 m2 ->
            Var.Map.merge_skewed m1 m2 ~combine:(fun ~key:_ s1 s2 ->
                match (s1, s2) with
                | `Anonymous, `Anonymous -> `Anonymous
                | `Universal, _ | _, `Universal -> `Universal
                | `Existential, _ | _, `Existential -> `Existential ) )
        |> Option.value ~default:m )
  in
  (m_stem, m)

let var_strength ?(xs = Var.Set.empty) q =
  let m =
    Var.Set.fold xs ~init:Var.Map.empty ~f:(fun m x ->
        Var.Map.set m ~key:x ~data:`Existential )
  in
  var_strength_ xs m q

let pp_memory x fs (siz, arr) = Term.ppx x fs (Term.memory ~siz ~arr)

let pp_seg x fs {loc; bas; len; siz; arr} =
  let term_pp = Term.ppx x in
  Format.fprintf fs "@[<2>%a@ @[@[-[%a)->@]@ %a@]@]" term_pp loc
    (fun fs (bas, len) ->
      if (not (Term.equal loc bas)) || not (Term.equal len siz) then
        Format.fprintf fs " %a, %a " term_pp bas term_pp len )
    (bas, len) (pp_memory x) (siz, arr)

let pp_seg_norm cong fs seg =
  let x _ = None in
  pp_seg x fs (map_seg seg ~f:(Equality.normalize cong))

let pp_block x fs segs =
  let is_full_alloc segs =
    match segs with
    | {loc; bas; len; _} :: _ -> (
        Term.equal loc bas
        &&
        match len with
        | Integer {data} -> (
          match
            List.fold segs ~init:(Some Z.zero) ~f:(fun len seg ->
                match (len, seg.siz) with
                | Some len, Integer {data} -> Some (Z.add len data)
                | _ -> None )
          with
          | Some blk_len -> Z.equal data blk_len
          | _ -> false )
        | _ -> false )
    | [] -> false
  in
  let term_pp = Term.ppx x in
  let pp_mems =
    List.pp "@,^" (fun fs seg -> pp_memory x fs (seg.siz, seg.arr))
  in
  match segs with
  | {loc; bas; len; _} :: _ ->
      Format.fprintf fs "@[<2>%a@ @[@[-[%t)->@]@ @[%a@]@]@]" term_pp loc
        (fun fs ->
          if not (is_full_alloc segs) then
            Format.fprintf fs " %a, %a " term_pp bas term_pp len )
        pp_mems segs
  | [] -> ()

let pp_heap x ?pre cong fs heap =
  let bas_off = function
    | Term.Add poly as sum ->
        let const = Term.Qset.count poly Term.one in
        (Term.sub sum (Term.rational const), const)
    | e -> (e, Q.zero)
  in
  let compare s1 s2 =
    [%compare: Term.t * (Term.t * Q.t)]
      ( Equality.normalize cong s1.bas
      , bas_off (Equality.normalize cong s1.loc) )
      ( Equality.normalize cong s2.bas
      , bas_off (Equality.normalize cong s2.loc) )
  in
  let break s1 s2 =
    (not (Term.equal s1.bas s2.bas))
    || (not (Term.equal s1.len s2.len))
    || not (Equality.entails_eq cong (Term.add s1.loc s1.siz) s2.loc)
  in
  let heap = List.map heap ~f:(map_seg ~f:(Equality.normalize cong)) in
  let blocks = List.group ~break (List.sort ~compare heap) in
  List.pp ?pre "@ * " (pp_block x) fs blocks

let pp_us ?(pre = ("" : _ fmt)) ?vs () fs us =
  match vs with
  | None ->
      if not (Var.Set.is_empty us) then
        [%Trace.fprintf fs "%( %)@[%a@] .@ " pre Var.Set.pp us]
  | Some vs ->
      if not (Var.Set.equal vs us) then
        [%Trace.fprintf
          fs "%( %)@[%a@] .@ " pre (Var.Set.pp_diff Var.pp) (vs, us)]

let rec pp_ ?var_strength vs parent_xs parent_cong fs
    {us; xs; cong; pure; heap; djns} =
  Format.pp_open_hvbox fs 0 ;
  let x v = Option.bind ~f:(fun (_, m) -> Var.Map.find m v) var_strength in
  pp_us ~vs () fs us ;
  let xs_d_vs, xs_i_vs =
    Var.Set.diff_inter
      (Var.Set.filter xs ~f:(fun v -> Poly.(x v <> Some `Anonymous)))
      vs
  in
  if not (Var.Set.is_empty xs_i_vs) then (
    Format.fprintf fs "@<2>∃ @[%a@] ." (Var.Set.ppx x) xs_i_vs ;
    if not (Var.Set.is_empty xs_d_vs) then Format.fprintf fs "@ " ) ;
  if not (Var.Set.is_empty xs_d_vs) then
    Format.fprintf fs "@<2>∃ @[%a@] .@ " (Var.Set.ppx x) xs_d_vs ;
  let first = Equality.entails parent_cong cong in
  if not first then Format.fprintf fs "  " ;
  Equality.ppx_classes_diff x fs (parent_cong, cong) ;
  let pure =
    if Option.is_none var_strength then pure
    else
      List.filter_map pure ~f:(fun e ->
          let e' = Equality.normalize cong e in
          if Term.is_true e' then None else Some e' )
  in
  List.pp
    ~pre:(if first then "@[  " else "@ @[@<2>∧ ")
    "@ @<2>∧ " (Term.ppx x) fs
    (List.dedup_and_sort ~compare:Term.compare pure)
    ~suf:"@]" ;
  let first = first && List.is_empty pure in
  if List.is_empty heap then
    Format.fprintf fs
      ( if first then if List.is_empty djns then "  emp" else ""
      else "@ @<5>∧ emp" )
  else pp_heap x ~pre:(if first then "  " else "@ @<2>∧ ") cong fs heap ;
  let first = first && List.is_empty heap in
  List.pp
    ~pre:(if first then "  " else "@ * ")
    "@ * "
    (pp_djn ?var_strength
       (Var.Set.union vs (Var.Set.union us xs))
       (Var.Set.union parent_xs xs)
       cong)
    fs djns ;
  Format.pp_close_box fs ()

and pp_djn ?var_strength vs xs cong fs = function
  | [] -> Format.fprintf fs "false"
  | djn ->
      Format.fprintf fs "@[<hv>( %a@ )@]"
        (List.pp "@ @<2>∨ " (fun fs sjn ->
             let var_strength =
               let+ var_strength_stem, _ = var_strength in
               var_strength_ xs var_strength_stem sjn
             in
             Format.fprintf fs "@[<hv 1>(%a)@]"
               (pp_ ?var_strength vs (Var.Set.union xs sjn.xs) cong)
               sjn ))
        djn

let pp_diff_eq ?(us = Var.Set.empty) ?(xs = Var.Set.empty) cong fs q =
  pp_ ~var_strength:(var_strength ~xs q) us xs cong fs q

let pp fs q = pp_diff_eq Equality.true_ fs q
let pp_djn fs d = pp_djn Var.Set.empty Var.Set.empty Equality.true_ fs d
let pp_raw fs q = pp_ Var.Set.empty Var.Set.empty Equality.true_ fs q
let fv_seg seg = fold_vars_seg seg ~f:Var.Set.add ~init:Var.Set.empty

let fv ?ignore_cong q =
  let rec fv_union init q =
    Var.Set.diff
      (fold_vars ?ignore_cong fv_union q ~init ~f:Var.Set.add)
      q.xs
  in
  fv_union Var.Set.empty q

let invariant_pure = function
  | Term.Integer {data} -> assert (not (Z.is_false data))
  | _ -> assert true

let invariant_seg _ = ()

let rec invariant q =
  Invariant.invariant [%here] q [%sexp_of: t]
  @@ fun () ->
  let {us; xs; cong; pure; heap; djns} = q in
  try
    assert (
      Var.Set.disjoint us xs
      || fail "inter: @[%a@]@\nq: @[%a@]" Var.Set.pp (Var.Set.inter us xs)
           pp q () ) ;
    assert (
      Var.Set.is_subset (fv q) ~of_:us
      || fail "unbound but free: %a" Var.Set.pp (Var.Set.diff (fv q) us) ()
    ) ;
    Equality.invariant cong ;
    ( match djns with
    | [[]] ->
        assert (Equality.is_true cong) ;
        assert (List.is_empty pure) ;
        assert (List.is_empty heap)
    | _ -> assert (not (Equality.is_false cong)) ) ;
    List.iter pure ~f:invariant_pure ;
    List.iter heap ~f:invariant_seg ;
    List.iter djns ~f:(fun djn ->
        List.iter djn ~f:(fun sjn ->
            assert (Var.Set.is_subset sjn.us ~of_:(Var.Set.union us xs)) ;
            invariant sjn ) )
  with exc -> [%Trace.info "%a" pp q] ; raise exc

(** Quantification and Vocabulary *)

(** primitive application of a substitution, ignores us and xs, may violate
    invariant *)
let rec apply_subst sub q =
  map q ~f_sjn:(rename sub)
    ~f_cong:(fun r -> Equality.rename r sub)
    ~f_trm:(Term.rename sub)
  |> check (fun q' ->
         assert (Var.Set.disjoint (fv q') (Var.Subst.domain sub)) )

and rename sub q =
  [%Trace.call fun {pf} -> pf "@[%a@]@ %a" Var.Subst.pp sub pp q]
  ;
  let sub = Var.Subst.restrict sub q.us in
  ( if Var.Subst.is_empty sub then q
  else
    let us = Var.Subst.apply_set sub q.us in
    assert (not (Var.Set.equal us q.us)) ;
    let q' = apply_subst sub (freshen_xs q ~wrt:(Var.Set.union q.us us)) in
    {q' with us} )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Var.Set.disjoint q'.us (Var.Subst.domain sub))]

(** freshen existentials, preserving vocabulary *)
and freshen_xs q ~wrt =
  [%Trace.call fun {pf} ->
    pf "{@[%a@]}@ %a" Var.Set.pp wrt pp q ;
    assert (Var.Set.is_subset q.us ~of_:wrt)]
  ;
  let sub = Var.Subst.freshen q.xs ~wrt in
  ( if Var.Subst.is_empty sub then q
  else
    let xs = Var.Subst.apply_set sub q.xs in
    let q' = apply_subst sub q in
    if xs == q.xs && q' == q then q else {q' with xs} )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a@ %a" Var.Subst.pp sub pp q' ;
    assert (Var.Set.equal q'.us q.us) ;
    assert (Var.Set.disjoint q'.xs (Var.Subst.domain sub)) ;
    assert (Var.Set.disjoint q'.xs (Var.Set.inter q.xs wrt)) ;
    invariant q']

let extend_us us q =
  let us = Var.Set.union us q.us in
  let q' = freshen_xs q ~wrt:us in
  (if us == q.us && q' == q then q else {q' with us}) |> check invariant

let freshen ~wrt q =
  let sub = Var.Subst.freshen q.us ~wrt:(Var.Set.union wrt q.xs) in
  let q' = extend_us wrt (rename sub q) in
  (if q' == q then (q, sub) else (q', sub))
  |> check (fun (q', _) ->
         invariant q' ;
         assert (Var.Set.is_subset wrt ~of_:q'.us) ;
         assert (Var.Set.disjoint wrt (fv q')) )

let bind_exists q ~wrt =
  [%Trace.call fun {pf} -> pf "{@[%a@]}@ %a" Var.Set.pp wrt pp q]
  ;
  let q' =
    if Var.Set.is_empty wrt then q
    else freshen_xs q ~wrt:(Var.Set.union q.us wrt)
  in
  (q'.xs, {q' with us= Var.Set.union q'.us q'.xs; xs= Var.Set.empty})
  |>
  [%Trace.retn fun {pf} (_, q') -> pf "%a" pp q']

let exists_fresh xs q =
  [%Trace.call fun {pf} ->
    pf "{@[%a@]}@ %a" Var.Set.pp xs pp q ;
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
  [%Trace.call fun {pf} -> pf "{@[%a@]}@ %a" Var.Set.pp xs pp q]
  ;
  assert (
    Var.Set.is_subset xs ~of_:q.us
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

(** Construct *)

(** conjoin an equality relation assuming vocabulary is compatible *)
let and_cong_ cong q =
  assert (Var.Set.is_subset (Equality.fv cong) ~of_:q.us) ;
  let xs, cong = Equality.and_ (Var.Set.union q.us q.xs) q.cong cong in
  if Equality.is_false cong then false_ q.us
  else exists_fresh xs {q with cong}

let and_cong cong q =
  [%Trace.call fun {pf} -> pf "%a@ %a" Equality.pp cong pp q]
  ;
  ( match q.djns with
  | [[]] -> q
  | _ -> and_cong_ cong (extend_us (Equality.fv cong) q) )
  |>
  [%Trace.retn fun {pf} q -> pf "%a" pp q ; invariant q]

let star q1 q2 =
  [%Trace.call fun {pf} -> pf "(%a)@ (%a)" pp q1 pp q2]
  ;
  ( match (q1, q2) with
  | {djns= [[]]; _}, _ | _, {djns= [[]]; _} ->
      false_ (Var.Set.union q1.us q2.us)
  | {us= _; xs= _; cong; pure= []; heap= []; djns= []}, _
    when Equality.is_true cong ->
      let us = Var.Set.union q1.us q2.us in
      if us == q2.us then q2 else {q2 with us}
  | _, {us= _; xs= _; cong; pure= []; heap= []; djns= []}
    when Equality.is_true cong ->
      let us = Var.Set.union q1.us q2.us in
      if us == q1.us then q1 else {q1 with us}
  | _ ->
      let us = Var.Set.union q1.us q2.us in
      let q1 = freshen_xs q1 ~wrt:(Var.Set.union us q2.xs) in
      let q2 = freshen_xs q2 ~wrt:(Var.Set.union us q1.xs) in
      let {us= us1; xs= xs1; cong= c1; pure= p1; heap= h1; djns= d1} = q1 in
      let {us= us2; xs= xs2; cong= c2; pure= p2; heap= h2; djns= d2} = q2 in
      assert (Var.Set.equal us (Var.Set.union us1 us2)) ;
      let xs, cong =
        Equality.and_ (Var.Set.union us (Var.Set.union xs1 xs2)) c1 c2
      in
      if Equality.is_false cong then false_ us
      else
        exists_fresh xs
          { us
          ; xs= Var.Set.union xs1 xs2
          ; cong
          ; pure= List.append p1 p2
          ; heap= List.append h1 h2
          ; djns= List.append d1 d2 } )
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp q ;
    invariant q ;
    assert (Var.Set.equal q.us (Var.Set.union q1.us q2.us))]

let starN = function
  | [] -> emp
  | [q] -> q
  | q :: qs -> List.fold ~f:star ~init:q qs

let or_ q1 q2 =
  [%Trace.call fun {pf} -> pf "(%a)@ (%a)" pp_raw q1 pp_raw q2]
  ;
  ( match (q1, q2) with
  | {djns= [[]]; _}, _ -> extend_us q1.us q2
  | _, {djns= [[]]; _} -> extend_us q2.us q1
  | ( ({djns= []; _} as q)
    , ({us= _; xs; cong= _; pure= []; heap= []; djns= [djn]} as d) )
    when Var.Set.is_empty xs ->
      {d with us= Var.Set.union q.us d.us; djns= [q :: djn]}
  | ( ({us= _; xs; cong= _; pure= []; heap= []; djns= [djn]} as d)
    , ({djns= []; _} as q) )
    when Var.Set.is_empty xs ->
      {d with us= Var.Set.union q.us d.us; djns= [q :: djn]}
  | _ ->
      { us= Var.Set.union q1.us q2.us
      ; xs= Var.Set.empty
      ; cong= Equality.true_
      ; pure= []
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
  | q :: qs -> List.fold ~f:or_ ~init:q qs

let rec pure (e : Term.t) =
  [%Trace.call fun {pf} -> pf "%a" Term.pp e]
  ;
  ( match e with
  | Or es ->
      let e0, e1N = Term.Set.pop_exn es in
      Term.Set.fold e1N ~init:(pure e0) ~f:(fun q e -> or_ q (pure e))
  | Ap3 (Conditional, cnd, thn, els) ->
      or_
        (star (pure cnd) (pure thn))
        (star (pure (Term.not_ cnd)) (pure els))
  | _ ->
      let us = Term.fv e in
      let xs, cong = Equality.(and_term us e true_) in
      if Equality.is_false cong then false_ us
      else exists_fresh xs {emp with us; cong; pure= [e]} )
  |>
  [%Trace.retn fun {pf} q -> pf "%a" pp q ; invariant q]

let and_ e q = star (pure e) q

let and_subst subst q =
  [%Trace.call fun {pf} -> pf "%a@ %a" Equality.Subst.pp subst pp q]
  ;
  Equality.Subst.fold
    ~f:(fun ~key ~data -> and_ (Term.eq key data))
    subst ~init:q
  |>
  [%Trace.retn fun {pf} q -> pf "%a" pp q ; invariant q]

let subst sub q =
  [%Trace.call fun {pf} -> pf "@[%a@]@ %a" Var.Subst.pp sub pp q]
  ;
  let dom, eqs =
    Var.Subst.fold sub ~init:(Var.Set.empty, Term.true_)
      ~f:(fun var trm (dom, eqs) ->
        ( Var.Set.add dom var
        , Term.and_ (Term.eq (Term.var var) (Term.var trm)) eqs ) )
  in
  exists dom (and_ eqs q)
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Var.Set.disjoint q'.us (Var.Subst.domain sub))]

let seg pt =
  let us = fv_seg pt in
  if Term.equal Term.null pt.loc then false_ us
  else {emp with us; heap= [pt]} |> check invariant

(** Update *)

let with_pure pure q = {q with pure} |> check invariant

let rem_seg seg q =
  {q with heap= List.remove_exn q.heap seg} |> check invariant

let filter_heap ~f q =
  {q with heap= List.filter q.heap ~f} |> check invariant

(** Query *)

let is_emp = function
  | {us= _; xs= _; cong= _; pure= []; heap= []; djns= []} -> true
  | _ -> false

let is_false = function
  | {djns= [[]]; _} -> true
  | {cong; pure; heap; _} ->
      List.exists pure ~f:(fun b ->
          Term.is_false (Equality.normalize cong b) )
      || List.exists heap ~f:(fun seg ->
             Equality.entails_eq cong seg.loc Term.null )

let rec pure_approx ({us; xs; cong; pure; heap= _; djns} as q) =
  let heap = emp.heap in
  let djns =
    List.map_endo djns ~f:(fun djn -> List.map_endo djn ~f:pure_approx)
  in
  if heap == q.heap && djns == q.djns then q
  else {us; xs; cong; pure; heap; djns} |> check invariant

let pure_approx q =
  [%Trace.call fun {pf} -> pf "%a" pp q]
  ;
  pure_approx q
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let fold_dnf ~conj ~disj sjn (xs, conjuncts) disjuncts =
  let rec add_disjunct pending_splits sjn (xs, conjuncts) disjuncts =
    let ys, sjn = bind_exists sjn ~wrt:xs in
    let xs = Var.Set.union ys xs in
    let djns = sjn.djns in
    let sjn = {sjn with djns= []} in
    split_case
      (List.rev_append djns pending_splits)
      (xs, conj sjn conjuncts)
      disjuncts
  and split_case pending_splits (xs, conjuncts) disjuncts =
    match pending_splits with
    | split :: pending_splits ->
        List.fold split ~init:disjuncts ~f:(fun disjuncts sjn ->
            add_disjunct pending_splits sjn (xs, conjuncts) disjuncts )
    | [] -> disj (xs, conjuncts) disjuncts
  in
  add_disjunct [] sjn (xs, conjuncts) disjuncts

let dnf q =
  [%Trace.call fun {pf} -> pf "%a" pp q]
  ;
  let conj sjn conjuncts = sjn :: conjuncts in
  let disj (xs, conjuncts) disjuncts =
    exists xs (starN conjuncts) :: disjuncts
  in
  fold_dnf ~conj ~disj q (Var.Set.empty, []) []
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp_djn]

(** Simplify *)

let rec norm_ s q =
  [%Trace.call fun {pf} -> pf "@[%a@]@ %a" Equality.Subst.pp s pp_raw q]
  ;
  let q =
    map q ~f_sjn:(norm_ s) ~f_cong:Fn.id ~f_trm:(Equality.Subst.subst s)
  in
  let xs, cong = Equality.apply_subst (Var.Set.union q.us q.xs) s q.cong in
  exists_fresh xs {q with cong}
  |>
  [%Trace.retn fun {pf} q' -> pf "%a" pp_raw q' ; invariant q']

let norm s q =
  [%Trace.call fun {pf} -> pf "@[%a@]@ %a" Equality.Subst.pp s pp_raw q]
  ;
  (if Equality.Subst.is_empty s then q else norm_ s q)
  |>
  [%Trace.retn fun {pf} q' -> pf "%a" pp_raw q' ; invariant q']

(** rename existentially quantified variables to avoid shadowing, and reduce
    quantifier scopes by sinking them as low as possible into disjunctions *)
let rec freshen_nested_xs q =
  [%Trace.call fun {pf} -> pf "%a" pp q]
  ;
  (* trim xs to those that appear in the stem and sink the rest *)
  let fv_stem = fv {q with xs= Var.Set.empty; djns= []} in
  let xs_sink, xs = Var.Set.diff_inter q.xs fv_stem in
  let xs_below, djns =
    List.fold_map ~init:Var.Set.empty q.djns ~f:(fun xs_below djn ->
        List.fold_map ~init:xs_below djn ~f:(fun xs_below dj ->
            (* quantify xs not in stem and freshen disjunct *)
            let dj' =
              freshen_nested_xs (exists (Var.Set.inter xs_sink dj.us) dj)
            in
            let xs_below' = Var.Set.union xs_below dj'.xs in
            (xs_below', dj') ) )
  in
  (* rename xs to miss all xs in subformulas *)
  freshen_xs {q with xs; djns} ~wrt:(Var.Set.union q.us xs_below)
  |>
  [%Trace.retn fun {pf} q' -> pf "%a" pp q' ; invariant q']

let rec propagate_equality_ ancestor_vs ancestor_cong q =
  [%Trace.call fun {pf} ->
    pf "(%a)@ %a" Equality.pp_classes ancestor_cong pp q]
  ;
  (* extend vocabulary with variables in scope above *)
  let ancestor_vs = Var.Set.union ancestor_vs (Var.Set.union q.us q.xs) in
  (* decompose formula *)
  let xs, stem, djns =
    (q.xs, {q with us= ancestor_vs; xs= emp.xs; djns= emp.djns}, q.djns)
  in
  (* strengthen equality relation with that from above *)
  let ancestor_stem = and_cong_ ancestor_cong stem in
  let ancestor_cong = ancestor_stem.cong in
  exists xs
    (List.fold djns ~init:ancestor_stem ~f:(fun q' djn ->
         let dj_congs, djn =
           List.rev_map_unzip djn ~f:(fun dj ->
               let dj = propagate_equality_ ancestor_vs ancestor_cong dj in
               (dj.cong, dj) )
         in
         let new_xs, djn_cong = Equality.orN ancestor_vs dj_congs in
         (* hoist xs appearing in disjunction's equality relation *)
         let djn_xs = Var.Set.diff (Equality.fv djn_cong) q'.us in
         let djn = List.map ~f:(elim_exists djn_xs) djn in
         let cong_djn = and_cong_ djn_cong (orN djn) in
         assert (is_false cong_djn || Var.Set.is_subset new_xs ~of_:djn_xs) ;
         star (exists djn_xs cong_djn) q' ))
  |>
  [%Trace.retn fun {pf} q' -> pf "%a" pp q' ; invariant q']

let propagate_equality ancestor_vs ancestor_cong q =
  [%Trace.call fun {pf} ->
    pf "(%a)@ %a" Equality.pp_classes ancestor_cong pp q]
  ;
  propagate_equality_ ancestor_vs ancestor_cong q
  |>
  [%Trace.retn fun {pf} q' -> pf "%a" pp q' ; invariant q']

let pp_vss fs vss =
  Format.fprintf fs "[@[%a@]]"
    (List.pp ";@ " (fun fs vs -> Format.fprintf fs "{@[%a@]}" Var.Set.pp vs))
    vss

let remove_absent_xs ks q =
  let ks = Var.Set.inter ks q.xs in
  if Var.Set.is_empty ks then q
  else
    let xs = Var.Set.diff q.xs ks in
    let cong = Equality.elim ks q.cong in
    let djns =
      let rec trim_ks ks djns =
        List.map djns ~f:(fun djn ->
            List.map djn ~f:(fun sjn ->
                { sjn with
                  us= Var.Set.diff sjn.us ks
                ; cong= Equality.elim ks sjn.cong
                ; djns= trim_ks ks sjn.djns } ) )
      in
      trim_ks ks q.djns
    in
    {q with xs; cong; djns}

let rec simplify_ us rev_xss q =
  [%Trace.call fun {pf} -> pf "%a@ %a" pp_vss (List.rev rev_xss) pp_raw q]
  ;
  let rev_xss = q.xs :: rev_xss in
  (* recursively simplify subformulas *)
  let q =
    exists q.xs
      (starN
         ( {q with us= Var.Set.union q.us q.xs; xs= emp.xs; djns= []}
         :: List.map q.djns ~f:(fun djn ->
                orN (List.map djn ~f:(fun sjn -> simplify_ us rev_xss sjn)) )
         ))
  in
  (* try to solve equations in cong for variables in xss *)
  let subst = Equality.solve_for_vars (us :: List.rev rev_xss) q.cong in
  (* simplification can reveal inconsistency *)
  ( if is_false q then false_ q.us
  else if Equality.Subst.is_empty subst then q
  else
    (* normalize wrt solutions *)
    let q = norm subst q in
    (* reconjoin only non-redundant equations *)
    let removed =
      Var.Set.diff
        (Var.Set.union_list rev_xss)
        (fv ~ignore_cong:() (elim_exists q.xs q))
    in
    let keep, removed, _ = Equality.Subst.partition_valid removed subst in
    let q = and_subst keep q in
    (* remove the eliminated variables from xs and subformulas' us *)
    remove_absent_xs removed q )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a@ %a" Equality.Subst.pp subst pp_raw q' ;
    invariant q']

let simplify q =
  [%Trace.call fun {pf} -> pf "%a" pp_raw q]
  ;
  let q = freshen_nested_xs q in
  let q = propagate_equality Var.Set.empty Equality.true_ q in
  let q = simplify_ q.us [] q in
  q
  |>
  [%Trace.retn fun {pf} q' -> pf "@\n" ; invariant q']
