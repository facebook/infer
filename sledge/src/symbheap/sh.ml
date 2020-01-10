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

let map_seg {loc; bas; len; siz; arr} ~f =
  {loc= f loc; bas= f bas; len= f len; siz= f siz; arr= f arr}

let fold_terms_seg {loc; bas; len; siz; arr} ~init ~f =
  let f b s = f s b in
  f loc (f bas (f len (f siz (f arr init))))

let fold_vars_seg seg ~init ~f =
  fold_terms_seg seg ~init ~f:(fun init -> Term.fold_vars ~f ~init)

let fold_vars fold_vars {us= _; xs= _; cong; pure; heap; djns} ~init ~f =
  Equality.fold_terms ~init cong ~f:(fun init -> Term.fold_vars ~f ~init)
  |> fun init ->
  List.fold ~init pure ~f:(fun init -> Term.fold_vars ~f ~init)
  |> fun init ->
  List.fold ~init heap ~f:(fun init -> fold_vars_seg ~f ~init)
  |> fun init ->
  List.fold ~init djns ~f:(fun init -> List.fold ~init ~f:fold_vars)

let var_strength q =
  let rec var_strength_ xs m q =
    let xs = Set.union xs q.xs in
    fold_vars (var_strength_ xs) {q with cong= Equality.true_} ~init:m
      ~f:(fun m var ->
        if not (Set.mem xs var) then Map.set m ~key:var ~data:`Universal
        else
          match Map.find m var with
          | None -> Map.set m ~key:var ~data:`Anonymous
          | Some `Anonymous -> Map.set m ~key:var ~data:`Existential
          | Some _ -> m )
  in
  var_strength_ Var.Set.empty Var.Map.empty q

let pp_seg x fs {loc; bas; len; siz; arr} =
  let term_pp = Term.ppx x in
  Format.fprintf fs "@[<2>%a@ @[@[-[%a)->@]@ %a@]@]" term_pp loc
    (fun fs (bas, len) ->
      if (not (Term.equal loc bas)) || not (Term.equal len siz) then
        Format.fprintf fs " %a, %a " term_pp bas term_pp len )
    (bas, len) term_pp (Term.memory ~siz ~arr)

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
    List.pp "@,^" (fun fs seg ->
        term_pp fs (Term.memory ~siz:seg.siz ~arr:seg.arr) )
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
        let const = Qset.count poly Term.one in
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

let pp_us ?(pre = ("" : _ fmt)) fs us =
  if not (Set.is_empty us) then
    [%Trace.fprintf fs "%( %)@[%a@] .@ " pre Var.Set.pp us]

let rec pp_ ?var_strength vs parent_cong fs {us; xs; cong; pure; heap; djns}
    =
  Format.pp_open_hvbox fs 0 ;
  let x v = Option.bind ~f:(fun m -> Map.find m v) var_strength in
  pp_us fs us ;
  let xs_d_vs, xs_i_vs =
    Set.diff_inter
      (Set.filter xs ~f:(fun v -> Poly.(x v <> Some `Anonymous)))
      vs
  in
  if not (Set.is_empty xs_i_vs) then (
    Format.fprintf fs "@<2>∃ @[%a@] ." (Var.Set.ppx x) xs_i_vs ;
    if not (Set.is_empty xs_d_vs) then Format.fprintf fs "@ " ) ;
  if not (Set.is_empty xs_d_vs) then
    Format.fprintf fs "@<2>∃ @[%a@] .@ " (Var.Set.ppx x) xs_d_vs ;
  let first = Equality.entails parent_cong cong in
  if not first then Format.fprintf fs "  " ;
  Equality.pp_classes_diff x fs (parent_cong, cong) ;
  let pure =
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
    (pp_djn ?var_strength (Set.union vs (Set.union us xs)) cong)
    fs djns ;
  Format.pp_close_box fs ()

and pp_djn ?var_strength:parent_var_strength vs cong fs = function
  | [] -> Format.fprintf fs "false"
  | djn ->
      Format.fprintf fs "@[<hv>( %a@ )@]"
        (List.pp "@ @<2>∨ " (fun fs sjn ->
             let var_strength =
               let combine ~key sjn_v m_v =
                 if Set.mem sjn.xs key then sjn_v else m_v
               in
               Option.map
                 ~f:(Map.merge_skewed ~combine (var_strength sjn))
                 parent_var_strength
             in
             Format.fprintf fs "@[<hv 1>(%a)@]"
               (pp_ ?var_strength vs cong)
               {sjn with us= Set.diff sjn.us vs} ))
        djn

let pp_diff_eq cong fs q =
  pp_ ~var_strength:(var_strength q) Var.Set.empty cong fs q

let pp fs q = pp_diff_eq Equality.true_ fs q
let pp_djn fs d = pp_djn Var.Set.empty Equality.true_ fs d
let fv_seg seg = fold_vars_seg seg ~f:Set.add ~init:Var.Set.empty

let rec fv_union init q =
  Set.diff (fold_vars fv_union q ~init ~f:Set.add) q.xs

let fv q = fv_union Var.Set.empty q

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
      Set.disjoint us xs
      || fail "inter: @[%a@]@\nq: @[%a@]" Var.Set.pp (Set.inter us xs) pp q
           () ) ;
    assert (
      Set.is_subset (fv q) ~of_:us
      || fail "unbound but free: %a" Var.Set.pp (Set.diff (fv q) us) () ) ;
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
            assert (Set.is_subset sjn.us ~of_:(Set.union us xs)) ;
            invariant sjn ) )
  with exc -> [%Trace.info "%a" pp q] ; raise exc

let rec simplify {us; xs; cong; pure; heap; djns} =
  [%Trace.call fun {pf} -> pf "%a" pp {us; xs; cong; pure; heap; djns}]
  ;
  let heap = List.map heap ~f:(map_seg ~f:(Equality.normalize cong)) in
  let pure = List.map pure ~f:(Equality.normalize cong) in
  let cong = Equality.true_ in
  let djns = List.map djns ~f:(List.map ~f:simplify) in
  let all_vars =
    fv {us= Set.union us xs; xs= Var.Set.empty; cong; pure; heap; djns}
  in
  let xs = Set.inter all_vars xs in
  let us = Set.inter all_vars us in
  {us; xs; cong; pure; heap; djns} |> check invariant
  |>
  [%Trace.retn fun {pf} s -> pf "%a" pp s]

(** Quantification and Vocabulary *)

let rename_seg sub ({loc; bas; len; siz; arr} as h) =
  let loc = Term.rename sub loc in
  let bas = Term.rename sub bas in
  let len = Term.rename sub len in
  let siz = Term.rename sub siz in
  let arr = Term.rename sub arr in
  ( if
    loc == h.loc && bas == h.bas && len == h.len && siz == h.siz
    && arr == h.arr
  then h
  else {loc; bas; len; siz; arr} )
  |> check (fun h' ->
         assert (Set.disjoint (fv_seg h') (Var.Subst.domain sub)) )

(** primitive application of a substitution, ignores us and xs, may violate
    invariant *)
let rec apply_subst sub ({us= _; xs= _; cong; pure; heap; djns} as q) =
  let cong = Equality.rename cong sub in
  let pure = List.map_preserving_phys_equal pure ~f:(Term.rename sub) in
  let heap = List.map_preserving_phys_equal heap ~f:(rename_seg sub) in
  let djns =
    List.map_preserving_phys_equal djns ~f:(fun d ->
        List.map_preserving_phys_equal d ~f:(fun q -> rename sub q) )
  in
  ( if cong == q.cong && pure == q.pure && heap == q.heap && djns == q.djns
  then q
  else {q with cong; pure; heap; djns} )
  |> check (fun q' -> assert (Set.disjoint (fv q') (Var.Subst.domain sub)))

and rename sub q =
  [%Trace.call fun {pf} -> pf "@[%a@]@ %a" Var.Subst.pp sub pp q]
  ;
  let sub = Var.Subst.restrict sub q.us in
  ( if Var.Subst.is_empty sub then q
  else
    let us = Var.Subst.apply_set sub q.us in
    assert (not (Set.equal us q.us)) ;
    let q' = apply_subst sub (freshen_xs q ~wrt:(Set.union q.us us)) in
    {q' with us} )
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Set.disjoint q'.us (Var.Subst.domain sub))]

(** freshen existentials, preserving vocabulary *)
and freshen_xs q ~wrt =
  [%Trace.call fun {pf} ->
    pf "{@[%a@]}@ %a" Var.Set.pp wrt pp q ;
    assert (Set.is_subset q.us ~of_:wrt)]
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
    assert (Set.equal q'.us q.us) ;
    assert (Set.disjoint q'.xs (Var.Subst.domain sub)) ;
    assert (Set.disjoint q'.xs (Set.inter q.xs wrt)) ;
    invariant q']

let extend_us us q =
  let us = Set.union us q.us in
  let q' = freshen_xs q ~wrt:us in
  (if us == q.us && q' == q then q else {q' with us}) |> check invariant

let freshen ~wrt q =
  let sub = Var.Subst.freshen q.us ~wrt:(Set.union wrt q.xs) in
  let q' = extend_us wrt (rename sub q) in
  (if q' == q then (q, sub) else (q', sub))
  |> check (fun (q', _) ->
         invariant q' ;
         assert (Set.is_subset wrt ~of_:q'.us) ;
         assert (Set.disjoint wrt (fv q')) )

let bind_exists q ~wrt =
  [%Trace.call fun {pf} -> pf "{@[%a@]}@ %a" Var.Set.pp wrt pp q]
  ;
  let q' = freshen_xs q ~wrt:(Set.union q.us wrt) in
  (q'.xs, {q' with us= Set.union q'.us q'.xs; xs= Var.Set.empty})
  |>
  [%Trace.retn fun {pf} (_, q') -> pf "%a" pp q']

let exists xs q =
  [%Trace.call fun {pf} -> pf "{@[%a@]}@ %a" Var.Set.pp xs pp q]
  ;
  assert (
    Set.is_subset xs ~of_:q.us
    || fail "Sh.exists xs - q.us: %a" Var.Set.pp (Set.diff xs q.us) () ) ;
  ( if Set.is_empty xs then q
  else
    {q with us= Set.diff q.us xs; xs= Set.union q.xs xs} |> check invariant
  )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

(** Construct *)

let emp =
  { us= Var.Set.empty
  ; xs= Var.Set.empty
  ; cong= Equality.true_
  ; pure= []
  ; heap= []
  ; djns= [] }
  |> check invariant

let false_ us = {emp with us; djns= [[]]} |> check invariant

let and_cong cong q =
  [%Trace.call fun {pf} -> pf "%a@ %a" Equality.pp cong pp q]
  ;
  let q = extend_us (Equality.fv cong) q in
  let cong = Equality.and_ q.cong cong in
  (if Equality.is_false cong then false_ q.us else {q with cong})
  |>
  [%Trace.retn fun {pf} q -> pf "%a" pp q ; invariant q]

let star q1 q2 =
  [%Trace.call fun {pf} -> pf "(%a)@ (%a)" pp q1 pp q2]
  ;
  ( match (q1, q2) with
  | {djns= [[]]; _}, _ | _, {djns= [[]]; _} ->
      false_ (Set.union q1.us q2.us)
  | {us= _; xs= _; cong; pure= []; heap= []; djns= []}, _
    when Equality.is_true cong ->
      let us = Set.union q1.us q2.us in
      if us == q2.us then q2 else {q2 with us}
  | _, {us= _; xs= _; cong; pure= []; heap= []; djns= []}
    when Equality.is_true cong ->
      let us = Set.union q1.us q2.us in
      if us == q1.us then q1 else {q1 with us}
  | _ ->
      let us = Set.union q1.us q2.us in
      let q1 = freshen_xs q1 ~wrt:(Set.union us q2.xs) in
      let q2 = freshen_xs q2 ~wrt:(Set.union us q1.xs) in
      let {us= us1; xs= xs1; cong= c1; pure= p1; heap= h1; djns= d1} = q1 in
      let {us= us2; xs= xs2; cong= c2; pure= p2; heap= h2; djns= d2} = q2 in
      assert (Set.equal us (Set.union us1 us2)) ;
      let cong = Equality.and_ c1 c2 in
      if Equality.is_false cong then false_ us
      else
        { us
        ; xs= Set.union xs1 xs2
        ; cong
        ; pure= List.append p1 p2
        ; heap= List.append h1 h2
        ; djns= List.append d1 d2 } )
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp q ;
    invariant q ;
    assert (Set.equal q.us (Set.union q1.us q2.us))]

let stars = function
  | [] -> emp
  | [q] -> q
  | q :: qs -> List.fold ~f:star ~init:q qs

let or_ q1 q2 =
  [%Trace.call fun {pf} -> pf "(%a)@ (%a)" pp q1 pp q2]
  ;
  ( match (q1, q2) with
  | {djns= [[]]; _}, _ ->
      let us = Set.union q1.us q2.us in
      if us == q2.us then q2 else {q2 with us}
  | _, {djns= [[]]; _} ->
      let us = Set.union q1.us q2.us in
      if us == q1.us then q1 else {q1 with us}
  | ( ({djns= []; _} as q)
    , ({us= _; xs; cong= _; pure= []; heap= []; djns= [djn]} as d) )
    when Set.is_empty xs ->
      {d with us= Set.union q.us d.us; djns= [q :: djn]}
  | ( ({us= _; xs; cong= _; pure= []; heap= []; djns= [djn]} as d)
    , ({djns= []; _} as q) )
    when Set.is_empty xs ->
      {d with us= Set.union q.us d.us; djns= [q :: djn]}
  | _ ->
      { us= Set.union q1.us q2.us
      ; xs= Var.Set.empty
      ; cong= Equality.true_
      ; pure= []
      ; heap= []
      ; djns= [[q1; q2]] } )
  |>
  [%Trace.retn fun {pf} q ->
    pf "%a" pp q ;
    invariant q ;
    assert (Set.equal q.us (Set.union q1.us q2.us))]

let rec pure (e : Term.t) =
  [%Trace.call fun {pf} -> pf "%a" Term.pp e]
  ;
  let us = Term.fv e in
  let eq_false b =
    let cong = Equality.and_eq b Term.false_ Equality.true_ in
    {emp with us; cong; pure= [e]}
  in
  ( match e with
  | Integer {data} -> if Z.is_false data then false_ us else emp
  (* ¬b ==> false = b *)
  | Ap2 (Xor, Integer {data}, arg) when Z.is_true data -> eq_false arg
  | Ap2 (Xor, arg, Integer {data}) when Z.is_true data -> eq_false arg
  | Ap2 (And, e1, e2) -> star (pure e1) (pure e2)
  | Ap2 (Or, e1, e2) -> or_ (pure e1) (pure e2)
  | Ap3 (Conditional, cnd, thn, els) ->
      or_
        (star (pure cnd) (pure thn))
        (star (pure (Term.not_ cnd)) (pure els))
  | Ap2 (Eq, e1, e2) ->
      let cong = Equality.(and_eq e1 e2 true_) in
      if Equality.is_false cong then false_ us
      else {emp with us; cong; pure= [e]}
  | _ -> {emp with us; pure= [e]} )
  |>
  [%Trace.retn fun {pf} q -> pf "%a" pp q ; invariant q]

let and_ e q = star (pure e) q

let subst sub q =
  [%Trace.call fun {pf} -> pf "@[%a@]@ %a" Var.Subst.pp sub pp q]
  ;
  let dom, eqs =
    Var.Subst.fold sub ~init:(Var.Set.empty, Term.true_)
      ~f:(fun var trm (dom, eqs) ->
        ( Set.add dom var
        , Term.and_ (Term.eq (Term.var var) (Term.var trm)) eqs ) )
  in
  exists dom (and_ eqs q)
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Set.disjoint q'.us (Var.Subst.domain sub))]

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
    List.map_preserving_phys_equal djns ~f:(fun djn ->
        List.map_preserving_phys_equal djn ~f:pure_approx )
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
    let xs = Set.union ys xs in
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
    exists xs (stars conjuncts) :: disjuncts
  in
  fold_dnf ~conj ~disj q (Var.Set.empty, []) []
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp_djn]
