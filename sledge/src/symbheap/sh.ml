(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

[@@@warning "+9"]

type seg = {loc: Exp.t; bas: Exp.t; len: Exp.t; siz: Exp.t; arr: Exp.t}
[@@deriving compare, equal, sexp]

type starjunction =
  { us: Var.Set.t
  ; xs: Var.Set.t
  ; cong: Equality.t
  ; pure: Exp.t list
  ; heap: seg list
  ; djns: disjunction list }
[@@deriving compare, equal, sexp]

and disjunction = starjunction list

type t = starjunction [@@deriving compare, equal, sexp]

let map_seg {loc; bas; len; siz; arr} ~f =
  {loc= f loc; bas= f bas; len= f len; siz= f siz; arr= f arr}

let pp_seg fs {loc; bas; len; siz; arr} =
  Format.fprintf fs "@[<2>%a@ @[@[-[%a)->@]@ %a@]@]" Exp.pp loc
    (fun fs (bas, len) ->
      if (not (Exp.equal loc bas)) || not (Exp.equal len siz) then
        Format.fprintf fs " %a, %a " Exp.pp bas Exp.pp len )
    (bas, len) Exp.pp (Exp.memory ~siz ~arr)

let pp_us ?(pre = ("" : _ fmt)) fs us =
  if not (Set.is_empty us) then
    [%Trace.fprintf fs "%( %)@[%a@] .@ " pre Var.Set.pp us]

let rec pp_ vs fs {us; xs; cong; pure; heap; djns} =
  Format.pp_open_hvbox fs 0 ;
  pp_us fs us ;
  if not (Set.is_empty xs) then
    Format.fprintf fs "@<2>∃ @[%a@] .@ ∃ @[%a@] .@ " Var.Set.pp
      (Set.inter xs vs) Var.Set.pp (Set.diff xs vs) ;
  let first = Equality.is_true cong in
  if not first then Format.fprintf fs "  " ;
  Equality.pp_classes fs cong ;
  let pure_exps =
    List.filter_map pure ~f:(fun e ->
        let e' = Equality.normalize cong e in
        if Exp.is_true e' then None else Some e' )
  in
  List.pp
    ~pre:(if first then "  " else "@ @<2>∧ ")
    "@ @<2>∧ " Exp.pp fs pure_exps ;
  let first = first && List.is_empty pure_exps in
  if List.is_empty heap then
    Format.fprintf fs (if first then "emp" else "@ @<5>∧ emp")
  else
    List.pp
      ~pre:(if first then "  " else "@ @<2>∧ ")
      "@ * " pp_seg fs
      (List.sort
         (List.map ~f:(map_seg ~f:(Equality.normalize cong)) heap)
         ~compare:(fun s1 s2 ->
           let b_o = function
             | Exp.App {op= App {op= Add _; arg}; arg= Integer {data; _}} ->
                 (arg, data)
             | e -> (e, Z.zero)
           in
           [%compare: Exp.t * (Exp.t * Z.t)]
             (s1.bas, b_o s1.loc)
             (s2.bas, b_o s2.loc) )) ;
  List.pp ~pre:"@ * " "@ * "
    (pp_djn (Set.union vs (Set.union us xs)))
    fs djns ;
  Format.pp_close_box fs ()

and pp_djn vs fs = function
  | [] -> Format.fprintf fs "false"
  | djn ->
      Format.fprintf fs "@[<hv>( %a@ )@]"
        (List.pp "@ @<2>∨ " (fun fs sjn ->
             Format.fprintf fs "@[<hv 1>(%a)@]" (pp_ vs)
               {sjn with us= Set.diff sjn.us vs} ))
        djn

let pp = pp_ Var.Set.empty

let fold_exps_seg {loc; bas; len; siz; arr} ~init ~f =
  let f b z = Exp.fold_exps b ~init:z ~f in
  f loc (f bas (f len (f siz (f arr init))))

let fold_vars_seg seg ~init ~f =
  fold_exps_seg seg ~init ~f:(fun init -> Exp.fold_vars ~f ~init)

let fv_seg seg = fold_vars_seg seg ~f:Set.add ~init:Var.Set.empty

let fold_exps fold_exps {us= _; xs= _; cong; pure; heap; djns} ~init ~f =
  Equality.fold_exps ~init cong ~f
  |> fun init ->
  List.fold ~init pure ~f:(fun init -> Exp.fold_exps ~f ~init)
  |> fun init ->
  List.fold ~init heap ~f:(fun init -> fold_exps_seg ~f ~init)
  |> fun init ->
  List.fold ~init djns ~f:(fun init -> List.fold ~init ~f:fold_exps)

let rec fv_union init q =
  Set.diff
    (fold_exps fv_union q ~init ~f:(fun init ->
         Exp.fold_vars ~f:Set.add ~init ))
    q.xs

let fv q = fv_union Var.Set.empty q

let invariant_pure = function
  | Exp.Integer {data; typ} ->
      assert (Typ.equal Typ.bool typ) ;
      assert (not (Z.equal Z.zero data))
  | _ -> assert true

let invariant_seg _ = ()

let rec invariant q =
  Invariant.invariant [%here] q [%sexp_of: t]
  @@ fun () ->
  let {us; xs; cong; pure; heap; djns} = q in
  try
    assert (
      Set.disjoint us xs
      || Trace.fail "inter: @[%a@]@\nq: @[%a@]" Var.Set.pp (Set.inter us xs)
           pp q ) ;
    assert (
      Set.is_subset (fv q) ~of_:us
      || Trace.fail "unbound but free: %a" Var.Set.pp (Set.diff (fv q) us)
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
            assert (Set.is_subset sjn.us ~of_:(Set.union us xs)) ;
            invariant sjn ) )
  with exc -> [%Trace.info "%a" pp q] ; raise exc

(** Quantification and Vocabulary *)

let rename_seg sub ({loc; bas; len; siz; arr} as h) =
  let loc = Exp.rename loc sub in
  let bas = Exp.rename bas sub in
  let len = Exp.rename len sub in
  let siz = Exp.rename siz sub in
  let arr = Exp.rename arr sub in
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
  let pure =
    List.map_preserving_phys_equal pure ~f:(fun b -> Exp.rename b sub)
  in
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
  let sub = Var.Subst.exclude sub q.xs in
  let us = Var.Subst.apply_set sub q.us in
  let q' = apply_subst sub (freshen_xs q ~wrt:(Var.Subst.range sub)) in
  (if us == q.us && q' == q then q else {q' with us})
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Set.disjoint q'.us (Var.Subst.domain sub))]

(** freshen existentials, preserving vocabulary *)
and freshen_xs q ~wrt =
  [%Trace.call fun {pf} -> pf "{@[%a@]}@ %a" Var.Set.pp wrt pp q]
  ;
  let sub = Var.Subst.freshen q.xs ~wrt in
  let xs = Var.Subst.apply_set sub q.xs in
  let q' = apply_subst sub q in
  (if xs == q.xs && q' == q then q else {q' with xs})
  |>
  [%Trace.retn fun {pf} q' ->
    pf "%a" pp q' ;
    invariant q' ;
    assert (Set.equal q'.us q.us) ;
    assert (Set.disjoint q'.xs (Var.Subst.domain sub)) ;
    assert (Set.disjoint q'.xs (Set.inter q.xs wrt))]

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
    || Trace.fail "%a" Var.Set.pp (Set.diff xs q.us) ) ;
  {q with us= Set.diff q.us xs; xs= Set.union q.xs xs} |> check invariant
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
  [%Trace.call fun {pf} -> pf "%a@ %a" pp q1 pp q2]
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

let or_ q1 q2 =
  [%Trace.call fun {pf} -> pf "%a@ %a" pp q1 pp q2]
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

let rec pure (e : Exp.t) =
  [%Trace.call fun {pf} -> pf "%a" Exp.pp e]
  ;
  let us = Exp.fv e in
  ( match e with
  | Integer {data; typ= Integer {bits= 1}} ->
      if Z.equal Z.zero data then false_ us else emp
  | App {op= App {op= And; arg= e1}; arg= e2} -> star (pure e1) (pure e2)
  | App {op= App {op= Or; arg= e1}; arg= e2} -> or_ (pure e1) (pure e2)
  | App {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}
    ->
      or_
        (star (pure cnd) (pure thn))
        (star (pure (Exp.not_ Typ.bool cnd)) (pure els))
  | App {op= App {op= Eq; arg= e1}; arg= e2} ->
      let cong = Equality.(and_eq e1 e2 true_) in
      if Equality.is_false cong then false_ us
      else {emp with us; cong; pure= [e]}
  | _ -> {emp with us; pure= [e]} )
  |>
  [%Trace.retn fun {pf} q -> pf "%a" pp q ; invariant q]

let and_ e q = star (pure e) q
let seg pt = {emp with us= fv_seg pt; heap= [pt]} |> check invariant

(** Update *)

let with_pure pure q = {q with pure} |> check invariant

let rem_seg seg q =
  {q with heap= List.remove_exn q.heap seg} |> check invariant

(** Query *)

let is_emp = function
  | {us= _; xs= _; cong= _; pure= []; heap= []; djns= []} -> true
  | _ -> false

let is_false = function
  | {djns= [[]]; _} -> true
  | {cong; pure; _} ->
      List.exists pure ~f:(fun b -> Exp.is_false (Equality.normalize cong b))

let rec pure_approx ({us; xs; cong; pure; heap= _; djns} as q) =
  let heap = emp.heap in
  let djns =
    List.map_preserving_phys_equal djns ~f:(fun djn ->
        List.map_preserving_phys_equal djn ~f:pure_approx )
  in
  if heap == q.heap && djns == q.djns then q
  else {us; xs; cong; pure; heap; djns} |> check invariant

let fold_disjunctions sjn ~init ~f = List.fold sjn.djns ~init ~f
let fold_disjuncts djn ~init ~f = List.fold djn ~init ~f

let fold_dnf ~conj ~disj sjn conjuncts disjuncts =
  let rec add_disjunct pending_splits sjn (conjuncts, disjuncts) =
    split_case
      (fold_disjunctions sjn ~init:pending_splits
         ~f:(fun pending_splits split -> split :: pending_splits ))
      (conj {sjn with djns= []} conjuncts, disjuncts)
  and split_case pending_splits (conjuncts, disjuncts) =
    match pending_splits with
    | split :: pending_splits ->
        fold_disjuncts split ~init:disjuncts ~f:(fun disjuncts sjn ->
            add_disjunct pending_splits sjn (conjuncts, disjuncts) )
    | [] -> disj conjuncts disjuncts
  in
  add_disjunct [] sjn (conjuncts, disjuncts)

let dnf q =
  let conj sjn conjuncts =
    assert (List.is_empty sjn.djns) ;
    assert (List.is_empty conjuncts.djns) ;
    star conjuncts sjn
  in
  let disj conjuncts disjuncts =
    assert (match conjuncts.djns with [] | [[]] -> true | _ -> false) ;
    conjuncts :: disjuncts
  in
  fold_dnf ~conj ~disj q emp []
