(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Frame Inference Solver over Symbolic Heaps *)

module Goal : sig
  (** Excision judgment

      ∀us. Common ❮ Minuend ⊢ ∃xs. Subtrahend ❯ ∃zs. Remainder

      is valid iff

      Common * Minuend ⊧ ∃xs. Common * Subtrahend * ∃zs. Remainder

      is universally valid semantically.

      Terminology analogous to arithmetic subtraction is used: "minuend" is
      a formula from which another, the subtrahend, is to be subtracted; and
      "subtrahend" is a formula to be subtracted from another, the minuend. *)
  type t = private
    { us: Var.Set.t  (** (universal) vocabulary of entire judgment *)
    ; com: Sh.t  (** common star-conjunct of minuend and subtrahend *)
    ; min: Sh.t
          (** minuend, cong strengthened by pure_approx (com * min) *)
    ; xs: Var.Set.t  (** existentials over subtrahend and remainder *)
    ; sub: Sh.t  (** subtrahend, cong strengthened by min.cong *)
    ; zs: Var.Set.t  (** existentials over remainder *)
    ; pgs: bool  (** indicates whether a deduction rule has been applied *)
    }

  val pp : t pp

  val goal :
       us:Var.Set.t
    -> com:Sh.t
    -> min:Sh.t
    -> xs:Var.Set.t
    -> sub:Sh.t
    -> zs:Var.Set.t
    -> pgs:bool
    -> t

  val with_ :
       ?us:Var.Set.t
    -> ?com:Sh.t
    -> ?min:Sh.t
    -> ?xs:Var.Set.t
    -> ?sub:Sh.t
    -> ?zs:Var.Set.t
    -> ?pgs:bool
    -> t
    -> t
end = struct
  type t =
    { us: Var.Set.t
    ; com: Sh.t
    ; min: Sh.t
    ; xs: Var.Set.t
    ; sub: Sh.t
    ; zs: Var.Set.t
    ; pgs: bool }
  [@@deriving sexp]

  let pp fs {com; min; xs; sub; pgs} =
    Format.fprintf fs "@[<hv>%s %a@ | %a@ @[\\- %a%a@]@]"
      (if pgs then "t" else "f")
      Sh.pp com Sh.pp min Var.Set.pp_xs xs (Sh.pp_diff_eq min.cong) sub

  let invariant g =
    Invariant.invariant [%here] g [%sexp_of: t]
    @@ fun () ->
    try
      let {us; com; min; xs; sub; zs; pgs= _} = g in
      assert (Set.equal us com.us) ;
      assert (Set.equal us min.us) ;
      assert (Set.equal (Set.union us xs) sub.us) ;
      assert (Set.disjoint us xs) ;
      assert (Set.is_subset zs ~of_:(Set.union us xs))
    with exc -> [%Trace.info "%a" pp g] ; raise exc

  let with_ ?us ?com ?min ?xs ?sub ?zs ?pgs g =
    let xs = Option.value xs ~default:g.xs in
    let zs = Option.value zs ~default:g.zs in
    let new_us =
      let us = Option.value us ~default:Var.Set.empty in
      let us =
        Option.fold
          ~f:(fun us sub -> Set.union (Set.diff sub.Sh.us xs) us)
          sub ~init:us
      in
      let union_us q_opt us' =
        Option.fold ~f:(fun us' q -> Set.union q.Sh.us us') q_opt ~init:us'
      in
      union_us com (union_us min us)
    in
    let com = Sh.extend_us new_us (Option.value com ~default:g.com) in
    let min = Sh.extend_us new_us (Option.value min ~default:g.min) in
    let xs, sub, zs =
      match sub with
      | Some sub ->
          let sub = Sh.extend_us (Set.union new_us xs) sub in
          let ys, sub = Sh.bind_exists sub ~wrt:xs in
          let xs = Set.union xs ys in
          let zs = Set.union zs ys in
          (xs, sub, zs)
      | None ->
          let sub = Sh.extend_us new_us (Option.value sub ~default:g.sub) in
          (xs, sub, zs)
    in
    let pgs = Option.value pgs ~default:g.pgs in
    {us= min.us; com; min; xs; sub; zs; pgs} |> check invariant

  let goal ~us ~com ~min ~xs ~sub ~zs ~pgs =
    with_ ~us ~com ~min ~xs ~sub ~zs ~pgs
      { us= Var.Set.empty
      ; com= Sh.emp
      ; min= Sh.emp
      ; xs= Var.Set.empty
      ; sub= Sh.emp
      ; zs= Var.Set.empty
      ; pgs= false }
end

open Goal

let fresh_var name vs zs ~wrt =
  let v, wrt = Var.fresh name ~wrt in
  let vs = Set.add vs v in
  let zs = Set.add zs v in
  let v = Term.var v in
  (v, vs, zs, wrt)

let excise k = [%Trace.infok k]
let trace k = [%Trace.infok k]

let excise_exists goal =
  trace (fun {pf} -> pf "@[<2>excise_exists@ %a@]" pp goal) ;
  if Set.is_empty goal.xs then goal
  else
    let solutions_for_xs =
      let xs =
        Set.diff goal.xs (Sh.fv ~ignore_cong:() (Sh.with_pure [] goal.sub))
      in
      Equality.solve_for_vars [Var.Set.empty; goal.us; xs] goal.sub.cong
    in
    if Equality.Subst.is_empty solutions_for_xs then goal
    else
      let removed =
        Set.diff goal.xs
          (Sh.fv ~ignore_cong:() (Sh.norm solutions_for_xs goal.sub))
      in
      if Set.is_empty removed then goal
      else
        let _, removed, witnesses =
          Equality.Subst.partition_valid removed solutions_for_xs
        in
        if Equality.Subst.is_empty witnesses then goal
        else (
          excise (fun {pf} ->
              pf "@[<2>excise_exists @[%a%a@]@]" Var.Set.pp_xs removed
                Equality.Subst.pp witnesses ) ;
          let us = Set.union goal.us removed in
          let xs = Set.diff goal.xs removed in
          let min = Sh.and_subst witnesses goal.min in
          goal |> with_ ~us ~min ~xs ~pgs:true )

let excise_term ({min} as goal) pure term =
  let term' = Equality.normalize min.cong term in
  if Term.is_false term' then None
  else if Term.is_true term' then (
    excise (fun {pf} -> pf "excise_pure %a" Term.pp term) ;
    Some (goal |> with_ ~pgs:true, pure) )
  else Some (goal, term' :: pure)

let excise_pure ({sub} as goal) =
  trace (fun {pf} -> pf "@[<2>excise_pure@ %a@]" pp goal) ;
  let+ goal, pure =
    List.fold_option sub.pure ~init:(goal, []) ~f:(fun (goal, pure) term ->
        excise_term goal pure term )
  in
  goal |> with_ ~sub:(Sh.with_pure pure sub)

(*   [k; o)
 * ⊢ [l; n)
 *
 *  _ ⊢ k=l * o=n
 *
 *  ∀us.
 *    C * k-[b;m)->⟨o,α⟩
 *  ❮ M
 *  ⊢ ∃xs.
 *    b=b' * m=m' * α=α' * S                                 ❯ R
 * --------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ R
 *)
let excise_seg_same ({com; min; sub} as goal) msg ssg =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_same@ %a@ \\- %a@]" (Sh.pp_seg_norm sub.cong)
        msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.bas= b; len= m; arr= a} = msg in
  let {Sh.bas= b'; len= m'; arr= a'} = ssg in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m') (Sh.and_ (Term.eq a a') (Sh.rem_seg ssg sub)))
  in
  goal |> with_ ~com ~min ~sub

(*   [k;   o)
 * ⊢ [l; n)
 *
 *  _ ⊢ k=l * o>n
 *
 *  ∀us,α₀,α₁.
 *    C * k-[b;m)->⟨n,α₀⟩
 *  ❮ k+n-[b;m)->⟨o-n,α₁⟩ * ⟨o,α⟩=⟨n,α₀⟩^⟨o-n,α₁⟩ * M
 *  ⊢ ∃xs.
 *    b=b' * m=m' * α₀=α' * S                                ❯ R
 * ----------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀,α₁. R
 *)
let excise_seg_sub_prefix ({us; com; min; xs; sub; zs} as goal) msg ssg o_n
    =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_sub_prefix@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let o_n = Term.integer o_n in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, _ = fresh_var "a1" us zs ~wrt in
  let xs = Set.diff xs (Term.fv n) in
  let com = Sh.star (Sh.seg {msg with siz= n; arr= a0}) com in
  let min =
    Sh.and_
      (Term.eq_concat (o, a) [|(n, a0); (o_n, a1)|])
      (Sh.star
         (Sh.seg {loc= Term.add k n; bas= b; len= m; siz= o_n; arr= a1})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_ (Term.eq a0 a') (Sh.rem_seg ssg sub)))
  in
  goal |> with_ ~us ~com ~min ~xs ~sub ~zs

(*   [k; o)
 * ⊢ [l;   n)
 *
 *  _ ⊢ k=l * o<n
 *
 *  ∀us.
 *    C * k-[b;m)->⟨o,α⟩
 *  ❮ M
 *  ⊢ ∃xs,α₁'.
 *    b=b' * m=m' * ⟨o,α⟩^⟨n-o,α₁'⟩=⟨n,α'⟩
 *    * l+o-[b';m')->⟨n-o,α₁'⟩ * S                           ❯ R
 * --------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₁'. R
 *)
let excise_seg_min_prefix ({us; com; min; xs; sub; zs} as goal) msg ssg n_o
    =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_min_prefix@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let n_o = Term.integer n_o in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let a1', xs, zs, _ = fresh_var "a1" xs zs ~wrt:(Set.union us xs) in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_
            (Term.eq_concat (n, a') [|(o, a); (n_o, a1')|])
            (Sh.star
               (Sh.seg
                  {loc= Term.add l o; bas= b'; len= m'; siz= n_o; arr= a1'})
               (Sh.rem_seg ssg sub))))
  in
  goal |> with_ ~com ~min ~xs ~sub ~zs

(*   [k;    o)
 * ⊢    [l; n)
 *
 *  _ ⊢ k<l * k+o=l+n
 *
 *  ∀us,α₀,α₁.
 *    C * l-[b;m)->⟨n,α₁⟩
 *  ❮ ⟨o,α⟩=⟨l-k,α₀⟩^⟨n,α₁⟩ * k-[b;m)->⟨l-k,α₀⟩ * M
 *  ⊢ ∃xs.
 *    b=b' * m=m' * α₁=α' * S                                ❯ R
 * ----------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀,α₁. R
 *)
let excise_seg_sub_suffix ({us; com; min; xs; sub; zs} as goal) msg ssg l_k
    =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_sub_suffix@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let l_k = Term.integer l_k in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, _ = fresh_var "a1" us zs ~wrt in
  let xs = Set.diff xs (Term.fv n) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a1}) com
  in
  let min =
    Sh.and_
      (Term.eq_concat (o, a) [|(l_k, a0); (n, a1)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; arr= a0})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_ (Term.eq a1 a') (Sh.rem_seg ssg sub)))
  in
  goal |> with_ ~us ~com ~min ~xs ~sub ~zs

(*   [k;      o)
 * ⊢    [l; n)
 *
 *  _ ⊢ k<l * k+o>l+n
 *
 *  ∀us,α₀,α₁,α₂.
 *    C * l-[b;m)->⟨n,α₁⟩
 *  ❮ k-[b;m)->⟨l-k,α₀⟩ * l+n-[b;m)->⟨k+o-(l+n),α₂⟩
 *    * ⟨o,α⟩=⟨l-k,α₀⟩^⟨n,α₁⟩^⟨k+o-(l+n),α₂⟩ * M
 *  ⊢ ∃xs.
 *    b=b' * m=m' * α₁=α' * S                                ❯ R
 * -------------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀,α₁,α₂. R
 *)
let excise_seg_sub_infix ({us; com; min; xs; sub; zs} as goal) msg ssg l_k
    ko_ln =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_sub_infix@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let l_k = Term.integer l_k in
  let ko_ln = Term.integer ko_ln in
  let ln = Term.add l n in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2, us, zs, _ = fresh_var "a2" us zs ~wrt in
  let xs = Set.diff xs (Set.union (Term.fv l) (Term.fv n)) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a1}) com
  in
  let min =
    Sh.and_
      (Term.eq_concat (o, a) [|(l_k, a0); (n, a1); (ko_ln, a2)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; arr= a0})
         (Sh.star
            (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; arr= a2})
            (Sh.rem_seg msg min)))
  in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_ (Term.eq a1 a') (Sh.rem_seg ssg sub)))
  in
  goal |> with_ ~us ~com ~min ~xs ~sub ~zs

(*   [k;   o)
 * ⊢    [l;  n)
 *
 *  _ ⊢ k<l * l<k+o * k+o<l+n
 *
 *  ∀us,α₀,α₁.
 *    C * l-[b;m)->⟨k+o-l,α₁⟩
 *  ❮ ⟨o,α⟩=⟨l-k,α₀⟩^⟨k+o-l,α₁⟩ * k-[b;m)->⟨l-k,α₀⟩ * M
 *  ⊢ ∃xs,α₂'.
 *    b=b' * m=m' * ⟨k+o-l,α₁⟩^⟨l+n-(k+o),α₂'⟩=⟨n,α'⟩
 *    * k+o-[b';m')->⟨l+n-(k+o),α₂'⟩ * S                     ❯ R
 * --------------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀,α₁,α₂'. R
 *)
let excise_seg_min_skew ({us; com; min; xs; sub; zs} as goal) msg ssg l_k
    ko_l ln_ko =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_min_skew@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let l_k = Term.integer l_k in
  let ko_l = Term.integer ko_l in
  let ln_ko = Term.integer ln_ko in
  let ko = Term.add k o in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2', xs, zs, _ = fresh_var "a2" xs zs ~wrt in
  let xs = Set.diff xs (Term.fv l) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= ko_l; arr= a1}) com
  in
  let min =
    Sh.and_
      (Term.eq_concat (o, a) [|(l_k, a0); (ko_l, a1)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; arr= a0})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_
            (Term.eq_concat (n, a') [|(ko_l, a1); (ln_ko, a2')|])
            (Sh.star
               (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; arr= a2'})
               (Sh.rem_seg ssg sub))))
  in
  goal |> with_ ~us ~com ~min ~xs ~sub ~zs

(*      [k; o)
 * ⊢ [l;    n)
 *
 *  _ ⊢ k>l * k+o=l+n
 *
 *  ∀us.
 *    C * k-[b;m)->⟨o,α⟩
 *  ❮ M
 *  ⊢ ∃xs,α₀'.
 *    b=b' * m=m' * ⟨k-l,α₀'⟩^⟨o,α⟩=⟨n,α'⟩
 *    * l-[b';m')->⟨k-l,α₀'⟩ * S                             ❯ R
 * --------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀'. R
 *)
let excise_seg_min_suffix ({us; com; min; xs; sub; zs} as goal) msg ssg k_l
    =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_min_suffix@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let k_l = Term.integer k_l in
  let a0', xs, zs, _ = fresh_var "a0" xs zs ~wrt:(Set.union us xs) in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_
            (Term.eq_concat (n, a') [|(k_l, a0'); (o, a)|])
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; arr= a0'})
               (Sh.rem_seg ssg sub))))
  in
  goal |> with_ ~com ~min ~xs ~sub ~zs

(*      [k; o)
 * ⊢ [l;      n)
 *
 *  _ ⊢ k>l * k+o<l+n
 *
 *  ∀us.
 *    C * k-[b;m)->⟨o,α⟩
 *  ❮ M
 *  ⊢ ∃xs,α₀',α₂'.
 *    b=b' * m=m' * ⟨k-l,α₀'⟩^⟨o,α⟩^⟨l+n-(k+o),α₂'⟩=⟨n,α'⟩
 *    * l-[b';m')->⟨k-l,α₀'⟩
 *    * k+o-[b';m')->⟨l+n-(k+o),α₂'⟩ * S                     ❯ R
 * ------------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀',α₂'. R
 *)
let excise_seg_min_infix ({us; com; min; xs; sub; zs} as goal) msg ssg k_l
    ln_ko =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_min_infix@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let k_l = Term.integer k_l in
  let ln_ko = Term.integer ln_ko in
  let ko = Term.add k o in
  let a0', xs, zs, wrt = fresh_var "a0" xs zs ~wrt:(Set.union us xs) in
  let a2', xs, zs, _ = fresh_var "a2" xs zs ~wrt in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_
            (Term.eq_concat (n, a') [|(k_l, a0'); (o, a); (ln_ko, a2')|])
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; arr= a0'})
               (Sh.star
                  (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; arr= a2'})
                  (Sh.rem_seg ssg sub)))))
  in
  goal |> with_ ~com ~min ~xs ~sub ~zs

(*      [k;  o)
 * ⊢ [l;   n)
 *
 *  _ ⊢ l<k * k<l+n * l+n<k+o
 *
 *  ∀us,α₁,α₂.
 *    C * k-[b;m)->⟨l+n-k,α₁⟩
 *  ❮ ⟨o,α⟩=⟨l+n-k,α₁⟩^⟨k+o-(l+n),α₂⟩ * l+n-[b;m)->⟨k+o-(l+n),α₂⟩ * M
 *  ⊢ ∃xs,α₀'.
 *    b=b' * m=m' * ⟨k-l,α₀'⟩^⟨l+n-k,α₁⟩=⟨n,α'⟩
 *    * l-[b';m')->⟨k-l,α₀'⟩ * S                             ❯ R
 * --------------------------------------------------------------------------
 *  ∀us. C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ ∃α₀',α₁,α₂. R
 *)
let excise_seg_sub_skew ({us; com; min; xs; sub; zs} as goal) msg ssg k_l
    ln_k ko_ln =
  excise (fun {pf} ->
      pf "@[<hv 2>excise_seg_sub_skew@ %a@ \\- %a@]"
        (Sh.pp_seg_norm sub.cong) msg (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let k_l = Term.integer k_l in
  let ln_k = Term.integer ln_k in
  let ko_ln = Term.integer ko_ln in
  let ln = Term.add l n in
  let a0', xs, zs, wrt = fresh_var "a0" xs zs ~wrt:(Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2, us, zs, _ = fresh_var "a2" us zs ~wrt in
  let com =
    Sh.star (Sh.seg {loc= k; bas= b; len= m; siz= ln_k; arr= a1}) com
  in
  let min =
    Sh.and_
      (Term.eq_concat (o, a) [|(ln_k, a1); (ko_ln, a2)|])
      (Sh.star
         (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; arr= a2})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Term.eq b b')
      (Sh.and_ (Term.eq m m')
         (Sh.and_
            (Term.eq_concat (n, a') [|(k_l, a0'); (ln_k, a1)|])
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; arr= a0'})
               (Sh.rem_seg ssg sub))))
  in
  goal |> with_ ~us ~com ~min ~xs ~sub ~zs

(* C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ R *)
let excise_seg ({sub} as goal) msg ssg =
  trace (fun {pf} ->
      pf "@[<2>excise_seg@ %a@  |-  %a@]" (Sh.pp_seg_norm sub.cong) msg
        (Sh.pp_seg_norm sub.cong) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n} = ssg in
  let* k_l = Equality.difference sub.cong k l in
  if
    (not (Equality.entails_eq sub.cong b b'))
    || not (Equality.entails_eq sub.cong m m')
  then
    Some
      ( goal
      |> with_
           ~sub:(Sh.and_ (Term.eq b b') (Sh.and_ (Term.eq m m') goal.sub))
      )
  else
    match Int.sign (Z.sign k_l) with
    (* k-l < 0 so k < l *)
    | Neg -> (
        let ko = Term.add k o in
        let ln = Term.add l n in
        let* ko_ln = Equality.difference sub.cong ko ln in
        match Int.sign (Z.sign ko_ln) with
        (* k+o-(l+n) < 0 so k+o < l+n *)
        | Neg -> (
            let* l_ko = Equality.difference sub.cong l ko in
            match Int.sign (Z.sign l_ko) with
            (* l-(k+o) < 0     [k;   o)
             * so l < k+o    ⊢    [l;  n) *)
            | Neg ->
                Some
                  (excise_seg_min_skew goal msg ssg (Z.neg k_l) (Z.neg l_ko)
                     (Z.neg ko_ln))
            | Zero | Pos -> None )
        (* k+o-(l+n) = 0     [k;    o)
         * so k+o = l+n    ⊢    [l; n) *)
        | Zero -> Some (excise_seg_sub_suffix goal msg ssg (Z.neg k_l))
        (* k+o-(l+n) > 0     [k;      o)
         * so k+o > l+n    ⊢    [l; n) *)
        | Pos -> Some (excise_seg_sub_infix goal msg ssg (Z.neg k_l) ko_ln)
        )
    (* k-l = 0 so k = l *)
    | Zero -> (
        let* o_n = Equality.difference sub.cong o n in
        match Int.sign (Z.sign o_n) with
        (* o-n < 0      [k; o)
         * so o < n   ⊢ [l;   n) *)
        | Neg -> Some (excise_seg_min_prefix goal msg ssg (Z.neg o_n))
        (* o-n = 0      [k; o)
         * so o = n   ⊢ [l; n) *)
        | Zero -> Some (excise_seg_same goal msg ssg)
        (* o-n > 0      [k;   o)
         * so o > n   ⊢ [l; n) *)
        | Pos -> Some (excise_seg_sub_prefix goal msg ssg o_n) )
    (* k-l > 0 so k > l *)
    | Pos -> (
        let ko = Term.add k o in
        let ln = Term.add l n in
        let* ko_ln = Equality.difference sub.cong ko ln in
        match Int.sign (Z.sign ko_ln) with
        (* k+o-(l+n) < 0        [k; o)
         * so k+o < l+n    ⊢ [l;      n) *)
        | Neg -> Some (excise_seg_min_infix goal msg ssg k_l (Z.neg ko_ln))
        (* k+o-(l+n) = 0        [k; o)
         * so k+o = l+n    ⊢ [l;    n) *)
        | Zero -> Some (excise_seg_min_suffix goal msg ssg k_l)
        (* k+o-(l+n) > 0 so k+o > l+n *)
        | Pos -> (
            let* k_ln = Equality.difference sub.cong k ln in
            match Int.sign (Z.sign k_ln) with
            (* k-(l+n) < 0        [k;  o)
             * so k < l+n    ⊢ [l;   n) *)
            | Neg ->
                Some
                  (excise_seg_sub_skew goal msg ssg k_l (Z.neg k_ln) ko_ln)
            | Zero | Pos -> None ) )

let excise_heap ({min; sub} as goal) =
  trace (fun {pf} -> pf "@[<2>excise_heap@ %a@]" pp goal) ;
  match
    List.find_map sub.heap ~f:(fun ssg ->
        List.find_map min.heap ~f:(fun msg -> excise_seg goal msg ssg) )
  with
  | Some goal -> Some (goal |> with_ ~pgs:true)
  | None -> Some goal

let rec excise ({min; xs; sub; zs; pgs} as goal) =
  [%Trace.info "@[<2>excise@ %a@]" pp goal] ;
  if Sh.is_false min then Some (Sh.false_ (Set.diff sub.us zs))
  else if Sh.is_emp sub then Some (Sh.exists zs (Sh.extend_us xs min))
  else if Sh.is_false sub then None
  else if pgs then
    goal |> with_ ~pgs:false |> excise_exists |> excise_pure >>= excise_heap
    >>= excise
  else None $> fun _ -> [%Trace.info "@[<2>excise fail@ %a@]" pp goal]

let excise_dnf : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option =
 fun minuend xs subtrahend ->
  let dnf_minuend = Sh.dnf minuend in
  let dnf_subtrahend = Sh.dnf subtrahend in
  List.fold_option dnf_minuend
    ~init:(Sh.false_ (Set.union minuend.us xs))
    ~f:(fun remainders minuend ->
      ([%Trace.call fun {pf} -> pf "@[<2>minuend@ %a@]" Sh.pp minuend]
      ;
      let zs, min = Sh.bind_exists minuend ~wrt:xs in
      let us = min.us in
      let com = Sh.emp in
      let+ remainder =
        List.find_map dnf_subtrahend ~f:(fun sub ->
            [%Trace.call fun {pf} -> pf "@[<2>subtrahend@ %a@]" Sh.pp sub]
            ;
            let sub = Sh.and_cong min.cong (Sh.extend_us us sub) in
            excise (goal ~us ~com ~min ~xs ~sub ~zs ~pgs:true)
            |>
            [%Trace.retn fun {pf} -> pf "%a" (Option.pp "%a" Sh.pp)] )
      in
      Sh.or_ remainders remainder)
      |>
      [%Trace.retn fun {pf} -> pf "%a" (Option.pp "%a" Sh.pp)] )

let infer_frame : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option =
 fun minuend xs subtrahend ->
  [%Trace.call fun {pf} ->
    pf "@[<hv>%a@ \\- %a%a@]" Sh.pp minuend Var.Set.pp_xs xs Sh.pp
      subtrahend]
  ;
  assert (Set.disjoint minuend.us xs) ;
  assert (Set.is_subset xs ~of_:subtrahend.us) ;
  assert (Set.is_subset (Set.diff subtrahend.us xs) ~of_:minuend.us) ;
  excise_dnf minuend xs subtrahend
  |>
  [%Trace.retn fun {pf} r ->
    pf "%a" (Option.pp "%a" Sh.pp) r ;
    Option.iter r ~f:(fun frame ->
        let lost = Set.diff (Set.union minuend.us xs) frame.us in
        let gain = Set.diff frame.us (Set.union minuend.us xs) in
        assert (Set.is_empty lost || fail "lost: %a" Var.Set.pp lost ()) ;
        assert (Set.is_empty gain || fail "gained: %a" Var.Set.pp gain ())
    )]
