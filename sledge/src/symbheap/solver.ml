(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Frame Inference Solver over Symbolic Heaps *)

type judgment =
  { us: Var.Set.t  (** (universal) vocabulary of entire judgment *)
  ; com: Sh.t  (** common star-conjunct of minuend and subtrahend *)
  ; min: Sh.t  (** minuend, cong strengthened by pure_approx (com * min) *)
  ; xs: Var.Set.t  (** existentials over subtrahend and remainder *)
  ; sub: Sh.t  (** subtrahend, with cong strengthened by min.cong *)
  ; zs: Var.Set.t  (** existentials over remainder *)
  ; pgs: bool  (** flag indicating whether progress has been made *) }

let pp_xs fs xs =
  if not (Set.is_empty xs) then
    Format.fprintf fs "∃ @[%a@] .@;<1 2>" Var.Set.pp xs

let pp fs {com; min; xs; sub; pgs} =
  Format.fprintf fs "@[%s %a@ | %a@ \\- %a%a@]"
    (if pgs then "t" else "f")
    Sh.pp com Sh.pp min pp_xs xs Sh.pp sub

open Option.Monad_infix

let fresh_var name vs zs ~wrt =
  let v, wrt = Var.fresh name ~wrt in
  let vs = Set.add vs v in
  let zs = Set.add zs v in
  let v = Exp.var v in
  (v, vs, zs, wrt)

let excise_exp {min} (us, witnesses, xs, pure, pgs) exp =
  let exp' = Congruence.normalize min.cong exp in
  if Exp.is_true exp' then Some (us, witnesses, xs, pure, true)
  else if Set.disjoint (Exp.fv exp') xs then None
  else
    match exp' with
    | App {op= App {op= Eq; arg= Var _ as x}}
      when Set.is_subset (Exp.fv x) ~of_:xs ->
        let vs = Exp.fv x in
        Some (Set.union us vs, exp' :: witnesses, Set.diff xs vs, pure, true)
    | App {op= App {op= Eq}; arg= Var _ as x}
      when Set.is_subset (Exp.fv x) ~of_:xs ->
        let vs = Exp.fv x in
        Some (Set.union us vs, exp' :: witnesses, Set.diff xs vs, pure, true)
    | _ -> Some (us, witnesses, xs, exp' :: pure, pgs)

let excise_pure ({us; min; xs; sub; pgs} as goal) =
  [%Trace.info "@[<2>excise_pure@ %a@]" pp goal] ;
  List.fold_option sub.pure ~init:(us, [], xs, [], pgs)
    ~f:(fun (us, witnesses, xs, pure, pgs) exp ->
      excise_exp goal (us, witnesses, xs, pure, pgs) exp )
  >>| fun (us, witnesses, xs, pure, pgs) ->
  let min = List.fold_right witnesses ~init:min ~f:Sh.and_ in
  {goal with us; min; xs; sub= Sh.with_pure pure sub; pgs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_same@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg ssg pp
      goal] ;
  let {Sh.bas= b; len= m; arr= a} = msg in
  let {Sh.bas= b'; len= m'; arr= a'} = ssg in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m') (Sh.and_ (Exp.eq a a') (Sh.rem_seg ssg sub)))
  in
  {goal with com; min; sub}

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
  [%Trace.info
    "@[<hv 2>excise_seg_sub_prefix@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg
      ssg pp goal] ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let o_n = Exp.integer o_n in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, _ = fresh_var "a1" us zs ~wrt in
  let com = Sh.star (Sh.seg {msg with siz= n; arr= a0}) com in
  let min =
    Sh.and_
      (Exp.eq
         (Exp.memory ~siz:o ~arr:a)
         (Exp.concat
            (Exp.memory ~siz:n ~arr:a0)
            (Exp.memory ~siz:o_n ~arr:a1)))
      (Sh.star
         (Sh.seg {loc= Exp.add k n; bas= b; len= m; siz= o_n; arr= a1})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m') (Sh.and_ (Exp.eq a0 a') (Sh.rem_seg ssg sub)))
  in
  {goal with us; com; min; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_min_prefix@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg
      ssg pp goal] ;
  let {Sh.bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let n_o = Exp.integer n_o in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let a1', xs, zs, _ = fresh_var "a1" xs zs ~wrt:(Set.union us xs) in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m')
         (Sh.and_
            (Exp.eq
               (Exp.concat
                  (Exp.memory ~siz:o ~arr:a)
                  (Exp.memory ~siz:n_o ~arr:a1'))
               (Exp.memory ~siz:n ~arr:a'))
            (Sh.star
               (Sh.seg
                  {loc= Exp.add l o; bas= b'; len= m'; siz= n_o; arr= a1'})
               (Sh.rem_seg ssg sub))))
  in
  {goal with us; com; min; xs; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_sub_suffix@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg
      ssg pp goal] ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let l_k = Exp.integer l_k in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, _ = fresh_var "a1" us zs ~wrt in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a1}) com
  in
  let min =
    Sh.and_
      (Exp.eq
         (Exp.memory ~siz:o ~arr:a)
         (Exp.concat
            (Exp.memory ~siz:l_k ~arr:a0)
            (Exp.memory ~siz:n ~arr:a1)))
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; arr= a0})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m') (Sh.and_ (Exp.eq a1 a') (Sh.rem_seg ssg sub)))
  in
  {goal with us; com; min; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_sub_infix@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg ssg
      pp goal] ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let l_k = Exp.integer l_k and ko_ln = Exp.integer ko_ln in
  let ln = Exp.add l n in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2, us, zs, _ = fresh_var "a2" us zs ~wrt in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; arr= a1}) com
  in
  let min =
    Sh.and_
      (Exp.eq
         (Exp.memory ~siz:o ~arr:a)
         (Exp.concat
            (Exp.memory ~siz:l_k ~arr:a0)
            (Exp.concat
               (Exp.memory ~siz:n ~arr:a1)
               (Exp.memory ~siz:ko_ln ~arr:a2))))
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; arr= a0})
         (Sh.star
            (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; arr= a2})
            (Sh.rem_seg msg min)))
  in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m') (Sh.and_ (Exp.eq a1 a') (Sh.rem_seg ssg sub)))
  in
  {goal with us; com; min; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_min_skew@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg ssg
      pp goal] ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let l_k = Exp.integer l_k in
  let ko_l = Exp.integer ko_l in
  let ln_ko = Exp.integer ln_ko in
  let ko = Exp.add k o in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2', xs, zs, _ = fresh_var "a2" xs zs ~wrt in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= ko_l; arr= a1}) com
  in
  let min =
    Sh.and_
      (Exp.eq
         (Exp.memory ~siz:o ~arr:a)
         (Exp.concat
            (Exp.memory ~siz:l_k ~arr:a0)
            (Exp.memory ~siz:ko_l ~arr:a1)))
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; arr= a0})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m')
         (Sh.and_
            (Exp.eq
               (Exp.concat
                  (Exp.memory ~siz:ko_l ~arr:a1)
                  (Exp.memory ~siz:ln_ko ~arr:a2'))
               (Exp.memory ~siz:n ~arr:a'))
            (Sh.star
               (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; arr= a2'})
               (Sh.rem_seg ssg sub))))
  in
  {goal with us; com; min; xs; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_min_suffix@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg
      ssg pp goal] ;
  let {Sh.bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let k_l = Exp.integer k_l in
  let a0', xs, zs, _ = fresh_var "a0" xs zs ~wrt:(Set.union us xs) in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m')
         (Sh.and_
            (Exp.eq
               (Exp.concat
                  (Exp.memory ~siz:k_l ~arr:a0')
                  (Exp.memory ~siz:o ~arr:a))
               (Exp.memory ~siz:n ~arr:a'))
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; arr= a0'})
               (Sh.rem_seg ssg sub))))
  in
  {goal with us; com; min; xs; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_min_infix@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg ssg
      pp goal] ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let k_l = Exp.integer k_l in
  let ln_ko = Exp.integer ln_ko in
  let ko = Exp.add k o in
  let a0', xs, zs, wrt = fresh_var "a0" xs zs ~wrt:(Set.union us xs) in
  let a2', xs, zs, _ = fresh_var "a2" xs zs ~wrt in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m')
         (Sh.and_
            (Exp.eq
               (Exp.concat
                  (Exp.memory ~siz:k_l ~arr:a0')
                  (Exp.concat
                     (Exp.memory ~siz:o ~arr:a)
                     (Exp.memory ~siz:ln_ko ~arr:a2')))
               (Exp.memory ~siz:n ~arr:a'))
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; arr= a0'})
               (Sh.star
                  (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; arr= a2'})
                  (Sh.rem_seg ssg sub)))))
  in
  {goal with us; com; min; xs; sub; zs}

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
  [%Trace.info
    "@[<hv 2>excise_seg_sub_skew@ %a@ %a@ %a@]" Sh.pp_seg msg Sh.pp_seg ssg
      pp goal] ;
  let {Sh.loc= k; bas= b; len= m; siz= o; arr= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; arr= a'} = ssg in
  let k_l = Exp.integer k_l in
  let ln_k = Exp.integer ln_k in
  let ko_ln = Exp.integer ko_ln in
  let ln = Exp.add l n in
  let a0', xs, zs, wrt = fresh_var "a0" xs zs ~wrt:(Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2, us, zs, _ = fresh_var "a2" us zs ~wrt in
  let com =
    Sh.star (Sh.seg {loc= k; bas= b; len= m; siz= ln_k; arr= a1}) com
  in
  let min =
    Sh.and_
      (Exp.eq
         (Exp.memory ~siz:o ~arr:a)
         (Exp.concat
            (Exp.memory ~siz:ln_k ~arr:a1)
            (Exp.memory ~siz:ko_ln ~arr:a2)))
      (Sh.star
         (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; arr= a2})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Exp.eq b b')
      (Sh.and_ (Exp.eq m m')
         (Sh.and_
            (Exp.eq
               (Exp.concat
                  (Exp.memory ~siz:k_l ~arr:a0')
                  (Exp.memory ~siz:ln_k ~arr:a1))
               (Exp.memory ~siz:n ~arr:a'))
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; arr= a0'})
               (Sh.rem_seg ssg sub))))
  in
  {goal with us; com; min; xs; sub; zs}

(* C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ R *)
let excise_seg ({sub} as goal) msg ssg =
  [%Trace.info "@[<2>excise_seg@ %a@  |-  %a@]" Sh.pp_seg msg Sh.pp_seg ssg] ;
  let {Sh.loc= k; siz= o} = msg in
  let {Sh.loc= l; siz= n} = ssg in
  Congruence.difference sub.cong k l
  >>= fun k_l ->
  match[@warning "-p"] Z.sign k_l with
  (* k-l < 0 so k < l *)
  | -1 -> (
      let ko = Exp.add k o in
      let ln = Exp.add l n in
      Congruence.difference sub.cong ko ln
      >>= fun ko_ln ->
      match[@warning "-p"] Z.sign ko_ln with
      (* k+o-(l+n) < 0 so k+o < l+n *)
      | -1 -> (
          Congruence.difference sub.cong l ko
          >>= fun l_ko ->
          match[@warning "-p"] Z.sign l_ko with
          (* l-(k+o) < 0     [k;   o)
           * so l < k+o    ⊢    [l;  n) *)
          | -1 ->
              Some
                (excise_seg_min_skew goal msg ssg (Z.neg k_l) (Z.neg l_ko)
                   (Z.neg ko_ln))
          | _ -> None )
      (* k+o-(l+n) = 0     [k;    o)
       * so k+o = l+n    ⊢    [l; n) *)
      | 0 -> Some (excise_seg_sub_suffix goal msg ssg (Z.neg k_l))
      (* k+o-(l+n) > 0     [k;      o)
       * so k+o > l+n    ⊢    [l; n) *)
      | 1 -> Some (excise_seg_sub_infix goal msg ssg (Z.neg k_l) ko_ln) )
  (* k-l = 0 so k = l *)
  | 0 -> (
    match Congruence.difference sub.cong o n with
    | None -> Some (excise_seg_same goal msg ssg)
    | Some o_n -> (
      match[@warning "-p"] Z.sign o_n with
      (* o-n < 0      [k; o)
       * so o < n   ⊢ [l;   n) *)
      | -1 -> Some (excise_seg_min_prefix goal msg ssg (Z.neg o_n))
      (* o-n = 0      [k; o)
       * so o = n   ⊢ [l; n) *)
      | 0 -> Some (excise_seg_same goal msg ssg)
      (* o-n > 0      [k;   o)
       * so o > n   ⊢ [l; n) *)
      | 1 -> Some (excise_seg_sub_prefix goal msg ssg o_n) ) )
  (* k-l > 0 so k > l *)
  | 1 -> (
      let ko = Exp.add k o in
      let ln = Exp.add l n in
      Congruence.difference sub.cong ko ln
      >>= fun ko_ln ->
      match[@warning "-p"] Z.sign ko_ln with
      (* k+o-(l+n) < 0        [k; o)
       * so k+o < l+n    ⊢ [l;      n) *)
      | -1 -> Some (excise_seg_min_infix goal msg ssg k_l (Z.neg ko_ln))
      (* k+o-(l+n) = 0        [k; o)
       * so k+o = l+n    ⊢ [l;    n) *)
      | 0 -> Some (excise_seg_min_suffix goal msg ssg k_l)
      (* k+o-(l+n) > 0 so k+o > l+n *)
      | 1 -> (
          Congruence.difference sub.cong k ln
          >>= fun k_ln ->
          match[@warning "-p"] Z.sign k_ln with
          (* k-(l+n) < 0        [k;  o)
           * so k < l+n    ⊢ [l;   n) *)
          | -1 ->
              Some (excise_seg_sub_skew goal msg ssg k_l (Z.neg k_ln) ko_ln)
          | _ -> None ) )

let excise_heap ({min; sub} as goal) =
  [%Trace.info "@[<2>excise_heap@ %a@]" pp goal] ;
  match
    List.find_map sub.heap ~f:(fun ssg ->
        List.find_map min.heap ~f:(fun msg -> excise_seg goal msg ssg) )
  with
  | Some goal -> Some {goal with pgs= true}
  | None -> Some goal

let rec excise ({min; xs; sub; zs; pgs} as goal) =
  [%Trace.info "@[<2>excise@ %a@]" pp goal] ;
  if Sh.is_false min then
    Some (Sh.false_ (Set.diff (Set.union min.us xs) zs))
  else if Sh.is_emp sub then Some (Sh.exists zs (Sh.extend_us xs min))
  else if Sh.is_false sub then None
  else if not pgs then None
  else {goal with pgs= false} |> excise_pure >>= excise_heap >>= excise

let excise_dnf : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option =
 fun minuend xs subtrahend ->
  let dnf_minuend = Sh.dnf minuend in
  let dnf_subtrahend = Sh.dnf subtrahend in
  List.fold_option dnf_minuend
    ~init:(Sh.false_ (Set.union minuend.us xs))
    ~f:(fun remainders minuend ->
      [%Trace.info "@[<2>minuend@ %a@]" Sh.pp minuend] ;
      let ys, min = Sh.bind_exists minuend ~wrt:xs in
      let us = min.us in
      let com = Sh.emp in
      List.find_map dnf_subtrahend ~f:(fun sub ->
          [%Trace.info "@[<2>subtrahend@ %a@]" Sh.pp sub] ;
          let sub = Sh.extend_us us sub in
          let ws, sub = Sh.bind_exists sub ~wrt:xs in
          let sub = Sh.and_cong min.cong sub in
          let zs = Var.Set.empty in
          excise {us; com; min; xs; sub; zs; pgs= true}
          >>| fun remainder -> Sh.exists (Set.union ys ws) remainder )
      >>| fun remainder -> Sh.or_ remainders remainder )

let infer_frame : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option =
 fun minuend xs subtrahend ->
  [%Trace.call fun {pf} ->
    pf "@[<hv>%a@ \\- %a%a@]" Sh.pp minuend pp_xs xs Sh.pp subtrahend]
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
        assert (Set.is_empty lost || Trace.report "lost: %a" Var.Set.pp lost) ;
        assert (
          Set.is_empty gain || Trace.report "gained: %a" Var.Set.pp gain )
    )]
