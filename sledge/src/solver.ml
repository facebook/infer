(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Frame Inference Solver over Symbolic Heaps *)

open Fol

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
    ; min: Sh.t  (** minuend, ctx strengthened by pure_approx (com * min) *)
    ; xs: Var.Set.t  (** existentials over subtrahend and remainder *)
    ; sub: Sh.t  (** subtrahend, ctx strengthened by min.ctx *)
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
      Sh.pp com Sh.pp min Var.Set.pp_xs xs
      (Sh.pp_diff_eq ~us:(Var.Set.union min.us sub.us) ~xs min.ctx)
      sub

  let invariant g =
    let@ () = Invariant.invariant [%here] g [%sexp_of: t] in
    try
      let {us; com; min; xs; sub; zs; pgs= _} = g in
      assert (Var.Set.equal us com.us) ;
      assert (Var.Set.equal us min.us) ;
      assert (Var.Set.equal (Var.Set.union us xs) sub.us) ;
      assert (Var.Set.disjoint us xs) ;
      assert (Var.Set.subset zs ~of_:(Var.Set.union us xs))
    with exc ->
      [%Trace.info "%a" pp g] ;
      raise exc

  let with_ ?us ?com ?min ?xs ?sub ?zs ?pgs g =
    let xs = Option.value xs ~default:g.xs in
    let zs = Option.value zs ~default:g.zs in
    let new_us =
      let us = Option.value us ~default:Var.Set.empty in
      let us =
        Option.fold
          ~f:(fun sub -> Var.Set.union (Var.Set.diff sub.Sh.us xs))
          sub us
      in
      let union_us q_opt us' =
        Option.fold ~f:(fun q -> Var.Set.union q.Sh.us) q_opt us'
      in
      union_us com (union_us min us)
    in
    let com = Sh.extend_us new_us (Option.value com ~default:g.com) in
    let min = Sh.extend_us new_us (Option.value min ~default:g.min) in
    let xs, sub, zs =
      match sub with
      | Some sub ->
          let sub = Sh.extend_us (Var.Set.union new_us xs) sub in
          let ys, sub = Sh.bind_exists sub ~wrt:xs in
          let xs = Var.Set.union xs ys in
          let zs = Var.Set.union zs ys in
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

let eq_concat (siz, seq) ms =
  Formula.eq (Term.sized ~siz ~seq)
    (Term.concat (Array.map ~f:(fun (siz, seq) -> Term.sized ~siz ~seq) ms))

let fresh_var name vs zs ~wrt =
  let v, wrt = Var.fresh name ~wrt in
  let vs = Var.Set.add v vs in
  let zs = Var.Set.add v zs in
  let v = Term.var v in
  (v, vs, zs, wrt)

let difference x e f = Term.get_z (Context.normalize x (Term.sub e f))
let excise (k : Trace.pf -> _) = [%Trace.infok k]
let trace (k : Trace.pf -> _) = [%Trace.infok k]

let excise_exists goal =
  trace (fun {pf} -> pf "@[<2>excise_exists@ %a@]" pp goal) ;
  if Var.Set.is_empty goal.xs then goal
  else
    let solutions_for_xs =
      let xs =
        Var.Set.diff goal.xs (Sh.fv ~ignore_ctx:() ~ignore_pure:() goal.sub)
      in
      Context.solve_for_vars [Var.Set.empty; goal.us; xs] goal.sub.ctx
    in
    if Context.Subst.is_empty solutions_for_xs then goal
    else
      let removed =
        Var.Set.diff goal.xs
          (Sh.fv ~ignore_ctx:() (Sh.norm solutions_for_xs goal.sub))
      in
      if Var.Set.is_empty removed then goal
      else
        let _, removed, witnesses =
          Context.Subst.partition_valid removed solutions_for_xs
        in
        if Context.Subst.is_empty witnesses then goal
        else (
          excise (fun {pf} ->
              pf "@[<2>excise_exists @[%a%a@]@]" Var.Set.pp_xs removed
                Context.Subst.pp witnesses ) ;
          let us = Var.Set.union goal.us removed in
          let xs = Var.Set.diff goal.xs removed in
          let min = Sh.and_subst witnesses goal.min in
          goal |> with_ ~us ~min ~xs ~pgs:true )

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
      pf "@[<hv 2>excise_seg_same@ %a@ \\- %a@]" (Sh.pp_seg_norm sub.ctx)
        msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.bas= b; len= m; cnt= a} = msg in
  let {Sh.bas= b'; len= m'; cnt= a'} = ssg in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_ (Formula.eq a a') (Sh.rem_seg ssg sub)))
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let o_n = Term.integer o_n in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Var.Set.union us xs) in
  let a1, us, zs, _ = fresh_var "a1" us zs ~wrt in
  let xs = Var.Set.diff xs (Term.fv n) in
  let com = Sh.star (Sh.seg {msg with siz= n; cnt= a0}) com in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(n, a0); (o_n, a1)|])
      (Sh.star
         (Sh.seg {loc= Term.add k n; bas= b; len= m; siz= o_n; cnt= a1})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_ (Formula.eq a0 a') (Sh.rem_seg ssg sub)))
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let n_o = Term.integer n_o in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let a1', xs, zs, _ = fresh_var "a1" xs zs ~wrt:(Var.Set.union us xs) in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_
            (eq_concat (n, a') [|(o, a); (n_o, a1')|])
            (Sh.star
               (Sh.seg
                  {loc= Term.add l o; bas= b'; len= m'; siz= n_o; cnt= a1'})
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let l_k = Term.integer l_k in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Var.Set.union us xs) in
  let a1, us, zs, _ = fresh_var "a1" us zs ~wrt in
  let xs = Var.Set.diff xs (Term.fv n) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; cnt= a1}) com
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(l_k, a0); (n, a1)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; cnt= a0})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_ (Formula.eq a1 a') (Sh.rem_seg ssg sub)))
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let l_k = Term.integer l_k in
  let ko_ln = Term.integer ko_ln in
  let ln = Term.add l n in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Var.Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2, us, zs, _ = fresh_var "a2" us zs ~wrt in
  let xs = Var.Set.diff xs (Var.Set.union (Term.fv l) (Term.fv n)) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; cnt= a1}) com
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(l_k, a0); (n, a1); (ko_ln, a2)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; cnt= a0})
         (Sh.star
            (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; cnt= a2})
            (Sh.rem_seg msg min)))
  in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_ (Formula.eq a1 a') (Sh.rem_seg ssg sub)))
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let l_k = Term.integer l_k in
  let ko_l = Term.integer ko_l in
  let ln_ko = Term.integer ln_ko in
  let ko = Term.add k o in
  let a0, us, zs, wrt = fresh_var "a0" us zs ~wrt:(Var.Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2', xs, zs, _ = fresh_var "a2" xs zs ~wrt in
  let xs = Var.Set.diff xs (Term.fv l) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= ko_l; cnt= a1}) com
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(l_k, a0); (ko_l, a1)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; cnt= a0})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_
            (eq_concat (n, a') [|(ko_l, a1); (ln_ko, a2')|])
            (Sh.star
               (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; cnt= a2'})
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let k_l = Term.integer k_l in
  let a0', xs, zs, _ = fresh_var "a0" xs zs ~wrt:(Var.Set.union us xs) in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_
            (eq_concat (n, a') [|(k_l, a0'); (o, a)|])
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; cnt= a0'})
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let k_l = Term.integer k_l in
  let ln_ko = Term.integer ln_ko in
  let ko = Term.add k o in
  let a0', xs, zs, wrt = fresh_var "a0" xs zs ~wrt:(Var.Set.union us xs) in
  let a2', xs, zs, _ = fresh_var "a2" xs zs ~wrt in
  let com = Sh.star (Sh.seg msg) com in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_
            (eq_concat (n, a') [|(k_l, a0'); (o, a); (ln_ko, a2')|])
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; cnt= a0'})
               (Sh.star
                  (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; cnt= a2'})
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
        (Sh.pp_seg_norm sub.ctx) msg (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let k_l = Term.integer k_l in
  let ln_k = Term.integer ln_k in
  let ko_ln = Term.integer ko_ln in
  let ln = Term.add l n in
  let a0', xs, zs, wrt = fresh_var "a0" xs zs ~wrt:(Var.Set.union us xs) in
  let a1, us, zs, wrt = fresh_var "a1" us zs ~wrt in
  let a2, us, zs, _ = fresh_var "a2" us zs ~wrt in
  let com =
    Sh.star (Sh.seg {loc= k; bas= b; len= m; siz= ln_k; cnt= a1}) com
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(ln_k, a1); (ko_ln, a2)|])
      (Sh.star
         (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; cnt= a2})
         (Sh.rem_seg msg min))
  in
  let sub =
    Sh.and_ (Formula.eq b b')
      (Sh.and_ (Formula.eq m m')
         (Sh.and_
            (eq_concat (n, a') [|(k_l, a0'); (ln_k, a1)|])
            (Sh.star
               (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; cnt= a0'})
               (Sh.rem_seg ssg sub))))
  in
  goal |> with_ ~us ~com ~min ~xs ~sub ~zs

(* C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ R *)
let excise_seg ({sub} as goal) msg ssg =
  trace (fun {pf} ->
      pf "@[<2>excise_seg@ %a@  |-  %a@]" (Sh.pp_seg_norm sub.ctx) msg
        (Sh.pp_seg_norm sub.ctx) ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n} = ssg in
  let* k_l = difference sub.ctx k l in
  if
    (not (Context.implies sub.ctx (Formula.eq b b')))
    || not (Context.implies sub.ctx (Formula.eq m m'))
  then
    Some
      ( goal
      |> with_
           ~sub:
             (Sh.and_ (Formula.eq b b')
                (Sh.and_ (Formula.eq m m') goal.sub)) )
  else
    match Int.sign (Z.sign k_l) with
    (* k-l < 0 so k < l *)
    | Neg -> (
        let ko = Term.add k o in
        let ln = Term.add l n in
        let* ko_ln = difference sub.ctx ko ln in
        match Int.sign (Z.sign ko_ln) with
        (* k+o-(l+n) < 0 so k+o < l+n *)
        | Neg -> (
            let* l_ko = difference sub.ctx l ko in
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
        let* o_n = difference sub.ctx o n in
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
        let* ko_ln = difference sub.ctx ko ln in
        match Int.sign (Z.sign ko_ln) with
        (* k+o-(l+n) < 0        [k; o)
         * so k+o < l+n    ⊢ [l;      n) *)
        | Neg -> Some (excise_seg_min_infix goal msg ssg k_l (Z.neg ko_ln))
        (* k+o-(l+n) = 0        [k; o)
         * so k+o = l+n    ⊢ [l;    n) *)
        | Zero -> Some (excise_seg_min_suffix goal msg ssg k_l)
        (* k+o-(l+n) > 0 so k+o > l+n *)
        | Pos -> (
            let* k_ln = difference sub.ctx k ln in
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

let pure_entails x q = Sh.is_empty q && Context.implies x (Sh.pure_approx q)

let rec excise ({min; xs; sub; zs; pgs} as goal) =
  [%Trace.info "@[<2>excise@ %a@]" pp goal] ;
  Report.step_solver () ;
  if Sh.is_unsat min then Some (Sh.false_ (Var.Set.diff sub.us zs))
  else if pure_entails min.ctx sub then
    Some (Sh.exists zs (Sh.extend_us xs min))
  else if Sh.is_unsat sub then None
  else if pgs then
    goal |> with_ ~pgs:false |> excise_exists |> excise_heap >>= excise
  else None $> fun _ -> [%Trace.info "@[<2>excise fail@ %a@]" pp goal]

let excise_dnf : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option =
 fun minuend xs subtrahend ->
  let dnf_minuend = Sh.dnf minuend in
  let dnf_subtrahend = Sh.dnf subtrahend in
  Iter.fold_opt
    (Iter.of_list dnf_minuend)
    (Sh.false_ (Var.Set.union minuend.us xs))
    ~f:(fun minuend remainders ->
      [%trace]
        ~call:(fun {pf} -> pf "@ @[<2>minuend@ %a@]" Sh.pp minuend)
        ~retn:(fun {pf} -> pf "%a" (Option.pp "%a" Sh.pp))
      @@ fun () ->
      let zs, min = Sh.bind_exists minuend ~wrt:xs in
      let us = min.us in
      let com = Sh.emp in
      let+ remainder =
        List.find_map dnf_subtrahend ~f:(fun sub ->
            [%trace]
              ~call:(fun {pf} -> pf "@ @[<2>subtrahend@ %a@]" Sh.pp sub)
              ~retn:(fun {pf} -> pf "%a" (Option.pp "%a" Sh.pp))
            @@ fun () ->
            let sub = Sh.and_ctx min.ctx (Sh.extend_us us sub) in
            excise (goal ~us ~com ~min ~xs ~sub ~zs ~pgs:true) )
      in
      Sh.or_ remainders remainder )

let query_count = ref (-1)

let infer_frame : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option =
 fun minuend xs subtrahend ->
  [%trace]
    ~call:(fun {pf} ->
      pf " %i@ @[<hv>%a@ \\- %a%a@]" !query_count Sh.pp minuend
        Var.Set.pp_xs xs Sh.pp subtrahend )
    ~retn:(fun {pf} r ->
      pf "%a" (Option.pp "%a" Sh.pp) r ;
      Option.iter r ~f:(fun frame ->
          let lost = Var.Set.diff (Var.Set.union minuend.us xs) frame.us in
          let gain = Var.Set.diff frame.us (Var.Set.union minuend.us xs) in
          assert (
            Var.Set.is_empty lost || fail "lost: %a" Var.Set.pp lost () ) ;
          assert (
            Var.Set.is_empty gain || fail "gained: %a" Var.Set.pp gain () ) )
      )
  @@ fun () ->
  assert (Var.Set.disjoint minuend.us xs) ;
  assert (Var.Set.subset xs ~of_:subtrahend.us) ;
  assert (Var.Set.subset (Var.Set.diff subtrahend.us xs) ~of_:minuend.us) ;
  excise_dnf minuend xs subtrahend

(*
 * Replay debugging
 *)

type call = Infer_frame of Sh.t * Var.Set.t * Sh.t [@@deriving sexp]

let replay c =
  match call_of_sexp (Sexp.of_string c) with
  | Infer_frame (minuend, xs, subtrahend) ->
      infer_frame minuend xs subtrahend |> ignore

let dump_query = ref (-1)

let infer_frame minuend xs subtrahend =
  Int.incr query_count ;
  if !query_count = !dump_query then
    fail "%a" Sexp.pp_hum
      (sexp_of_call (Infer_frame (minuend, xs, subtrahend)))
      ()
  else infer_frame minuend xs subtrahend
