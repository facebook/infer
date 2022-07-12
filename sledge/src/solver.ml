(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Frame Inference Solver over Symbolic Heaps *)

open Fol
open Symbolic_heap

module Goal = struct
  (** Excision judgment

      Common ❮ Minuend ⊢ ∃xs. Subtrahend ❯ ∃zs. Remainder

      is valid iff

      Common * Minuend ⊧ ∃xs. Common * Subtrahend * ∃zs. Remainder

      is universally valid semantically.

      Terminology analogous to arithmetic subtraction is used: "minuend" is
      a formula from which another, the subtrahend, is to be subtracted; and
      "subtrahend" is a formula to be subtracted from another, the minuend. *)
  type t =
    { com: Sh.t  (** common star-conjunct of minuend and subtrahend *)
    ; min: Sh.t  (** minuend, ctx strengthened by pure_approx (com * min) *)
    ; xs: Var.Set.t  (** existentials over subtrahend and remainder *)
    ; sub: Sh.t  (** subtrahend, ctx strengthened by min.ctx *)
    ; pgs: bool  (** indicates whether a deduction rule has been applied *)
    }
  [@@deriving sexp]

  let pp fs {com; min; xs; sub; pgs} =
    Format.fprintf fs "%s@ @[<hv>%a@ | %a@ @[\\- %a%a@]@]"
      (if pgs then "t" else "f")
      Sh.pp com Sh.pp min Var.Set.pp_xs xs
      (Sh.pp_diff_eq (Sh.ctx min))
      sub

  let pp_raw fs ({com; min; xs; sub; pgs}, vx) =
    Format.fprintf fs "%s@ @[<hv>%a%a@ | %a@ @[\\- %a%a%a@]@]"
      (if pgs then "t" else "f")
      Var.Context.pp_voc vx Sh.pp_raw com Sh.pp_raw min Var.Set.pp_xs xs
      Sh.pp_raw sub Var.Set.pp_xs (Var.Context.xs vx)

  let invariant g vx =
    let@ () =
      Invariant.invariant [%here] (g, !vx) [%sexp_of: t * Var.Context.t]
    in
    try
      let {com; min; xs; sub; pgs= _} = g in
      assert (Var.Context.contains !vx (Sh.fv com)) ;
      assert (Var.Context.contains !vx (Sh.fv min)) ;
      assert (Var.Context.contains !vx xs) ;
      assert (Var.Context.contains !vx (Sh.fv sub))
    with exc ->
      [%Dbg.info "@ %a" pp_raw (g, !vx)] ;
      raise exc

  let empty =
    {com= Sh.emp; min= Sh.emp; xs= Var.Set.empty; sub= Sh.emp; pgs= false}
end

open Goal

let eq_concat (siz, seq) xs =
  let ys, len =
    Array.fold_map xs Term.zero ~f:(fun (siz, seq) len ->
        ({Term.siz; seq}, Term.add siz len) )
  in
  Formula.and_ (Formula.eq siz len) (Formula.eq seq (Term.concat ys))

let fresh_v name vx =
  let v = Var.Fresh.var name vx in
  Term.var v

let fresh_x name xs vx =
  let v = Var.Fresh.var name vx in
  let xs = Var.Set.add v xs in
  (Term.var v, xs)

let difference x e f = Term.get_z (Context.normalize x (Term.sub e f))

(* debug tracing *)
let excise (k : Dbg.pf -> _) = [%Dbg.infok k]
let trace (k : Dbg.pf -> _) = [%Dbg.infok k]

let excise_exists goal vx =
  trace (fun {pf} -> pf "excise_exists:@ %a" pp goal) ;
  if Var.Set.is_empty goal.xs then goal
  else
    let solutions_for_xs =
      let fv_non_fol = Sh.fv ~ignore_ctx:() ~ignore_pure:() goal.sub in
      let ks, not_ito = Var.Set.diff_inter goal.xs fv_non_fol in
      Context.solve_for ks ~not_ito (Sh.ctx goal.sub) vx
    in
    if Context.Subst.is_empty solutions_for_xs then goal
    else
      let removed =
        let sub = Sh.norm solutions_for_xs goal.sub vx in
        Var.Set.diff goal.xs (Sh.fv ~ignore_ctx:() sub)
      in
      if Var.Set.is_empty removed then goal
      else
        let _, removed, witnesses =
          Context.Subst.partition_valid removed solutions_for_xs
        in
        if Context.Subst.is_empty witnesses then goal
        else (
          excise (fun {pf} ->
              pf "excise_exists: @[%a%a@]" Var.Set.pp_xs removed
                Context.Subst.pp witnesses ) ;
          let xs = Var.Set.diff goal.xs removed in
          let min = Sh.and_subst witnesses goal.min vx in
          {goal with min; xs; pgs= true} )

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
let excise_seg_same ({com; min; sub} as goal) msg ssg vx =
  excise (fun {pf} ->
      pf "excise_seg_same:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.bas= b; len= m; cnt= a} = msg in
  let {Sh.bas= b'; len= m'; cnt= a'} = ssg in
  let com = Sh.star (Sh.seg msg) com vx in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.andN
      [Formula.eq b b'; Formula.eq m m'; Formula.eq a a']
      (Sh.rem_seg ssg sub) vx
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
let excise_seg_sub_prefix ({com; min; xs; sub} as goal) msg ssg o_n vx =
  excise (fun {pf} ->
      pf "excise_seg_sub_prefix:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let o_n = Term.integer o_n in
  let a0 = fresh_v "a0" vx in
  let a1 = fresh_v "a1" vx in
  let xs = Var.Set.diff xs (Term.fv n) in
  let com = Sh.star (Sh.seg {msg with siz= n; cnt= a0}) com vx in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(n, a0); (o_n, a1)|])
      (Sh.star
         (Sh.seg {loc= Term.add k n; bas= b; len= m; siz= o_n; cnt= a1})
         (Sh.rem_seg msg min) vx )
      vx
  in
  let sub =
    Sh.andN
      [Formula.eq b b'; Formula.eq m m'; Formula.eq a0 a']
      (Sh.rem_seg ssg sub) vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_min_prefix ({com; min; xs; sub} as goal) msg ssg n_o vx =
  excise (fun {pf} ->
      pf "excise_seg_min_prefix:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let n_o = Term.integer n_o in
  let com = Sh.star (Sh.seg msg) com vx in
  let min = Sh.rem_seg msg min in
  let a1', xs = fresh_x "a1" xs vx in
  let sub =
    Sh.andN
      [ Formula.eq b b'
      ; Formula.eq m m'
      ; eq_concat (n, a') [|(o, a); (n_o, a1')|] ]
      (Sh.star
         (Sh.seg {loc= Term.add l o; bas= b'; len= m'; siz= n_o; cnt= a1'})
         (Sh.rem_seg ssg sub) vx )
      vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_sub_suffix ({com; min; xs; sub} as goal) msg ssg l_k vx =
  excise (fun {pf} ->
      pf "excise_seg_sub_suffix:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let l_k = Term.integer l_k in
  let a0 = fresh_v "a0" vx in
  let a1 = fresh_v "a1" vx in
  let xs = Var.Set.diff xs (Term.fv n) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; cnt= a1}) com vx
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(l_k, a0); (n, a1)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; cnt= a0})
         (Sh.rem_seg msg min) vx )
      vx
  in
  let sub =
    Sh.andN
      [Formula.eq b b'; Formula.eq m m'; Formula.eq a1 a']
      (Sh.rem_seg ssg sub) vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_sub_infix ({com; min; xs; sub} as goal) msg ssg l_k ko_ln vx
    =
  excise (fun {pf} ->
      pf "excise_seg_sub_infix:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let l_k = Term.integer l_k in
  let ko_ln = Term.integer ko_ln in
  let ln = Term.add l n in
  let a0 = fresh_v "a0" vx in
  let a1 = fresh_v "a1" vx in
  let a2 = fresh_v "a2" vx in
  let xs = Var.Set.diff xs (Var.Set.union (Term.fv l) (Term.fv n)) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= n; cnt= a1}) com vx
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(l_k, a0); (n, a1); (ko_ln, a2)|])
      (Sh.starN
         [ Sh.seg {loc= k; bas= b; len= m; siz= l_k; cnt= a0}
         ; Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; cnt= a2}
         ; Sh.rem_seg msg min ]
         vx )
      vx
  in
  let sub =
    Sh.andN
      [Formula.eq b b'; Formula.eq m m'; Formula.eq a1 a']
      (Sh.rem_seg ssg sub) vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_min_skew ({com; min; xs; sub} as goal) msg ssg l_k ko_l ln_ko
    vx =
  excise (fun {pf} ->
      pf "excise_seg_min_skew:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let l_k = Term.integer l_k in
  let ko_l = Term.integer ko_l in
  let ln_ko = Term.integer ln_ko in
  let ko = Term.add k o in
  let a0 = fresh_v "a0" vx in
  let a1 = fresh_v "a1" vx in
  let a2', xs = fresh_x "a2" xs vx in
  let xs = Var.Set.diff xs (Term.fv l) in
  let com =
    Sh.star (Sh.seg {loc= l; bas= b; len= m; siz= ko_l; cnt= a1}) com vx
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(l_k, a0); (ko_l, a1)|])
      (Sh.star
         (Sh.seg {loc= k; bas= b; len= m; siz= l_k; cnt= a0})
         (Sh.rem_seg msg min) vx )
      vx
  in
  let sub =
    Sh.andN
      [ Formula.eq b b'
      ; Formula.eq m m'
      ; eq_concat (n, a') [|(ko_l, a1); (ln_ko, a2')|] ]
      (Sh.star
         (Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; cnt= a2'})
         (Sh.rem_seg ssg sub) vx )
      vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_min_suffix ({com; min; xs; sub} as goal) msg ssg k_l vx =
  excise (fun {pf} ->
      pf "excise_seg_min_suffix:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let k_l = Term.integer k_l in
  let a0', xs = fresh_x "a0" xs vx in
  let com = Sh.star (Sh.seg msg) com vx in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.andN
      [ Formula.eq b b'
      ; Formula.eq m m'
      ; eq_concat (n, a') [|(k_l, a0'); (o, a)|] ]
      (Sh.star
         (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; cnt= a0'})
         (Sh.rem_seg ssg sub) vx )
      vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_min_infix ({com; min; xs; sub} as goal) msg ssg k_l ln_ko vx
    =
  excise (fun {pf} ->
      pf "excise_seg_min_infix:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let k_l = Term.integer k_l in
  let ln_ko = Term.integer ln_ko in
  let ko = Term.add k o in
  let a0', xs = fresh_x "a0" xs vx in
  let a2', xs = fresh_x "a2" xs vx in
  let com = Sh.star (Sh.seg msg) com vx in
  let min = Sh.rem_seg msg min in
  let sub =
    Sh.andN
      [ Formula.eq b b'
      ; Formula.eq m m'
      ; eq_concat (n, a') [|(k_l, a0'); (o, a); (ln_ko, a2')|] ]
      (Sh.starN
         [ Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; cnt= a0'}
         ; Sh.seg {loc= ko; bas= b'; len= m'; siz= ln_ko; cnt= a2'}
         ; Sh.rem_seg ssg sub ]
         vx )
      vx
  in
  {goal with com; min; xs; sub}

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
let excise_seg_sub_skew ({com; min; xs; sub} as goal) msg ssg k_l ln_k ko_ln
    vx =
  excise (fun {pf} ->
      pf "excise_seg_sub_skew:@ %a@ \\- %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o; cnt= a} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n; cnt= a'} = ssg in
  let k_l = Term.integer k_l in
  let ln_k = Term.integer ln_k in
  let ko_ln = Term.integer ko_ln in
  let ln = Term.add l n in
  let a0', xs = fresh_x "a0" xs vx in
  let a1 = fresh_v "a1" vx in
  let a2 = fresh_v "a2" vx in
  let com =
    Sh.star (Sh.seg {loc= k; bas= b; len= m; siz= ln_k; cnt= a1}) com vx
  in
  let min =
    Sh.and_
      (eq_concat (o, a) [|(ln_k, a1); (ko_ln, a2)|])
      (Sh.star
         (Sh.seg {loc= ln; bas= b; len= m; siz= ko_ln; cnt= a2})
         (Sh.rem_seg msg min) vx )
      vx
  in
  let sub =
    Sh.andN
      [ Formula.eq b b'
      ; Formula.eq m m'
      ; eq_concat (n, a') [|(k_l, a0'); (ln_k, a1)|] ]
      (Sh.star
         (Sh.seg {loc= l; bas= b'; len= m'; siz= k_l; cnt= a0'})
         (Sh.rem_seg ssg sub) vx )
      vx
  in
  {goal with com; min; xs; sub}

(* C ❮ k-[b;m)->⟨o,α⟩ * M ⊢ ∃xs. l-[b';m')->⟨n,α'⟩ * S ❯ R *)
let excise_seg ({sub} as goal) msg ssg vx =
  trace (fun {pf} ->
      pf "excise_seg:@ %a@  |-  %a"
        (Sh.pp_seg_norm (Sh.ctx sub))
        msg
        (Sh.pp_seg_norm (Sh.ctx sub))
        ssg ) ;
  let {Sh.loc= k; bas= b; len= m; siz= o} = msg in
  let {Sh.loc= l; bas= b'; len= m'; siz= n} = ssg in
  let* k_l = difference (Sh.ctx sub) k l in
  if
    (not (Context.implies (Sh.ctx sub) (Formula.eq b b')))
    || not (Context.implies (Sh.ctx sub) (Formula.eq m m'))
  then
    let sub = Sh.andN [Formula.eq b b'; Formula.eq m m'] goal.sub vx in
    Some {goal with sub}
  else
    match Int.sign (Z.sign k_l) with
    (* k-l < 0 so k < l *)
    | Neg -> (
        let ko = Term.add k o in
        let ln = Term.add l n in
        let* ko_ln = difference (Sh.ctx sub) ko ln in
        match Int.sign (Z.sign ko_ln) with
        (* k+o-(l+n) < 0 so k+o < l+n *)
        | Neg -> (
            let* l_ko = difference (Sh.ctx sub) l ko in
            match Int.sign (Z.sign l_ko) with
            (* l-(k+o) < 0     [k;   o)
             * so l < k+o    ⊢    [l;  n) *)
            | Neg ->
                Some
                  (excise_seg_min_skew goal msg ssg (Z.neg k_l) (Z.neg l_ko)
                     (Z.neg ko_ln) vx )
            | Zero | Pos -> None )
        (* k+o-(l+n) = 0     [k;    o)
         * so k+o = l+n    ⊢    [l; n) *)
        | Zero -> Some (excise_seg_sub_suffix goal msg ssg (Z.neg k_l) vx)
        (* k+o-(l+n) > 0     [k;      o)
         * so k+o > l+n    ⊢    [l; n) *)
        | Pos ->
            Some (excise_seg_sub_infix goal msg ssg (Z.neg k_l) ko_ln vx) )
    (* k-l = 0 so k = l *)
    | Zero -> (
        let* o_n = difference (Sh.ctx sub) o n in
        match Int.sign (Z.sign o_n) with
        (* o-n < 0      [k; o)
         * so o < n   ⊢ [l;   n) *)
        | Neg -> Some (excise_seg_min_prefix goal msg ssg (Z.neg o_n) vx)
        (* o-n = 0      [k; o)
         * so o = n   ⊢ [l; n) *)
        | Zero -> Some (excise_seg_same goal msg ssg vx)
        (* o-n > 0      [k;   o)
         * so o > n   ⊢ [l; n) *)
        | Pos -> Some (excise_seg_sub_prefix goal msg ssg o_n vx) )
    (* k-l > 0 so k > l *)
    | Pos -> (
        let ko = Term.add k o in
        let ln = Term.add l n in
        let* ko_ln = difference (Sh.ctx sub) ko ln in
        match Int.sign (Z.sign ko_ln) with
        (* k+o-(l+n) < 0        [k; o)
         * so k+o < l+n    ⊢ [l;      n) *)
        | Neg ->
            Some (excise_seg_min_infix goal msg ssg k_l (Z.neg ko_ln) vx)
        (* k+o-(l+n) = 0        [k; o)
         * so k+o = l+n    ⊢ [l;    n) *)
        | Zero -> Some (excise_seg_min_suffix goal msg ssg k_l vx)
        (* k+o-(l+n) > 0 so k+o > l+n *)
        | Pos -> (
            let* k_ln = difference (Sh.ctx sub) k ln in
            match Int.sign (Z.sign k_ln) with
            (* k-(l+n) < 0        [k;  o)
             * so k < l+n    ⊢ [l;   n) *)
            | Neg ->
                Some
                  (excise_seg_sub_skew goal msg ssg k_l (Z.neg k_ln) ko_ln
                     vx )
            | Zero | Pos -> None ) )

let excise_heap ({min; sub} as goal) vx =
  trace (fun {pf} -> pf "excise_heap:@ %a" pp goal) ;
  match
    Iter.find_map (Sh.heap sub) ~f:(fun ssg ->
        Iter.find_map (Sh.heap min) ~f:(fun msg ->
            excise_seg goal msg ssg vx ) )
  with
  | None -> goal
  | Some goal -> {goal with pgs= true}

let pure_entails x q = Sh.is_empty q && Context.implies x (Sh.pure_approx q)

let rec excise ({min; sub; pgs} as goal) vx =
  [%Dbg.info " %a" pp goal] ;
  let goal = check (fun g -> Goal.invariant g vx) goal in
  Report.step_solver () ;
  let is_unsat_min = Sh.is_unsat min vx in
  if is_unsat_min then Some Sh.false_
  else if pure_entails (Sh.ctx min) sub then Some min
  else
    let is_unsat_sub = Sh.is_unsat sub vx in
    if is_unsat_sub then None
    else if pgs then
      let goal = {goal with pgs= false} in
      let goal = excise_exists goal vx in
      let goal = excise_heap goal vx in
      excise goal vx
    else None $> fun _ -> [%Dbg.info " fail@ %a" pp goal]

let excise_dnf : Sh.t -> Var.Set.t -> Sh.t -> Sh.t option Var.Fresh.m =
 fun minuend xs subtrahend vx ->
  let dnf_minuend = Sh.dnf minuend vx in
  let dnf_minuend = Sh.Set.to_iter dnf_minuend in
  let dnf_subtrahend = Sh.dnf subtrahend vx in
  let dnf_subtrahend = Sh.Set.to_iter dnf_subtrahend in
  let excise_subtrahend min sub =
    [%dbg]
      ~call:(fun {pf} -> pf "@ %a" Sh.pp sub)
      ~retn:(fun {pf} -> pf "%a" (Option.pp "%a" Sh.pp))
    @@ fun () ->
    let sub = Sh.and_ctx (Sh.ctx min) sub vx in
    excise {empty with min; xs; sub; pgs= true} vx
  in
  let from_minuend min remainders =
    [%dbg]
      ~call:(fun {pf} -> pf "@ %a" Sh.pp min)
      ~retn:(fun {pf} ->
        pf "%a" (Option.pp "%a" (fun fs rs -> Sh.pp fs (Sh.orN rs))) )
    @@ fun () ->
    let remainder =
      Iter.find_map dnf_subtrahend ~f:(fun sub ->
          excise_subtrahend min sub )
    in
    let+ remainder in
    Sh.Set.add remainder remainders
  in
  let remainders =
    Iter.fold_opt dnf_minuend Sh.Set.empty ~f:(fun min remainders ->
        from_minuend min remainders )
  in
  let+ remainders in
  Sh.orN remainders

let query_count = ref (-1)

let infer_frame : Sh.t -> Var.Set.t -> Sh.t -> Xsh.t option Var.Fresh.m =
 fun minuend xs subtrahend ->
  [%dbgs]
    ~call:(fun {pf} ->
      pf " %i@ @[<hv>%a@ \\- %a%a@]" !query_count Sh.pp minuend
        Var.Set.pp_xs xs Sh.pp subtrahend )
    ~retn:(fun {pf} (r, vxd) ->
      pf "@,%a%a" Var.Context.pp_diff vxd (Option.pp "%a" Xsh.pp) r )
  @@ fun vx ->
  let+ frame = excise_dnf minuend xs subtrahend vx in
  Xsh.exists_fresh frame vx

(*
 * Replay debugging
 *)

type call = Infer_frame of Sh.t * Var.Set.t * Sh.t * Var.Context.t
[@@deriving sexp]

let replay c =
  match call_of_sexp (Sexp.of_string c) with
  | Infer_frame (minuend, xs, subtrahend, vx) ->
      Var.Fresh.gen_ vx (infer_frame minuend xs subtrahend) |> ignore

let dump_query = ref (-1)

let infer_frame minuend xs subtrahend vx =
  Int.incr query_count ;
  if !query_count = !dump_query then
    fail "%a" Sexp.pp_hum
      (sexp_of_call (Infer_frame (minuend, xs, subtrahend, !vx)))
      ()
  else infer_frame minuend xs subtrahend vx
