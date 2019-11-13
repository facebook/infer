(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

[@@@warning "+9"]

(** generic command: ∀xs. {foot ∧ sub} ms := - {post} *)
type spec =
  {xs: Var.Set.t; foot: Sh.t; sub: Var.Subst.t; ms: Var.Set.t; post: Sh.t}

type xseg = {us: Var.Set.t; xs: Var.Set.t; seg: Sh.seg}

let fresh_var nam us xs =
  let var, us = Var.fresh nam ~wrt:us in
  (Term.var var, us, Set.add xs var)

let fresh_seg ~loc ?bas ?len ?siz ?arr ?(xs = Var.Set.empty) us =
  let freshen term nam us xs =
    match term with
    | Some term -> (term, us, xs)
    | None -> fresh_var nam us xs
  in
  let bas, us, xs = freshen bas "b" us xs in
  let len, us, xs = freshen len "m" us xs in
  let siz, us, xs = freshen siz "n" us xs in
  let arr, us, xs = freshen arr "a" us xs in
  {us; xs; seg= {loc; bas; len; siz; arr}}

let null_eq ptr = Sh.pure (Term.eq Term.null ptr)

(* Overwritten variables renaming and remaining modified variables. [ws] are
   the written variables; [rs] are the variables read or in the
   precondition; [us] are the variables to which ghosts must be chosen
   fresh. *)
let assign ~ws ~rs ~us =
  let ovs = Set.inter ws rs in
  let sub = Var.Subst.freshen ovs ~wrt:us in
  let us = Set.union us (Var.Subst.range sub) in
  let ms = Set.diff ws (Var.Subst.domain sub) in
  (sub, ms, us)

(*
 * Instruction small axioms
 *)

(* { emp }
 *   rs := es
 * { *ᵢ rᵢ=eᵢΘ }
 *)
let move_spec us reg_exps =
  let xs = Var.Set.empty in
  let foot = Sh.emp in
  let ws, rs =
    Vector.fold reg_exps ~init:(Var.Set.empty, Var.Set.empty)
      ~f:(fun (ws, rs) (reg, exp) ->
        (Set.add ws reg, Set.union rs (Term.fv exp)) )
  in
  let sub, ms, _ = assign ~ws ~rs ~us in
  let post =
    Vector.fold reg_exps ~init:Sh.emp ~f:(fun post (reg, exp) ->
        Sh.and_ (Term.eq (Term.var reg) (Term.rename sub exp)) post )
  in
  {xs; foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   load l r p
 * { r=αΘ * (p-[b;m)->⟨l,α⟩)Θ }
 *)
let load_spec us reg ptr len =
  let {us; xs; seg} = fresh_seg ~loc:ptr ~siz:len us in
  let foot = Sh.seg seg in
  let sub, ms, _ = assign ~ws:(Var.Set.of_ reg) ~rs:foot.us ~us in
  let post =
    Sh.and_
      (Term.eq (Term.var reg) (Term.rename sub seg.arr))
      (Sh.rename sub foot)
  in
  {xs; foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   store l p e
 * { p-[b;m)->⟨l,e⟩ }
 *)
let store_spec us ptr exp len =
  let {us= _; xs; seg} = fresh_seg ~loc:ptr ~siz:len us in
  let foot = Sh.seg seg in
  let post = Sh.seg {seg with arr= exp} in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ }
 *   memset l d b
 * { d-[b;m)->⟨l,b^l⟩ }
 *)
let memset_spec us dst byt len =
  let {us= _; xs; seg} = fresh_seg ~loc:dst ~siz:len us in
  let foot = Sh.seg seg in
  let post = Sh.seg {seg with arr= Term.splat ~byt ~siz:len} in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d=s * l=0 * d-[b;m)->⟨l,α⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memcpy_eq_spec us dst src len =
  let {us= _; xs; seg} = fresh_seg ~loc:dst ~len us in
  let dst_heap = Sh.seg seg in
  let foot =
    Sh.and_ (Term.eq dst src) (Sh.and_ (Term.eq len Term.zero) dst_heap)
  in
  let post = dst_heap in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memcpy_dj_spec us dst src len =
  let {us; xs; seg= dst_seg} = fresh_seg ~loc:dst ~siz:len us in
  let dst_heap = Sh.seg dst_seg in
  let {us= _; xs; seg= src_seg} = fresh_seg ~loc:src ~siz:len ~xs us in
  let src_heap = Sh.seg src_seg in
  let dst_seg' = {dst_seg with arr= src_seg.arr} in
  let dst_heap' = Sh.seg dst_seg' in
  let foot = Sh.star dst_heap src_heap in
  let post = Sh.star dst_heap' src_heap in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let memcpy_specs us dst src len =
  [memcpy_eq_spec us dst src len; memcpy_dj_spec us dst src len]

(* { d=s * d-[b;m)->⟨l,α⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memmov_eq_spec us dst src len =
  let {us= _; xs; seg= dst_seg} = fresh_seg ~loc:dst ~len us in
  let dst_heap = Sh.seg dst_seg in
  let foot = Sh.and_ (Term.eq dst src) dst_heap in
  let post = dst_heap in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memmov_dj_spec = memcpy_dj_spec

(* memmov footprint for dst < src case *)
let memmov_foot us dst src len =
  let xs = Var.Set.empty in
  let bas, us, xs = fresh_var "b" us xs in
  let siz, us, xs = fresh_var "m" us xs in
  let arr_dst, us, xs = fresh_var "a" us xs in
  let arr_mid, us, xs = fresh_var "a" us xs in
  let arr_src, us, xs = fresh_var "a" us xs in
  let src_dst = Term.sub src dst in
  let mem_dst = Term.memory ~siz:src_dst ~arr:arr_dst in
  let siz_mid = Term.sub len src_dst in
  let mem_mid = Term.memory ~siz:siz_mid ~arr:arr_mid in
  let mem_src = Term.memory ~siz:src_dst ~arr:arr_src in
  let mem_dst_mid_src = Term.concat [|mem_dst; mem_mid; mem_src|] in
  let siz_dst_mid_src, us, xs = fresh_var "m" us xs in
  let arr_dst_mid_src, _, xs = fresh_var "a" us xs in
  let eq_mem_dst_mid_src =
    Term.eq mem_dst_mid_src
      (Term.memory ~siz:siz_dst_mid_src ~arr:arr_dst_mid_src)
  in
  let seg =
    Sh.seg
      {loc= dst; bas; len= siz; siz= siz_dst_mid_src; arr= arr_dst_mid_src}
  in
  let foot =
    Sh.and_ eq_mem_dst_mid_src
      (Sh.and_ (Term.lt dst src)
         (Sh.and_ (Term.lt src (Term.add dst len)) seg))
  in
  (xs, bas, siz, mem_dst, mem_mid, mem_src, foot)

(* { d<s * s<d+l * d-[b;m)->⟨s-d,α⟩^⟨l-(s-d),β⟩^⟨s-d,γ⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l-(s-d),β⟩^⟨s-d,γ⟩^⟨s-d,γ⟩ }
 *)
let memmov_dn_spec us dst src len =
  let xs, bas, siz, _, mem_mid, mem_src, foot =
    memmov_foot us dst src len
  in
  let mem_mid_src_src = Term.concat [|mem_mid; mem_src; mem_src|] in
  let siz_mid_src_src, us, xs = fresh_var "m" us xs in
  let arr_mid_src_src, _, xs = fresh_var "a" us xs in
  let eq_mem_mid_src_src =
    Term.eq mem_mid_src_src
      (Term.memory ~siz:siz_mid_src_src ~arr:arr_mid_src_src)
  in
  let post =
    Sh.and_ eq_mem_mid_src_src
      (Sh.seg
         { loc= dst
         ; bas
         ; len= siz
         ; siz= siz_mid_src_src
         ; arr= arr_mid_src_src })
  in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { s<d * d<s+l * s-[b;m)->⟨d-s,α⟩^⟨l-(d-s),β⟩^⟨d-s,γ⟩ }
 *   memmov l d s
 * { s-[b;m)->⟨d-s,α⟩^⟨d-s,α⟩^⟨l-(d-s),β⟩ }
 *)
let memmov_up_spec us dst src len =
  let xs, bas, siz, mem_src, mem_mid, _, foot =
    memmov_foot us src dst len
  in
  let mem_src_src_mid = Term.concat [|mem_src; mem_src; mem_mid|] in
  let siz_src_src_mid, us, xs = fresh_var "m" us xs in
  let arr_src_src_mid, _, xs = fresh_var "a" us xs in
  let eq_mem_src_src_mid =
    Term.eq mem_src_src_mid
      (Term.memory ~siz:siz_src_src_mid ~arr:arr_src_src_mid)
  in
  let post =
    Sh.and_ eq_mem_src_src_mid
      (Sh.seg
         { loc= src
         ; bas
         ; len= siz
         ; siz= siz_src_src_mid
         ; arr= arr_src_src_mid })
  in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let memmov_specs us dst src len =
  [ memmov_eq_spec us dst src len; memmov_dj_spec us dst src len
  ; memmov_dn_spec us dst src len; memmov_up_spec us dst src len ]

(* { emp }
 *   alloc r [n × l]
 * { ∃α'. r-[r;(n×l)Θ)->⟨(n×l)Θ,α'⟩ }
 *)
let alloc_spec us reg num len =
  let foot = Sh.emp in
  let siz = Term.mul num len in
  let sub, ms, us = assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) ~us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let {us= _; xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz us in
  let post = Sh.seg seg in
  {xs; foot; sub; ms; post}

(*
 * Memory management - see e.g. http://jemalloc.net/jemalloc.3.html
 *)

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   free p
 * { emp }
 *)
let free_spec us ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us= _; xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.or_ (null_eq ptr) (Sh.seg seg) in
  let post = Sh.emp in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   dallocx p
 * { emp }
 *)
let dallocx_spec us ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us= _; xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.seg seg in
  let post = Sh.emp in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { emp }
 *   malloc r s
 * { r=0 ∨ ∃α'. r-[r;sΘ)->⟨sΘ,α'⟩ }
 *)
let malloc_spec us reg siz =
  let foot = Sh.emp in
  let sub, ms, us = assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) ~us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let {us= _; xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz us in
  let post = Sh.or_ (null_eq (Term.var reg)) (Sh.seg seg) in
  {xs; foot; sub; ms; post}

(* { s≠0 }
 *   mallocx r s
 * { r=0 ∨ ∃α'. r-[r;sΘ)->⟨sΘ,α'⟩ }
 *)
let mallocx_spec us reg siz =
  let foot = Sh.pure Term.(dq siz zero) in
  let sub, ms, us = assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) ~us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let {us= _; xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz us in
  let post = Sh.or_ (null_eq (Term.var reg)) (Sh.seg seg) in
  {xs; foot; sub; ms; post}

(* { emp }
 *   calloc r [n × l]
 * { r=0 ∨ r-[r;(n×l)Θ)->⟨(n×l)Θ,0^(n×l)Θ⟩ }
 *)
let calloc_spec us reg num len =
  let foot = Sh.emp in
  let siz = Term.mul num len in
  let sub, ms, us = assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) ~us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let arr = Term.splat ~byt:Term.zero ~siz in
  let {us= _; xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz ~arr us in
  let post = Sh.or_ (null_eq (Term.var reg)) (Sh.seg seg) in
  {xs; foot; sub; ms; post}

let size_of_ptr = Term.size_of Typ.ptr

(* { p-[_;_)->⟨W,_⟩ }
 *   posix_memalign r p s
 * {   r=ENOMEM * (p-[_;_)->⟨W,_⟩)Θ
 *   ∨ ∃α',q. r=0 * (p-[_;_)->⟨W,q⟩ * q-[q;s)->⟨s,α'⟩)Θ }
 * where W = sizeof void*
 *)
let posix_memalign_spec us reg ptr siz =
  let {us; xs; seg= pseg} = fresh_seg ~loc:ptr ~siz:size_of_ptr us in
  let foot = Sh.seg pseg in
  let sub, ms, us =
    assign ~ws:(Var.Set.of_ reg) ~rs:(Set.union foot.us (Term.fv siz)) ~us
  in
  let q, us, xs = fresh_var "q" us xs in
  let pseg' = {pseg with arr= q} in
  let {us= _; xs; seg= qseg} =
    fresh_seg ~loc:q ~bas:q ~len:siz ~siz ~xs us
  in
  let eok = Term.zero in
  let enomem = Term.integer (Z.of_int 12) in
  let post =
    Sh.or_
      (Sh.and_ (Term.eq (Term.var reg) enomem) (Sh.rename sub foot))
      (Sh.and_
         (Term.eq (Term.var reg) eok)
         (Sh.rename sub (Sh.star (Sh.seg pseg') (Sh.seg qseg))))
  in
  {xs; foot; sub; ms; post}

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   realloc r p s
 * {   (r=0 * (pΘ=0 ∨ pΘ-[pΘ;m)->⟨m,α⟩))
 *   ∨ ∃α',α'' . r-[r;sΘ)->⟨sΘ,α'⟩
 *     * (m≤sΘ ? ⟨sΘ,α'⟩=⟨m,α⟩^⟨sΘ-m,α''⟩ : ⟨m,α⟩=⟨sΘ,α'⟩^⟨m-sΘ,α''⟩) }
 *)
let realloc_spec us reg ptr siz =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg= pseg} =
    fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us
  in
  let foot = Sh.or_ (null_eq ptr) (Sh.seg pseg) in
  let sub, ms, us =
    assign ~ws:(Var.Set.of_ reg) ~rs:(Set.union foot.us (Term.fv siz)) ~us
  in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let {us; xs; seg= rseg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz ~xs us in
  let a0 = pseg.arr in
  let a1 = rseg.arr in
  let a2, _, xs = fresh_var "a" us xs in
  let post =
    Sh.or_
      (Sh.and_ Term.(eq loc null) (Sh.rename sub foot))
      (Sh.and_
         Term.(
           conditional ~cnd:(le len siz)
             ~thn:
               (eq (memory ~siz ~arr:a1)
                  (concat
                     [| memory ~siz:len ~arr:a0
                      ; memory ~siz:(sub siz len) ~arr:a2 |]))
             ~els:
               (eq (memory ~siz:len ~arr:a0)
                  (concat
                     [| memory ~siz ~arr:a1
                      ; memory ~siz:(sub len siz) ~arr:a2 |])))
         (Sh.seg rseg))
  in
  {xs; foot; sub; ms; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   rallocx r p s
 * {   (r=0 * pΘ-[pΘ;m)->⟨m,α⟩)
 *   ∨ ∃α',α'' . r-[r;sΘ)->⟨sΘ,α'⟩
 *     * (m≤sΘ ? ⟨sΘ,α'⟩=⟨m,α⟩^⟨sΘ-m,α''⟩ : ⟨m,α⟩=⟨sΘ,α'⟩^⟨m-sΘ,α''⟩) }
 *)
let rallocx_spec us reg ptr siz =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg= pseg} =
    fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us
  in
  let pheap = Sh.seg pseg in
  let foot = Sh.and_ Term.(dq siz zero) pheap in
  let sub, ms, us = assign ~ws:(Var.Set.of_ reg) ~rs:foot.us ~us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let {us; xs; seg= rseg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz ~xs us in
  let a0 = pseg.arr in
  let a1 = rseg.arr in
  let a2, _, xs = fresh_var "a" us xs in
  let post =
    Sh.or_
      (Sh.and_ Term.(eq loc null) (Sh.rename sub pheap))
      (Sh.and_
         Term.(
           conditional ~cnd:(le len siz)
             ~thn:
               (eq (memory ~siz ~arr:a1)
                  (concat
                     [| memory ~siz:len ~arr:a0
                      ; memory ~siz:(sub siz len) ~arr:a2 |]))
             ~els:
               (eq (memory ~siz:len ~arr:a0)
                  (concat
                     [| memory ~siz ~arr:a1
                      ; memory ~siz:(sub len siz) ~arr:a2 |])))
         (Sh.seg rseg))
  in
  {xs; foot; sub; ms; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   xallocx r p s e
 * { ∃α',α'' . sΘ≤r≤(s+e)Θ * pΘ-[pΘ;r)->⟨r,α'⟩
 *   * (m≤r ? ⟨r,α'⟩=⟨m,α⟩^⟨r-m,α''⟩ : ⟨m,α⟩=⟨r,α'⟩^⟨m-r,α''⟩) }
 *)
let xallocx_spec us reg ptr siz ext =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.and_ Term.(dq siz zero) (Sh.seg seg) in
  let sub, ms, us =
    assign ~ws:(Var.Set.of_ reg)
      ~rs:Set.(union foot.us (union (Term.fv siz) (Term.fv ext)))
      ~us
  in
  let reg = Term.var reg in
  let ptr = Term.rename sub ptr in
  let siz = Term.rename sub siz in
  let ext = Term.rename sub ext in
  let {us; xs; seg= seg'} =
    fresh_seg ~loc:ptr ~bas:ptr ~len:reg ~siz:reg ~xs us
  in
  let a0 = seg.arr in
  let a1 = seg'.arr in
  let a2, _, xs = fresh_var "a" us xs in
  let post =
    Sh.and_
      Term.(
        and_
          (conditional ~cnd:(le len siz)
             ~thn:
               (eq (memory ~siz ~arr:a1)
                  (concat
                     [| memory ~siz:len ~arr:a0
                      ; memory ~siz:(sub siz len) ~arr:a2 |]))
             ~els:
               (eq (memory ~siz:len ~arr:a0)
                  (concat
                     [| memory ~siz ~arr:a1
                      ; memory ~siz:(sub len siz) ~arr:a2 |])))
          (and_ (le siz reg) (le reg (add siz ext))))
      (Sh.seg seg')
  in
  {xs; foot; sub; ms; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   sallocx r p
 * { r=m * (p-[p;m)->⟨m,α⟩)Θ }
 *)
let sallocx_spec us reg ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.seg seg in
  let sub, ms, _ = assign ~ws:(Var.Set.of_ reg) ~rs:foot.us ~us in
  let post = Sh.and_ Term.(eq (var reg) len) (Sh.rename sub foot) in
  {xs; foot; sub; ms; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   malloc_usable_size r p
 * { m≤r * (p-[p;m)->⟨m,α⟩)Θ }
 *)
let malloc_usable_size_spec us reg ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.seg seg in
  let sub, ms, _ = assign ~ws:(Var.Set.of_ reg) ~rs:foot.us ~us in
  let post = Sh.and_ Term.(le len (var reg)) (Sh.rename sub foot) in
  {xs; foot; sub; ms; post}

(* { s≠0 }
 *   r = nallocx s
 * { r=0 ∨ r=sΘ }
 *)
let nallocx_spec us reg siz =
  let xs = Var.Set.empty in
  let foot = Sh.pure (Term.dq siz Term.zero) in
  let sub, ms, _ = assign ~ws:(Var.Set.of_ reg) ~rs:foot.us ~us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let post = Sh.or_ (null_eq loc) (Sh.pure (Term.eq loc siz)) in
  {xs; foot; sub; ms; post}

let size_of_int_mul = Term.mul (Term.size_of Typ.siz)

(* { r-[_;_)->⟨m,_⟩ * i-[_;_)->⟨_,m⟩ * w=0 * n=0 }
 *   mallctl r i w n
 * { ∃α'. r-[_;_)->⟨m,α'⟩ * i-[_;_)->⟨_,m⟩ }
 *)
let mallctl_read_spec us r i w n =
  let {us; xs; seg= iseg} = fresh_seg ~loc:i us in
  let {us; xs; seg= rseg} = fresh_seg ~loc:r ~siz:iseg.arr ~xs us in
  let a, _, xs = fresh_var "a" us xs in
  let foot =
    Sh.and_
      Term.(eq w null)
      (Sh.and_ Term.(eq n zero) (Sh.star (Sh.seg iseg) (Sh.seg rseg)))
  in
  let rseg' = {rseg with arr= a} in
  let post = Sh.star (Sh.seg rseg') (Sh.seg iseg) in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[_;_)->⟨W×l,_⟩ * r-[_;_)->⟨m,_⟩ * i-[_;_)->⟨_,m⟩ * w=0 * n=0 }
 *   mallctlbymib p l r i w n
 * { ∃α'. p-[_;_)->⟨W×l,_⟩ * r-[_;_)->⟨m,α'⟩ * i-[_;_)->⟨_,m⟩ }
 * where W = sizeof int
 *)
let mallctlbymib_read_spec us p l r i w n =
  let wl = size_of_int_mul l in
  let {us; xs; seg= pseg} = fresh_seg ~loc:p ~siz:wl us in
  let {us; xs; seg= iseg} = fresh_seg ~loc:i ~xs us in
  let m = iseg.arr in
  let {us; xs; seg= rseg} = fresh_seg ~loc:r ~siz:m ~xs us in
  let const = Sh.star (Sh.seg pseg) (Sh.seg iseg) in
  let a, _, xs = fresh_var "a" us xs in
  let foot =
    Sh.and_
      Term.(eq w null)
      (Sh.and_ Term.(eq n zero) (Sh.star const (Sh.seg rseg)))
  in
  let rseg' = {rseg with arr= a} in
  let post = Sh.star (Sh.seg rseg') const in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl r i w n
 * { w-[_;_)->⟨n,_⟩ }
 *)
let mallctl_write_spec us r i w n =
  let {us= _; xs; seg} = fresh_seg ~loc:w ~siz:n us in
  let post = Sh.seg seg in
  let foot = Sh.and_ Term.(eq r null) (Sh.and_ Term.(eq i zero) post) in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[_;_)->⟨W×l,_⟩ * r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl r i w n
 * { p-[_;_)->⟨W×l,_⟩ * w-[_;_)->⟨n,_⟩ }
 * where W = sizeof int
 *)
let mallctlbymib_write_spec us p l r i w n =
  let wl = size_of_int_mul l in
  let {us; xs; seg= pseg} = fresh_seg ~loc:p ~siz:wl us in
  let {us= _; xs; seg= wseg} = fresh_seg ~loc:w ~siz:n ~xs us in
  let post = Sh.star (Sh.seg pseg) (Sh.seg wseg) in
  let foot = Sh.and_ Term.(eq r null) (Sh.and_ Term.(eq i zero) post) in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let mallctl_specs us r i w n =
  [mallctl_read_spec us r i w n; mallctl_write_spec us r i w n]

let mallctlbymib_specs us p j r i w n =
  [ mallctlbymib_read_spec us p j r i w n
  ; mallctlbymib_write_spec us p j r i w n ]

(* { p-[_;_)->⟨W×n,α⟩ * o-[_;_)->⟨_,n⟩ }
 *   mallctlnametomib p o
 * { ∃α'.
 *   p-[_;_)->⟨W×n,α'⟩ * o-[_;_)->⟨_,n⟩ }
 * where W = sizeof int
 *
 * Note: post is too strong, more accurate would be:
 * { ∃α',α²,α³,n'. ⟨W×n,α⟩=⟨W×n',α³⟩^⟨W×(n-n'),α²⟩ *
 *   p-[_;_)->⟨W×n',α'⟩ * p+W×n'-[_;_)->⟨W×(n-n'),α²⟩ * o-[_;_)->⟨_,n'⟩ }
 *)
let mallctlnametomib_spec us p o =
  let {us; xs; seg= oseg} = fresh_seg ~loc:o us in
  let n = oseg.arr in
  let wn = size_of_int_mul n in
  let {us; xs; seg= pseg} = fresh_seg ~loc:p ~siz:wn ~xs us in
  let a, _, xs = fresh_var "a" us xs in
  let foot = Sh.star (Sh.seg oseg) (Sh.seg pseg) in
  let pseg' = {pseg with arr= a} in
  let post = Sh.star (Sh.seg pseg') (Sh.seg oseg) in
  {xs; foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(*
 * cstring - see e.g. http://www.cplusplus.com/reference/cstring/
 *)

(* { p-[b;m)->⟨l,α⟩ }
 *   r = strlen p
 * { r=(b+m-p-1)Θ * (p-[b;m)->⟨l,α⟩)Θ }
 *)
let strlen_spec us reg ptr =
  let {us; xs; seg} = fresh_seg ~loc:ptr us in
  let foot = Sh.seg seg in
  let sub, ms, _ = assign ~ws:(Var.Set.of_ reg) ~rs:foot.us ~us in
  let {Sh.loc= p; bas= b; len= m; _} = seg in
  let ret = Term.sub (Term.sub (Term.add b m) p) Term.one in
  let post =
    Sh.and_
      (Term.eq (Term.var reg) (Term.rename sub ret))
      (Sh.rename sub foot)
  in
  {xs; foot; sub; ms; post}

(*
 * Symbolic Execution
 *)

let check_preserve_us (q0 : Sh.t) (q1 : Sh.t) =
  let gain_us = Set.diff q1.us q0.us in
  let lose_us = Set.diff q0.us q1.us in
  (Set.is_empty gain_us || fail "gain us: %a" Var.Set.pp gain_us ())
  && (Set.is_empty lose_us || fail "lose us: %a" Var.Set.pp lose_us ())

(* execute a command with given spec from pre *)
let exec_spec pre0 {xs; foot; sub; ms; post} =
  [%Trace.call fun {pf} ->
    pf "@[%a@]@ @[<2>%a@,@[<hv>{%a  %a}@;<1 -1>%a--@ {%a  }@]@]" Sh.pp pre0
      (Sh.pp_us ~pre:"@<2>∀ ")
      xs Sh.pp foot
      (fun fs sub ->
        if not (Var.Subst.is_empty sub) then
          Format.fprintf fs "∧ %a" Var.Subst.pp sub )
      sub
      (fun fs ms ->
        if not (Set.is_empty ms) then
          Format.fprintf fs "%a := " Var.Set.pp ms )
      ms Sh.pp post ;
    assert (
      let vs = Set.diff (Set.diff foot.us xs) pre0.us in
      Set.is_empty vs || fail "unbound foot: {%a}" Var.Set.pp vs () ) ;
    assert (
      let vs = Set.diff (Set.diff post.us xs) pre0.us in
      Set.is_empty vs || fail "unbound post: {%a}" Var.Set.pp vs () )]
  ;
  let foot = Sh.extend_us xs foot in
  let zs, pre = Sh.bind_exists pre0 ~wrt:xs in
  ( Solver.infer_frame pre xs foot
  >>| fun frame ->
  Sh.exists (Set.union zs xs)
    (Sh.star post (Sh.exists ms (Sh.rename sub frame))) )
  |>
  [%Trace.retn fun {pf} r ->
    pf "%a" (Option.pp "%a" Sh.pp) r ;
    assert (Option.for_all ~f:(check_preserve_us pre0) r)]

(* execute a multiple-spec command, where the disjunction of the specs
   preconditions are known to be tautologous *)
let rec exec_specs pre = function
  | ({xs; foot; _} as spec) :: specs ->
      let foot = Sh.extend_us xs foot in
      let pre_pure = Sh.star (Sh.exists xs (Sh.pure_approx foot)) pre in
      exec_spec pre_pure spec
      >>= fun post ->
      exec_specs pre specs >>| fun posts -> Sh.or_ post posts
  | [] -> Some (Sh.false_ pre.us)

let exec_specs pre specs =
  [%Trace.call fun _ -> ()]
  ;
  exec_specs pre specs
  |>
  [%Trace.retn fun _ r ->
    assert (Option.for_all ~f:(check_preserve_us pre) r)]

(*
 * Exposed interface
 *)

let assume pre cnd =
  let post = Sh.and_ cnd pre in
  if Sh.is_false post then None else Some post

let kill pre reg =
  let ms = Var.Set.of_ reg in
  Sh.extend_us ms (Sh.exists ms pre)

let move pre reg_exps =
  exec_spec pre (move_spec pre.us reg_exps)
  |> function Some post -> post | _ -> fail "Exec.move failed" ()

let load pre ~reg ~ptr ~len = exec_spec pre (load_spec pre.us reg ptr len)
let store pre ~ptr ~exp ~len = exec_spec pre (store_spec pre.us ptr exp len)

let memset pre ~dst ~byt ~len =
  exec_spec pre (memset_spec pre.us dst byt len)

let memcpy pre ~dst ~src ~len =
  exec_specs pre (memcpy_specs pre.us dst src len)

let memmov pre ~dst ~src ~len =
  exec_specs pre (memmov_specs pre.us dst src len)

let alloc pre ~reg ~num ~len = exec_spec pre (alloc_spec pre.us reg num len)
let free pre ~ptr = exec_spec pre (free_spec pre.us ptr)
let nondet pre = function Some reg -> kill pre reg | None -> pre
let abort _ = None

let intrinsic ~skip_throw :
    Sh.t -> Var.t option -> Var.t -> Term.t list -> Sh.t option option =
 fun pre areturn intrinsic actuals ->
  let us = pre.us in
  let name =
    let n = Var.name intrinsic in
    match String.index n '.' with None -> n | Some i -> String.prefix n i
  in
  let skip pre = Some (Some pre) in
  ( match (areturn, name, actuals) with
  (*
   * cstdlib - memory management
   *)
  (* void* malloc(size_t size) *)
  | Some reg, "malloc", [size]
  (* void* aligned_alloc(size_t alignment, size_t size) *)
   |Some reg, "aligned_alloc", [size; _] ->
      Some (exec_spec pre (malloc_spec us reg size))
  (* void* calloc(size_t number, size_t size) *)
  | Some reg, "calloc", [size; number] ->
      Some (exec_spec pre (calloc_spec us reg number size))
  (* int posix_memalign(void** ptr, size_t alignment, size_t size) *)
  | Some reg, "posix_memalign", [size; _; ptr] ->
      Some (exec_spec pre (posix_memalign_spec us reg ptr size))
  (* void* realloc(void* ptr, size_t size) *)
  | Some reg, "realloc", [size; ptr] ->
      Some (exec_spec pre (realloc_spec us reg ptr size))
  (*
   * jemalloc - non-standard API
   *)
  (* void* mallocx(size_t size, int flags) *)
  | Some reg, "mallocx", [_; size] ->
      Some (exec_spec pre (mallocx_spec us reg size))
  (* void* rallocx(void* ptr, size_t size, int flags) *)
  | Some reg, "rallocx", [_; size; ptr] ->
      Some (exec_spec pre (rallocx_spec us reg ptr size))
  (* size_t xallocx(void* ptr, size_t size, size_t extra, int flags) *)
  | Some reg, "xallocx", [_; extra; size; ptr] ->
      Some (exec_spec pre (xallocx_spec us reg ptr size extra))
  (* size_t sallocx(void* ptr, int flags) *)
  | Some reg, "sallocx", [_; ptr] ->
      Some (exec_spec pre (sallocx_spec us reg ptr))
  (* void dallocx(void* ptr, int flags) *)
  | None, "dallocx", [_; ptr]
  (* void sdallocx(void* ptr, size_t size, int flags) *)
   |None, "sdallocx", [_; _; ptr] ->
      Some (exec_spec pre (dallocx_spec us ptr))
  (* size_t nallocx(size_t size, int flags) *)
  | Some reg, "nallocx", [_; size] ->
      Some (exec_spec pre (nallocx_spec us reg size))
  (* size_t malloc_usable_size(void* ptr) *)
  | Some reg, "malloc_usable_size", [ptr] ->
      Some (exec_spec pre (malloc_usable_size_spec us reg ptr))
  (* int mallctl(const char* name, void* oldp, size_t* oldlenp, void* newp,
     size_t newlen) *)
  | Some _, "mallctl", [newlen; newp; oldlenp; oldp; _] ->
      Some (exec_specs pre (mallctl_specs us oldp oldlenp newp newlen))
  (* int mallctlnametomib(const char* name, size_t* mibp, size_t* miblenp) *)
  | Some _, "mallctlnametomib", [miblenp; mibp; _] ->
      Some (exec_spec pre (mallctlnametomib_spec us mibp miblenp))
  (* int mallctlbymib(const size_t* mib, size_t miblen, void* oldp, size_t*
     oldlenp, void* newp, size_t newlen); *)
  | Some _, "mallctlbymib", [newlen; newp; oldlenp; oldp; miblen; mib] ->
      Some
        (exec_specs pre
           (mallctlbymib_specs us mib miblen oldp oldlenp newp newlen))
  | _, "malloc_stats_print", _ -> skip pre
  (*
   * cstring
   *)
  (* size_t strlen (const char* ptr) *)
  | Some reg, "strlen", [ptr] ->
      Some (exec_spec pre (strlen_spec us reg ptr))
  (*
   * cxxabi
   *)
  | Some _, "__cxa_allocate_exception", [_] when skip_throw ->
      skip (Sh.false_ pre.us)
  (*
   * folly
   *)
  (* bool folly::usingJEMalloc() *)
  | Some _, "_ZN5folly13usingJEMallocEv", [] -> skip pre
  | _ -> None )
  $> function
  | None -> ()
  | Some _ ->
      [%Trace.info
        "@[<2>exec intrinsic@ @[%a%a(@[%a@])@] from@ @[{ %a@ }@]@]"
          (Option.pp "%a := " Var.pp)
          areturn Var.pp intrinsic (List.pp ",@ " Term.pp)
          (List.rev actuals) Sh.pp pre]
