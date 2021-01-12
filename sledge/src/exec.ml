(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

open Fol

[@@@warning "+9"]

module Fresh : sig
  (** Monad to manage generation of fresh variables. A value of type ['a t]
      is a value of type ['a] which may contain as-yet-unnamed variables. *)
  include Monad.S

  val gen : wrt:Var.Set.t -> 'a t -> Var.Set.t * 'a
  (** [gen ~wrt a] generates names that are fresh with respect to [wrt] for
      all unnamed variables in [a], and yields the set of generated
      variables together with [a] expressed in terms of those variables. *)

  val var : string -> Term.t t
  (** Fresh variable whose name is based on the given string. *)

  val seg :
       ?bas:Term.t
    -> ?len:Term.t
    -> ?siz:Term.t
    -> ?cnt:Term.t
    -> Term.t
    -> Sh.seg t
  (** Segment with fresh variables for omitted arguments. *)

  val assign : ws:Var.Set.t -> rs:Var.Set.t -> (Var.Subst.t * Var.Set.t) t
  (** Renaming of fresh ghosts for overwritten variables, and remaining
      modified but not read variables, given written variables [ws] and read
      (or appearing in the precondition) variables [rs]. *)
end = struct
  type ctx = {wrt: Var.Set.t; xs: Var.Set.t}

  include Monad.State (struct
    type t = ctx
  end)

  open Import

  let gen ~wrt m =
    let a, {xs; wrt= _} = run m {wrt; xs= Var.Set.empty} in
    (xs, a)

  let var nam {wrt; xs} =
    let x, wrt = Var.fresh nam ~wrt in
    let xs = Var.Set.add x xs in
    return (Term.var x) {wrt; xs}

  let seg ?bas ?len ?siz ?cnt loc =
    let freshen term nam =
      match term with Some term -> return term | None -> var nam
    in
    let* bas = freshen bas "b" in
    let* len = freshen len "m" in
    let* siz = freshen siz "n" in
    let* cnt = freshen cnt "a" in
    return Sh.{loc; bas; len; siz; cnt}

  let assign ~ws ~rs {wrt; xs} =
    let ovs = Var.Set.inter ws rs in
    let Var.Subst.{sub; dom; rng}, wrt = Var.Subst.freshen ovs ~wrt in
    let ms = Var.Set.diff ws dom in
    let xs = Var.Set.union xs rng in
    return (sub, ms) {wrt; xs}
end

(** generic command: [{foot ∧ sub} ms := - {post}] *)
type spec = {foot: Sh.t; sub: Var.Subst.t; ms: Var.Set.t; post: Sh.t}

let gen_spec us specm =
  let xs, spec = Fresh.gen ~wrt:us specm in
  let us = Var.Set.union xs (Var.Set.union spec.foot.us spec.post.us) in
  let foot = Sh.extend_us us spec.foot in
  let post = Sh.extend_us us spec.post in
  (xs, {spec with foot; post})

(*
 * Instruction small axioms
 *)

let null_eq ptr = Sh.pure (Formula.eq0 ptr)

let eq_concat (siz, cnt) ms =
  Formula.eq
    (Term.sized ~siz ~seq:cnt)
    (Term.concat
       (Array.map ~f:(fun (siz, cnt) -> Term.sized ~siz ~seq:cnt) ms))

open Fresh.Import

(* { emp }
 *   rs := es
 * { *ᵢ rᵢ=eᵢΘ }
 *)
let move_spec reg_exps =
  let foot = Sh.emp in
  let ws, rs =
    IArray.fold reg_exps (Var.Set.empty, Var.Set.empty)
      ~f:(fun (reg, exp) (ws, rs) ->
        (Var.Set.add reg ws, Var.Set.union rs (Term.fv exp)) )
  in
  let+ sub, ms = Fresh.assign ~ws ~rs in
  let post =
    IArray.fold reg_exps Sh.emp ~f:(fun (reg, exp) post ->
        Sh.and_ (Formula.eq (Term.var reg) (Term.rename sub exp)) post )
  in
  {foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   load l r p
 * { r=αΘ * (p-[b;m)->⟨l,α⟩)Θ }
 *)
let load_spec reg ptr len =
  let* seg = Fresh.seg ptr ~siz:len in
  let foot = Sh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:foot.us in
  let post =
    Sh.and_
      (Formula.eq (Term.var reg) (Term.rename sub seg.cnt))
      (Sh.rename sub foot)
  in
  {foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   store l p e
 * { p-[b;m)->⟨l,e⟩ }
 *)
let store_spec ptr exp len =
  let+ seg = Fresh.seg ptr ~siz:len in
  let foot = Sh.seg seg in
  let post = Sh.seg {seg with cnt= exp} in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ }
 *   memset l d b
 * { d-[b;m)->⟨l,b^⟩ }
 *)
let memset_spec dst byt len =
  let+ seg = Fresh.seg dst ~siz:len in
  let foot = Sh.seg seg in
  let post = Sh.seg {seg with cnt= Term.splat byt} in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d=s * l=0 * d-[b;m)->⟨l,α⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memcpy_eq_spec dst src len =
  let+ seg = Fresh.seg dst ~len in
  let dst_heap = Sh.seg seg in
  let foot =
    Sh.and_ (Formula.eq dst src) (Sh.and_ (Formula.eq0 len) dst_heap)
  in
  let post = dst_heap in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memcpy_dj_spec dst src len =
  let* dst_seg = Fresh.seg dst ~siz:len in
  let dst_heap = Sh.seg dst_seg in
  let+ src_seg = Fresh.seg src ~siz:len in
  let src_heap = Sh.seg src_seg in
  let dst_seg' = {dst_seg with cnt= src_seg.cnt} in
  let dst_heap' = Sh.seg dst_seg' in
  let foot = Sh.star dst_heap src_heap in
  let post = Sh.star dst_heap' src_heap in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let memcpy_specs dst src len =
  [memcpy_eq_spec dst src len; memcpy_dj_spec dst src len]

(* { d=s * d-[b;m)->⟨l,α⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memmov_eq_spec dst src len =
  let+ dst_seg = Fresh.seg dst ~len in
  let dst_heap = Sh.seg dst_seg in
  let foot = Sh.and_ (Formula.eq dst src) dst_heap in
  let post = dst_heap in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memmov_dj_spec = memcpy_dj_spec

(* memmov footprint for dst < src case *)
let memmov_foot dst src len =
  let* bas = Fresh.var "b" in
  let* siz = Fresh.var "m" in
  let* cnt_dst = Fresh.var "a" in
  let* cnt_mid = Fresh.var "a" in
  let* cnt_src = Fresh.var "a" in
  let src_dst = Term.sub src dst in
  let mem_dst = (src_dst, cnt_dst) in
  let siz_mid = Term.sub len src_dst in
  let mem_mid = (siz_mid, cnt_mid) in
  let mem_src = (src_dst, cnt_src) in
  let mem_dst_mid_src = [|mem_dst; mem_mid; mem_src|] in
  let* siz_dst_mid_src = Fresh.var "m" in
  let+ cnt_dst_mid_src = Fresh.var "a" in
  let eq_mem_dst_mid_src =
    eq_concat (siz_dst_mid_src, cnt_dst_mid_src) mem_dst_mid_src
  in
  let seg =
    Sh.seg
      {loc= dst; bas; len= siz; siz= siz_dst_mid_src; cnt= cnt_dst_mid_src}
  in
  let foot =
    Sh.and_ eq_mem_dst_mid_src
      (Sh.and_ (Formula.lt dst src)
         (Sh.and_ (Formula.lt src (Term.add dst len)) seg))
  in
  (bas, siz, mem_dst, mem_mid, mem_src, foot)

(* { d<s * s<d+l * d-[b;m)->⟨s-d,α⟩^⟨l-(s-d),β⟩^⟨s-d,γ⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l-(s-d),β⟩^⟨s-d,γ⟩^⟨s-d,γ⟩ }
 *)
let memmov_dn_spec dst src len =
  let* bas, siz, _, mem_mid, mem_src, foot = memmov_foot dst src len in
  let mem_mid_src_src = [|mem_mid; mem_src; mem_src|] in
  let* siz_mid_src_src = Fresh.var "m" in
  let+ cnt_mid_src_src = Fresh.var "a" in
  let eq_mem_mid_src_src =
    eq_concat (siz_mid_src_src, cnt_mid_src_src) mem_mid_src_src
  in
  let post =
    Sh.and_ eq_mem_mid_src_src
      (Sh.seg
         { loc= dst
         ; bas
         ; len= siz
         ; siz= siz_mid_src_src
         ; cnt= cnt_mid_src_src })
  in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { s<d * d<s+l * s-[b;m)->⟨d-s,α⟩^⟨l-(d-s),β⟩^⟨d-s,γ⟩ }
 *   memmov l d s
 * { s-[b;m)->⟨d-s,α⟩^⟨d-s,α⟩^⟨l-(d-s),β⟩ }
 *)
let memmov_up_spec dst src len =
  let* bas, siz, mem_src, mem_mid, _, foot = memmov_foot src dst len in
  let mem_src_src_mid = [|mem_src; mem_src; mem_mid|] in
  let* siz_src_src_mid = Fresh.var "m" in
  let+ cnt_src_src_mid = Fresh.var "a" in
  let eq_mem_src_src_mid =
    eq_concat (siz_src_src_mid, cnt_src_src_mid) mem_src_src_mid
  in
  let post =
    Sh.and_ eq_mem_src_src_mid
      (Sh.seg
         { loc= src
         ; bas
         ; len= siz
         ; siz= siz_src_src_mid
         ; cnt= cnt_src_src_mid })
  in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let memmov_specs dst src len =
  [ memmov_eq_spec dst src len
  ; memmov_dj_spec dst src len
  ; memmov_dn_spec dst src len
  ; memmov_up_spec dst src len ]

(* { emp }
 *   alloc r [n × l]
 * { ∃α'. r-[r;(n×l)Θ)->⟨(n×l)Θ,α'⟩ }
 *)
let alloc_spec reg num len =
  let foot = Sh.emp in
  let siz = Term.mulq (Q.of_int len) num in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let post = Sh.seg seg in
  {foot; sub; ms; post}

(*
 * Memory management - see e.g. http://jemalloc.net/jemalloc.3.html
 *)

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   free p
 * { emp }
 *)
let free_spec ptr =
  let* len = Fresh.var "m" in
  let+ seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Sh.or_ (null_eq ptr) (Sh.seg seg) in
  let post = Sh.emp in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   dallocx p
 * { emp }
 *)
let dallocx_spec ptr =
  let* len = Fresh.var "m" in
  let+ seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Sh.seg seg in
  let post = Sh.emp in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { emp }
 *   malloc r s
 * { r=0 ∨ ∃α'. r-[r;sΘ)->⟨sΘ,α'⟩ }
 *)
let malloc_spec reg siz =
  let foot = Sh.emp in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let post = Sh.or_ (null_eq (Term.var reg)) (Sh.seg seg) in
  {foot; sub; ms; post}

(* { s≠0 }
 *   mallocx r s
 * { r=0 ∨ ∃α'. r-[r;sΘ)->⟨sΘ,α'⟩ }
 *)
let mallocx_spec reg siz =
  let foot = Sh.pure (Formula.dq0 siz) in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let post = Sh.or_ (null_eq (Term.var reg)) (Sh.seg seg) in
  {foot; sub; ms; post}

(* { emp }
 *   calloc r [n × l]
 * { r=0 ∨ r-[r;(n×l)Θ)->⟨(n×l)Θ,0^⟩ }
 *)
let calloc_spec reg num len =
  let foot = Sh.emp in
  let siz = Term.mul num len in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let cnt = Term.splat Term.zero in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz ~cnt in
  let post = Sh.or_ (null_eq (Term.var reg)) (Sh.seg seg) in
  {foot; sub; ms; post}

let size_of_ptr = Term.integer (Z.of_int Llair.Typ.(size_of ptr))
let size_of_siz = Term.integer (Z.of_int Llair.Typ.(size_of siz))

(* { p-[_;_)->⟨W,_⟩ }
 *   posix_memalign r p s
 * {   r=ENOMEM * (p-[_;_)->⟨W,_⟩)Θ
 *   ∨ ∃α',q. r=0 * (p-[_;_)->⟨W,q⟩ * q-[q;s)->⟨s,α'⟩)Θ }
 * where W = sizeof void*
 *)
let posix_memalign_spec reg ptr siz =
  let* pseg = Fresh.seg ptr ~siz:size_of_ptr in
  let foot = Sh.seg pseg in
  let* sub, ms =
    Fresh.assign ~ws:(Var.Set.of_ reg)
      ~rs:(Var.Set.union foot.us (Term.fv siz))
  in
  let* q = Fresh.var "q" in
  let pseg' = {pseg with cnt= q} in
  let+ qseg = Fresh.seg q ~bas:q ~len:siz ~siz in
  let eok = Term.zero in
  let enomem = Term.integer (Z.of_int 12) in
  let post =
    Sh.or_
      (Sh.and_ (Formula.eq (Term.var reg) enomem) (Sh.rename sub foot))
      (Sh.and_
         (Formula.eq (Term.var reg) eok)
         (Sh.rename sub (Sh.star (Sh.seg pseg') (Sh.seg qseg))))
  in
  {foot; sub; ms; post}

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   realloc r p s
 * {   (r=0 * (pΘ=0 ∨ pΘ-[pΘ;m)->⟨m,α⟩))
 *   ∨ ∃α',α'' . r-[r;sΘ)->⟨sΘ,α'⟩
 *     * (m≤sΘ ? ⟨sΘ,α'⟩=⟨m,α⟩^⟨sΘ-m,α''⟩ : ⟨m,α⟩=⟨sΘ,α'⟩^⟨m-sΘ,α''⟩) }
 *)
let realloc_spec reg ptr siz =
  let* len = Fresh.var "m" in
  let* pseg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Sh.or_ (null_eq ptr) (Sh.seg pseg) in
  let* sub, ms =
    Fresh.assign ~ws:(Var.Set.of_ reg)
      ~rs:(Var.Set.union foot.us (Term.fv siz))
  in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let* rseg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let a0 = pseg.cnt in
  let a1 = rseg.cnt in
  let+ a2 = Fresh.var "a" in
  let post =
    Sh.or_
      (Sh.and_ (Formula.eq0 loc) (Sh.rename sub foot))
      (Sh.and_
         (Formula.cond ~cnd:(Formula.le len siz)
            ~pos:(eq_concat (siz, a1) [|(len, a0); (Term.sub siz len, a2)|])
            ~neg:(eq_concat (len, a0) [|(siz, a1); (Term.sub len siz, a2)|]))
         (Sh.seg rseg))
  in
  {foot; sub; ms; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   rallocx r p s
 * {   (r=0 * pΘ-[pΘ;m)->⟨m,α⟩)
 *   ∨ ∃α',α'' . r-[r;sΘ)->⟨sΘ,α'⟩
 *     * (m≤sΘ ? ⟨sΘ,α'⟩=⟨m,α⟩^⟨sΘ-m,α''⟩ : ⟨m,α⟩=⟨sΘ,α'⟩^⟨m-sΘ,α''⟩) }
 *)
let rallocx_spec reg ptr siz =
  let* len = Fresh.var "m" in
  let* pseg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let pheap = Sh.seg pseg in
  let foot = Sh.and_ (Formula.dq0 siz) pheap in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:foot.us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let* rseg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let a0 = pseg.cnt in
  let a1 = rseg.cnt in
  let+ a2 = Fresh.var "a" in
  let post =
    Sh.or_
      (Sh.and_ (Formula.eq0 loc) (Sh.rename sub pheap))
      (Sh.and_
         (Formula.cond ~cnd:(Formula.le len siz)
            ~pos:(eq_concat (siz, a1) [|(len, a0); (Term.sub siz len, a2)|])
            ~neg:(eq_concat (len, a0) [|(siz, a1); (Term.sub len siz, a2)|]))
         (Sh.seg rseg))
  in
  {foot; sub; ms; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   xallocx r p s e
 * { ∃α',α'' . sΘ≤r≤(s+e)Θ * pΘ-[pΘ;r)->⟨r,α'⟩
 *   * (m≤r ? ⟨r,α'⟩=⟨m,α⟩^⟨r-m,α''⟩ : ⟨m,α⟩=⟨r,α'⟩^⟨m-r,α''⟩) }
 *)
let xallocx_spec reg ptr siz ext =
  let* len = Fresh.var "m" in
  let* seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Sh.and_ (Formula.dq0 siz) (Sh.seg seg) in
  let* sub, ms =
    Fresh.assign ~ws:(Var.Set.of_ reg)
      ~rs:Var.Set.(union foot.us (union (Term.fv siz) (Term.fv ext)))
  in
  let reg = Term.var reg in
  let ptr = Term.rename sub ptr in
  let siz = Term.rename sub siz in
  let ext = Term.rename sub ext in
  let* seg' = Fresh.seg ptr ~bas:ptr ~len:reg ~siz:reg in
  let a0 = seg.cnt in
  let a1 = seg'.cnt in
  let+ a2 = Fresh.var "a" in
  let post =
    Sh.and_
      (Formula.and_
         (Formula.cond ~cnd:(Formula.le len siz)
            ~pos:(eq_concat (siz, a1) [|(len, a0); (Term.sub siz len, a2)|])
            ~neg:(eq_concat (len, a0) [|(siz, a1); (Term.sub len siz, a2)|]))
         (Formula.and_ (Formula.le siz reg)
            (Formula.le reg (Term.add siz ext))))
      (Sh.seg seg')
  in
  {foot; sub; ms; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   sallocx r p
 * { r=m * (p-[p;m)->⟨m,α⟩)Θ }
 *)
let sallocx_spec reg ptr =
  let* len = Fresh.var "m" in
  let* seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Sh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:foot.us in
  let post = Sh.and_ (Formula.eq (Term.var reg) len) (Sh.rename sub foot) in
  {foot; sub; ms; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   malloc_usable_size r p
 * { m≤r * (p-[p;m)->⟨m,α⟩)Θ }
 *)
let malloc_usable_size_spec reg ptr =
  let* len = Fresh.var "m" in
  let* seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Sh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:foot.us in
  let post = Sh.and_ (Formula.le len (Term.var reg)) (Sh.rename sub foot) in
  {foot; sub; ms; post}

(* { s≠0 }
 *   r = nallocx s
 * { r=0 ∨ r=sΘ }
 *)
let nallocx_spec reg siz =
  let foot = Sh.pure (Formula.dq0 siz) in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:foot.us in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let post = Sh.or_ (null_eq loc) (Sh.pure (Formula.eq loc siz)) in
  {foot; sub; ms; post}

let size_of_int_mul = Term.mulq (Q.of_int Llair.Typ.(size_of siz))

(* { r-[_;_)->⟨m,_⟩ * i-[_;_)->⟨W,m⟩ * w=0 * n=0 }
 *   mallctl (_, r, i, w, n)
 * { ∃α'. r-[_;_)->⟨m,α'⟩ * i-[_;_)->⟨W,m⟩ }
 * where W = sizeof size_t
 *)
let mallctl_read_spec r i w n =
  let* iseg = Fresh.seg i ~siz:size_of_siz in
  let* rseg = Fresh.seg r ~siz:iseg.cnt in
  let+ a = Fresh.var "a" in
  let foot =
    Sh.and_ (Formula.eq0 w)
      (Sh.and_ (Formula.eq0 n) (Sh.star (Sh.seg iseg) (Sh.seg rseg)))
  in
  let rseg' = {rseg with cnt= a} in
  let post = Sh.star (Sh.seg rseg') (Sh.seg iseg) in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[_;_)->⟨W×l,_⟩ * r-[_;_)->⟨m,_⟩ * i-[_;_)->⟨_,m⟩ * w=0 * n=0 }
 *   mallctlbymib p l r i w n
 * { ∃α'. p-[_;_)->⟨W×l,_⟩ * r-[_;_)->⟨m,α'⟩ * i-[_;_)->⟨_,m⟩ }
 * where W = sizeof int
 *)
let mallctlbymib_read_spec p l r i w n =
  let wl = size_of_int_mul l in
  let* pseg = Fresh.seg p ~siz:wl in
  let* iseg = Fresh.seg i in
  let m = iseg.cnt in
  let* rseg = Fresh.seg r ~siz:m in
  let const = Sh.star (Sh.seg pseg) (Sh.seg iseg) in
  let+ a = Fresh.var "a" in
  let foot =
    Sh.and_ (Formula.eq0 w)
      (Sh.and_ (Formula.eq0 n) (Sh.star const (Sh.seg rseg)))
  in
  let rseg' = {rseg with cnt= a} in
  let post = Sh.star (Sh.seg rseg') const in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl (_, r, i, w, n)
 * { w-[_;_)->⟨n,_⟩ }
 *)
let mallctl_write_spec r i w n =
  let+ seg = Fresh.seg w ~siz:n in
  let post = Sh.seg seg in
  let foot = Sh.and_ (Formula.eq0 r) (Sh.and_ (Formula.eq0 i) post) in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[_;_)->⟨W×l,_⟩ * r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl (_, r, i, w, n)
 * { p-[_;_)->⟨W×l,_⟩ * w-[_;_)->⟨n,_⟩ }
 * where W = sizeof int
 *)
let mallctlbymib_write_spec p l r i w n =
  let wl = size_of_int_mul l in
  let* pseg = Fresh.seg p ~siz:wl in
  let+ wseg = Fresh.seg w ~siz:n in
  let post = Sh.star (Sh.seg pseg) (Sh.seg wseg) in
  let foot = Sh.and_ (Formula.eq0 r) (Sh.and_ (Formula.eq0 i) post) in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let mallctl_specs r i w n =
  [mallctl_read_spec r i w n; mallctl_write_spec r i w n]

let mallctlbymib_specs p j r i w n =
  [mallctlbymib_read_spec p j r i w n; mallctlbymib_write_spec p j r i w n]

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
let mallctlnametomib_spec p o =
  let* oseg = Fresh.seg o in
  let n = oseg.cnt in
  let wn = size_of_int_mul n in
  let* pseg = Fresh.seg p ~siz:wn in
  let+ a = Fresh.var "a" in
  let foot = Sh.star (Sh.seg oseg) (Sh.seg pseg) in
  let pseg' = {pseg with cnt= a} in
  let post = Sh.star (Sh.seg pseg') (Sh.seg oseg) in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(*
 * cstring - see e.g. http://www.cplusplus.com/reference/cstring/
 *)

(* { p-[b;m)->⟨l,α⟩ }
 *   r = strlen p
 * { r=(b+m-p-1)Θ * (p-[b;m)->⟨l,α⟩)Θ }
 *)
let strlen_spec reg ptr =
  let* seg = Fresh.seg ptr in
  let foot = Sh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:foot.us in
  let {Sh.loc= p; bas= b; len= m; _} = seg in
  let ret = Term.sub (Term.sub (Term.add b m) p) Term.one in
  let post =
    Sh.and_
      (Formula.eq (Term.var reg) (Term.rename sub ret))
      (Sh.rename sub foot)
  in
  {foot; sub; ms; post}

(*
 * Symbolic Execution
 *)

open Option.Import

let pp ppf q = Sh.pp ppf (Option.value q ~default:(Sh.false_ Var.Set.empty))

let check_preserve_us (q0 : Sh.t) (q1 : Sh.t) =
  let gain_us = Var.Set.diff q1.us q0.us in
  let lose_us = Var.Set.diff q0.us q1.us in
  (Var.Set.is_empty gain_us || fail "gain us: %a" Var.Set.pp gain_us ())
  && (Var.Set.is_empty lose_us || fail "lose us: %a" Var.Set.pp lose_us ())

(* execute a command with given explicitly-quantified spec from
   explicitly-quantified pre *)
let exec_spec_ (xs, pre) (gs, {foot; sub; ms; post}) =
  ([%Trace.call fun {pf} ->
     pf "@ @[%a@]@ @[<2>%a@,@[<hv>{%a  %a}@;<1 -1>%a--@ {%a  }@]@]" Sh.pp
       pre Sh.pp_us gs Sh.pp foot
       (fun fs sub ->
         if not (Var.Subst.is_empty sub) then
           Format.fprintf fs "∧ %a" Var.Subst.pp sub )
       sub
       (fun fs ms ->
         if not (Var.Set.is_empty ms) then
           Format.fprintf fs "%a := " Var.Set.pp ms )
       ms Sh.pp post ;
     (* gs contains all vars in spec not in pre.us *)
     assert (
       let vs = Var.Set.(diff (diff foot.us gs) pre.us) in
       Var.Set.is_empty vs || fail "unbound foot: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs = Var.Set.(diff (diff ms gs) pre.us) in
       Var.Set.is_empty vs || fail "unbound modif: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs = Var.Set.(diff (diff (Var.Subst.domain sub) gs) pre.us) in
       Var.Set.is_empty vs || fail "unbound write: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs = Var.Set.(diff (diff (Var.Subst.range sub) gs) pre.us) in
       Var.Set.is_empty vs || fail "unbound ghost: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs = Var.Set.(diff (diff post.us gs) pre.us) in
       Var.Set.is_empty vs || fail "unbound post: {%a}" Var.Set.pp vs () )]
  ;
  let+ frame = Solver.infer_frame pre gs foot in
  Sh.exists (Var.Set.union xs gs)
    (Sh.star post (Sh.exists ms (Sh.rename sub frame))))
  |>
  [%Trace.retn fun {pf} r ->
    pf "%a" pp r ;
    assert (Option.for_all ~f:(check_preserve_us (Sh.exists xs pre)) r)]

(* execute a command with given spec from pre *)
let exec_spec pre specm =
  let xs, pre = Sh.bind_exists pre ~wrt:Var.Set.empty in
  exec_spec_ (xs, pre) (gen_spec pre.us specm)

(* execute a multiple-spec command, where the disjunction of the specs
   preconditions are known to be tautologous *)
let exec_specs pre =
  let xs, pre = Sh.bind_exists pre ~wrt:Var.Set.empty in
  let rec exec_specs_ (xs, pre) = function
    | specm :: specs ->
        let gs, spec = gen_spec pre.Sh.us specm in
        let pure = Sh.pure (Sh.pure_approx spec.foot) in
        let pre_pure =
          Sh.star (Sh.exists (Var.Set.inter gs pure.us) pure) pre
        in
        let* post = exec_spec_ (xs, pre_pure) (gs, spec) in
        let+ posts = exec_specs_ (xs, pre) specs in
        Sh.or_ post posts
    | [] -> Some (Sh.false_ Var.Set.empty)
  in
  exec_specs_ (xs, pre)

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
  [%trace]
    ~call:(fun {pf} -> pf "@ %a" Formula.pp cnd)
    ~retn:(fun {pf} -> pf "%a" Sh.pp)
  @@ fun () -> Sh.and_ cnd pre

let kill pre reg =
  let ms = Var.Set.of_ reg in
  Sh.extend_us ms (Sh.exists ms pre)

let move pre reg_exps =
  exec_spec pre (move_spec reg_exps)
  |> function Some post -> post | _ -> fail "Exec.move failed" ()

let load pre ~reg ~ptr ~len = exec_spec pre (load_spec reg ptr len)
let store pre ~ptr ~exp ~len = exec_spec pre (store_spec ptr exp len)
let alloc pre ~reg ~num ~len = exec_spec pre (alloc_spec reg num len)
let free pre ~ptr = exec_spec pre (free_spec ptr)
let nondet pre = function Some reg -> kill pre reg | None -> pre
let abort _ = None

let intrinsic :
       Sh.t
    -> Var.t option
    -> Llair.Intrinsic.t
    -> Term.t iarray
    -> Sh.t option =
 fun pre areturn intrinsic actuals ->
  match (areturn, intrinsic, IArray.to_array actuals) with
  (*
   * llvm intrinsics
   *)
  | None, `memset, [|dst; byt; len; _isvolatile|] ->
      exec_spec pre (memset_spec dst byt len)
  | None, `memcpy, [|dst; src; len; _isvolatile|] ->
      exec_specs pre (memcpy_specs dst src len)
  | None, `memmove, [|dst; src; len; _isvolatile|] ->
      exec_specs pre (memmov_specs dst src len)
  (*
   * cstdlib - memory management
   *)
  (* void* malloc(size_t size) *)
  | Some reg, `malloc, [|size|]
  (* void* aligned_alloc(size_t alignment, size_t size) *)
   |Some reg, `aligned_alloc, [|_; size|] ->
      exec_spec pre (malloc_spec reg size)
  (* void* calloc(size_t number, size_t size) *)
  | Some reg, `calloc, [|number; size|] ->
      exec_spec pre (calloc_spec reg number size)
  (* int posix_memalign(void** ptr, size_t alignment, size_t size) *)
  | Some reg, `posix_memalign, [|ptr; _; size|] ->
      exec_spec pre (posix_memalign_spec reg ptr size)
  (* void* realloc(void* ptr, size_t size) *)
  | Some reg, `realloc, [|ptr; size|] ->
      exec_spec pre (realloc_spec reg ptr size)
  (*
   * jemalloc - non-standard API
   *)
  (* void* mallocx(size_t size, int flags) *)
  | Some reg, `mallocx, [|size; _|] -> exec_spec pre (mallocx_spec reg size)
  (* void* rallocx(void* ptr, size_t size, int flags) *)
  | Some reg, `rallocx, [|ptr; size; _|] ->
      exec_spec pre (rallocx_spec reg ptr size)
  (* size_t xallocx(void* ptr, size_t size, size_t extra, int flags) *)
  | Some reg, `xallocx, [|ptr; size; extra; _|] ->
      exec_spec pre (xallocx_spec reg ptr size extra)
  (* size_t sallocx(void* ptr, int flags) *)
  | Some reg, `sallocx, [|ptr; _|] -> exec_spec pre (sallocx_spec reg ptr)
  (* void dallocx(void* ptr, int flags) *)
  | None, `dallocx, [|ptr; _|]
  (* void sdallocx(void* ptr, size_t size, int flags) *)
   |None, `sdallocx, [|ptr; _; _|] ->
      exec_spec pre (dallocx_spec ptr)
  (* size_t nallocx(size_t size, int flags) *)
  | Some reg, `nallocx, [|size; _|] -> exec_spec pre (nallocx_spec reg size)
  (* size_t malloc_usable_size(void* ptr) *)
  | Some reg, `malloc_usable_size, [|ptr|] ->
      exec_spec pre (malloc_usable_size_spec reg ptr)
  (* int mallctl(const char* name, void* oldp, size_t* oldlenp, void* newp,
     size_t newlen) *)
  | Some _, `mallctl, [|_; oldp; oldlenp; newp; newlen|] ->
      exec_specs pre (mallctl_specs oldp oldlenp newp newlen)
  (* int mallctlnametomib(const char* name, size_t* mibp, size_t* miblenp) *)
  | Some _, `mallctlnametomib, [|_; mibp; miblenp|] ->
      exec_spec pre (mallctlnametomib_spec mibp miblenp)
  (* int mallctlbymib(const size_t* mib, size_t miblen, void* oldp, size_t*
     oldlenp, void* newp, size_t newlen); *)
  | Some _, `mallctlbymib, [|mib; miblen; oldp; oldlenp; newp; newlen|] ->
      exec_specs pre
        (mallctlbymib_specs mib miblen oldp oldlenp newp newlen)
  | _, `malloc_stats_print, _ -> Some pre
  (*
   * cstring
   *)
  (* size_t strlen (const char* ptr) *)
  | Some reg, `strlen, [|ptr|] -> exec_spec pre (strlen_spec reg ptr)
  (*
   * folly
   *)
  (* bool folly::usingJEMalloc() *)
  | Some _, `_ZN5folly13usingJEMallocEv, [||] -> Some pre
  (*
   * signature mismatch
   *)
  | ( _
    , ( `memset | `memcpy | `memmove | `malloc | `aligned_alloc | `calloc
      | `posix_memalign | `realloc | `mallocx | `rallocx | `xallocx
      | `sallocx | `dallocx | `sdallocx | `nallocx | `malloc_usable_size
      | `mallctl | `mallctlnametomib | `mallctlbymib | `strlen
      | `_ZN5folly13usingJEMallocEv )
    , _ ) ->
      fail "%aintrinsic %a%a;"
        (Option.pp "%a := " Var.pp)
        areturn Llair.Intrinsic.pp intrinsic (IArray.pp "@ " Term.pp)
        actuals ()
