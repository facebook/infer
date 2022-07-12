(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

open Fol
open Symbolic_heap

[@@@warning "+missing-record-field-pattern"]

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
type spec = {foot: Xsh.t; sub: Var.Subst.t; ms: Var.Set.t; post: Xsh.t}

let gen_spec us specm =
  let xs, spec = Fresh.gen ~wrt:us specm in
  let us =
    Var.Set.union xs (Var.Set.union (Xsh.us spec.foot) (Xsh.us spec.post))
  in
  let foot = Xsh.extend_us us spec.foot in
  let post = Xsh.extend_us us spec.post in
  (xs, {spec with foot; post})

(*
 * Instruction small axioms
 *)

let null_eq ptr = Xsh.pure (Formula.eq0 ptr)

open Fresh.Import

(* { emp }
 *   rs := es
 * { *ᵢ rᵢ=eᵢΘ }
 *)
let move_spec reg_exps =
  let foot = Xsh.emp in
  let ws, rs =
    IArray.fold reg_exps (Var.Set.empty, Var.Set.empty)
      ~f:(fun (reg, exp) (ws, rs) ->
        (Var.Set.add reg ws, Var.Set.union rs (Term.fv exp)) )
  in
  let+ sub, ms = Fresh.assign ~ws ~rs in
  let post =
    Xsh.pure
      (Formula.andN
         (IArray.fold reg_exps [] ~f:(fun (reg, exp) eqs ->
              Formula.eq (Term.var reg) (Term.rename sub exp) :: eqs ) ) )
  in
  {foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   load l r p
 * { r=α * (p-[b;m)->⟨l,α⟩)Θ }
 *)
let load_spec reg ptr len =
  let* seg = Fresh.seg ptr ~siz:len in
  let foot = Xsh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let post =
    Xsh.and_ (Formula.eq (Term.var reg) seg.cnt) (Xsh.rename sub foot)
  in
  {foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   store l p e
 * { p-[b;m)->⟨l,e⟩ }
 *)
let store_spec ptr exp len =
  let+ seg = Fresh.seg ptr ~siz:len in
  let foot = Xsh.seg seg in
  let post = Xsh.seg {seg with cnt= exp} in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   r := atomic_rmw l p e
 * { r=α * pΘ-[b;m)->⟨lΘ,e⟩ }
 *)
let atomic_rmw_spec reg ptr exp len =
  let* seg = Fresh.seg ptr ~siz:len in
  let foot = Xsh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let post =
    Xsh.and_
      (Formula.eq (Term.var reg) seg.cnt)
      (Xsh.seg
         { seg with
           loc= Term.rename sub seg.loc
         ; siz= Term.rename sub seg.siz
         ; cnt= exp } )
  in
  {foot; sub; ms; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   r := cmpxchg l,l₁ p c e
 * { r=(⟨l,α⟩^⟨l₁,1⟩)Θ * (α=c * p-[b;m)->⟨l,e⟩)Θ
 * ∨ r=(⟨l,α⟩^⟨l₁,0⟩)Θ * (α≠c * p-[b;m)->⟨l,α⟩)Θ }
 *)
let atomic_cmpxchg_spec reg ptr cmp exp len len1 =
  let* foot_seg = Fresh.seg ptr ~siz:len in
  let foot = Xsh.seg foot_seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let a = foot_seg.cnt in
  let l_a = {Term.siz= Term.rename sub len; seq= a} in
  let len1' = Term.rename sub len1 in
  let bit b = {Term.siz= len1'; seq= Formula.inject b} in
  let a_eq_c = Formula.rename sub (Formula.eq cmp a) in
  let a_dq_c = Formula.not_ a_eq_c in
  let post_succ =
    Xsh.andN
      [ Formula.eq (Term.var reg) (Term.concat [|l_a; bit Formula.tt|])
      ; a_eq_c ]
      (Xsh.rename sub (Xsh.seg {foot_seg with cnt= exp}))
  in
  let post_fail =
    Xsh.andN
      [ Formula.eq (Term.var reg) (Term.concat [|l_a; bit Formula.ff|])
      ; a_dq_c ]
      (Xsh.rename sub foot)
  in
  let post = Xsh.or_ post_succ post_fail in
  {foot; sub; ms; post}

(* { d-[b;m)->⟨l,α⟩ }
 *   memset l d b
 * { d-[b;m)->⟨l,b^⟩ }
 *)
let memset_spec dst byt len =
  let+ seg = Fresh.seg dst ~siz:len in
  let foot = Xsh.seg seg in
  let post = Xsh.seg {seg with cnt= Term.splat byt} in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d=s * l=0 * d-[b;m)->⟨l,α⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memcpy_eq_spec dst src len =
  let+ seg = Fresh.seg dst ~len in
  let dst_heap = Xsh.seg seg in
  let foot = Xsh.andN [Formula.eq dst src; Formula.eq0 len] dst_heap in
  let post = dst_heap in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memcpy_dj_spec dst src len =
  let* dst_seg = Fresh.seg dst ~siz:len in
  let dst_heap = Xsh.seg dst_seg in
  let+ src_seg = Fresh.seg src ~siz:len in
  let src_heap = Xsh.seg src_seg in
  let dst_seg' = {dst_seg with cnt= src_seg.cnt} in
  let dst_heap' = Xsh.seg dst_seg' in
  let foot = Xsh.star dst_heap src_heap in
  let post = Xsh.star dst_heap' src_heap in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let memcpy_specs = [memcpy_eq_spec; memcpy_dj_spec]

(* { d=s * d-[b;m)->⟨l,α⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memmov_eq_spec dst src len =
  let+ dst_seg = Fresh.seg dst ~len in
  let dst_heap = Xsh.seg dst_seg in
  let foot = Xsh.and_ (Formula.eq dst src) dst_heap in
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
  let+ cnt_src = Fresh.var "a" in
  let src_dst = Term.sub src dst in
  let mem_dst = {Term.siz= src_dst; seq= cnt_dst} in
  let siz_mid = Term.sub len src_dst in
  let mem_mid = {Term.siz= siz_mid; seq= cnt_mid} in
  let mem_src = {Term.siz= src_dst; seq= cnt_src} in
  let cnt = Term.concat [|mem_dst; mem_mid; mem_src|] in
  let seg_siz = Term.add len (Term.sub src dst) in
  let seg = Xsh.seg {loc= dst; bas; len= siz; siz= seg_siz; cnt} in
  let foot =
    Xsh.andN [Formula.lt dst src; Formula.lt src (Term.add dst len)] seg
  in
  (bas, siz, mem_dst, mem_mid, mem_src, seg_siz, foot)

(* { d<s * s<d+l * d-[b;m)->⟨s-d,α⟩^⟨l-(s-d),β⟩^⟨s-d,γ⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l-(s-d),β⟩^⟨s-d,γ⟩^⟨s-d,γ⟩ }
 *)
let memmov_dn_spec dst src len =
  let+ bas, len, _, mem_mid, mem_src, siz, foot = memmov_foot dst src len in
  let cnt = Term.concat [|mem_mid; mem_src; mem_src|] in
  let post = Xsh.seg {loc= dst; bas; len; siz; cnt} in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { s<d * d<s+l * s-[b;m)->⟨d-s,α⟩^⟨l-(d-s),β⟩^⟨d-s,γ⟩ }
 *   memmov l d s
 * { s-[b;m)->⟨d-s,α⟩^⟨d-s,α⟩^⟨l-(d-s),β⟩ }
 *)
let memmov_up_spec dst src len =
  let+ bas, len, mem_src, mem_mid, _, siz, foot = memmov_foot src dst len in
  let cnt = Term.concat [|mem_src; mem_src; mem_mid|] in
  let post = Xsh.seg {loc= src; bas; len; siz; cnt} in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let memmov_specs =
  [memmov_eq_spec; memmov_dj_spec; memmov_dn_spec; memmov_up_spec]

(* { emp }
 *   alloc r [n × l]
 * { ∃α'. r-[r;(n×l)Θ)->⟨(n×l)Θ,α'⟩ }
 *)
let alloc_spec reg num len =
  let foot = Xsh.emp in
  let siz = Term.mulq (Q.of_int len) num in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let post = Xsh.seg seg in
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
  let foot = Xsh.or_ (null_eq ptr) (Xsh.seg seg) in
  let post = Xsh.emp in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   dallocx p
 * { emp }
 *)
let dallocx_spec ptr =
  let* len = Fresh.var "m" in
  let+ seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Xsh.seg seg in
  let post = Xsh.emp in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { emp }
 *   malloc r s
 * { r=0 ∨ ∃α'. r-[r;sΘ)->⟨sΘ,α'⟩ }
 *)
let malloc_spec reg siz =
  let foot = Xsh.emp in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let post = Xsh.or_ (null_eq (Term.var reg)) (Xsh.seg seg) in
  {foot; sub; ms; post}

(* { s≠0 }
 *   mallocx r s
 * { r=0 ∨ ∃α'. r-[r;sΘ)->⟨sΘ,α'⟩ }
 *)
let mallocx_spec reg siz =
  let foot = Xsh.pure (Formula.dq0 siz) in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let post = Xsh.or_ (null_eq (Term.var reg)) (Xsh.seg seg) in
  {foot; sub; ms; post}

(* { emp }
 *   calloc r [n × l]
 * { r=0 ∨ r-[r;(n×l)Θ)->⟨(n×l)Θ,0^⟩ }
 *)
let calloc_spec reg num len =
  let foot = Xsh.emp in
  let siz = Term.mul num len in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Term.fv siz) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let cnt = Term.splat Term.zero in
  let+ seg = Fresh.seg loc ~bas:loc ~len:siz ~siz ~cnt in
  let post = Xsh.or_ (null_eq (Term.var reg)) (Xsh.seg seg) in
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
  let foot = Xsh.seg pseg in
  let* sub, ms =
    Fresh.assign ~ws:(Var.Set.of_ reg)
      ~rs:(Var.Set.union (Xsh.us foot) (Term.fv siz))
  in
  let* q = Fresh.var "q" in
  let pseg' = {pseg with cnt= q} in
  let+ qseg = Fresh.seg q ~bas:q ~len:siz ~siz in
  let eok = Term.zero in
  let enomem = Term.integer (Z.of_int 12) in
  let post =
    Xsh.or_
      (Xsh.and_ (Formula.eq (Term.var reg) enomem) (Xsh.rename sub foot))
      (Xsh.and_
         (Formula.eq (Term.var reg) eok)
         (Xsh.rename sub (Xsh.star (Xsh.seg pseg') (Xsh.seg qseg))) )
  in
  {foot; sub; ms; post}

(* (cnd ? α'=⟨m,α⟩^⟨n-m,α''⟩ : α=⟨n,α'⟩^⟨m-n,α''⟩) *)
let realloc_seq cnd a0 m a1 n a2 =
  Formula.cond ~cnd
    ~pos:
      (Formula.eq a1
         (Term.concat [|{siz= m; seq= a0}; {siz= Term.sub n m; seq= a2}|]) )
    ~neg:
      (Formula.eq a0
         (Term.concat [|{siz= n; seq= a1}; {siz= Term.sub m n; seq= a2}|]) )

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   realloc r p s
 * {   (r=0 * (pΘ=0 ∨ pΘ-[pΘ;m)->⟨m,α⟩))
 *   ∨ ∃α',α'' . r-[r;sΘ)->⟨sΘ,α'⟩
 *     * (m≤sΘ ? α'=⟨m,α⟩^⟨sΘ-m,α''⟩ : α=⟨sΘ,α'⟩^⟨m-sΘ,α''⟩) }
 *)
let realloc_spec reg ptr siz =
  let* len = Fresh.var "m" in
  let* pseg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Xsh.or_ (null_eq ptr) (Xsh.seg pseg) in
  let* sub, ms =
    Fresh.assign ~ws:(Var.Set.of_ reg)
      ~rs:(Var.Set.union (Xsh.us foot) (Term.fv siz))
  in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let* rseg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let a0 = pseg.cnt in
  let a1 = rseg.cnt in
  let+ a2 = Fresh.var "a" in
  let post =
    Xsh.or_
      (Xsh.and_ (Formula.eq0 loc) (Xsh.rename sub foot))
      (Xsh.and_
         (realloc_seq (Formula.le len siz) a0 len a1 siz a2)
         (Xsh.seg rseg) )
  in
  {foot; sub; ms; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   rallocx r p s
 * {   (r=0 * pΘ-[pΘ;m)->⟨m,α⟩)
 *   ∨ ∃α',α'' . r-[r;sΘ)->⟨sΘ,α'⟩
 *     * (m≤sΘ ? α'=⟨m,α⟩^⟨sΘ-m,α''⟩ : α=⟨sΘ,α'⟩^⟨m-sΘ,α''⟩) }
 *)
let rallocx_spec reg ptr siz =
  let* len = Fresh.var "m" in
  let* pseg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let pheap = Xsh.seg pseg in
  let foot = Xsh.and_ (Formula.dq0 siz) pheap in
  let* sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let* rseg = Fresh.seg loc ~bas:loc ~len:siz ~siz in
  let a0 = pseg.cnt in
  let a1 = rseg.cnt in
  let+ a2 = Fresh.var "a" in
  let post =
    Xsh.or_
      (Xsh.and_ (Formula.eq0 loc) (Xsh.rename sub pheap))
      (Xsh.and_
         (realloc_seq (Formula.le len siz) a0 len a1 siz a2)
         (Xsh.seg rseg) )
  in
  {foot; sub; ms; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   xallocx r p s e
 * { ∃α',α'' . sΘ≤r≤(s+e)Θ * pΘ-[pΘ;r)->⟨r,α'⟩
 *   * (m≤r ? α'=⟨m,α⟩^⟨r-m,α''⟩ : α=⟨r,α'⟩^⟨m-r,α''⟩) }
 *)
let xallocx_spec reg ptr siz ext =
  let* len = Fresh.var "m" in
  let* seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Xsh.and_ (Formula.dq0 siz) (Xsh.seg seg) in
  let* sub, ms =
    Fresh.assign ~ws:(Var.Set.of_ reg)
      ~rs:Var.Set.(union (Xsh.us foot) (union (Term.fv siz) (Term.fv ext)))
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
    Xsh.andN
      [ realloc_seq (Formula.le len siz) a0 len a1 siz a2
      ; Formula.le siz reg
      ; Formula.le reg (Term.add siz ext) ]
      (Xsh.seg seg')
  in
  {foot; sub; ms; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   sallocx r p
 * { r=m * (p-[p;m)->⟨m,α⟩)Θ }
 *)
let sallocx_spec reg ptr =
  let* len = Fresh.var "m" in
  let* seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Xsh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let post =
    Xsh.and_ (Formula.eq (Term.var reg) len) (Xsh.rename sub foot)
  in
  {foot; sub; ms; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   malloc_usable_size r p
 * { m≤r * (p-[p;m)->⟨m,α⟩)Θ }
 *)
let malloc_usable_size_spec reg ptr =
  let* len = Fresh.var "m" in
  let* seg = Fresh.seg ptr ~bas:ptr ~len ~siz:len in
  let foot = Xsh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let post =
    Xsh.and_ (Formula.le len (Term.var reg)) (Xsh.rename sub foot)
  in
  {foot; sub; ms; post}

(* { s≠0 }
 *   r = nallocx s
 * { r=0 ∨ r=sΘ }
 *)
let nallocx_spec reg siz =
  let foot = Xsh.pure (Formula.dq0 siz) in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let loc = Term.var reg in
  let siz = Term.rename sub siz in
  let post = Xsh.or_ (null_eq loc) (Xsh.pure (Formula.eq loc siz)) in
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
    Xsh.andN
      [Formula.eq0 w; Formula.eq0 n]
      (Xsh.star (Xsh.seg iseg) (Xsh.seg rseg))
  in
  let rseg' = {rseg with cnt= a} in
  let post = Xsh.star (Xsh.seg rseg') (Xsh.seg iseg) in
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
  let const = Xsh.star (Xsh.seg pseg) (Xsh.seg iseg) in
  let+ a = Fresh.var "a" in
  let foot =
    Xsh.andN [Formula.eq0 w; Formula.eq0 n] (Xsh.star const (Xsh.seg rseg))
  in
  let rseg' = {rseg with cnt= a} in
  let post = Xsh.star (Xsh.seg rseg') const in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

(* { r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl (_, r, i, w, n)
 * { w-[_;_)->⟨n,_⟩ }
 *)
let mallctl_write_spec r i w n =
  let+ seg = Fresh.seg w ~siz:n in
  let post = Xsh.seg seg in
  let foot = Xsh.andN [Formula.eq0 r; Formula.eq0 i] post in
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
  let post = Xsh.star (Xsh.seg pseg) (Xsh.seg wseg) in
  let foot = Xsh.andN [Formula.eq0 r; Formula.eq0 i] post in
  {foot; sub= Var.Subst.empty; ms= Var.Set.empty; post}

let mallctl_specs = [mallctl_read_spec; mallctl_write_spec]
let mallctlbymib_specs = [mallctlbymib_read_spec; mallctlbymib_write_spec]

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
  let foot = Xsh.star (Xsh.seg oseg) (Xsh.seg pseg) in
  let pseg' = {pseg with cnt= a} in
  let post = Xsh.star (Xsh.seg pseg') (Xsh.seg oseg) in
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
  let foot = Xsh.seg seg in
  let+ sub, ms = Fresh.assign ~ws:(Var.Set.of_ reg) ~rs:(Xsh.us foot) in
  let {Sh.loc= p; bas= b; len= m; _} = seg in
  let ret = Term.sub (Term.sub (Term.add b m) p) Term.one in
  let post =
    Xsh.and_
      (Formula.eq (Term.var reg) (Term.rename sub ret))
      (Xsh.rename sub foot)
  in
  {foot; sub; ms; post}

(*
 * Symbolic Execution
 *)

open Option.Import

let pp ppf q =
  Xsh.pp ppf (Option.value q ~default:(Xsh.false_ Var.Set.empty))

let check_preserve_us q0 q1 =
  let gain_us = Var.Set.diff (Xsh.us q1) (Xsh.us q0) in
  let lose_us = Var.Set.diff (Xsh.us q0) (Xsh.us q1) in
  (Var.Set.is_empty gain_us || fail "gain us: %a" Var.Set.pp gain_us ())
  && (Var.Set.is_empty lose_us || fail "lose us: %a" Var.Set.pp lose_us ())

(* execute a command with given explicitly-quantified spec from
   explicitly-quantified pre *)
let exec_spec_ (xs, pre) (gs, {foot; sub; ms; post}) =
  ([%Dbg.call fun {pf} ->
     pf "@ @[%a@]@ @[<2>%a@,@[<hv>{%a  %a}@;<1 -1>%a--@ {%a  }@]@]" Xsh.pp
       pre Xsh.pp_us gs Xsh.pp foot
       (fun fs sub ->
         if not (Var.Subst.is_empty sub) then
           Format.fprintf fs "∧ %a" Var.Subst.pp sub )
       sub
       (fun fs ms ->
         if not (Var.Set.is_empty ms) then
           Format.fprintf fs "%a := " Var.Set.pp ms )
       ms Xsh.pp post ;
     (* gs contains all vars in spec not in pre.us *)
     assert (
       let vs = Var.Set.(diff (diff (Xsh.us foot) gs) (Xsh.us pre)) in
       Var.Set.is_empty vs || fail "unbound foot: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs = Var.Set.(diff (diff ms gs) (Xsh.us pre)) in
       Var.Set.is_empty vs || fail "unbound modif: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs =
         Var.Set.(diff (diff (Var.Subst.domain sub) gs) (Xsh.us pre))
       in
       Var.Set.is_empty vs || fail "unbound write: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs =
         Var.Set.(diff (diff (Var.Subst.range sub) gs) (Xsh.us pre))
       in
       Var.Set.is_empty vs || fail "unbound ghost: {%a}" Var.Set.pp vs () ) ;
     assert (
       let vs = Var.Set.(diff (diff (Xsh.us post) gs) (Xsh.us pre)) in
       Var.Set.is_empty vs || fail "unbound post: {%a}" Var.Set.pp vs () )]
  ;
  let+ frame = Solver.infer_frame pre gs foot in
  Xsh.exists (Var.Set.union xs gs)
    (Xsh.star post (Xsh.exists ms (Xsh.rename sub frame))) )
  |>
  [%Dbg.retn fun {pf} r ->
    pf "%a" pp r ;
    assert (Option.for_all ~f:(check_preserve_us (Xsh.exists xs pre)) r)]

(* execute a command with given spec from pre *)
let exec_spec pre specm =
  let xs, pre = Xsh.bind_exists pre ~wrt:Var.Set.empty in
  exec_spec_ (xs, pre) (gen_spec (Xsh.us pre) specm)

(* execute a multiple-spec command, where the disjunction of the specs
   preconditions are known to be tautologous *)
let exec_specs pre =
  let xs, pre = Xsh.bind_exists pre ~wrt:Var.Set.empty in
  let rec exec_specs_ (xs, pre) = function
    | specm :: specs ->
        let gs, spec = gen_spec (Xsh.us pre) specm in
        let pure = Xsh.pure (Xsh.pure_approx spec.foot) in
        let pre_pure =
          Xsh.star (Xsh.exists (Var.Set.inter gs (Xsh.us pure)) pure) pre
        in
        let* post = exec_spec_ (xs, pre_pure) (gs, spec) in
        let+ posts = exec_specs_ (xs, pre) specs in
        Xsh.or_ post posts
    | [] -> Some (Xsh.false_ Var.Set.empty)
  in
  exec_specs_ (xs, pre)

let exec_specs pre specs =
  [%Dbg.call fun _ -> ()]
  ;
  exec_specs pre specs
  |>
  [%Dbg.retn fun _ r ->
    assert (Option.for_all ~f:(check_preserve_us pre) r)]

(*
 * Exposed interface
 *)

let assume pre cnd =
  [%dbg]
    ~call:(fun {pf} -> pf "@ %a" Formula.pp cnd)
    ~retn:(fun {pf} -> pf "%a" Xsh.pp)
  @@ fun () ->
  let post = Xsh.and_ cnd pre in
  if Xsh.is_unsat post then Xsh.false_ (Xsh.us post) else post

let kill pre reg =
  let ms = Var.Set.of_ reg in
  Xsh.extend_us ms (Xsh.exists ms pre)

let move pre reg_exps =
  exec_spec pre (move_spec reg_exps)
  |> function Some post -> post | _ -> fail "Exec.move failed" ()

let load pre ~reg ~ptr ~len = exec_spec pre (load_spec reg ptr len)
let store pre ~ptr ~exp ~len = exec_spec pre (store_spec ptr exp len)

let atomic_rmw pre ~reg ~ptr ~exp ~len =
  exec_spec pre (atomic_rmw_spec reg ptr exp len)

let atomic_cmpxchg pre ~reg ~ptr ~cmp ~exp ~len ~len1 =
  exec_spec pre (atomic_cmpxchg_spec reg ptr cmp exp len len1)

let alloc pre ~reg ~num ~len = exec_spec pre (alloc_spec reg num len)
let free pre ~ptr = exec_spec pre (free_spec ptr)
let nondet pre = function Some reg -> kill pre reg | None -> pre

let builtin :
       Xsh.t
    -> Var.t option
    -> Llair.Builtin.t
    -> Term.t iarray
    -> Xsh.t option =
 fun pre areturn builtin actuals ->
  match (areturn, builtin, IArray.to_array actuals) with
  (*
   * llvm intrinsics
   *)
  | None, `memset, [|dst; byt; len; _isvolatile|] ->
      exec_spec pre (memset_spec dst byt len)
  | None, `memcpy, [|dst; src; len; _isvolatile|] ->
      exec_specs pre (List.map memcpy_specs ~f:(fun fn -> fn dst src len))
  | None, `memmove, [|dst; src; len; _isvolatile|] ->
      exec_specs pre (List.map memmov_specs ~f:(fun fn -> fn dst src len))
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
      exec_specs pre
        (List.map mallctl_specs ~f:(fun fn -> fn oldp oldlenp newp newlen))
  (* int mallctlnametomib(const char* name, size_t* mibp, size_t*
     miblenp) *)
  | Some _, `mallctlnametomib, [|_; mibp; miblenp|] ->
      exec_spec pre (mallctlnametomib_spec mibp miblenp)
  (* int mallctlbymib(const size_t* mib, size_t miblen, void* oldp, size_t*
     oldlenp, void* newp, size_t newlen); *)
  | Some _, `mallctlbymib, [|mib; miblen; oldp; oldlenp; newp; newlen|] ->
      exec_specs pre
        (List.map mallctlbymib_specs ~f:(fun fn ->
             fn mib miblen oldp oldlenp newp newlen ) )
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
   * cct
   *)
  (* void cct_point() *)
  | None, `cct_point, [||] -> Some pre
  (*
   * signature mismatch
   *)
  | ( _
    , ( `memset | `memcpy | `memmove | `malloc | `aligned_alloc | `calloc
      | `posix_memalign | `realloc | `mallocx | `rallocx | `xallocx
      | `sallocx | `dallocx | `sdallocx | `nallocx | `malloc_usable_size
      | `mallctl | `mallctlnametomib | `mallctlbymib | `strlen
      | `_ZN5folly13usingJEMallocEv | `cct_point )
    , _ ) ->
      fail "%abuiltin %a%a;"
        (Option.pp "%a := " Var.pp)
        areturn Llair.Builtin.pp builtin (IArray.pp "@ " Term.pp) actuals ()
