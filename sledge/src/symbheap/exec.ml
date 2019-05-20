(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

(** generic command: ∀xs.{foot}-{post} *)
type spec = {xs: Var.Set.t; foot: Sh.t; post: Sh.t}

type xseg = {us: Var.Set.t; xs: Var.Set.t; seg: Sh.seg}

let fresh_var nam us xs =
  let var, us = Var.fresh nam ~wrt:us in
  (Exp.var var, us, Set.add xs var)

let fresh_seg ~loc ?bas ?len ?siz ?arr ?(xs = Var.Set.empty) us =
  let freshen exp nam us xs =
    match exp with Some exp -> (exp, us, xs) | None -> fresh_var nam us xs
  in
  let bas, us, xs = freshen bas "b" us xs in
  let len, us, xs = freshen len "m" us xs in
  let siz, us, xs = freshen siz "n" us xs in
  let arr, us, xs = freshen arr "a" us xs in
  {us; xs; seg= {loc; bas; len; siz; arr}}

let null_eq ptr = Sh.pure (Exp.eq Exp.null ptr)
let zero = Exp.integer Z.zero Typ.siz

(*
 * Instruction small axioms
 *)

let assume cnd pre =
  let post = Sh.and_ cnd pre in
  if Sh.is_false post then None else Some post

(* { p-[b;m)->⟨l,α⟩ }
 *   load l r p
 * { r=α * p-[b;m)->⟨l,α⟩ }
 *)
let load_spec us reg ptr len =
  let {xs; seg} = fresh_seg ~loc:ptr ~siz:len us in
  let foot = Sh.seg seg in
  let post = Sh.and_ (Exp.eq (Exp.var reg) seg.arr) foot in
  {xs; foot; post}

(* { p-[b;m)->⟨l,α⟩ }
 *   store l p e
 * { p-[b;m)->⟨l,e⟩ }
 *)
let store_spec us ptr exp len =
  let {xs; seg} = fresh_seg ~loc:ptr ~siz:len us in
  let foot = Sh.seg seg in
  let post = Sh.seg {seg with arr= exp} in
  {xs; foot; post}

(* { d-[b;m)->⟨l,α⟩ }
 *   memset l d b
 * { d-[b;m)->⟨l,b^l⟩ }
 *)
let memset_spec us dst byt len =
  let {xs; seg} = fresh_seg ~loc:dst ~siz:len us in
  let foot = Sh.seg seg in
  let post = Sh.seg {seg with arr= Exp.splat ~byt ~siz:len} in
  {xs; foot; post}

(* { d=s * l=0 * d-[b;m)->⟨l,α⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memcpy_eq_spec us dst src len =
  let {xs; seg} = fresh_seg ~loc:dst ~len us in
  let dst_heap = Sh.seg seg in
  let foot =
    Sh.and_ (Exp.eq dst src) (Sh.and_ (Exp.eq len zero) dst_heap)
  in
  let post = dst_heap in
  {xs; foot; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memcpy_dj_spec us dst src len =
  let {us; xs; seg= dst_seg} = fresh_seg ~loc:dst ~siz:len us in
  let dst_heap = Sh.seg dst_seg in
  let {us; xs; seg= src_seg} = fresh_seg ~loc:src ~siz:len ~xs us in
  let src_heap = Sh.seg src_seg in
  let {seg= dst_seg'} =
    fresh_seg ~loc:dst ~bas:dst_seg.bas ~len:dst_seg.len ~siz:dst_seg.siz
      ~arr:src_seg.arr us
  in
  let dst_heap' = Sh.seg dst_seg' in
  let foot = Sh.star dst_heap src_heap in
  let post = Sh.star dst_heap' src_heap in
  {xs; foot; post}

let memcpy_specs us dst src len =
  [memcpy_eq_spec us dst src len; memcpy_dj_spec us dst src len]

(* { d=s * d-[b;m)->⟨l,α⟩ }
 *   memmov l d s
 * { d-[b;m)->⟨l,α⟩ }
 *)
let memmov_eq_spec us dst src len =
  let {xs; seg= dst_seg} = fresh_seg ~loc:dst ~len us in
  let dst_heap = Sh.seg dst_seg in
  let foot = Sh.and_ (Exp.eq dst src) dst_heap in
  let post = dst_heap in
  {xs; foot; post}

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
  let src_dst = Exp.sub Typ.siz src dst in
  let mem_dst = Exp.memory ~siz:src_dst ~arr:arr_dst in
  let siz_mid = Exp.sub Typ.siz len src_dst in
  let mem_mid = Exp.memory ~siz:siz_mid ~arr:arr_mid in
  let mem_src = Exp.memory ~siz:src_dst ~arr:arr_src in
  let mem_dst_mid_src = Exp.concat [|mem_dst; mem_mid; mem_src|] in
  let siz_dst_mid_src, us, xs = fresh_var "m" us xs in
  let arr_dst_mid_src, _, xs = fresh_var "a" us xs in
  let eq_mem_dst_mid_src =
    Exp.eq mem_dst_mid_src
      (Exp.memory ~siz:siz_dst_mid_src ~arr:arr_dst_mid_src)
  in
  let seg =
    Sh.seg
      {loc= dst; bas; len= siz; siz= siz_dst_mid_src; arr= arr_dst_mid_src}
  in
  let foot =
    Sh.and_ eq_mem_dst_mid_src
      (Sh.and_ (Exp.lt dst src)
         (Sh.and_ (Exp.lt src (Exp.add Typ.ptr dst len)) seg))
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
  let mem_mid_src_src = Exp.concat [|mem_mid; mem_src; mem_src|] in
  let siz_mid_src_src, us, xs = fresh_var "m" us xs in
  let arr_mid_src_src, _, xs = fresh_var "a" us xs in
  let eq_mem_mid_src_src =
    Exp.eq mem_mid_src_src
      (Exp.memory ~siz:siz_mid_src_src ~arr:arr_mid_src_src)
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
  {xs; foot; post}

(* { s<d * d<s+l * s-[b;m)->⟨d-s,α⟩^⟨l-(d-s),β⟩^⟨d-s,γ⟩ }
 *   memmov l d s
 * { s-[b;m)->⟨d-s,α⟩^⟨d-s,α⟩^⟨l-(d-s),β⟩ }
 *)
let memmov_up_spec us dst src len =
  let xs, bas, siz, mem_src, mem_mid, _, foot =
    memmov_foot us src dst len
  in
  let mem_src_src_mid = Exp.concat [|mem_src; mem_src; mem_mid|] in
  let siz_src_src_mid, us, xs = fresh_var "m" us xs in
  let arr_src_src_mid, _, xs = fresh_var "a" us xs in
  let eq_mem_src_src_mid =
    Exp.eq mem_src_src_mid
      (Exp.memory ~siz:siz_src_src_mid ~arr:arr_src_src_mid)
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
  {xs; foot; post}

let memmov_specs us dst src len =
  [ memmov_eq_spec us dst src len
  ; memmov_dj_spec us dst src len
  ; memmov_dn_spec us dst src len
  ; memmov_up_spec us dst src len ]

(* { emp }
 *   alloc r [n × l]
 * { ∃α'. r-[r;n×l)->⟨n×l,α'⟩ }
 *)
let alloc_spec us reg num len =
  let loc = Exp.var reg in
  let siz = Exp.mul Typ.siz num len in
  let {xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz us in
  let post = Sh.seg seg in
  let foot = Sh.extend_us xs Sh.emp in
  {xs; foot; post}

(*
 * Memory management - see e.g. http://jemalloc.net/jemalloc.3.html
 *)

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   free p
 * { emp }
 *)
let free_spec us ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.or_ (null_eq ptr) (Sh.seg seg) in
  let post = Sh.emp in
  {xs; foot; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   dallocx p
 * { emp }
 *)
let dallocx_spec us ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.seg seg in
  let post = Sh.emp in
  {xs; foot; post}

(* { emp }
 *   malloc r s
 * { r=0 ∨ ∃α'. r-[r;s)->⟨s,α'⟩ }
 *)
let malloc_spec us reg siz =
  let loc = Exp.var reg in
  let {xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz us in
  let foot = Sh.extend_us xs Sh.emp in
  let post = Sh.or_ (null_eq (Exp.var reg)) (Sh.seg seg) in
  {xs; foot; post}

(* { s≠0 }
 *   mallocx r s
 * { r=0 ∨ ∃α'. r-[r;s)->⟨s,α'⟩ }
 *)
let mallocx_spec us reg siz =
  let loc = Exp.var reg in
  let {xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz us in
  let foot = Sh.extend_us xs (Sh.pure Exp.(dq siz zero)) in
  let post = Sh.or_ (null_eq (Exp.var reg)) (Sh.seg seg) in
  {xs; foot; post}

(* { emp }
 *   calloc r [n × l]
 * { r=0 ∨ r-[r;n×l)->⟨n×l,0^n×l⟩ }
 *)
let calloc_spec us reg num len =
  let loc = Exp.var reg in
  let byt = Exp.integer Z.zero Typ.byt in
  let siz = Exp.mul Typ.siz num len in
  let arr = Exp.splat ~byt ~siz in
  let {xs; seg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz ~arr us in
  let foot = Sh.emp in
  let post = Sh.or_ (null_eq (Exp.var reg)) (Sh.seg seg) in
  {xs; foot; post}

let size_of_ptr = Option.value_exn (Exp.size_of Typ.ptr)

(* { p-[_;_)->⟨W,_⟩ }
 *   posix_memalign r p s
 * {   r=ENOMEM * p-[_;_)->⟨W,_⟩
 *   ∨ ∃α',q. r=0 * p-[_;_)->⟨W,q⟩ * q-[q;s)->⟨s,α'⟩ }
 * where W = sizeof void*
 *)
let posix_memalign_spec us reg ptr siz =
  let {xs; seg= pseg} = fresh_seg ~loc:ptr ~siz:size_of_ptr us in
  let q, us, xs = fresh_var "q" us xs in
  let pseg' = {pseg with arr= q} in
  let {xs; seg= qseg} = fresh_seg ~loc:q ~bas:q ~len:siz ~siz ~xs us in
  let foot = Sh.extend_us xs (Sh.seg pseg) in
  let eok = Exp.integer (Z.of_int 0) Typ.int in
  let enomem = Exp.integer (Z.of_int 12) Typ.int in
  let post =
    Sh.or_
      (Sh.and_ (Exp.eq (Exp.var reg) enomem) (Sh.seg pseg))
      (Sh.and_
         (Exp.eq (Exp.var reg) eok)
         (Sh.star (Sh.seg pseg') (Sh.seg qseg)))
  in
  {xs; foot; post}

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   realloc r p s
 * {   (r=0 * (p=0 ∨ p-[p;m)->⟨m,α⟩))
 *   ∨ ∃α',α'' . r-[r;s)->⟨s,α'⟩
 *     * (m≤s ? ⟨s,α'⟩=⟨m,α⟩^⟨s-m,α''⟩ : ⟨m,α⟩=⟨s,α'⟩^⟨m-s,α''⟩) }
 *)
let realloc_spec us reg ptr siz =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg= pseg} =
    fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us
  in
  let loc = Exp.var reg in
  let {us; xs; seg= rseg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz ~xs us in
  let a0 = pseg.arr in
  let a1 = rseg.arr in
  let a2, _, xs = fresh_var "a" us xs in
  let foot = Sh.extend_us xs (Sh.or_ (null_eq ptr) (Sh.seg pseg)) in
  let post =
    Sh.or_
      (Sh.and_ Exp.(eq loc null) foot)
      (Sh.and_
         Exp.(
           conditional ~cnd:(le len siz)
             ~thn:
               (eq (memory ~siz ~arr:a1)
                  (concat
                     [| memory ~siz:len ~arr:a0
                      ; memory ~siz:(sub Typ.siz siz len) ~arr:a2 |]))
             ~els:
               (eq (memory ~siz:len ~arr:a0)
                  (concat
                     [| memory ~siz ~arr:a1
                      ; memory ~siz:(sub Typ.siz len siz) ~arr:a2 |])))
         (Sh.seg rseg))
  in
  {xs; foot; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   rallocx r p s
 * {   (r=0 * p-[p;m)->⟨m,α⟩)
 *   ∨ ∃α',α'' . r-[r;s)->⟨s,α'⟩
 *     * (m≤s ? ⟨s,α'⟩=⟨m,α⟩^⟨s-m,α''⟩ : ⟨m,α⟩=⟨s,α'⟩^⟨m-s,α''⟩) }
 *)
let rallocx_spec us reg ptr siz =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg= pseg} =
    fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us
  in
  let loc = Exp.var reg in
  let {us; xs; seg= rseg} = fresh_seg ~loc ~bas:loc ~len:siz ~siz ~xs us in
  let a0 = pseg.arr in
  let a1 = rseg.arr in
  let a2, _, xs = fresh_var "a" us xs in
  let foot = Sh.extend_us xs (Sh.and_ Exp.(dq siz zero) (Sh.seg pseg)) in
  let post =
    Sh.or_
      (Sh.and_ Exp.(eq loc null) foot)
      (Sh.and_
         Exp.(
           conditional ~cnd:(le len siz)
             ~thn:
               (eq (memory ~siz ~arr:a1)
                  (concat
                     [| memory ~siz:len ~arr:a0
                      ; memory ~siz:(sub Typ.siz siz len) ~arr:a2 |]))
             ~els:
               (eq (memory ~siz:len ~arr:a0)
                  (concat
                     [| memory ~siz ~arr:a1
                      ; memory ~siz:(sub Typ.siz len siz) ~arr:a2 |])))
         (Sh.seg rseg))
  in
  {xs; foot; post}

(* { s≠0 * p-[p;m)->⟨m,α⟩ }
 *   xallocx r p s e
 * { ∃α',α'' . s≤r≤s+e * p-[p;r)->⟨r,α'⟩
 *   * (m≤r ? ⟨r,α'⟩=⟨m,α⟩^⟨r-m,α''⟩ : ⟨m,α⟩=⟨r,α'⟩^⟨m-r,α''⟩) }
 *)
let xallocx_spec us reg ptr siz ext =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {us; xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let reg = Exp.var reg in
  let {us; xs; seg= seg'} =
    fresh_seg ~loc:ptr ~bas:ptr ~len:reg ~siz:reg ~xs us
  in
  let a0 = seg.arr in
  let a1 = seg'.arr in
  let a2, _, xs = fresh_var "a" us xs in
  let foot = Sh.extend_us xs (Sh.and_ Exp.(dq siz zero) (Sh.seg seg)) in
  let post =
    Sh.and_
      Exp.(
        and_
          (conditional ~cnd:(le len siz)
             ~thn:
               (eq (memory ~siz ~arr:a1)
                  (concat
                     [| memory ~siz:len ~arr:a0
                      ; memory ~siz:(sub Typ.siz siz len) ~arr:a2 |]))
             ~els:
               (eq (memory ~siz:len ~arr:a0)
                  (concat
                     [| memory ~siz ~arr:a1
                      ; memory ~siz:(sub Typ.siz len siz) ~arr:a2 |])))
          (and_ (le siz reg) (le reg (add Typ.siz siz ext))))
      (Sh.seg seg')
  in
  {xs; foot; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   sallocx r p
 * { r=m * p-[p;m)->⟨m,α⟩ }
 *)
let sallocx_spec us reg ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.seg seg in
  let post = Sh.and_ Exp.(eq (var reg) len) foot in
  {xs; foot; post}

(* { p-[p;m)->⟨m,α⟩ }
 *   malloc_usable_size r p
 * { m≤r * p-[p;m)->⟨m,α⟩ }
 *)
let malloc_usable_size_spec us reg ptr =
  let len, us, xs = fresh_var "m" us Var.Set.empty in
  let {xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len ~siz:len ~xs us in
  let foot = Sh.seg seg in
  let post = Sh.and_ Exp.(le len (var reg)) foot in
  {xs; foot; post}

(* { s≠0 }
 *   r = nallocx s
 * { r=0 ∨ r=s }
 *)
let nallocx_spec _ reg siz =
  let loc = Exp.var reg in
  let xs = Var.Set.empty in
  let foot = Sh.pure (Exp.dq siz zero) in
  let post = Sh.or_ (null_eq loc) (Sh.pure (Exp.eq loc siz)) in
  {xs; foot; post}

let size_of_int_mul n =
  Exp.mul Typ.siz (Option.value_exn (Exp.size_of Typ.siz)) n

(* { r-[_;_)->⟨m,_⟩ * i-[_;_)->⟨_,m⟩ * w=0 * n=0 }
 *   mallctl r i w n
 * { ∃α'. r-[_;_)->⟨m,α'⟩ * i-[_;_)->⟨_,m⟩ }
 *)
let mallctl_read_spec us r i w n =
  let {us; xs; seg= iseg} = fresh_seg ~loc:i us in
  let {us; xs; seg= rseg} = fresh_seg ~loc:r ~siz:iseg.arr ~xs us in
  let a, _, xs = fresh_var "a" us xs in
  let foot =
    Sh.extend_us xs
      (Sh.and_
         Exp.(eq w null)
         (Sh.and_ Exp.(eq n zero) (Sh.star (Sh.seg iseg) (Sh.seg rseg))))
  in
  let rseg' = {rseg with arr= a} in
  let post = Sh.star (Sh.seg rseg') (Sh.seg iseg) in
  {xs; foot; post}

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
    Sh.extend_us xs
      (Sh.and_
         Exp.(eq w null)
         (Sh.and_ Exp.(eq n zero) (Sh.star const (Sh.seg rseg))))
  in
  let rseg' = {rseg with arr= a} in
  let post = Sh.star (Sh.seg rseg') const in
  {xs; foot; post}

(* { r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl r i w n
 * { w-[_;_)->⟨n,_⟩ }
 *)
let mallctl_write_spec us r i w n =
  let {xs; seg} = fresh_seg ~loc:w ~siz:n us in
  let post = Sh.seg seg in
  let foot = Sh.and_ Exp.(eq r null) (Sh.and_ Exp.(eq i zero) post) in
  {xs; foot; post}

(* { p-[_;_)->⟨W×l,_⟩ * r=0 * i=0 * w-[_;_)->⟨n,_⟩ }
 *   mallctl r i w n
 * { p-[_;_)->⟨W×l,_⟩ * w-[_;_)->⟨n,_⟩ }
 * where W = sizeof int
 *)
let mallctlbymib_write_spec us p l r i w n =
  let wl = size_of_int_mul l in
  let {us; xs; seg= pseg} = fresh_seg ~loc:p ~siz:wl us in
  let {xs; seg= wseg} = fresh_seg ~loc:w ~siz:n ~xs us in
  let post = Sh.star (Sh.seg pseg) (Sh.seg wseg) in
  let foot = Sh.and_ Exp.(eq r null) (Sh.and_ Exp.(eq i zero) post) in
  {xs; foot; post}

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
  let foot = Sh.extend_us xs (Sh.star (Sh.seg oseg) (Sh.seg pseg)) in
  let pseg' = {pseg with arr= a} in
  let post = Sh.star (Sh.seg pseg') (Sh.seg oseg) in
  {xs; foot; post}

(*
 * cstring - see e.g. http://www.cplusplus.com/reference/cstring/
 *)

(* { p-[b;m)->⟨l,α⟩ }
 *   r = strlen p
 * { r=b+m-p-1 * p-[b;m)->⟨l,α⟩ }
 *)
let strlen_spec us reg ptr =
  let {xs; seg} = fresh_seg ~loc:ptr us in
  let foot = Sh.seg seg in
  let {Sh.loc= p; bas= b; len= m} = seg in
  let ret =
    Exp.sub Typ.siz
      (Exp.sub Typ.siz (Exp.add Typ.siz b m) p)
      (Exp.integer Z.one Typ.siz)
  in
  let post = Sh.and_ (Exp.eq (Exp.var reg) ret) foot in
  {xs; foot; post}

(*
 * Symbolic Execution
 *)

(* execute a command with given spec from pre *)
let exec_spec pre {xs; foot; post} =
  [%Trace.call fun {pf} ->
    pf "@[%a@]@ @[<2>%a@\n@[<hv>{%a  }@;<1 -1>--@ {%a  }@]@]" Sh.pp pre
      (Sh.pp_us ~pre:"@<2>∀ ")
      xs Sh.pp foot Sh.pp post ;
    assert (
      let vs = Set.diff (Set.diff foot.Sh.us xs) pre.Sh.us in
      Set.is_empty vs || Trace.fail "unbound foot: {%a}" Var.Set.pp vs ) ;
    assert (
      let vs = Set.diff (Set.diff post.Sh.us xs) pre.Sh.us in
      Set.is_empty vs || Trace.fail "unbound post: {%a}" Var.Set.pp vs )]
  ;
  let zs, pre = Sh.bind_exists pre ~wrt:xs in
  ( match Solver.infer_frame pre xs foot with
  | Some frame -> Ok (Sh.exists (Set.union zs xs) (Sh.star frame post))
  | None -> Error () )
  |>
  [%Trace.retn fun {pf} r -> pf "%a" (Result.pp "%a" Sh.pp) r]

(* execute a multiple-spec command, where the disjunction of the specs
   preconditions are known to be tautologous *)
let rec exec_specs pre = function
  | ({xs; foot} as spec) :: specs ->
      let open Result.Monad_infix in
      let pre_pure = Sh.star (Sh.exists xs (Sh.pure_approx foot)) pre in
      exec_spec pre_pure spec
      >>= fun post ->
      exec_specs pre specs >>| fun posts -> Sh.or_ post posts
  | [] -> Ok (Sh.false_ pre.us)

let inst : Sh.t -> Llair.inst -> (Sh.t, unit) result =
 fun pre inst ->
  [%Trace.info
    "@[<2>exec inst %a from@ @[{ %a@ }@]@]" Llair.Inst.pp inst Sh.pp pre] ;
  assert (Set.disjoint (Sh.fv pre) (Llair.Inst.locals inst)) ;
  let us = pre.us in
  match inst with
  | Load {reg; ptr; len} -> exec_spec pre (load_spec us reg ptr len)
  | Store {ptr; exp; len} -> exec_spec pre (store_spec us ptr exp len)
  | Memset {dst; byt; len} -> exec_spec pre (memset_spec us dst byt len)
  | Memcpy {dst; src; len} -> exec_specs pre (memcpy_specs us dst src len)
  | Memmov {dst; src; len} -> exec_specs pre (memmov_specs us dst src len)
  | Alloc {reg; num; len} -> exec_spec pre (alloc_spec us reg num len)
  | Free {ptr} -> exec_spec pre (free_spec us ptr)
  | Nondet _ -> Ok pre
  | Abort _ -> Error ()

let skip : Sh.t -> (Sh.t, _) result option = fun pre -> Some (Ok pre)

let intrinsic :
       Sh.t
    -> Var.t option
    -> Var.t
    -> Exp.t list
    -> (Sh.t, unit) result option =
 fun pre result intrinsic actuals ->
  [%Trace.info
    "@[<2>exec intrinsic@ @[%a%a(@[%a@])@] from@ @[{ %a@ }@]@]"
      (Option.pp "%a := " Var.pp)
      result Var.pp intrinsic (List.pp ",@ " Exp.pp) (List.rev actuals)
      Sh.pp pre] ;
  let us = pre.us in
  match (result, Var.name intrinsic, actuals) with
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
  | _ -> None
