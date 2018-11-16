(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

(** generic command: [∀xs.{foot}-{post}] *)
type spec = {xs: Var.Set.t; foot: Sh.t; post: Sh.t}

type xseg = {us: Var.Set.t; xs: Var.Set.t; seg: Sh.seg}

let fresh_var nam us xs =
  let var, us = Var.fresh nam ~wrt:us in
  (Exp.var var, us, Set.add xs var)

let fresh_seg ~loc ?bas ?len ?siz ?arr us =
  let freshen exp nam us xs =
    match exp with Some exp -> (exp, us, xs) | None -> fresh_var nam us xs
  in
  let xs = Var.Set.empty in
  let bas, us, xs = freshen bas "b" us xs in
  let len, us, xs = freshen len "m" us xs in
  let siz, us, xs = freshen siz "n" us xs in
  let arr, us, xs = freshen arr "a" us xs in
  {us; xs; seg= {loc; bas; len; siz; arr}}

let null_eq ptr = Sh.pure (Exp.eq Exp.null ptr)

let assume cnd pre =
  let post = Sh.and_ cnd pre in
  if Sh.is_false post then None else Some post

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

(* { p=0 ∨ p-[p;m)->⟨m,α⟩ }
 *   free p
 * { emp }
 *)
let free_spec us ptr =
  let len, us = Var.fresh "m" ~wrt:us in
  let siz = Exp.var len in
  let {xs; seg} = fresh_seg ~loc:ptr ~bas:ptr ~len:siz ~siz us in
  let xs = Set.add xs len in
  let foot = Sh.or_ (null_eq ptr) (Sh.seg seg) in
  let post = Sh.emp in
  {xs; foot; post}

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
    Sh.and_ (Exp.eq dst src)
      (Sh.and_ (Exp.eq len (Exp.integer Z.zero Typ.siz)) dst_heap)
  in
  let post = dst_heap in
  {xs; foot; post}

(* { d-[b;m)->⟨l,α⟩ * s-[b';m')->⟨l,α'⟩ }
 *   memcpy l d s
 * { d-[b;m)->⟨l,α'⟩ * s-[b';m')->⟨l,α'⟩ }
 *)
let memcpy_dj_spec us dst src len =
  let {us; xs= dst_xs; seg= dst_seg} = fresh_seg ~loc:dst ~siz:len us in
  let dst_heap = Sh.seg dst_seg in
  let {us; xs= src_xs; seg= src_seg} = fresh_seg ~loc:src ~siz:len us in
  let src_heap = Sh.seg src_seg in
  let {seg= dst_seg'} =
    fresh_seg ~loc:dst ~bas:dst_seg.bas ~len:dst_seg.len ~siz:dst_seg.siz
      ~arr:src_seg.arr us
  in
  let dst_heap' = Sh.seg dst_seg' in
  let xs = Set.union dst_xs src_xs in
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
  let mem_mid_src = Exp.concat mem_mid mem_src in
  let mem_dst_mid_src = Exp.concat mem_dst mem_mid_src in
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
  let mem_mid_src_src = Exp.concat (Exp.concat mem_mid mem_src) mem_src in
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
  let mem_src_src_mid = Exp.concat mem_src (Exp.concat mem_src mem_mid) in
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
      (Exp.add Typ.siz (Exp.sub Typ.siz b p) m)
      (Exp.integer Z.one Typ.siz)
  in
  let post = Sh.and_ (Exp.eq (Exp.var reg) ret) foot in
  {xs; foot; post}

(* execute a command with given spec from pre *)
let exec_spec pre {xs; foot; post} =
  [%Trace.call fun {pf} ->
    pf "@[%a@]@ @[<2>%a@,@[{%a}@;<0 -1>-{%a}@]@]" Sh.pp pre
      (Sh.pp_us ~pre:"@<2>∀ ")
      xs Sh.pp foot Sh.pp post ;
    assert (
      let vs = Set.diff (Set.diff foot.Sh.us xs) pre.Sh.us in
      Set.is_empty vs || Trace.report "unbound foot: {%a}" Var.Set.pp vs ) ;
    assert (
      let vs = Set.diff (Set.diff post.Sh.us xs) pre.Sh.us in
      Set.is_empty vs || Trace.report "unbound post: {%a}" Var.Set.pp vs )]
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

let inst : Sh.t -> Llair.inst -> (Sh.t, _) result =
 fun pre inst ->
  [%Trace.info
    "@[<2>exec inst %a from@ @[{ %a@ }@]@]" Llair.Inst.pp inst Sh.pp pre] ;
  assert (Set.disjoint (Sh.fv pre) (Llair.Inst.locals inst)) ;
  let us = pre.us in
  ( match inst with
  | Nondet _ -> Ok pre
  | Alloc {reg; num; len} -> exec_spec pre (alloc_spec us reg num len)
  | Malloc {reg; siz} -> exec_spec pre (malloc_spec us reg siz)
  | Free {ptr} -> exec_spec pre (free_spec us ptr)
  | Load {reg; ptr; len} -> exec_spec pre (load_spec us reg ptr len)
  | Store {ptr; exp; len} -> exec_spec pre (store_spec us ptr exp len)
  | Memset {dst; byt; len} -> exec_spec pre (memset_spec us dst byt len)
  | Memcpy {dst; src; len} -> exec_specs pre (memcpy_specs us dst src len)
  | Memmov {dst; src; len} -> exec_specs pre (memmov_specs us dst src len)
  | Strlen {reg; ptr} -> exec_spec pre (strlen_spec us reg ptr) )
  |> Result.map_error ~f:(fun () -> (pre, inst))
