(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

open Fol

[@@@warning "+missing-record-field-pattern"]

let do_normalize = ref false

(** enable stronger unsat checking during normalization *)
let strong_unsat = false

(***************************************************************************
 *                Quantifier-Free Symbolic Heap Formulas                   *
 ***************************************************************************)

(** Quantifier-free Symbolic Heap Formulas *)
module Sh = struct
  type seg =
    {loc: Term.t; bas: Term.t; len: Term.t; siz: Term.t; cnt: Term.t}
  [@@deriving compare, equal, sexp]

  module Seg = struct
    type t = seg [@@deriving compare, equal, sexp]
  end

  module Segs : sig
    type t [@@deriving compare, equal, sexp]

    val empty : t
    val of_ : seg -> t
    val remove : seg -> t -> t
    val union : t -> t -> t option
    val is_empty : t -> bool
    val map : t -> f:(seg -> seg) -> t option
    val filter : t -> f:(seg -> bool) -> t
    val fold : t -> 's -> f:(seg -> 's -> 's) -> 's
    val to_iter : t -> seg iter
    val to_list : t -> seg list
  end = struct
    include Set.Make (Seg)
    include Provide_of_sexp (Seg)

    let add x s =
      let s' = add x s in
      if s' == s then None else Some s'

    let union s t = if disjoint s t then Some (union s t) else None

    let map s ~f =
      let s, ys =
        fold s (s, []) ~f:(fun x (s, ys) ->
            let y = f x in
            if y == x then (s, ys) else (remove x s, y :: ys) )
      in
      List.fold ys (Some s) ~f:(fun y -> function
        | Some s -> add y s | None -> None )
  end

  module T = struct
    type compare [@@deriving compare, equal, sexp]

    (** Quantifier-free Symbolic Heap Formulas *)
    type t =
      { heap: Segs.t  (** star-conjunction of segment atomic formulas *)
      ; djns: disjunction list  (** star-conjunction of disjunctions *)
      ; pure: Formula.t  (** pure boolean constraints *)
      ; ctx: Context.t [@ignore]
            (** first-order logical context induced by rest of formula *) }

    and disjunction = (t, compare) Set.t [@@deriving compare, equal, sexp]

    (** Quantifier-free symbolic heap formula with fresh variables that are
        not yet named, which can be quantified by [Xsh.exists_fresh] and
        named by [Xsh.name_exists] *)
    type u = t Var.Fresh.m
  end

  include T

  module Starjunction = struct
    include T
    include Comparer.Counterfeit (T)
  end

  module Set = struct
    include Set.Make_from_Comparer (Starjunction)
    include Provide_of_sexp (Starjunction)
  end

  (** Accessors *)

  let ctx q = q.ctx
  let heap q = Segs.to_iter q.heap

  (** Traversals *)

  let map_seg ~f h =
    let loc = f h.loc in
    let bas = f h.bas in
    let len = f h.len in
    let siz = f h.siz in
    let cnt = f h.cnt in
    if
      loc == h.loc
      && bas == h.bas
      && len == h.len
      && siz == h.siz
      && cnt == h.cnt
    then h
    else {loc; bas; len; siz; cnt}

  let iter_terms_seg {loc; bas; len; siz; cnt} ~f =
    f loc ;
    f bas ;
    f len ;
    f siz ;
    f cnt

  let iter_vars_seg seg ~f =
    iter_terms_seg ~f:(fun e -> Iter.iter ~f (Term.vars e)) seg

  let iter_vars_stem ~ignore_ctx ~ignore_pure q ~f =
    let {ctx; pure; heap; djns= _} = q in
    Iter.iter ~f:(iter_vars_seg ~f) (Segs.to_iter heap) ;
    if not ignore_pure then Iter.iter ~f (Formula.vars pure) ;
    if not ignore_ctx then Iter.iter ~f (Context.vars ctx)

  let rec iter_vars ~ignore_ctx ~ignore_pure q ~f =
    iter_vars_stem ~ignore_ctx ~ignore_pure ~f q ;
    List.iter q.djns ~f:(fun djn ->
        Set.iter djn ~f:(fun djt ->
            iter_vars ~ignore_ctx ~ignore_pure ~f djt ) )

  let vars_seg seg = Iter.from_labelled_iter (iter_vars_seg seg)

  let vars_stem ~ignore_ctx ~ignore_pure q =
    Iter.from_labelled_iter (iter_vars_stem ~ignore_ctx ~ignore_pure q)

  let vars ~ignore_ctx ~ignore_pure q =
    Iter.from_labelled_iter (iter_vars ~ignore_ctx ~ignore_pure q)

  (** Free variables *)

  let fv ?(ignore_ctx : unit option) ?(ignore_pure : unit option) q =
    let ignore_ctx = Option.is_some ignore_ctx in
    let ignore_pure = Option.is_some ignore_pure in
    Var.Set.of_iter (vars ~ignore_ctx ~ignore_pure q)

  (** Pretty-printing *)

  let pp_chunk x fs (siz, cnt) =
    Format.fprintf fs "@[<2>@<1>⟨%a,%a@<1>⟩@]" (Term.ppx x) siz (Term.ppx x)
      cnt

  let pp_seg x fs {loc; bas; len; siz; cnt} =
    let term_pp = Term.ppx x in
    Format.fprintf fs "@[<2>%a@ @[@[-[%a)->@]@ %a@]@]" term_pp loc
      (fun fs (bas, len) ->
        if (not (Term.equal loc bas)) || not (Term.equal len siz) then
          Format.fprintf fs " %a, %a " term_pp bas term_pp len )
      (bas, len) (pp_chunk x) (siz, cnt)

  let pp_seg_norm ctx fs seg =
    let x _ = None in
    pp_seg x fs (map_seg seg ~f:(Context.normalize ctx))

  let pp_block x fs segs =
    let is_full_alloc segs =
      match segs with
      | {loc; bas; len; _} :: _ -> (
          Term.equal loc bas
          &&
          match Term.get_z len with
          | Some z -> (
            match
              List.fold segs (Some Z.zero) ~f:(fun seg len ->
                  match (len, Term.get_z seg.siz) with
                  | Some len, Some siz -> Some (Z.add len siz)
                  | _ -> None )
            with
            | Some blk_len -> Z.equal z blk_len
            | _ -> false )
          | _ -> false )
      | [] -> false
    in
    let term_pp = Term.ppx x in
    let pp_chunks =
      List.pp "@,^" (fun fs seg -> pp_chunk x fs (seg.siz, seg.cnt))
    in
    match segs with
    | {loc; bas; len; _} :: _ ->
        Format.fprintf fs "@[<2>%a@ @[@[-[%t)->@]@ @[%a@]@]@]" term_pp loc
          (fun fs ->
            if not (is_full_alloc segs) then
              Format.fprintf fs " %a, %a " term_pp bas term_pp len )
          pp_chunks segs
    | [] -> ()

  let pp_heap x ?pre ctx fs heap =
    let bas_off = Term.split_const in
    let cmp s1 s2 =
      [%compare: Term.t * (Term.t * Q.t)]
        ( Context.normalize ctx s1.bas
        , bas_off (Context.normalize ctx s1.loc) )
        ( Context.normalize ctx s2.bas
        , bas_off (Context.normalize ctx s2.loc) )
    in
    let eq s1 s2 =
      Term.equal s1.bas s2.bas
      && Term.equal s1.len s2.len
      && Context.implies ctx (Formula.eq (Term.add s1.loc s1.siz) s2.loc)
    in
    let heap = List.map ~f:(map_seg ~f:(Context.normalize ctx)) heap in
    let blocks = List.group_succ ~eq (List.sort ~cmp heap) in
    List.pp ?pre "@ * " (pp_block x) fs blocks

  let rec pp_ ?var_strength raw parent_ctx fs {ctx; pure; heap; djns} =
    Format.pp_open_hvbox fs 0 ;
    let x v =
      let* m = var_strength in
      Var.Map.find v m
    in
    ( match djns with
    | [djn] when (not raw) && Set.is_empty djn -> Format.fprintf fs "false"
    | _ ->
        let first =
          if not raw then Context.ppx_diff x fs parent_ctx pure ctx
          else (
            Format.fprintf fs "@[  %a@ @<2>∧ %a@]" Context.pp ctx Formula.pp
              pure ;
            false )
        in
        if Segs.is_empty heap then
          Format.fprintf fs
            ( if first then if List.is_empty djns then "  emp" else ""
            else "@ @<5>∧ emp" )
        else
          pp_heap x
            ~pre:(if first then "  " else "@ @<2>∧ ")
            (if not raw then ctx else Context.empty)
            fs (Segs.to_list heap) ;
        let first = first && Segs.is_empty heap in
        List.pp
          ~pre:(if first then "  " else "@ * ")
          "@ * "
          (pp_djn ?var_strength raw
             (if not raw then ctx else Context.empty) )
          fs djns ) ;
    Format.pp_close_box fs ()

  and pp_djn ?var_strength raw parent_ctx fs djn =
    if Set.is_empty djn then Format.fprintf fs "false"
    else
      Format.fprintf fs "@[<hv>( %a@ )@]"
        (List.pp "@ @<2>∨ " (fun fs sjn ->
             Format.fprintf fs "@[<hv 1>(%a)@]"
               (pp_ ?var_strength raw parent_ctx)
               sjn ) )
        (Set.to_list djn)

  let pp fs q = pp_ false Context.empty fs q
  let pp_raw fs q = pp_ true Context.empty fs q
  let pp_diff_eq ctx fs q = pp_ false ctx fs q

  (** Invariants *)

  let rec invariant q =
    let@ () = Invariant.invariant [%here] q [%sexp_of: t] in
    let {ctx; pure; heap; djns} = q in
    Context.invariant ctx ;
    match djns with
    | [djn] when Set.is_empty djn ->
        assert (Context.is_unsat ctx) ;
        assert (Formula.equal Formula.ff pure) ;
        assert (Segs.is_empty heap)
    | _ ->
        assert (not (Context.is_unsat ctx)) ;
        assert (not (Formula.equal Formula.ff pure)) ;
        assert (not (List.exists djns ~f:Set.is_empty)) ;
        List.iter djns ~f:(fun djn ->
            assert (Set.cardinal djn > 1) ;
            Set.iter ~f:invariant djn )

  (** Syntactic Query *)

  (** syntactically empty: empty heap and no pure constraints *)
  let is_emp q =
    Context.is_empty q.ctx
    && Formula.equal Formula.tt q.pure
    && Segs.is_empty q.heap
    && List.is_empty q.djns

  (** (incomplete syntactic) test that all satisfying heaps are empty *)
  let rec is_empty q =
    Segs.is_empty q.heap && List.for_all ~f:(Set.for_all ~f:is_empty) q.djns

  (** (incomplete syntactic) test for inconsistency *)
  let is_false q =
    match q.djns with [djn] -> Set.is_empty djn | _ -> false

  (** Construct *)

  let emp =
    {ctx= Context.empty; pure= Formula.tt; heap= Segs.empty; djns= []}

  let false_ =
    { ctx= Context.unsat
    ; pure= Formula.ff
    ; heap= Segs.empty
    ; djns= [Set.empty] }

  let star q1 q2 =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ (%a)@ (%a)" pp q1 pp q2)
      ~retn:(fun {pf} (q, vxd) ->
        pf "%a%a" Var.Context.pp_diff vxd pp q ;
        invariant q )
    @@ fun vx ->
    if is_false q1 || is_false q2 then false_
    else if is_emp q1 then q2
    else if is_emp q2 then q1
    else
      let {ctx= c1; pure= p1; heap= h1; djns= d1} = q1 in
      let {ctx= c2; pure= p2; heap= h2; djns= d2} = q2 in
      match Segs.union h1 h2 with
      | None -> false_
      | Some heap ->
          let pure = Formula.and_ p1 p2 in
          if Formula.equal Formula.ff pure then false_
          else
            let ctx = Context.union c1 c2 vx in
            if Context.is_unsat ctx then false_
            else
              let djns = List.append d1 d2 in
              {ctx; pure; heap; djns}

  let starN qs vx = List.fold ~f:(fun q1 q2 -> star q1 q2 vx) qs emp

  let or_ q1 q2 =
    match (q1, q2) with
    | _ when is_false q1 -> q2
    | _ when is_false q2 -> q1
    | ({djns= []; _} as q), ({ctx= _; pure; heap; djns= [djn]} as d)
      when Formula.(equal tt pure) && Segs.is_empty heap ->
        {d with djns= [Set.add q djn]}
    | ({ctx= _; pure; heap; djns= [djn]} as d), ({djns= []; _} as q)
      when Formula.(equal tt pure) && Segs.is_empty heap ->
        {d with djns= [Set.add q djn]}
    | _ when equal q1 q2 -> q1
    | _ ->
        { ctx= Context.empty
        ; pure= Formula.tt
        ; heap= Segs.empty
        ; djns= [Set.add q1 (Set.of_ q2)] }

  let orN qs = Set.fold ~f:or_ qs false_

  let and_ctx ctx q vx =
    let ctx = Context.union ctx q.ctx vx in
    if Context.is_unsat ctx then false_ else {q with ctx}

  let pure b =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ %a" Formula.pp b)
      ~retn:(fun {pf} (q', vxd) -> pf "%a%a" Var.Context.pp_diff vxd pp q')
    @@ fun vx ->
    Var.Fresh.fold_iter (Context.dnf b) false_ vx ~f:(fun (cube, ctx) q ->
        if Context.is_unsat ctx || Formula.equal Formula.ff cube then q
        else or_ {emp with ctx; pure= cube} q )

  let and_ b q =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ (%a)@ (%a)" Formula.pp b pp q)
      ~retn:(fun {pf} (q', vxd) -> pf "%a%a" Var.Context.pp_diff vxd pp q')
    @@ fun vx ->
    let p = pure (Formula.map_terms ~f:(Context.normalize q.ctx) b) vx in
    if is_emp p then q else star p q vx

  let andN bs q vx = and_ (Formula.andN bs) q vx

  let and_subst subst q =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ %a@ %a" Context.Subst.pp subst pp q)
      ~retn:(fun {pf} (q', vxd) ->
        pf "%a%a" Var.Context.pp_diff vxd pp q' ;
        invariant q' )
    @@ fun vx -> Context.Subst.fold_eqs ~f:(fun b q -> and_ b q vx) subst q

  let seg pt =
    if Term.equal Term.zero pt.loc then false_
    else {emp with heap= Segs.of_ pt}

  (** Update *)

  let rem_seg seg q = {q with heap= Segs.remove seg q.heap}
  let filter_heap ~f q = {q with heap= Segs.filter q.heap ~f}

  (** Transform *)

  let rec map ~f_ctx ~f_trm ~f_fml ({ctx; pure; heap; djns} as q) vx =
    let pure = f_fml pure in
    if Formula.equal Formula.ff pure then false_
    else
      let ctx = f_ctx ctx vx in
      if Context.is_unsat ctx then false_
      else
        match Segs.map heap ~f:(map_seg ~f:f_trm) with
        | None -> false_
        | Some heap ->
            let djns, hoisted =
              List.partition_map_endo djns ~f:(fun djn ->
                  let djn' =
                    Set.filter_map djn ~f:(fun sjn ->
                        let sjn' = map ~f_ctx ~f_trm ~f_fml sjn vx in
                        if is_false sjn' then None else Some sjn' )
                  in
                  match Set.classify djn' with
                  | One dj -> Right dj
                  | _ -> Left djn' )
            in
            if
              ctx == q.ctx
              && pure == q.pure
              && heap == q.heap
              && djns == q.djns
            then q
            else if List.exists ~f:Set.is_empty djns then false_
            else
              List.fold hoisted {ctx; pure; heap; djns} ~f:(fun q1 q2 ->
                  star q1 q2 vx )

  (** primitive application of a substitution, ignores us and xs, may
      violate invariant *)
  let rename sub q =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp q)
      ~retn:(fun {pf} (q', vxd) ->
        pf "%a%a" Var.Context.pp_diff vxd pp q' ;
        invariant q' ;
        assert (Var.Set.disjoint (fv q') (Var.Subst.domain sub)) )
    @@ fun vx ->
    if Var.Subst.is_empty sub then q
    else
      let f_ctx x _ = Context.rename x sub in
      let f_trm = Term.rename sub in
      let f_fml = Formula.rename sub in
      map ~f_ctx ~f_trm ~f_fml q vx

  (** Disjunctive-Normal Form *)

  (** Lazily enumerate the cubes and clauses of a disjunctive-normal form
      expansion. [iter_dnf] is a tree of computations, one for each branch
      of the DNF expansion, yielding a separate state at the tip of each
      branch. *)
  let iter_dnf :
         conj:(t -> 'conjuncts -> 'conjuncts Var.Fresh.m)
      -> t
      -> 'conjuncts
      -> f:('conjuncts * Var.Context.t -> unit)
      -> unit Var.Fresh.m =
   fun ~conj sjn cjn ~f vx ->
    let add_conjunct sjn (cjn, splits) =
      let sjn, djns = ({sjn with djns= []}, sjn.djns) in
      let cjn = conj sjn cjn vx in
      let splits = Iter.append (Iter.of_list djns) splits in
      (cjn, splits)
    in
    let rec add_disjunct sjn (cjn, splits) vx =
      let cjn, splits = add_conjunct sjn (cjn, splits) in
      let vx = !vx in
      match Iter.pop splits with
      | Some (split, splits) ->
          Set.iter split ~f:(fun sjn ->
              Var.Fresh.gen_ vx (add_disjunct sjn (cjn, splits)) )
      | None -> f (cjn, vx)
    in
    if not (is_false sjn) then add_disjunct sjn (cjn, Iter.empty) vx

  let dnf_iter :
         conj:(t -> 'conjuncts -> 'conjuncts Var.Fresh.m)
      -> t
      -> 'conjuncts
      -> ('conjuncts * Var.Context.t) iter Var.Fresh.m =
   fun ~conj sjn cjn vx ->
    let vx = !vx in
    Iter.from_iter (fun f -> Var.Fresh.gen_ vx (iter_dnf ~conj sjn cjn ~f))

  let dnf q vx =
    Var.Fresh.fold_iter ~f:Set.add (dnf_iter ~conj:star q emp) Set.empty vx

  (** Logical Query *)

  (** first-order approximation of heap constraints *)
  let rec pure_approx q =
    Formula.andN
      ( [ q.pure
        ; Formula.distinct
            (Array.of_list
               (Segs.fold q.heap [Term.zero] ~f:(fun seg locs ->
                    seg.loc :: locs ) ) ) ]
      |> List.fold q.djns ~f:(fun djn p ->
             Formula.orN
               (Set.fold ~f:(fun dj ps -> pure_approx dj :: ps) djn [])
             :: p ) )

  (** enumerate a DNF-expansion of a symbolic heap's first-order constraints
      conjoined with a first-order approximation of the heap constraints
      until a branch that is not unsatisfiable is found *)
  let is_unsat_dnf q vx =
    let conj sjn (ctx, fml) vx =
      let ctx = Context.union ctx sjn.ctx vx in
      let fml = Formula.and_ fml sjn.pure in
      let fml =
        Segs.fold sjn.heap fml ~f:(fun seg ->
            Formula.and_ (Formula.dq0 seg.loc) )
      in
      (ctx, fml)
    in
    let dnf = dnf_iter ~conj q (emp.ctx, emp.pure) vx in
    Iter.for_all dnf ~f:(fun ((ctx, fml), _) ->
        Context.is_unsat ctx || Context.refutes ctx fml )

  let is_unsat q vx =
    if strong_unsat then is_unsat_dnf q vx
    else Context.refutes q.ctx (pure_approx q)

  let to_z3 ctx q =
    let dnf_iter :
           conj:(t -> 'conjuncts -> 'conjuncts)
        -> t
        -> 'conjuncts
        -> 'conjuncts iter =
     fun ~conj sjn cjn ->
      Iter.from_iter
      @@ fun yield ->
      let add_conjunct sjn (cjn, splits) =
        let sjn, djns = ({sjn with djns= []}, sjn.djns) in
        let cjn = conj sjn cjn in
        let splits = Iter.append (Iter.of_list djns) splits in
        (cjn, splits)
      in
      let rec add_disjunct sjn (cjn, splits) =
        let cjn, splits = add_conjunct sjn (cjn, splits) in
        match Iter.pop splits with
        | Some (split, splits) ->
            Set.iter split ~f:(fun sjn -> add_disjunct sjn (cjn, splits))
        | None -> yield cjn
      in
      if not (is_false sjn) then add_disjunct sjn (cjn, Iter.empty)
    in
    (* k-[b;m)->⟨n,α⟩ |-
     * (m >= 0) * (n >= 0) * (k >= b) * (k+n <= b+m) * (n = 0 <=> m = 0) *)
    let add_seg_valid ctx {loc; bas; len; siz; cnt= _} ps =
      let addresses_and_sizes_are_ints =
        vars_seg {loc; bas; len; siz; cnt= Term.concat [||]}
        |> Iter.map ~f:(fun v ->
               Z3.Arithmetic.Real.mk_is_integer ctx
                 (Term.to_z3 ctx (Term.var v)) )
      in
      let k = Term.to_z3 ctx loc in
      let b = Term.to_z3 ctx bas in
      let m = Term.to_z3 ctx len in
      let n = Term.to_z3 ctx siz in
      let zero =
        Z3.Expr.mk_numeral_int ctx 0 (Z3.Arithmetic.Real.mk_sort ctx)
      in
      Z3.Arithmetic.(
        mk_ge ctx k b
        :: mk_ge ctx m zero
        :: mk_ge ctx n zero
        :: mk_le ctx (mk_add ctx [k; n]) (mk_add ctx [b; m])
        :: Z3.Boolean.(mk_iff ctx (mk_eq ctx n zero) (mk_eq ctx m zero))
        :: Iter.fold addresses_and_sizes_are_ints ~f:List.cons ps)
    in
    let conj q (pure, heap) =
      ( Z3.Boolean.mk_and ctx
          (Segs.fold ~f:(add_seg_valid ctx) q.heap
             [Formula.to_z3 ctx q.pure; pure] )
      , Segs.fold ~f:List.cons q.heap heap )
    in
    (* [a,m) and [b,n) are disjoint iff a+m ≤ b ∨ b+n ≤ a *)
    let disjoint (a, m) (b, n) =
      let a = Term.to_z3 ctx a in
      let am = Z3.Arithmetic.mk_add ctx [a; Term.to_z3 ctx m] in
      let b = Term.to_z3 ctx b in
      let bn = Z3.Arithmetic.mk_add ctx [b; Term.to_z3 ctx n] in
      Z3.Boolean.mk_or ctx
        [Z3.Arithmetic.mk_le ctx am b; Z3.Arithmetic.mk_le ctx bn a]
    in
    let equal (a, m) (b, n) =
      Z3.Boolean.mk_and ctx
        [ Z3.Boolean.mk_eq ctx (Term.to_z3 ctx a) (Term.to_z3 ctx b)
        ; Z3.Boolean.mk_eq ctx (Term.to_z3 ctx m) (Term.to_z3 ctx n) ]
    in
    let compatible ((h : seg), (k : seg)) =
      Z3.Boolean.mk_and ctx
        [ disjoint (h.loc, h.siz) (k.loc, k.siz)
        ; Z3.Boolean.mk_or ctx
            [ disjoint (h.bas, h.len) (k.bas, k.len)
            ; equal (h.bas, h.len) (k.bas, k.len) ] ]
    in
    dnf_iter ~conj q (Z3.Boolean.mk_true ctx, [])
    |> Iter.map ~f:(fun (pure, heap) ->
           let compat = Iter.map ~f:compatible (Iter.diagonal_l heap) in
           Z3.Boolean.mk_and ctx (pure :: Iter.to_list compat) )
    |> Iter.to_list
    |> Z3.Boolean.mk_or ctx

  let is_unsat_strong q =
    let ctx = Z3.mk_context [] in
    let solver = Z3.Solver.mk_solver ctx None in
    Z3.Solver.add solver [to_z3 ctx q] ;
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> true
    | UNKNOWN | SATISFIABLE -> false

  (** Simplify *)

  let norm ?(ignore_ctx : unit option) s q vx =
    let ignore_ctx = Option.is_some ignore_ctx in
    if Context.Subst.is_empty s then q
    else
      let f_ctx x vx =
        if ignore_ctx then x else Context.apply_subst s x vx
      in
      let f_trm = Context.Subst.subst s in
      let f_fml = Formula.map_terms ~f:(Context.Subst.subst s) in
      map ~f_ctx ~f_trm ~f_fml q vx

  let normalize q =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ %a" pp q)
      ~retn:(fun {pf} (q', vxd) ->
        pf "%a%a" Var.Context.pp_diff vxd pp q' ;
        invariant q' )
    @@ fun vx ->
    if not !do_normalize then q
    else
      let q' =
        let f_ctx x _ = x in
        let f_trm = Context.normalize q.ctx in
        let f_fml = Formula.map_terms ~f:(Context.normalize q.ctx) in
        map ~f_ctx ~f_trm ~f_fml q vx
      in
      let pure = Context.fold_eqs ~f:Formula.and_ q.ctx q'.pure in
      {q' with pure}

  let rec propagate_context ancestor_ctx q =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ (%a)@ %a" Context.pp ancestor_ctx pp q)
      ~retn:(fun {pf} (q', vxd) ->
        pf "%a%a" Var.Context.pp_diff vxd pp q' ;
        invariant q' )
    @@ fun vx ->
    (* strengthen context with that from above *)
    let q = and_ctx ancestor_ctx q vx in
    if is_false q then false_
    else
      (* decompose formula *)
      let stem, djns = ({q with djns= []}, q.djns) in
      (* propagate over disjunctions *)
      let q' =
        List.fold djns stem ~f:(fun djn q' ->
            let djn, ctx =
              Set.fold_map djn Context.unsat ~f:(fun dj ctx ->
                  let dj = propagate_context q.ctx dj vx in
                  let ctx = Context.inter dj.ctx ctx vx in
                  (dj, ctx) )
            in
            let q' = and_ctx ctx q' vx in
            star (orN djn) q' vx )
      in
      (* strengthening contexts can reveal inconsistency *)
      if is_false q' then false_ else q'

  let rec simplify xs survived ancestor_subst q =
    [%dbgs]
      ~call:(fun {pf} -> pf "@ %a@ %a" Context.Subst.pp ancestor_subst pp q)
      ~retn:(fun {pf} (q', vxd) ->
        pf "%a%a" Var.Context.pp_diff vxd pp q' ;
        invariant q' )
    @@ fun vx ->
    let q0 = q in
    assert (not (is_false q)) ;
    let stem_subst = Context.solve_for xs q.ctx vx in
    let subst = Context.Subst.compose ancestor_subst stem_subst in
    if Context.Subst.is_empty subst then normalize q0 vx
    else
      (* normalize context wrt solutions *)
      let ctx, removed = Context.apply_and_elim xs subst q.ctx vx in
      let q = {q with ctx} in
      (* normalize stem wrt its context *)
      let stem = normalize {q with djns= []} vx in
      (* normalize stem wrt both ancestor and current solutions *)
      let stem' = norm subst {stem with ctx= emp.ctx} vx in
      (* opt: ctx already normalized so just preserve it *)
      let stem = {stem' with ctx= q.ctx} in
      if strong_unsat && is_unsat stem vx then false_
      else
        (* recursively simplify subformulas *)
        let survived = Var.Set.union survived (fv stem) in
        let djns, hoisted =
          List.rev_partition_map q.djns ~f:(fun djn ->
              let djn =
                Set.map ~f:(fun q -> simplify xs survived subst q vx) djn
              in
              match Set.classify djn with
              | One sjn -> Right sjn
              | _ -> Left (orN djn) )
        in
        let q = starN ((stem :: hoisted) @ djns) vx in
        if is_false q then false_
        else
          let removed = Var.Set.diff removed survived in
          let removed =
            if Var.Set.is_empty removed then Var.Set.empty
            else Var.Set.diff removed (fv q)
          in
          (* removed may not contain all variables stem_subst has solutions
             for, so the equations in [∃ removed. stem_subst] that are not
             universally valid need to be re-conjoined since they have
             already been normalized out *)
          let keep, _, _ =
            Context.Subst.partition_valid removed stem_subst
          in
          and_subst keep q vx
end

(***************************************************************************
 *            Existentially-Quantified Symbolic Heap Formulas              *
 ***************************************************************************)

module Xsh = struct
  module T = struct
    (** Existentially-quantified Symbolic Heap Formulas

        [Var.Context.t = {voc; xs}] consists of a "vocabulary" [voc] and a
        subset [xs] of the vocabulary which is interpreted as an existential
        prefix. That is, [(q, {voc; xs})] represents [voc-xs ⊢ ∃xs. q]. *)
    type t = Sh.t * Var.Context.t [@@deriving sexp]

    let compare xq1 xq2 =
      let open Ord.Infix in
      if xq1 == xq2 then 0
      else
        let q1, vx1 = xq1 in
        let q2, vx2 = xq2 in
        Sh.compare q1 q2 <?> (Var.Context.compare, vx1, vx2)

    let equal = [%compare.equal: t]

    (** Accessors *)

    let vx (_, vx) = vx
    let xs (_, vx) = Var.Context.xs vx
    let qf (q, _) = q
    let ctx xq = Sh.ctx (qf xq)
    let heap xq = Sh.heap (qf xq)
  end

  include T

  (** Replay debugging *)

  type call = Extend_voc of Var.Set.t * t | Simplify of t
  [@@deriving sexp]

  (** Free variables *)

  let fv ?ignore_ctx ?ignore_pure (q, vx) =
    Var.Set.diff (Sh.fv ?ignore_ctx ?ignore_pure q) (Var.Context.xs vx)

  (** Pretty-printing *)

  let rec var_strength_ xs m q =
    let m_stem =
      Iter.fold (Sh.vars_stem ~ignore_ctx:true ~ignore_pure:false q) m
        ~f:(fun var m ->
          if not (Var.Set.mem var xs) then
            Var.Map.add ~key:var ~data:`Universal m
          else
            Var.Map.update var m ~f:(function
              | None -> Some `Anonymous
              | Some `Anonymous -> Some `Existential
              | o -> o ) )
    in
    List.fold q.djns m_stem ~f:(fun djn m ->
        let ms =
          Sh.Set.fold djn [] ~f:(fun dj ms -> var_strength_ xs m dj :: ms)
        in
        List.reduce ms ~f:(fun m1 m2 ->
            Var.Map.union m1 m2 ~f:(fun _ s1 s2 ->
                match (s1, s2) with
                | `Anonymous, `Anonymous -> Some `Anonymous
                | `Universal, _ | _, `Universal -> Some `Universal
                | `Existential, _ | _, `Existential -> Some `Existential ) )
        |> Option.value ~default:m )

  let var_strength ?(xs = Var.Set.empty) (q, vx) =
    let m =
      Var.Set.fold xs Var.Map.empty ~f:(fun x ->
          Var.Map.add ~key:x ~data:`Existential )
    in
    let xs = Var.Set.union xs (Var.Context.xs vx) in
    var_strength_ xs m q

  let pp_ ?var_strength ctx fs xq =
    Format.pp_open_hvbox fs 0 ;
    let x v =
      let* m = var_strength in
      Var.Map.find v m
    in
    Var.Context.pp_voc fs (snd xq) ;
    let xs =
      if Option.is_some var_strength then
        Var.Set.filter
          ~f:(fun v -> Poly.(x v <> Some `Anonymous))
          (Var.Set.inter (Sh.fv (qf xq)) (xs xq))
      else xs xq
    in
    if not (Var.Set.is_empty xs) then
      Format.fprintf fs "@<2>∃ @[%a@] .@ " (Var.Set.ppx x) xs ;
    Sh.pp_ ?var_strength (Option.is_none var_strength) ctx fs (qf xq) ;
    Format.pp_close_box fs ()

  let pp fs q = pp_ ~var_strength:(var_strength q) Context.empty fs q
  let pp_raw fs q = pp_ ?var_strength:None Context.empty fs q

  (** Set of existential symbolic heaps *)

  module Set = struct
    include Set.Make (T)
    include Provide_of_sexp (T)

    let pp_ pp fs xqs =
      if is_empty xqs then Format.fprintf fs "false"
      else
        Format.fprintf fs "@[<hv>( %a@ )@]"
          (List.pp "@ @<2>∨ " (fun fs xq ->
               Format.fprintf fs "@[<hv 1>(%a)@]" pp xq ) )
          (to_list xqs)

    let pp = pp_ pp
    let pp_raw = pp_ pp_raw
  end

  (** Invariants *)

  let invariant xq =
    let@ () = Invariant.invariant [%here] xq [%sexp_of: t] in
    try
      assert (
        Var.Context.contains (snd xq) (xs xq)
        || fail "inter: @[%a@]@\nq: @[%a@]" Var.Set.pp
             (Var.Context.diff (xs xq) (snd xq))
             pp xq () ) ;
      assert (
        Var.Context.contains (snd xq) (fv xq)
        || fail "unbound but free: %a" Var.Set.pp
             (Var.Context.diff (fv xq) (snd xq))
             () ) ;
      Sh.invariant (qf xq)
    with exc ->
      let bt = Printexc.get_raw_backtrace () in
      [%Dbg.info " %a" pp_raw xq] ;
      Printexc.raise_with_backtrace exc bt

  (** Syntactic Query *)

  let is_empty (q, _) = Sh.is_empty q
  let is_false (q, _) = Sh.is_false q

  (** Quantification and Vocabulary *)

  (** In the context of [Var.Fresh], ['a m] is isomorphic to
      [vx -> 'a * vx], so ['a -> 'b m] is isomorphic to
      ['a -> vx -> 'b * vx] and then to ['a * vx -> 'b * vx]. The logical
      specification of functions producing [Var.Fresh.m] values uses this
      transformation-of-pairs view, with the interpretation of [Xsh.t] where
      [(q, {voc; xs})] represents [voc-xs ⊢ ∃xs. q]. *)

  (** Eliminate existential quantifier, naming/unbinding existentially-bound
      variables. *)
  let name_exists : t -> (Var.Set.t * Sh.t) * Var.Context.t =
   fun xq ->
    [%Dbg.call fun {pf} -> pf "@ %a" pp xq]
    ;
    let q, vx = xq in
    let xs, vx0 = Var.Fresh.gen vx Var.Fresh.extract_xs in
    ((xs, q), vx0)
    |>
    [%Dbg.retn fun {pf} ((xs, q'), _) ->
      pf "%a%a" Var.Set.pp_xs xs Sh.pp q']

  (** Existentially quantify the existentials of the current
      [Var.Context.t]. *)
  let exists_fresh : Sh.t -> t Var.Fresh.m =
   fun q vx ->
    let vx0 = Var.Fresh.reset_xs vx in
    (q, vx0)

  (** Embed a quantifier-free formula as a quantified formula with empty
      prefix. *)
  let qf : Sh.t -> t Var.Fresh.m =
   fun q vx ->
    let vx = Var.Context.with_xs Var.Set.empty !vx in
    (q, vx)

  (** Extend vocabulary, freshening existentials to avoid clashing. *)
  let extend_voc : Var.Set.t -> t -> t =
   fun vs xq ->
    [%dbg]
      ~call:(fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp vs pp xq)
      ~retn:(fun {pf} xq' ->
        pf "%a" pp xq' ;
        invariant xq' ;
        assert (Var.Set.disjoint (xs xq') vs) )
      ~rais:(fun {pf} exc _bt ->
        pf "%s@\n%a" (Printexc.to_string exc) Sexp.pp
          (sexp_of_call (Extend_voc (vs, xq))) )
    @@ fun () ->
    let q, vx = xq in
    let no_clash, clash = Var.Set.diff_inter (xs xq) vs in
    Var.Fresh.gen Var.Context.(merge (of_vars vs) (with_xs no_clash vx))
    @@ fun vx ->
    let sub = Var.Fresh.subst clash vx in
    Sh.rename sub q vx

  (** Introduce existential quantifier, binding a set of variables required
      to already be in the vocabulary. *)
  let exists : Var.Set.t -> t -> t =
   fun xs ((q, vx) as xq) ->
    [%Dbg.call fun {pf} ->
      pf "@ {@[%a@]}@ %a" Var.Set.pp xs pp xq ;
      assert (
        Var.Context.contains vx xs
        || is_false xq
        || fail "Sh.exists xs - us: %a" Var.Set.pp (Var.Context.diff xs vx)
             () )]
    ;
    ( if Var.Set.is_empty xs || is_false xq then xq
    else
      let vx =
        Var.Context.with_xs (Var.Set.union xs (Var.Context.xs vx)) vx
      in
      (q, vx) |> check invariant )
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp]

  (** Freshen free variables with respect to [wrt], and add [wrt] to
      vocabulary. *)
  let freshen : t -> wrt:Var.Set.t -> t * Var.Subst.t =
   fun ((q, vx) as xq) ~wrt ->
    [%dbg]
      ~call:(fun {pf} -> pf "@ {@[%a@]}@ %a" Var.Set.pp wrt pp_raw xq)
      ~retn:(fun {pf} (xq', sub) ->
        pf "%a@ %a" Var.Subst.pp sub pp_raw xq' ;
        assert (Var.Context.contains (snd xq') wrt) )
    @@ fun () ->
    let clash = Var.Context.inter wrt vx in
    let clash_us, clash_xs, no_clash =
      Var.Set.diff_inter_diff clash (xs xq)
    in
    ( Var.Fresh.gen Var.Context.(merge (with_xs no_clash vx) (of_vars wrt))
    @@ fun vx ->
    let sub_us =
      Var.Set.fold clash_us Var.Subst.empty ~f:(fun x sub ->
          Var.Fresh.rename ~existential:false x sub vx )
    in
    let sub =
      Var.Set.fold clash_xs sub_us ~f:(fun x sub ->
          Var.Fresh.rename ~existential:true x sub vx )
    in
    let q' = Sh.rename sub q vx in
    (q', sub_us) )
    |> fun ((q', sub_us), vx) -> ((q', vx), sub_us)

  (** Lift an operation on a quantifier-free Sh to an existential Xsh:
      [lift ~wrt (∃xs. q) ~f] is [∃xs',zs. f q] where [xs] have been renamed
      to [xs'] to avoid clashing with [wrt], and [zs] have been generated
      fresh by [f q].

      Note that [wrt] must include all variables that might appear in [f q]
      that [f] did not generate and are not in the vocabulary of [(∃xs. q)]. *)
  let lift ?(wrt = Var.Set.empty) ~f xq =
    [%dbg]
      ~call:(fun {pf} -> pf "@ %a" pp_raw xq)
      ~retn:(fun {pf} ((q', _) as xq') ->
        pf "%a" pp_raw xq' ;
        assert (
          let gain = Var.Context.diff (Sh.fv q') (snd xq) in
          let gain = Var.Set.(diff (diff gain wrt) (xs xq')) in
          Var.Set.is_empty gain || fail "gain: {@[%a@]}" Var.Set.pp gain () )
        )
    @@ fun () ->
    let q, vx = extend_voc wrt xq in
    Var.Fresh.gen vx (f q)

  (** rename xs of xq1 that appear *free* in xq2 *)
  let diff_inter_xs_fv xs1 xq2 = Var.Set.diff_inter xs1 (fv xq2)

  (** rename xs of xq1 that appear *bound* or *free* in xq2 *)
  let _diff_inter_xs_vs xs1 (q2, _) = Var.Set.diff_inter xs1 (Sh.fv q2)

  (** rename xs of xq1 that are in the *vocabulary* of xq2 *)
  let diff_inter_xs_vx xs1 (_, vx2) = Var.Context.diff_inter xs1 vx2

  (** Lift a binary operation on quantifier-free Sh's to existential Xsh's:
      [lift2 ~f (∃ws.P) (∃zs.Q)] is [∃us,vs,xs,ys. f P' Q'] where
      [∃us,vs.P'] is α-equivalent to [∃ws.P] and [∃xs,ys.Q'] is α-equivalent
      to [∃zs.Q]. The choice of existentials that are α-renamed, that is the
      partitioning of [ws] into [us] and [vs] (and of [zs] into [xs] and
      [ys]) is determined by [diff_inter]: [diff_inter ws (∃zs.Q)] =
      [us, vs]. *)
  let lift2 diff_inter ~f ((q1, vx1) as xq1) ((q2, vx2) as xq2) =
    [%dbg]
      ~call:(fun {pf} -> pf "@ (%a)@ (%a)" pp_raw xq1 pp_raw xq2)
      ~retn:(fun {pf} ((q', _) as xq') ->
        pf "%a" pp_raw xq' ;
        assert (
          let gain = Var.Context.diff (Sh.fv q') vx1 in
          let gain = Var.Context.diff gain vx2 in
          let gain = Var.Set.(diff gain (xs xq')) in
          Var.Set.is_empty gain || fail "gain: {@[%a@]}" Var.Set.pp gain () )
        )
    @@ fun () ->
    let no_clash1, clash1 = diff_inter (xs xq1) xq2 in
    let no_clash2, clash2 = diff_inter (xs xq2) xq1 in
    Var.Fresh.gen
      Var.Context.(merge (with_xs no_clash1 vx1) (with_xs no_clash2 vx2))
    @@ fun vx ->
    let sub1 = Var.Fresh.subst clash1 vx in
    let sub2 = Var.Fresh.subst clash2 vx in
    let q1' = Sh.rename sub1 q1 vx in
    let q2' = Sh.rename sub2 q2 vx in
    f q1' q2' vx

  (** Construct *)

  let emp = (Sh.emp, Var.Context.empty)
  let false_ = (Sh.false_, Var.Context.empty)

  let star xq1 xq2 =
    [%Dbg.call fun {pf} -> pf "@ (%a)@ (%a)" pp_raw xq1 pp_raw xq2]
    ;
    lift2 diff_inter_xs_vx ~f:Sh.star xq1 xq2
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp_raw xq ;
      invariant xq]

  let starN qs = List.fold ~f:star qs emp

  let or_ xq1 xq2 =
    [%Dbg.call fun {pf} -> pf "@ (%a)@ (%a)" pp_raw xq1 pp_raw xq2]
    ;
    lift2 diff_inter_xs_fv ~f:(fun q1 q2 _ -> Sh.or_ q1 q2) xq1 xq2
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp_raw xq ;
      invariant xq]

  let orN qs = Set.fold ~f:or_ qs false_

  let and_ctx ctx xq =
    [%Dbg.call fun {pf} -> pf "@ %a@ %a" Context.pp ctx pp xq]
    ;
    lift ~wrt:(Context.fv ctx) ~f:(Sh.and_ctx ctx) xq
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp xq ;
      invariant xq]

  let pure b =
    [%Dbg.call fun {pf} -> pf "@ %a" Formula.pp b]
    ;
    Var.Fresh.gen (Var.Context.of_vars (Formula.fv b)) (Sh.pure b)
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp xq ;
      invariant xq]

  let and_ b xq =
    [%Dbg.call fun {pf} -> pf "@ (%a)@ (%a)" Formula.pp b pp xq]
    ;
    lift ~wrt:(Formula.fv b) ~f:(Sh.and_ b) xq
    |>
    [%Dbg.retn fun {pf} -> pf "%a" pp]

  let andN bs q = and_ (Formula.andN bs) q

  let and_subst sub xq =
    [%Dbg.call fun {pf} -> pf "@ %a@ %a" Context.Subst.pp sub pp xq]
    ;
    lift ~wrt:(Context.Subst.fv sub) ~f:(Sh.and_subst sub) xq
    |>
    [%Dbg.retn fun {pf} xq ->
      pf "%a" pp xq ;
      invariant xq]

  let rename sub xq =
    [%Dbg.call fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp_raw xq]
    ;
    let wrt = Var.Subst.range sub in
    let sub = Var.Subst.restrict_dom sub (xs xq) in
    lift ~wrt ~f:(Sh.rename sub) xq
    |>
    [%Dbg.retn fun {pf} xq' ->
      pf "%a" pp_raw xq' ;
      assert (Var.Set.disjoint (fv xq') (Var.Subst.domain sub))]

  let seg pt =
    let vx = Var.Context.of_vars (Var.Set.of_iter (Sh.vars_seg pt)) in
    (Sh.seg pt, vx) |> check invariant

  (** Update *)

  let rem_seg seg xq =
    lift xq ~f:(fun q _ -> Sh.rem_seg seg q) |> check invariant

  let filter_heap ~f xq =
    lift xq ~f:(fun q _ -> Sh.filter_heap ~f q) |> check invariant

  (** Disjunctive-Normal Form *)

  let dnf (q, vx) =
    Var.Fresh.gen_ vx (fun vx ->
        let cubes = Sh.dnf_iter ~conj:Sh.star q Sh.emp vx in
        Iter.fold_filter_map cubes Set.empty ~f:(fun xq s ->
            let s' = Set.add xq s in
            if s' == s then (None, s) else (Some xq, s') ) )

  (** Logical Query *)

  let pure_approx ((q, _) as xq) =
    [%Dbg.call fun {pf} -> pf "@ %a" pp xq]
    ;
    Sh.pure_approx q
    |>
    [%Dbg.retn fun {pf} -> pf "%a" Formula.pp]

  let is_unsat_dnf (q, vx) = Var.Fresh.gen_ vx (Sh.is_unsat_dnf q)
  let is_unsat (q, vx) = Var.Fresh.gen_ vx (Sh.is_unsat q)
  let is_unsat_strong (q, _) = Sh.is_unsat_strong q

  (** Simplify *)

  let norm s xq =
    [%Dbg.call fun {pf} ->
      pf "@ @[%a@]@ %a" Context.Subst.pp s pp_raw xq ;
      assert (
        let unbound = Var.Context.diff (Context.Subst.fv s) (snd xq) in
        Var.Set.is_empty unbound
        || fail "unbound subst vars: %a" Var.Set.pp unbound () )]
    ;
    lift ~f:(Sh.norm s) xq
    |>
    [%Dbg.retn fun {pf} xq' ->
      pf "%a" pp_raw xq' ;
      invariant xq']

  let count_simplify = ref (-1)

  let simplify xq =
    [%Dbg.call fun {pf} -> pf " %i@ %a" !count_simplify pp_raw xq]
    ;
    ( if is_false xq then false_
    else
      lift xq ~f:(fun q vx ->
          let q = Sh.propagate_context Context.empty q vx in
          if Sh.is_false q then Sh.false_
          else
            let q' =
              Sh.simplify (Var.Context.xs !vx) Var.Set.empty
                Context.Subst.empty q vx
            in
            Var.Fresh.inter_xs (Sh.fv q') vx ;
            q' ) )
    |>
    [%Dbg.retn fun {pf} xq' ->
      pf "%a" pp_raw xq' ;
      invariant xq']

  (*************************************************************************
   *                           Replay debugging                            *
   *************************************************************************)

  let replay c =
    match call_of_sexp (Sexp.of_string c) with
    | Extend_voc (wrt, xq) -> extend_voc wrt xq |> ignore
    | Simplify xq -> simplify xq |> ignore

  let dump_simplify = ref (-1)

  let simplify xq =
    Int.incr count_simplify ;
    if !count_simplify = !dump_simplify then
      fail "%a" Sexp.pp_hum (sexp_of_call (Simplify xq)) ()
    else
      try simplify xq
      with exc ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf "@\n%a@." Sexp.pp_hum (sexp_of_call (Simplify xq)) ;
        Printexc.raise_with_backtrace exc bt
end
