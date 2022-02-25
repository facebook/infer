(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Equality over uninterpreted functions and linear rational arithmetic *)

open Exp

(* Solution Substitutions ================================================*)

module Subst : sig
  type t = Trm.t Trm.Map.t [@@deriving compare, equal, sexp]

  val pp : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val is_empty : t -> bool
  val apply : t -> Trm.t -> Trm.t
  val norm : t -> Trm.t -> Trm.t
  val canon : t -> Trm.t -> Trm.t
  val subst : t -> Term.t -> Term.t
  val fold_eqs : t -> 's -> f:(Fml.t -> 's -> 's) -> 's
  val fv : t -> Var.Set.t
  val compose1 : Theory.oriented_equality -> t -> t
  val compose : t -> t -> t
  val map_entries : f:(Trm.t -> Trm.t) -> t -> t
  val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
end = struct
  type t = Trm.t Trm.Map.t [@@deriving compare, equal, sexp_of]

  let t_of_sexp = Trm.Map.t_of_sexp Trm.t_of_sexp
  let pp = Trm.Map.pp Trm.pp Trm.pp
  let pp_diff = Trm.Map.pp_diff ~eq:Trm.equal Trm.pp Trm.pp Trm.pp_diff
  let empty = Trm.Map.empty
  let is_empty = Trm.Map.is_empty

  let fold_eqs s z ~f =
    Trm.Map.fold ~f:(fun ~key ~data -> f (Fml.eq key data)) s z

  let trms s =
    Iter.flat_map ~f:(fun (k, v) -> Iter.doubleton k v) (Trm.Map.to_iter s)

  let vars s = Iter.flat_map ~f:Trm.vars (trms s)
  let fv s = Var.Set.of_iter (vars s)

  (** apply a substitution, considered as the identity function overridden
      for finitely-many terms *)
  let apply s a =
    [%trace]
      ~call:(fun {pf} -> pf "@ %a" Trm.pp a)
      ~retn:(fun {pf} -> pf "%a" Trm.pp)
    @@ fun () -> match Trm.Map.find a s with Some a' -> a' | None -> a

  (** apply a substitution to maximal noninterpreted subterms *)
  let norm s a =
    [%trace]
      ~call:(fun {pf} -> pf "@ %a" Trm.pp a)
      ~retn:(fun {pf} a' ->
        pf "%a" Trm.pp a' ;
        (* It is possible for [Trm] constructors to simplify an interpreted
           term to a solvable one. For example, normalizing [a = x×y+n],
           whose solvables are [{x×y, n}], with [n ↦ 0] yields [a' = x×y]
           (since [x×y+0] simplifies to [x×y]). However, [Trm]
           simplification is non-expansive wrt solvables: simplification
           does not create solvables that do not occur in the input. If it
           could, a new solvable might be in the domain of [s], which would
           lead to failure of [compose] to preserve idempotence of
           substitutions. *)
        let new_solvables =
          Trm.Set.diff
            (Trm.Set.of_iter (Trm.solvables a'))
            (Trm.Set.of_iter
               (Iter.flat_map ~f:Trm.solvables
                  (Iter.map ~f:(apply s) (Trm.solvables a)) ) )
        in
        assert (
          Trm.Set.is_empty new_solvables
          || fail "new solvables %a in %a not in %a" Trm.Set.pp
               new_solvables Trm.pp a' Trm.pp a () ) )
    @@ fun () -> Trm.map_solvables ~f:(apply s) a

  (** apply a substitution recursively, bottom-up *)
  let rec canon s a = apply s (Trm.map ~f:(canon s) a)

  (** lift apply from trm to term *)
  let subst s e = Term.map_trms ~f:(canon s) e

  (** compose two substitutions *)
  let compose r s =
    [%Trace.call fun {pf} -> pf "@ %a@ %a" pp r pp s]
    ;
    ( if is_empty s then r
    else
      let r' = Trm.Map.map_endo ~f:(norm s) r in
      Trm.Map.union_absent r' s )
    |>
    [%Trace.retn fun {pf} r' ->
      pf "%a" pp_diff (r, r') ;
      assert (r' == r || not (equal r' r))]

  (** compose a substitution with a mapping *)
  let compose1 {Theory.var; rep} r =
    if Trm.equal var rep then r
    else (
      assert (
        Option.for_all ~f:(Trm.equal var) (Trm.Map.find var r)
        || fail "domains intersect: %a ↦ %a in %a" Trm.pp var Trm.pp rep pp
             r () ) ;
      let s = Trm.Map.singleton var rep in
      let r' = Trm.Map.map_endo ~f:(norm s) r in
      Trm.Map.add ~key:var ~data:rep r' )

  (** map over a subst, applying [f] to both domain and range, requires that
      [f] is injective and for any set of terms [E], [f\[E\]] is disjoint
      from [E] *)
  let map_entries ~f s =
    Trm.Map.fold s s ~f:(fun ~key ~data s ->
        let key' = f key in
        let data' = f data in
        if Trm.equal key' key then
          if Trm.equal data' data then s else Trm.Map.add ~key ~data:data' s
        else
          let s = Trm.Map.remove key s in
          match (key' : Trm.t) with
          | Z _ | Q _ -> s
          | _ -> Trm.Map.add_exn ~key:key' ~data:data' s )

  (** Holds only if [true ⊢ ∃xs. e=f]. Clients assume
      [not (is_valid_eq xs e f)] implies [not (is_valid_eq ys e f)] for
      [ys ⊆ xs]. *)
  let is_valid_eq xs e f =
    let is_var_in xs e = Trm.Set.mem e (xs : Var.Set.t :> Trm.Set.t) in
    let noninterp_with_solvable_var_in xs e =
      is_var_in xs e
      || Trm.non_interpreted e
         && Iter.exists ~f:(is_var_in xs) (Trm.solvable_trms e)
    in
    ( noninterp_with_solvable_var_in xs e
    || noninterp_with_solvable_var_in xs f )
    $> fun b ->
    [%Trace.info " %a%a=%a = %b" Var.Set.pp_xs xs Trm.pp e Trm.pp f b]

  (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks and ν are
      maximal where ∃ks. ν is universally valid, xs ⊇ ks and ks ∩ fv(τ) = ∅. *)
  let partition_valid xs s =
    [%trace]
      ~call:(fun {pf} -> pf "@ @[%a@ %a@]" Var.Set.pp_xs xs pp s)
      ~retn:(fun {pf} (t, ks, u) ->
        pf "%a@ %a@ %a" pp t Var.Set.pp_xs ks pp u )
    @@ fun () ->
    (* Move equations e=f from s to t when ∃ks.e=f fails to be provably
       valid. When moving an equation, reduce ks by fv(e=f) to maintain ks ∩
       fv(t) = ∅. This reduction may cause equations in s to no longer be
       valid, so loop until no change. *)
    let rec partition_valid_ t ks s =
      let t', ks', s' =
        Trm.Map.fold s (t, ks, s) ~f:(fun ~key ~data (t, ks, s) ->
            if is_valid_eq ks key data then (t, ks, s)
            else
              let t = Trm.Map.add ~key ~data t in
              let ks =
                Var.Set.diff ks (Var.Set.union (Trm.fv key) (Trm.fv data))
              in
              let s = Trm.Map.remove key s in
              (t, ks, s) )
      in
      if s' != s then partition_valid_ t' ks' s' else (t', ks', s')
    in
    if Var.Set.is_empty xs then (s, Var.Set.empty, empty)
    else partition_valid_ empty xs s
end

(* Equality classes ======================================================*)

module Cls : sig
  type t [@@deriving compare, equal, sexp]

  val ppx : Trm.Var.strength -> t pp
  val pp : t pp
  val pp_raw : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val add : Trm.t -> t -> t
  val remove : Trm.t -> t -> t
  val union : t -> t -> t
  val is_empty : t -> bool
  val mem : Trm.t -> t -> bool
  val pop : t -> (Trm.t * t) option
  val fold : t -> 's -> f:(Trm.t -> 's -> 's) -> 's
  val filter : t -> f:(Trm.t -> bool) -> t
  val partition : t -> f:(Trm.t -> bool) -> t * t
  val map : t -> f:(Trm.t -> Trm.t) -> t
  val to_iter : t -> Trm.t iter
end = struct
  include Trm.Set

  let ppx x fs es = pp_full ~sep:"@ = " (Trm.ppx x) fs es
  let pp = ppx (fun _ -> None)
  let pp_raw fs es = pp_full ~pre:"{@[" ~suf:"@]}" ~sep:",@ " Trm.pp fs es
end

(* Use lists / Super-expressions =========================================*)

module Use : sig
  type t [@@deriving compare, equal, sexp]

  val pp : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val add : Trm.t -> t -> t
  val iter : t -> f:(Trm.t -> unit) -> unit
  val exists : t -> f:(Trm.t -> bool) -> bool
  val fold : t -> 's -> f:(Trm.t -> 's -> 's) -> 's
  val map : t -> f:(Trm.t -> Trm.t) -> t
  val filter : t -> f:(Trm.t -> bool) -> t
  val iter_of_opt : t option -> Trm.t iter
end = struct
  include Trm.Set

  let iter_of_opt o = Iter.flat_map ~f:to_iter (Iter.of_opt o)
end

(* Conjunctions of atomic formula assumptions ============================*)

(** See also {!invariant} which checks many of the representation
    invariants. *)
type t =
  { xs: Var.Set.t
        (** existential variables that did not appear in input formulas *)
  ; sat: bool  (** [false] only if constraints are inconsistent *)
  ; rep: Subst.t
        (** Functional set of oriented equations that map solvable terms to
            their representatives: [a ↦ a'] indicates that [x ⊢ a = a'], and
            that [a'] is the "rep(resentative)" of [a]. We write [rep(a)]
            for [Subst.apply x.rep a].

            - Only solvable terms appear in the domain of [rep], while the
              range may also include interpreted terms.

            - The representative map is idempotent in the sense that
              solvables of representative terms are normalized, that is,
              [Subst.norm x.rep a' = a'].

            - The representative map is sparse in the sense that identity
              mappings for atoms are omitted, and if [x ⊢ f(a)=b] then [x]
              need not contain a mapping [f(c)↦b] for every non-interpreted
              term [f(c)] such that [x ⊢ f(c)=b]; only mappings for the
              terms whose subterms are representatives are required.
              Therefore the basic operation of finding the representative of
              an uninterpreted application is to [norm]alize the solvable
              subterms and then [apply] the representatives substitution. *)
  ; cls: Cls.t Trm.Map.t
        (** Map representatives to their classes: [a ∈ cls(a')] iff
            [rep(a) = rep(a') = a' ≠ a]. Note that the range of [cls] does
            not include the representatives.

            - The class map is sparse in the sense that mappings are omitted
              for representatives whose classes consist of only the
              representative itself. *)
  ; use: Use.t Trm.Map.t
        (** Map solvable representatives to their "uses". A "use" of a term
            is similar to an immediate super-term, but modulo the
            representatives map:

            - Every use of solvable [a'] has a solvable subterm whose
              representative's solvables contain [a']: if
              [f(b₀,…,bₙ) ∈ use(a')], then [∃i. a' ∈ solvables(rep(bᵢ))].
              Also [solvables(f(b₀,…,bₙ)) ⊆ dom(rep) ∪ dom(cls)].

            - Every representative is congruent to some use of the
              representative of each of its solvable subterms with solvable
              reps: if [f(b₀,…,bₙ) ∈ rng(rep)] then for each [i], if
              [rep(bᵢ)] is solvable, then [∃ f(c₀,…,cₙ) ∈ use(rep(bᵢ))] such
              that [rep(cⱼ) = rep(bⱼ)] for [0≤j≤n]. *)
  ; pnd: (Trm.t * Trm.t) list
        (** Equations between terms whose solvables are in the carrier, to
            add pending reestablishment of invariants. During propagation,
            equations can be discovered when invariants temporarily do not
            hold. The pending list is used to record such equations to be
            added once invariants are reestablished. *) }
[@@deriving compare, equal, sexp]

(* Pretty-printing =======================================================*)

let pp_eq fs (e, f) = Format.fprintf fs "@[%a = %a@]" Trm.pp e Trm.pp f

let pp_raw fs {sat; rep; cls; use; pnd} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ @[%a@]@]]" pp_k k pp_v (k, v)
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_trm_v fs (k, v) = if not (Trm.equal k v) then Trm.pp fs v in
  let pp_cls_v fs (_, cls) = Cls.pp_raw fs cls in
  let pp_use_v fs (_, use) = Use.pp fs use in
  let pp_pnd fs pnd =
    List.pp ~pre:";@ pnd= [@[" ";@ " ~suf:"@]]" pp_eq fs pnd
  in
  Format.fprintf fs
    "@[{ @[<hv>sat= %b;@ rep= %a;@ cls= %a;@ use= %a%a@] }@]" sat
    (pp_alist Trm.pp pp_trm_v)
    (Trm.Map.to_list rep)
    (pp_alist Trm.pp pp_cls_v)
    (Trm.Map.to_list cls)
    (pp_alist Trm.pp pp_use_v)
    (Trm.Map.to_list use) pp_pnd pnd

let pp_diff fs (r, s) =
  let pp_sat fs =
    if not (Bool.equal r.sat s.sat) then
      Format.fprintf fs "sat= @[-- %b@ ++ %b@];@ " r.sat s.sat
  in
  let pp_rep fs =
    Trm.Map.pp_diff ~eq:Trm.equal ~pre:"rep= @[" ~sep:";@ " ~suf:"@]" Trm.pp
      Trm.pp Trm.pp_diff fs (r.rep, s.rep)
  in
  let pp_cls fs =
    Trm.Map.pp_diff ~eq:Cls.equal ~pre:";@ cls= @[" ~sep:";@ " ~suf:"@]"
      Trm.pp Cls.pp_raw Cls.pp_diff fs (r.cls, s.cls)
  in
  let pp_use fs =
    Trm.Map.pp_diff ~eq:Use.equal ~pre:";@ use= @[" ~sep:";@ " ~suf:"@]"
      Trm.pp Use.pp Use.pp_diff fs (r.use, s.use)
  in
  let pp_pnd fs =
    List.pp_diff ~cmp:[%compare: Trm.t * Trm.t] ~pre:";@ pnd= [@[" ";@ "
      ~suf:"@]]" pp_eq fs (r.pnd, s.pnd)
  in
  Format.fprintf fs "@[{ @[<hv>%t%t%t%t%t }@]@]" pp_sat pp_rep pp_cls pp_use
    pp_pnd

let ppx_classes x fs clss =
  List.pp "@ @<2>∧ "
    (fun fs (rep, cls) ->
      if not (Cls.is_empty cls) then
        Format.fprintf fs "@[%a@ = %a@]" (Trm.ppx x) rep (Cls.ppx x) cls )
    fs (Trm.Map.to_list clss)

let pp_classes fs r = ppx_classes (fun _ -> None) fs r.cls

let pp fs r =
  if Trm.Map.is_empty r.cls then
    Format.fprintf fs (if r.sat then "tt" else "ff")
  else pp_classes fs r

let ppx var_strength fs clss noneqs =
  let without_anon_vars es =
    Cls.filter es ~f:(fun e ->
        match Var.of_trm e with
        | Some v -> Poly.(var_strength v <> Some `Anonymous)
        | None -> true )
  in
  let clss =
    Trm.Map.fold clss Trm.Map.empty ~f:(fun ~key:rep ~data:cls m ->
        let cls = without_anon_vars cls in
        if not (Cls.is_empty cls) then Trm.Map.add ~key:rep ~data:cls m
        else m )
  in
  let first = Trm.Map.is_empty clss in
  if not first then Format.fprintf fs "  " ;
  ppx_classes var_strength fs clss ;
  List.pp
    ~pre:(if first then "@[  " else "@ @[@<2>∧ ")
    "@ @<2>∧ " (Fml.ppx var_strength) fs noneqs ~suf:"@]" ;
  first && List.is_empty noneqs

let pp_diff_cls = Trm.Map.pp_diff ~eq:Cls.equal Trm.pp Cls.pp Cls.pp_diff

(* Basic representation queries ==========================================*)

(** test if a term is in the carrier *)
let in_car a x =
  match Trm.classify a with
  | InterpAtom | NonInterpAtom -> true
  | InterpApp -> Trm.Map.mem a x.cls
  | UninterpApp -> Trm.Map.mem a x.rep

(** (non-atomic) terms in the carrier *)
let car x = Iter.append (Trm.Map.keys x.rep) (Trm.Map.keys x.cls)

(** terms that are representatives *)
let reps x = Trm.Set.to_iter (Trm.Set.of_iter (Trm.Map.values x.rep))

(** free variables of terms in the carrier *)
let fv x = Var.Set.of_iter (Iter.flat_map ~f:Trm.vars (car x))

(** free variables of terms in the carrier *)
let vars x = Var.Set.to_iter (fv x)

let cls_of a' x = Trm.Map.find a' x.cls |> Option.value ~default:Cls.empty
let rep_cls_of a' x = Cls.add a' (cls_of a' x)
let use_of a' x = Trm.Map.find a' x.use |> Option.value ~default:Use.empty

(* Normalization and Canonization ========================================*)

(** [apply x a], the [x]-representative of [a], is [a'] obtained by simply
    looking up [a] in [x.rep]. Therefore [x ⊢ a=a'], and
    [apply x a = apply x b] whenever [x ⊢ a=b] using only the equality
    theory without the Congruence rule.

    This is useful to find the representative of a term whose solvable
    subterms are already normalized to their representatives. *)
let apply x a = Subst.apply x.rep a

(** [norm x a], the [x]-normal form of [a], is [a'] which is [a] with
    solvables replaced by their solutions in [x.rep]. Therefore [x ⊢ a=a'],
    and [norm x a = norm x b] whenever [x ⊢ a=b] using only the equality
    theory with at most one application of the Congruence rule.

    This is useful to find the representative of a term whose solvables are
    in the carrier. *)
let rec norm x a =
  (* [norm x] is equivalent to [Subst.norm x.rep] but optimized based on
     context invariants *)
  match Trm.classify a with
  | InterpAtom -> a
  | NonInterpAtom | UninterpApp -> apply x a
  | InterpApp ->
      if Trm.Map.mem a x.cls then
        (* Optim: no need to recurse when [a] is already an interpreted
           representative since they are normalized *)
        a
      else Trm.map ~f:(norm x) a

let norm x a =
  [%trace]
    ~call:(fun {pf} -> pf " %a@ %a" Trm.pp a pp_raw x)
    ~retn:(fun {pf} a' ->
      pf "%a" Trm.pp a' ;
      assert (
        let a'' = Subst.norm x.rep a in
        Trm.equal a' a''
        || fail "norm %a ≠ %a@ %a" Trm.pp a' Trm.pp a'' pp_raw x () ) )
  @@ fun () -> norm x a

(* test if all solvable subterms are representatives *)
let is_normalized x a =
  Iter.for_all (Trm.solvable_trms a) ~f:(fun b ->
      Option.for_all ~f:(Trm.equal b) (Trm.Map.find b x.rep) )

(** [canon x a], the [x]-canonical form of [a], is [a'] which is [a]
    expressed using the solutions for the solvables in [x.rep]. Therefore
    [x ⊢ a=a'], and [canon x a = canon x b] whenever [x ⊢ a=b].

    This is useful to find the representative of a term whose subterms need
    not be in the carrier. For terms whose solvables are in the carrier,
    [norm] is equivalent to [canon].

    Note that canonical terms may have non-canonical subterms. Solvable
    subterms of interpreted canonical terms are guaranteed to be
    representatives, but it is not in general possible to finitely express
    solvable subterms of uninterpreted applications in terms of
    representatives. *)
let rec canon x a =
  (* [canon x] is equivalent to [Subst.canon x.rep] but optimized based on
     context invariants *)
  match Trm.classify a with
  | InterpAtom -> a
  | NonInterpAtom -> apply x a
  | InterpApp ->
      if Trm.Map.mem a x.cls then
        (* Optim: no need to recurse when [a] is already an interpreted
           representative since they are normalized *)
        a
      else canon_trms x a
  | UninterpApp -> (
    match Trm.Map.find a x.rep with
    | Some a' ->
        (* Optim: no need to recurse when [a] in rep *)
        a'
    | None -> canon_trms x a )

and canon_trms x a =
  (* Recursively canonize subterms and rebuild terms applying term
     simplifications. It is possible for the result of term simplification
     (performed by [Trm.map]) to be a new solvable. For example, canonizing
     subterms of [a = ((x+n)×y)+n] with [n ↦ 0; x×y ↦ z] yields [a' = x×y].
     Canonization finishes by applying the substitution, yielding [z]. This
     use of [apply] requires that the solvable subterms of any solvable [a']
     are already normalized. This is ensured since term simplification is
     guaranteed to be non-expansive wrt solvables, see
     {!Trm.solvables_contained_in}. Otherwise [canon] would not necessarily
     be idempotent. *)
  let a' = Trm.map ~f:(canon x) a in
  assert (is_normalized x a') ;
  if a' == a || Trm.is_interpreted a' then
    (* Optim: [a'] cannot be in [x.rep] since either it is [a] and [find]
       just failed or it is interpreted *)
    a'
  else apply x a'

let canon x a =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ | %a" Trm.pp a pp_raw x)
    ~retn:(fun {pf} a' ->
      pf "%a" Trm.pp a' ;
      assert (
        let a'' = canon x a' in
        Trm.equal a'' a'
        || fail "canon not idempotent: %a ≠ %a@ %a" Trm.pp a'' Trm.pp a'
             pp_raw x () ) ;
      assert (
        let a'' = Subst.canon x.rep a' in
        Trm.equal a'' a'
        || fail "canon %a ≠ %a@ %a" Trm.pp a'' Trm.pp a' pp_raw x () ) )
  @@ fun () -> canon x a

(* Invariant =============================================================*)

let congruent x a b =
  let norm_trms x e = Trm.map ~f:(Subst.norm x) e in
  Trm.equal (norm_trms x a) (norm_trms x b)

let pre_invariant x =
  let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
  try
    Iter.iter (car x) ~f:(fun a ->
        Iter.iter (Trm.solvable_trms a) ~f:(fun b ->
            (* carrier is closed under solvable subterms *)
            assert (
              in_car b x
              || fail "@[subterm %a@ of %a@ not in carrier@]" Trm.pp b
                   Trm.pp a () ) ) ) ;
    Trm.Map.iteri x.rep ~f:(fun ~key:a ~data:a' ->
        (* only noninterpreted terms in dom of rep *)
        assert (
          Trm.non_interpreted a
          || fail "@[interp in rep dom %a@]" Trm.pp a () ) ;
        (* no identity mappings for atoms *)
        assert (
          (not (Trm.equal a a'))
          || (not (Trm.is_atomic a))
          || fail "@[identity mapping for atom: %a@]" Trm.pp a () ) ;
        (* rep is idempotent *)
        assert (
          let a'' = Subst.norm x.rep a' in
          Trm.equal a' a''
          || fail "@[rep not idempotent:@ @[%a@ ≠ %a@]@ %a@]" Trm.pp a'
               Trm.pp a'' Subst.pp x.rep () ) ;
        (* every term is in class of its rep *)
        assert (
          let a'_cls = rep_cls_of a' x in
          Cls.mem a a'_cls
          || fail "@[%a not in cls of %a = {%a}@]" Trm.pp a Trm.pp a' Cls.pp
               a'_cls () ) ) ;
    Trm.Map.iteri x.cls ~f:(fun ~key:a' ~data:cls ->
        (* each class does not include its rep *)
        assert (
          (not (Cls.mem a' cls))
          || fail "@[rep %a in cls %a@]" Trm.pp a' Cls.pp cls () ) ;
        (* no mappings for singleton classes *)
        assert (
          (not (Cls.is_empty cls))
          || fail "@[singleton class for %a@]" Trm.pp a' () ) ;
        Iter.iter (Cls.to_iter cls) ~f:(fun e ->
            (* rep of every element in class of [a'] is [a'] *)
            assert (
              let e' = apply x e in
              Trm.equal e' a'
              || fail "@[rep of %a = %a but should = %a, cls: %a@]" Trm.pp e
                   Trm.pp e' Trm.pp a' Cls.pp cls () ) ) ) ;
    Iter.iter (reps x) ~f:(fun a' ->
        Iter.iter (Trm.solvable_trms a') ~f:(fun b ->
            (* every rep is congruent to some use of the representative of
               each of its solvable subterms with solvable rep *)
            assert (
              let b' = apply x b in
              let b'_use = use_of b' x in
              Trm.is_interpreted b'
              || Use.exists ~f:(congruent x.rep a') b'_use
              || fail
                   "@[no use congruent to %a for subtrm %a with rep %a in \
                    {%a}@]"
                   Trm.pp a' Trm.pp b Trm.pp b' Use.pp b'_use () ) ) ) ;
    Trm.Map.iteri x.use ~f:(fun ~key:a' ~data:use ->
        (* [a'] is a solvable *)
        assert (
          Trm.non_interpreted a'
          || fail "@[interpreted in dom of use: %a@]" Trm.pp a' () ) ;
        (* [a'] is a representative or absent atom *)
        assert (
          ( match Trm.Map.find a' x.rep with
          | Some a'' -> Trm.equal a' a''
          | None -> Trm.is_atomic a' )
          || fail "@[non-rep in dom of use: %a@]" Trm.pp a' () ) ;
        Use.iter use ~f:(fun u ->
            Iter.iter (Trm.solvables u) ~f:(fun b ->
                (* carrier is closed under solvables of uses *)
                assert (
                  in_car b x
                  || fail "@[solvable %a of use %a of %a not in carrier@]"
                       Trm.pp b Trm.pp u Trm.pp a' () ) ) ;
            (* every use of [a'] has a solvable subterm whose rep's
               solvables contain [a'] *)
            assert (
              Iter.exists (Trm.solvable_trms u) ~f:(fun b ->
                  Iter.mem ~eq:Trm.equal a' (Trm.solvables (apply x b)) )
              || fail "@[extra use %a of %a@]" Trm.pp u Trm.pp a' () ) ) ) ;
    List.iter x.pnd ~f:(fun (a, b) ->
        (* carrier is closed under solvables of pending equations *)
        Iter.iter
          (Iter.append (Trm.solvables a) (Trm.solvables b))
          ~f:(fun e ->
            assert (
              in_car e x
              || fail "@[solvable %a of pending %a = %a not in carrier@]"
                   Trm.pp e Trm.pp a Trm.pp b () ) ) )
  with exc ->
    let bt = Printexc.get_raw_backtrace () in
    [%Trace.info "@ %a" pp_raw x] ;
    Printexc.raise_with_backtrace exc bt

let invariant x =
  let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
  assert (List.is_empty x.pnd) ;
  pre_invariant x ;
  assert (
    (not x.sat)
    || Trm.Map.for_alli x.rep ~f:(fun ~key:a ~data:a' ->
           Trm.Map.for_alli x.rep ~f:(fun ~key:b ~data:b' ->
               Trm.compare a b >= 0
               || (not (congruent x.rep a b))
               || Trm.equal a' b'
               || fail "not congruent %a@ %a in@ @[%a@]" Trm.pp a Trm.pp b
                    pp_raw x () ) ) )

(* Extending the carrier =================================================*)

(** [add_use_of ~sup ~sub'] adds [sup] as a use of [sub'], assuming that
    there exists a subterm [sub] of [sup] such that [apply x sub = sub']. *)
let add_use_of ~sup ~sub' use =
  Trm.Map.update sub' use ~f:(fun u ->
      Some (Use.add sup (Option.value ~default:Use.empty u)) )

(** [add_uses_of a] adds [a] as a use of it's subterms, assuming that the
    solvable subterms of [a] are normalized. *)
let add_uses_of a use =
  Iter.fold
    ~f:(fun sub' use -> add_use_of ~sup:a ~sub' use)
    (Trm.solvable_trms a) use

(** add a canonical term to the carrier *)
let rec extend a x =
  [%trace]
    ~call:(fun {pf} ->
      pf "@ %a@ | %a" Trm.pp a pp_raw x ;
      assert (Trm.equal a (canon x a)) )
    ~retn:(fun {pf} x' -> pf "%a" pp_diff (x, x'))
  @@ fun () ->
  let extend_app a x = Iter.fold ~f:extend (Trm.trms a) x in
  match Trm.classify a with
  | InterpAtom | NonInterpAtom -> x
  | InterpApp ->
      if Trm.Map.mem a x.cls then
        (* Optim: no need to recurse when [a] already in carrier *)
        x
      else extend_app a x
  | UninterpApp ->
      let rep = Trm.Map.add_absent ~key:a ~data:a x.rep in
      if rep == x.rep then
        (* Optim: no need to recurse when [a] already in carrier *)
        x
      else
        let use = add_uses_of a x.use in
        extend_app a {x with rep; use}

(** [canon_extend x a] is [a', x'] where [a'] is [canon x a] and [x'] is [x]
    extended so that [a' = norm x' a']. This recursively adds solvable
    subterms of [a'] to the carrier. *)
let canon_extend x a =
  [%trace]
    ~call:(fun {pf} -> pf " %a@ %a" Trm.pp a pp_raw x)
    ~retn:(fun {pf} (a', x') ->
      pf "%a@ %a" Trm.pp a' pp_diff (x, x') ;
      invariant x' ;
      assert (
        let a'' = norm x' a' in
        Trm.equal a' a'' || fail "%a ≠ %a" Trm.pp a' Trm.pp a'' () ) )
  @@ fun () ->
  let a' = canon x a in
  (a', extend a' x)

(* Propagation ===========================================================*)

let propagate_use x0 t u x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Trm.pp u pp_raw x)
    ~retn:(fun {pf} x' -> pf "%a" pp_diff (x, x'))
  @@ fun () ->
  (* Normalize interpreted uses without the just-added v ↦ t *)
  let u = if Trm.is_interpreted u then (norm x0) u else u in
  (* Apply congruence once, using the just-added v ↦ t *)
  let w = Trm.map_solvable_trms ~f:(apply x) u in
  if w == u then x
  else if Trm.is_interpreted u then
    (* v ↦ t, v ∈ u, u interp, so u a rep, so move cls(u) to u[v ↦ t] *)
    let use =
      if Trm.Map.mem w x.cls then x.use
      else
        (* w is a new term, so add uses for its solvable subterms *)
        add_uses_of w x.use
    in
    let u_cls, cls = Trm.Map.find_and_remove u x.cls in
    let u_cls = Option.value u_cls ~default:Cls.empty in
    let cls =
      Trm.Map.update w cls ~f:(fun w_cls ->
          Some (Option.fold ~f:Cls.union w_cls u_cls) )
    in
    let rep =
      Cls.fold u_cls x.rep ~f:(fun e rep -> Trm.Map.add ~key:e ~data:w rep)
    in
    {x with rep; cls; use}
  else
    match Trm.classify w with
    | NonInterpAtom | UninterpApp -> (
      match Trm.Map.find_or_add_lazy w ~f:(fun () -> norm x u) x.rep with
      | `Found w', rep ->
          (* Now u ~ w and w ↦ w' already, so add u = w' to continue
             propagation. *)
          {x with rep; pnd= (u, w') :: x.pnd}
      | `Added u', rep ->
          (* Now u ~ w and w just added to carrier (↦ u'), so no further
             propagation needed. But u ∈ use(v) and v ↦ t, so there exists a
             solvable of u whose rep's solvables contain t, so add u to
             use(solvables(t)). *)
          let cls =
            Trm.Map.update u' x.cls ~f:(fun u_cls ->
                Some (Cls.add w (Option.value u_cls ~default:Cls.empty)) )
          in
          let use = Trm.Map.remove w x.use in
          let use =
            Iter.fold (Trm.solvables t) use ~f:(fun sub' ->
                add_use_of ~sup:u ~sub' )
          in
          {x with rep; cls; use} )
    | InterpAtom ->
        (* Now u ~ w and w is interpreted, so add u = w in order to
           Theory.solve it *)
        {x with pnd= (u, w) :: x.pnd}
    | InterpApp ->
        (* Now u ~ w and w is interpreted, so add u = w in order to
           Theory.solve it *)
        let pnd = (u, w) :: x.pnd in
        let use =
          if Trm.Map.mem w x.cls then x.use
          else
            (* w is a new term, so add uses for its solvable subterms *)
            add_uses_of w x.use
        in
        {x with use; pnd}

(** add v ↦ t to x *)
let propagate1 {Theory.var= v; rep= t} x =
  [%trace]
    ~call:(fun {pf} ->
      pf "@ @[%a ↦ %a@]@ %a" Trm.pp v Trm.pp t pp_raw x ;
      (* v is a solvable term that is a representative or absent atom *)
      assert (Trm.non_interpreted v) ;
      assert (
        match Trm.Map.find v x.rep with
        | Some v' -> Trm.equal v v'
        | None -> Trm.is_atomic v ) ;
      (* if t is not interpreted, then it must be in the carrier *)
      assert (
        Trm.is_interpreted t
        || in_car t x
        || fail "new rep not in carrier %a" Trm.pp t () ) ;
      (* t must already be normalized *)
      assert (
        let t' = Subst.norm x.rep t in
        Trm.equal t t'
        || fail "new rep not normal %a != %a" Trm.pp t Trm.pp t' () ) )
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      pre_invariant x' )
  @@ fun () ->
  let v_cls, cls = Trm.Map.find_and_remove v x.cls in
  let v_cls = Cls.add v (Option.value v_cls ~default:Cls.empty) in
  let cls =
    Trm.Map.update t cls ~f:(fun t_cls ->
        Some (Option.fold ~f:Cls.union t_cls v_cls) )
  in
  let rep =
    Cls.fold v_cls x.rep ~f:(fun e rep -> Trm.Map.add ~key:e ~data:t rep)
  in
  let v_use, use = Trm.Map.find_and_remove v x.use in
  let use = if Trm.is_interp_app t then add_uses_of t use else use in
  Iter.fold ~f:(propagate_use x t) (Use.iter_of_opt v_use)
    {x with rep; cls; use}

let solve ~wrt ~xs d e pending =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Trm.pp d Trm.pp e)
    ~retn:(fun {pf} -> pf "%a" Theory.pp)
  @@ fun () ->
  Theory.solve d e
    {wrt; no_fresh= false; fresh= xs; solved= Some []; pending}

let rec propagate_ ~wrt x =
  match x.pnd with
  | [] -> x
  | (a, b) :: pnd -> (
      [%Trace.info "@ %a = %a" Trm.pp a Trm.pp b] ;
      let a' = norm x a in
      let b' = norm x b in
      if Trm.equal a' b' then propagate_ ~wrt {x with pnd}
      else
        match solve ~wrt ~xs:x.xs a' b' pnd with
        | {solved= Some solved; wrt; fresh; pending} ->
            let xs = Var.Set.union x.xs fresh in
            let x = {x with xs; pnd= pending} in
            propagate_ ~wrt (List.fold ~f:propagate1 solved x)
        | {solved= None} -> {x with sat= false; pnd= []} )

let propagate ~wrt x =
  [%trace]
    ~call:(fun {pf} ->
      pf "@ %a" pp_raw x ;
      pre_invariant x )
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      invariant x' )
  @@ fun () -> propagate_ ~wrt x

(* Core operations =======================================================*)

let empty =
  { xs= Var.Set.empty
  ; sat= true
  ; rep= Subst.empty
  ; cls= Trm.Map.empty
  ; use= Trm.Map.empty
  ; pnd= [] }
  |> check invariant

let unsat = {empty with sat= false}

let merge ~wrt a b x =
  [%trace]
    ~call:(fun {pf} -> pf " @[%a =@ %a@] |@ %a" Trm.pp a Trm.pp b pp x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      invariant x' )
  @@ fun () ->
  let x = {x with pnd= (a, b) :: x.pnd} in
  propagate ~wrt x

let and_eq ~wrt a b x =
  [%trace]
    ~call:(fun {pf} -> pf "@ @[%a = %a@]@ | %a" Trm.pp a Trm.pp b pp x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      invariant x' )
  @@ fun () ->
  if not x.sat then x
  else
    let a', x = canon_extend x a in
    let b', x = canon_extend x b in
    if Trm.equal a' b' then x else merge ~wrt a' b' x

let extract_xs r = (r.xs, {r with xs= Var.Set.empty})

(* Exposed interface =====================================================*)

let is_empty {sat; rep} =
  sat && Trm.Map.for_alli rep ~f:(fun ~key:a ~data:a' -> Trm.equal a a')

let is_unsat {sat} = not sat

let canon_f x b =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ | %a" Fml.pp b pp_raw x)
    ~retn:(fun {pf} -> pf "%a" Fml.pp)
  @@ fun () -> Fml.map_trms ~f:(canon x) b

let normalize x a =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ | %a" Term.pp a pp x)
    ~retn:(fun {pf} -> pf "%a" Term.pp)
  @@ fun () -> Term.map_trms ~f:(canon x) a

let implies r b =
  [%Trace.call fun {pf} -> pf "@ %a@ | %a" Fml.pp b pp r]
  ;
  Fml.equal Fml.tt (canon_f r b)
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

let refutes r b = Fml.equal Fml.ff (canon_f r b)
let fold_eqs x = Subst.fold_eqs x.rep

let class_of r e =
  match Term.get_trm (normalize r e) with
  | Some e' ->
      Iter.to_list (Iter.map ~f:Term.of_trm (Cls.to_iter (rep_cls_of e' r)))
  | None -> []

let diff_classes r s =
  Trm.Map.filter_mapi r.cls ~f:(fun ~key:rep ~data:cls ->
      let cls' =
        Cls.filter cls ~f:(fun exp -> not (implies s (Fml.eq rep exp)))
      in
      if Cls.is_empty cls' then None else Some cls' )

let ppx_diff var_strength fs parent_ctx fml ctx =
  let fml' = canon_f ctx fml in
  ppx var_strength fs
    (diff_classes ctx parent_ctx)
    (if Fml.(equal tt fml') then [] else [fml'])

let apply_subst wrt s r =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Subst.pp s pp r]
  ;
  ( if Subst.is_empty s then r
  else
    Trm.Map.fold r.cls
      {r with rep= Subst.empty; cls= Trm.Map.empty; use= Trm.Map.empty}
      ~f:(fun ~key:rep ~data:cls r ->
        let rep' = Subst.canon s rep in
        Cls.fold cls r ~f:(fun trm r ->
            let trm' = Subst.canon s trm in
            and_eq ~wrt trm' rep' r ) ) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (xs, r') ->
    pf "%a%a" Var.Set.pp_xs xs pp_diff (r, r') ;
    invariant r']

let union wrt r s =
  [%Trace.call fun {pf} -> pf "@ @[<hv 1>   %a@ @<2>∧ %a@]" pp r pp s]
  ;
  ( if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Trm.Map.length s.rep <= Trm.Map.length r.rep then (s, r) else (r, s)
    in
    Trm.Map.fold s.rep r ~f:(fun ~key:e ~data:e' r -> and_eq ~wrt e e' r) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let inter wrt (xs, r) (ys, s) =
  [%Trace.call fun {pf} ->
    pf "@ @[  @[<hv 2>{ %a }@]@ @<2>∨ @[<hv 2>{ %a }@]@]" pp r pp s]
  ;
  ( if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems wrt (xs, r) (ys, s) i =
      let unbound vs = Var.Set.disjoint xs vs && Var.Set.disjoint ys vs in
      Trm.Map.fold s.cls i ~f:(fun ~key:rep ~data:cls i ->
          let reps = if unbound (Trm.fv rep) then [rep] else [] in
          Cls.fold cls (reps, i) ~f:(fun exp (reps, i) ->
              if not (unbound (Trm.fv exp)) then (reps, i)
              else
                match
                  List.find ~f:(fun rep -> implies r (Fml.eq exp rep)) reps
                with
                | Some rep -> (reps, and_eq ~wrt exp rep i)
                | None -> (exp :: reps, i) )
          |> snd )
    in
    let i = empty in
    let i = merge_mems wrt (xs, r) (ys, s) i in
    let i = merge_mems wrt (ys, s) (xs, r) i in
    i )
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let interN us xrIN =
  ( match xrIN with
  | [] -> unsat
  | (xs0, r0) :: xr1N ->
      List.fold xr1N (Var.Set.empty, xs0, r0)
        ~f:(fun (xsK, rK) (xs0I, xsJ, i0J) ->
          let xs0J = Var.Set.union xs0I xsJ in
          let i0K = inter us (xsK, rK) (xs0J, i0J) in
          (xs0J, xsK, i0K) )
      |> trd3 )
  |> extract_xs

let rec add_ wrt b r =
  match (b : Fml.t) with
  | Tt -> r
  | Not Tt -> unsat
  | And {pos; neg} -> Fml.fold_pos_neg ~f:(add_ wrt) ~pos ~neg r
  | Eq (d, e) -> and_eq ~wrt d e r
  | Eq0 e -> and_eq ~wrt Trm.zero e r
  | Distinct _ | Pos _ | Not _ | Or _ | Iff _ | Cond _ | Lit _ -> r

let add us b r =
  [%Trace.call fun {pf} -> pf "@ %a@ | %a" Fml.pp b pp r]
  ;
  add_ us b r |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let dnf f =
  let meet1 a (vs, p, x) =
    let vs, x = add vs a x in
    (vs, Fml.and_ p a, x)
  in
  let top = (Var.Set.empty, Fml.tt, empty) in
  Iter.from_labelled_iter (Fml.iter_dnf ~meet1 ~top f)

let rename x sub =
  [%trace]
    ~call:(fun {pf} -> pf "@ @[%a@]@ %a" Var.Subst.pp sub pp x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      invariant x' )
  @@ fun () ->
  let apply_sub = Trm.map_vars ~f:(Var.Subst.apply sub) in
  let rep = Subst.map_entries ~f:apply_sub x.rep in
  let cls =
    Trm.Map.fold x.cls x.cls ~f:(fun ~key:a0' ~data:a0'_cls cls ->
        let a' = apply_sub a0' in
        let a'_cls = Cls.map ~f:apply_sub a0'_cls in
        Trm.Map.add ~key:a' ~data:a'_cls
          (if a' == a0' then cls else Trm.Map.remove a0' cls) )
  in
  let use =
    Trm.Map.fold x.use x.use ~f:(fun ~key:a0' ~data:a0'_use use ->
        let a' = apply_sub a0' in
        let a'_use = Use.map a0'_use ~f:apply_sub in
        Trm.Map.add ~key:a' ~data:a'_use
          (if a' == a0' then use else Trm.Map.remove a0' use) )
  in
  if rep == x.rep && cls == x.cls && use == x.use then x
  else {x with rep; cls; use}

let apply_and_elim ~wrt xs s r =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a%a@ %a" Var.Set.pp_xs xs Subst.pp s pp_raw r)
    ~retn:(fun {pf} (zs, r', ks) ->
      pf "%a@ %a@ %a" Var.Set.pp_xs zs pp_raw r' Var.Set.pp_xs ks ;
      invariant r' ;
      assert (Var.Set.subset ks ~of_:xs) ;
      assert (Var.Set.disjoint ks (fv r')) )
  @@ fun () ->
  if Subst.is_empty s then (Var.Set.empty, r, Var.Set.empty)
  else
    let zs, r = apply_subst wrt s r in
    if is_unsat r then (Var.Set.empty, unsat, Var.Set.empty)
    else
      let ks = Var.Set.diff xs (fv r) in
      (zs, r, ks)

(* Existential Witnessing and Elimination ================================*)

let subst_invariant us s0 s =
  assert (s0 == s || not (Subst.equal s0 s)) ;
  assert (
    Trm.Map.iteri s ~f:(fun ~key ~data ->
        (* rep not ito us implies trm not ito us *)
        assert (
          Var.Set.subset (Trm.fv data) ~of_:us
          || not (Var.Set.subset (Trm.fv key) ~of_:us) ) ) ;
    true )

(** try to solve [p = q] such that [fv (p - q) ⊆ us ∪ xs] and [p - q] has at
    most one maximal solvable subterm, [kill], where [fv kill ⊈ us]; solve
    [p = q] for [kill]; extend subst mapping [kill] to the solution *)
let solve_poly_eq us p' q' subst =
  [%Trace.call fun {pf} -> pf "@ %a = %a" Trm.pp p' Trm.pp q']
  ;
  let diff = Trm.sub p' q' in
  let max_solvables_not_ito_us =
    Iter.fold (Trm.solvables diff) Zero ~f:(fun solvable_subterm -> function
      | Many -> Many
      | zom when Var.Set.subset (Trm.fv solvable_subterm) ~of_:us -> zom
      | One _ -> Many
      | Zero -> One solvable_subterm )
  in
  ( match max_solvables_not_ito_us with
  | One kill ->
      let+ kill, keep = Trm.Arith.solve_zero_eq diff ~for_:kill in
      Subst.compose1 {var= Trm.arith kill; rep= Trm.arith keep} subst
  | Many | Zero -> None )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst)]

let rec solve_pending (s : Theory.t) soln =
  match s.pending with
  | (a, b) :: pending -> (
      let a' = Subst.norm soln a in
      let b' = Subst.norm soln b in
      match Theory.solve a' b' {s with pending} with
      | {solved= Some solved} as s ->
          solve_pending {s with solved= Some []}
            (List.fold ~f:Subst.compose1 solved soln)
      | {solved= None} -> None )
  | [] -> Some soln

let solve_seq_eq us e' f' subst =
  [%Trace.call fun {pf} -> pf "@ %a = %a" Trm.pp e' Trm.pp f']
  ;
  let x_ito_us x u =
    (not (Var.Set.subset (Trm.fv x) ~of_:us))
    && Var.Set.subset (Trm.fv u) ~of_:us
  in
  let solve_concat c ms a =
    let n =
      match Trm.seq_size a with Some n -> n | None -> Trm.seq_size_exn c
    in
    solve_pending
      (Theory.solve_concat ms a n
         { wrt= Var.Set.empty
         ; no_fresh= true
         ; fresh= Var.Set.empty
         ; solved= Some []
         ; pending= [] } )
      subst
  in
  ( match ((e' : Trm.t), (f' : Trm.t)) with
  | (Concat ms as c), a when x_ito_us c a -> solve_concat c ms a
  | a, (Concat ms as c) when x_ito_us c a -> solve_concat c ms a
  | (Var _ as v), u when x_ito_us v u ->
      Some (Subst.compose1 {var= v; rep= u} subst)
  | u, (Var _ as v) when x_ito_us v u ->
      Some (Subst.compose1 {var= v; rep= u} subst)
  | _ -> None )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst)]

let solve_interp_eq us e' (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "@ trm: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Trm.pp e' Cls.pp cls
      Subst.pp subst]
  ;
  Iter.find_map (Cls.to_iter cls) ~f:(fun f ->
      let f' = Subst.norm subst f in
      match solve_seq_eq us e' f' subst with
      | Some subst -> Some subst
      | None -> solve_poly_eq us e' f' subst )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst) ;
    Option.iter ~f:(subst_invariant us subst) subst']

(** move equations from [cls] to [subst] which are between interpreted terms
    and can be expressed, after normalizing with [subst], as [x ↦ u] where
    [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else [fv u ⊆ us ∪ xs] *)
let rec solve_interp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "@ cls: @[%a@]@ subst: @[%a@]" Cls.pp cls Subst.pp subst]
  ;
  let rec solve_interp_eqs_ cls' (cls, subst) =
    match Cls.pop cls with
    | None -> (cls', subst)
    | Some (trm, cls) ->
        let trm' = Subst.norm subst trm in
        if Trm.is_interpreted trm' then
          match solve_interp_eq us trm' (cls, subst) with
          | Some subst -> solve_interp_eqs_ cls' (cls, subst)
          | None -> solve_interp_eqs_ (Cls.add trm' cls') (cls, subst)
        else solve_interp_eqs_ (Cls.add trm' cls') (cls, subst)
  in
  let cls', subst' = solve_interp_eqs_ Cls.empty (cls, subst) in
  ( if subst' != subst then solve_interp_eqs us (cls', subst')
  else (cls', subst') )
  |>
  [%Trace.retn fun {pf} (cls', subst') ->
    pf "cls: @[%a@]@ subst: @[%a@]" Cls.pp_diff (cls, cls') Subst.pp_diff
      (subst, subst')]

type cls_solve_state =
  { rep_us: Trm.t option  (** rep, that is ito us, for class *)
  ; cls_us: Cls.t  (** cls that is ito us, or interpreted *)
  ; rep_xs: Trm.t option  (** rep, that is *not* ito us, for class *)
  ; cls_xs: Cls.t  (** cls that is *not* ito us *) }

(** move equations from [cls] (which is assumed to be normalized by [subst])
    to [subst] which can be expressed as [x ↦ u] where [x] is noninterpreted
    [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else [fv u ⊆ us ∪ xs] *)
let solve_uninterp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "@ cls: @[%a@]@ subst: @[%a@]" Cls.pp cls Subst.pp subst]
  ;
  let compare e f =
    [%compare: Trm.kind * Trm.t] (Trm.classify e, e) (Trm.classify f, f)
  in
  let {rep_us; cls_us; rep_xs; cls_xs} =
    Cls.fold cls
      {rep_us= None; cls_us= Cls.empty; rep_xs= None; cls_xs= Cls.empty}
      ~f:(fun trm ({rep_us; cls_us; rep_xs; cls_xs} as s) ->
        if Var.Set.subset (Trm.fv trm) ~of_:us then
          match rep_us with
          | Some rep when compare rep trm <= 0 ->
              {s with cls_us= Cls.add trm cls_us}
          | Some rep -> {s with rep_us= Some trm; cls_us= Cls.add rep cls_us}
          | None -> {s with rep_us= Some trm}
        else
          match rep_xs with
          | Some rep ->
              if compare rep trm <= 0 then
                if Trm.non_interpreted trm then
                  {s with cls_xs= Cls.add trm cls_xs}
                else {s with cls_us= Cls.add trm cls_us}
              else if Trm.non_interpreted rep then
                {s with rep_xs= Some trm; cls_xs= Cls.add rep cls_xs}
              else {s with rep_xs= Some trm; cls_us= Cls.add rep cls_us}
          | None -> {s with rep_xs= Some trm} )
  in
  ( match rep_us with
  | Some rep_us ->
      let cls = Cls.add rep_us cls_us in
      let cls, cls_xs =
        match rep_xs with
        | Some rep ->
            if Trm.non_interpreted rep then (cls, Cls.add rep cls_xs)
            else (Cls.add rep cls, cls_xs)
        | None -> (cls, cls_xs)
      in
      let subst =
        Cls.fold cls_xs subst ~f:(fun trm_xs subst ->
            let trm_xs = Subst.canon subst trm_xs in
            let rep_us = Subst.canon subst rep_us in
            Subst.compose1 {var= trm_xs; rep= rep_us} subst )
      in
      (cls, subst)
  | None -> (
    match rep_xs with
    | Some rep_xs ->
        let cls = Cls.add rep_xs cls_us in
        let subst =
          Cls.fold cls_xs subst ~f:(fun trm_xs subst ->
              let trm_xs = Subst.canon subst trm_xs in
              let rep_xs = Subst.canon subst rep_xs in
              Subst.compose1 {var= trm_xs; rep= rep_xs} subst )
        in
        (cls, subst)
    | None -> (cls, subst) ) )
  |>
  [%Trace.retn fun {pf} (cls', subst') ->
    pf "cls: @[%a@]@ subst: @[%a@]" Cls.pp_diff (cls, cls') Subst.pp_diff
      (subst, subst') ;
    subst_invariant us subst subst']

(** move equations between terms in [rep]'s class [cls] from [classes] to
    [subst] which can be expressed, after normalizing with [subst], as
    [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else
    [fv u ⊆ us ∪ xs] *)
let solve_class us us_xs ~key:rep ~data:cls (classes, subst) =
  let classes0 = classes in
  [%Trace.call fun {pf} ->
    pf "@ rep: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Trm.pp rep Cls.pp cls
      Subst.pp subst]
  ;
  let cls, cls_not_ito_us_xs =
    Cls.partition
      ~f:(fun e -> Var.Set.subset (Trm.fv e) ~of_:us_xs)
      (Cls.add rep cls)
  in
  let cls, subst = solve_interp_eqs us (cls, subst) in
  let cls, subst = solve_uninterp_eqs us (cls, subst) in
  let cls = Cls.union cls_not_ito_us_xs cls in
  let cls = Cls.remove (Subst.norm subst rep) cls in
  let classes =
    if Cls.is_empty cls then Trm.Map.remove rep classes
    else Trm.Map.add ~key:rep ~data:cls classes
  in
  (classes, subst)
  |>
  [%Trace.retn fun {pf} (classes', subst') ->
    pf "subst: @[%a@]@ classes: @[%a@]" Subst.pp_diff (subst, subst')
      pp_diff_cls (classes0, classes')]

let solve_concat_extracts_eq r x =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Trm.pp x pp r]
  ;
  (* find terms of form [Extract {_=x}] *)
  let extract_uses =
    Use.filter (use_of x r) ~f:(function Extract _ -> true | _ -> false)
  in
  let find_extracts_at_off off =
    Use.filter extract_uses ~f:(function
      | Extract {off= o} -> implies r (Fml.eq o off)
      | _ -> false )
  in
  let rec find_extracts full_rev_extracts rev_prefix off =
    Use.fold (find_extracts_at_off off) full_rev_extracts
      ~f:(fun e full_rev_extracts ->
        match e with
        | Extract {siz= n; off= o; len= l} ->
            let o_l = Trm.add o l in
            if implies r (Fml.eq n o_l) then
              (e :: rev_prefix) :: full_rev_extracts
            else find_extracts full_rev_extracts (e :: rev_prefix) o_l
        | _ -> full_rev_extracts )
  in
  find_extracts [] [] Trm.zero
  |>
  [%Trace.retn fun {pf} ->
    pf "@[[%a]@]" (List.pp ";@ " (List.pp ",@ " Trm.pp))]

let solve_concat_extracts r us x (classes, subst, us_xs) =
  match
    List.filter_map (solve_concat_extracts_eq r x) ~f:(fun rev_extracts ->
        Iter.fold_opt (Iter.of_list rev_extracts) [] ~f:(fun e suffix ->
            let+ rep_ito_us =
              let e_cls = rep_cls_of (norm r e) r in
              Cls.fold e_cls None ~f:(fun trm rep_ito_us ->
                  match rep_ito_us with
                  | Some rep when Trm.compare rep trm <= 0 -> rep_ito_us
                  | _ when Var.Set.subset (Trm.fv trm) ~of_:us -> Some trm
                  | _ -> rep_ito_us )
            in
            Trm.{seq= rep_ito_us; siz= Trm.seq_size_exn e} :: suffix ) )
    |> Iter.of_list
    |> Iter.min ~lt:(fun xs ys -> [%compare: Trm.sized list] xs ys < 0)
  with
  | Some extracts ->
      let concat = Trm.concat (Array.of_list extracts) in
      let subst = Subst.compose1 {var= x; rep= concat} subst in
      (classes, subst, us_xs)
  | None -> (classes, subst, us_xs)

let solve_for_xs r us xs =
  Var.Set.fold xs ~f:(fun x (classes, subst, us_xs) ->
      let x = Trm.var x in
      if Trm.Map.mem x subst then (classes, subst, us_xs)
      else solve_concat_extracts r us x (classes, subst, us_xs) )

(** move equations from [classes] to [subst] which can be expressed, after
    normalizing with [subst], as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us] and
    [fv u ⊆ us] or else [fv u ⊆ us ∪ xs]. *)
let solve_classes r xs (classes, subst, us) =
  [%Trace.call fun {pf} ->
    pf "@ us: {@[%a@]}@ xs: {@[%a@]}" Var.Set.pp us Var.Set.pp xs]
  ;
  let rec solve_classes_ (classes0, subst0, us_xs) =
    let classes, subst =
      Trm.Map.fold ~f:(solve_class us us_xs) classes0 (classes0, subst0)
    in
    if subst != subst0 then solve_classes_ (classes, subst, us_xs)
    else (classes, subst, us_xs)
  in
  (classes, subst, Var.Set.union us xs)
  |> solve_classes_
  |> solve_for_xs r us xs
  |>
  [%Trace.retn fun {pf} (classes', subst', _) ->
    pf "subst: @[%a@]@ classes: @[%a@]" Subst.pp_diff (subst, subst')
      pp_diff_cls (classes, classes')]

let pp_vss fs vss =
  Format.fprintf fs "[@[%a@]]"
    (List.pp ";@ " (fun fs vs -> Format.fprintf fs "{@[%a@]}" Var.Set.pp vs))
    vss

(** enumerate variable contexts vᵢ in [v₁;…] and accumulate a solution subst
    with entries [x ↦ u] where [r] entails [x = u] and
    [⋃ⱼ₌₁ⁱ vⱼ ⊇ fv x ⊈ ⋃ⱼ₌₁ⁱ⁻¹ vⱼ] and [fv u ⊆ ⋃ⱼ₌₁ⁱ⁻¹ vⱼ] if possible and
    otherwise [fv u ⊆ ⋃ⱼ₌₁ⁱ vⱼ] *)
let solve_for_vars vss r =
  [%Trace.call fun {pf} ->
    pf "@ %a@ @[%a@]" pp_vss vss pp r ;
    invariant r]
  ;
  let us, vss =
    match vss with us :: vss -> (us, vss) | [] -> (Var.Set.empty, vss)
  in
  List.fold ~f:(solve_classes r) vss (r.cls, Subst.empty, us) |> snd3
  |>
  [%Trace.retn fun {pf} subst ->
    pf "%a" Subst.pp subst ;
    Trm.Map.iteri subst ~f:(fun ~key ~data ->
        assert (
          implies r (Fml.eq key data)
          || fail "@[%a@ = %a@ not entailed by@ @[%a@]@]" Trm.pp key Trm.pp
               data pp_classes r () ) ;
        assert (
          Iter.fold_until (Iter.of_list vss) us
            ~f:(fun xs us ->
              let us_xs = Var.Set.union us xs in
              let ks = Trm.fv key in
              let ds = Trm.fv data in
              if
                Var.Set.subset ks ~of_:us_xs
                && Var.Set.subset ds ~of_:us_xs
                && ( Var.Set.subset ds ~of_:us
                   || not (Var.Set.subset ks ~of_:us) )
              then `Stop true
              else `Continue us_xs )
            ~finish:(fun _ -> false) ) )]

(* Replay debugging ======================================================*)

type call =
  | Add of Var.Set.t * Formula.t * t
  | Union of Var.Set.t * t * t
  | InterN of Var.Set.t * (Var.Set.t * t) list
  | Dnf of Formula.t
  | Rename of t * Var.Subst.t
  | Is_unsat of t
  | Implies of t * Formula.t
  | Refutes of t * Formula.t
  | Normalize of t * Term.t
  | Apply_subst of Var.Set.t * Subst.t * t
  | Solve_for_vars of Var.Set.t list * t
  | Apply_and_elim of Var.Set.t * Var.Set.t * Subst.t * t
[@@deriving sexp]

let replay c =
  match call_of_sexp (Sexp.of_string c) with
  | Add (us, e, r) -> add us e r |> ignore
  | Union (us, r, s) -> union us r s |> ignore
  | InterN (us, xrs) -> interN us xrs |> ignore
  | Dnf f -> dnf f |> Iter.iter ~f:ignore
  | Rename (r, s) -> rename r s |> ignore
  | Is_unsat r -> is_unsat r |> ignore
  | Implies (r, f) -> implies r f |> ignore
  | Refutes (r, f) -> refutes r f |> ignore
  | Normalize (r, e) -> normalize r e |> ignore
  | Apply_subst (us, s, r) -> apply_subst us s r |> ignore
  | Solve_for_vars (vss, r) -> solve_for_vars vss r |> ignore
  | Apply_and_elim (wrt, xs, s, r) -> apply_and_elim ~wrt xs s r |> ignore

(* Debug wrappers *)

let report ~name ~elapsed ~aggregate ~count =
  Format.eprintf "%15s time: %12.3f ms  %12.3f ms  %12d calls@." name
    elapsed aggregate count

let dump_threshold = ref 1000.

let[@warning "-32"] wrap tmr f call =
  let f () =
    Timer.start tmr ;
    let r = f () in
    Timer.stop_report tmr (fun ~name ~elapsed ~aggregate ~count ->
        report ~name ~elapsed ~aggregate ~count ;
        if Float.(elapsed > !dump_threshold) then (
          dump_threshold := 2. *. !dump_threshold ;
          Format.eprintf "@\n%a@\n@." Sexp.pp_hum (sexp_of_call (call ())) ) ) ;
    r
  in
  if not [%debug] then f ()
  else
    try f ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let exn = Replay (exn, sexp_of_call (call ())) in
      Printexc.raise_with_backtrace exn bt

let wrap _ f _ = f ()
let add_tmr = Timer.create "add" ~at_exit:report
let union_tmr = Timer.create "union" ~at_exit:report
let interN_tmr = Timer.create "interN" ~at_exit:report
let dnf_tmr = Timer.create "dnf" ~at_exit:report
let rename_tmr = Timer.create "rename" ~at_exit:report
let is_unsat_tmr = Timer.create "is_unsat" ~at_exit:report
let implies_tmr = Timer.create "implies" ~at_exit:report
let refutes_tmr = Timer.create "refutes" ~at_exit:report
let normalize_tmr = Timer.create "normalize" ~at_exit:report
let apply_subst_tmr = Timer.create "apply_subst" ~at_exit:report
let solve_for_vars_tmr = Timer.create "solve_for_vars" ~at_exit:report
let apply_and_elim_tmr = Timer.create "apply_and_elim" ~at_exit:report

let add us e r =
  wrap add_tmr (fun () -> add us e r) (fun () -> Add (us, e, r))

let union us r s =
  wrap union_tmr (fun () -> union us r s) (fun () -> Union (us, r, s))

let interN us rs =
  wrap interN_tmr (fun () -> interN us rs) (fun () -> InterN (us, rs))

let dnf f = wrap dnf_tmr (fun () -> dnf f) (fun () -> Dnf f)

let rename r s =
  wrap rename_tmr (fun () -> rename r s) (fun () -> Rename (r, s))

let is_unsat r =
  wrap is_unsat_tmr (fun () -> is_unsat r) (fun () -> Is_unsat r)

let implies r f =
  wrap implies_tmr (fun () -> implies r f) (fun () -> Implies (r, f))

let refutes r f =
  wrap refutes_tmr (fun () -> refutes r f) (fun () -> Refutes (r, f))

let normalize r e =
  wrap normalize_tmr (fun () -> normalize r e) (fun () -> Normalize (r, e))

let apply_subst us s r =
  wrap apply_subst_tmr
    (fun () -> apply_subst us s r)
    (fun () -> Apply_subst (us, s, r))

let solve_for_vars vss r =
  wrap solve_for_vars_tmr
    (fun () -> solve_for_vars vss r)
    (fun () -> Solve_for_vars (vss, r))

let apply_and_elim ~wrt xs s r =
  wrap apply_and_elim_tmr
    (fun () -> apply_and_elim ~wrt xs s r)
    (fun () -> Apply_and_elim (wrt, xs, s, r))
