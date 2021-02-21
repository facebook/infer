(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Equality over uninterpreted functions and linear rational arithmetic *)

open Exp

(* Solution Substitutions =================================================*)

module Subst : sig
  type t = Trm.t Trm.Map.t [@@deriving compare, equal, sexp]

  val pp : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val is_empty : t -> bool
  val length : t -> int
  val mem : Trm.t -> t -> bool
  val find : Trm.t -> t -> Trm.t option
  val apply : t -> Trm.t -> Trm.t
  val norm : t -> Trm.t -> Trm.t
  val apply_rec : t -> Trm.t -> Trm.t
  val subst : t -> Term.t -> Term.t
  val fold : t -> 's -> f:(key:Trm.t -> data:Trm.t -> 's -> 's) -> 's
  val fold_eqs : t -> 's -> f:(Fml.t -> 's -> 's) -> 's
  val iteri : t -> f:(key:Trm.t -> data:Trm.t -> unit) -> unit
  val for_alli : t -> f:(key:Trm.t -> data:Trm.t -> bool) -> bool
  val to_iter : t -> (Trm.t * Trm.t) iter
  val to_list : t -> (Trm.t * Trm.t) list
  val compose1 : key:Trm.t -> data:Trm.t -> t -> t
  val compose : t -> t -> t
  val map_entries : f:(Trm.t -> Trm.t) -> t -> t
  val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t

  (* direct representation manipulation *)
  val add : key:Trm.t -> data:Trm.t -> t -> t
  val remove : Trm.t -> t -> t
end = struct
  type t = Trm.t Trm.Map.t [@@deriving compare, equal, sexp_of]

  let t_of_sexp = Trm.Map.t_of_sexp Trm.t_of_sexp
  let pp = Trm.Map.pp Trm.pp Trm.pp
  let pp_diff = Trm.Map.pp_diff ~eq:Trm.equal Trm.pp Trm.pp Trm.pp_diff
  let empty = Trm.Map.empty
  let is_empty = Trm.Map.is_empty
  let length = Trm.Map.length
  let mem = Trm.Map.mem
  let fold = Trm.Map.fold

  let fold_eqs s z ~f =
    Trm.Map.fold ~f:(fun ~key ~data -> f (Fml.eq key data)) s z

  let iteri = Trm.Map.iteri
  let for_alli = Trm.Map.for_alli
  let to_iter = Trm.Map.to_iter
  let to_list = Trm.Map.to_list

  (** look up a term in a substitution *)
  let find = Trm.Map.find

  (** apply a substitution, considered as the identity function overridden
      for finitely-many terms *)
  let apply s a = Trm.Map.find a s |> Option.value ~default:a

  (** apply a substitution to maximal noninterpreted subterms *)
  let norm s a =
    [%trace]
      ~call:(fun {pf} -> pf "@ %a" Trm.pp a)
      ~retn:(fun {pf} -> pf "%a" Trm.pp)
    @@ fun () -> Theory.map_solvables ~f:(apply s) a

  (** apply a substitution recursively, bottom-up *)
  let rec apply_rec s a = apply s (Trm.map ~f:(apply_rec s) a)

  (** lift apply from trm to term *)
  let subst s e = Term.map_trms ~f:(apply_rec s) e

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
  let compose1 ~key ~data r =
    if Trm.equal key data then r
    else (
      assert (
        Option.for_all ~f:(Trm.equal key) (Trm.Map.find key r)
        || fail "domains intersect: %a ↦ %a in %a" Trm.pp key Trm.pp data
             pp r () ) ;
      let s = Trm.Map.singleton key data in
      let r' = Trm.Map.map_endo ~f:(norm s) r in
      Trm.Map.add ~key ~data r' )

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
      || Theory.is_noninterpreted e
         && Iter.exists ~f:(is_var_in xs) (Theory.solvable_trms e)
    in
    ( noninterp_with_solvable_var_in xs e
    || noninterp_with_solvable_var_in xs f )
    $> fun b ->
    [%Trace.info
      "is_valid_eq %a%a=%a = %b" Var.Set.pp_xs xs Trm.pp e Trm.pp f b]

  (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks
      and ν are maximal where ∃ks. ν is universally valid, xs ⊇ ks and
      ks ∩ fv(τ) = ∅. *)
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

  (* direct representation manipulation *)

  let add = Trm.Map.add
  let remove = Trm.Map.remove
end

(* Equality classes =======================================================*)

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
  val pop : t -> (Trm.t * t) option
  val fold : t -> 's -> f:(Trm.t -> 's -> 's) -> 's
  val filter : t -> f:(Trm.t -> bool) -> t
  val partition : t -> f:(Trm.t -> bool) -> t * t
  val map : t -> f:(Trm.t -> Trm.t) -> t
  val to_iter : t -> Trm.t iter
  val to_set : t -> Trm.Set.t
  val of_set : Trm.Set.t -> t
end = struct
  include Trm.Set

  let to_set s = s
  let of_set s = s
  let ppx x fs es = pp_full ~sep:"@ = " (Trm.ppx x) fs es
  let pp = ppx (fun _ -> None)
  let pp_raw fs es = pp_full ~pre:"{@[" ~suf:"@]}" ~sep:",@ " Trm.pp fs es
end

(* Use lists / Super-expressions ==========================================*)

module Use : sig
  type t [@@deriving compare, equal, sexp]

  val pp : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val add : Trm.t -> t -> t
  val remove : Trm.t -> t -> t
  val is_empty : t -> bool
  val mem : Trm.t -> t -> bool
  val iter : t -> f:(Trm.t -> unit) -> unit
  val fold : t -> 's -> f:(Trm.t -> 's -> 's) -> 's
  val map : t -> f:(Trm.t -> Trm.t) -> t
  val flat_map : t -> f:(Trm.t -> t) -> t
  val filter : t -> f:(Trm.t -> bool) -> t
end =
  Trm.Set

(* Conjunctions of atomic formula assumptions =============================*)

(** see also [invariant] *)
type t =
  { xs: Var.Set.t
        (** existential variables that did not appear in input formulas *)
  ; sat: bool  (** [false] only if constraints are inconsistent *)
  ; rep: Subst.t
        (** functional set of oriented equations: map [a] to [a'],
            indicating that [a = a'] holds, and that [a'] is the
            'rep(resentative)' of [a] *)
  ; cls: Cls.t Trm.Map.t
        (** map each representative to the set of terms in its class *)
  ; use: Use.t Trm.Map.t
        (** map each solvable in the carrier to its immediate super-terms *)
  ; pnd: (Trm.t * Trm.t) list
        (** pending equations to add (once invariants are reestablished) *)
  }
[@@deriving compare, equal, sexp]

(* Pretty-printing ========================================================*)

let pp_eq fs (e, f) = Format.fprintf fs "@[%a = %a@]" Trm.pp e Trm.pp f

let pp_raw fs {sat; rep; cls; use; pnd} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v (k, v)
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
    (Subst.to_list rep)
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

(* Basic representation queries ===========================================*)

let trms r =
  Iter.flat_map ~f:(fun (k, v) -> Iter.doubleton k v) (Subst.to_iter r.rep)

let vars r = Iter.flat_map ~f:Trm.vars (trms r)
let fv r = Var.Set.of_iter (vars r)

(** test if a term is in the carrier *)
let in_car a x = Subst.mem a x.rep

(** test if a term is a representative *)
let is_rep a x =
  match Subst.find a x.rep with Some a' -> Trm.equal a a' | None -> false

let cls_of a x = Trm.Map.find a x.cls |> Option.value ~default:Cls.empty
let use_of a x = Trm.Map.find a x.use |> Option.value ~default:Use.empty

(* Invariant ==============================================================*)

(** terms are congruent if equal after normalizing subterms *)
let congruent r a b =
  Trm.equal
    (Trm.map ~f:(Subst.norm r.rep) a)
    (Trm.map ~f:(Subst.norm r.rep) b)

let find_check a x =
  if not (Theory.is_noninterpreted a) then a
  else
    match Trm.Map.find a x.rep with
    | Some a' -> a'
    | None -> fail "%a not in carrier" Trm.pp a ()

let pre_invariant x =
  let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
  try
    Subst.iteri x.rep ~f:(fun ~key:a ~data:a' ->
        (* only noninterpreted terms in dom of rep, except reps of
           nontrivial classes *)
        assert (
          Theory.is_noninterpreted a
          || (is_rep a x && not (Cls.is_empty (cls_of a' x)))
          || fail "interp in rep dom %a" Trm.pp a () ) ;
        (* carrier is closed under solvable subterms *)
        Iter.iter (Theory.solvable_trms a) ~f:(fun b ->
            assert (
              in_car b x
              || fail "@[subterm %a@ of %a@ not in carrier@]" Trm.pp b
                   Trm.pp a () ) ) ;
        (* rep is idempotent *)
        assert (
          let a'' = Subst.norm x.rep a' in
          Trm.equal a' a''
          || fail "not idempotent:@ @[%a@ <> %a@]@ %a" Trm.pp a' Trm.pp a''
               Subst.pp x.rep () ) ;
        (* every term is in class of its rep *)
        assert (
          let a'_cls = cls_of a' x in
          Trm.equal a a'
          || Trm.Set.mem a (Cls.to_set a'_cls)
          || fail "%a not in cls of %a = {%a}" Trm.pp a Trm.pp a' Cls.pp
               a'_cls () ) ;
        (* each term in carrier is in use list of each of its solvable
           subterms *)
        Iter.iter (Theory.solvable_trms a) ~f:(fun b ->
            assert (
              let b_use = use_of b x in
              Use.mem a b_use
              || fail "@[subterm %a@ of %a@ not in use %a@]" Trm.pp b Trm.pp
                   a Use.pp b_use () ) ) ) ;
    Trm.Map.iteri x.cls ~f:(fun ~key:a' ~data:cls ->
        (* each class does not include its rep *)
        assert (
          (not (Trm.Set.mem a' (Cls.to_set cls)))
          || fail "rep %a in cls %a" Trm.pp a' Cls.pp cls () ) ;
        (* rep of every element in class of [a'] is [a'] *)
        Iter.iter (Cls.to_iter cls) ~f:(fun e ->
            assert (
              let e' = find_check e x in
              Trm.equal e' a'
              || fail "rep of %a = %a but should = %a, cls: %a" Trm.pp e
                   Trm.pp e' Trm.pp a' Cls.pp cls () ) ) ) ;
    Trm.Map.iteri x.use ~f:(fun ~key:a ~data:use ->
        (* dom of use are solvable terms *)
        assert (Theory.is_noninterpreted a) ;
        (* terms occur in each of their uses *)
        Use.iter use ~f:(fun u ->
            assert (
              Iter.mem ~eq:Trm.equal a (Theory.solvable_trms u)
              || fail "%a does not occur in its use %a" Trm.pp a Trm.pp u ()
            ) ) )
  with exc ->
    let bt = Printexc.get_raw_backtrace () in
    [%Trace.info "%a" pp_raw x] ;
    Printexc.raise_with_backtrace exc bt

let invariant x =
  let@ () = Invariant.invariant [%here] x [%sexp_of: t] in
  assert (List.is_empty x.pnd) ;
  pre_invariant x ;
  assert (
    (not x.sat)
    || Subst.for_alli x.rep ~f:(fun ~key:a ~data:a' ->
           Subst.for_alli x.rep ~f:(fun ~key:b ~data:b' ->
               Trm.compare a b >= 0
               || (not (congruent x a b))
               || Trm.equal a' b'
               || fail "not congruent %a@ %a@ in@ %a" Trm.pp a Trm.pp b pp x
                    () ) ) )

(* Representation queries =================================================*)

(** [norm0 s a = norm0 s b] if [a] and [b] are equal in [s], that is,
    congruent using 0 applications of the congruence rule. *)
let norm0 s a = Subst.apply s a

(** [norm1 s a = norm1 s b] if [a] and [b] are congruent in [s] using 0 or 1
    application of the congruence rule. *)
let norm1 s a =
  match Theory.classify a with
  | InterpAtom -> a
  | NonInterpAtom -> norm0 s a
  | InterpApp | UninterpApp -> (
    match Trm.Map.find a s with
    | Some a' -> a'
    | None ->
        let a_norm = Trm.map ~f:(Theory.map_solvables ~f:(norm0 s)) a in
        if a_norm == a then a_norm else norm0 s a_norm )

(** rewrite a term into canonical form using rep recursively *)
let rec canon x a =
  match Theory.classify a with
  | InterpAtom -> a
  | NonInterpAtom -> norm0 x.rep a
  | InterpApp | UninterpApp -> (
    match Trm.Map.find a x.rep with
    | Some a' -> a'
    | None ->
        let a_can = Trm.map ~f:(Theory.map_solvables ~f:(canon x)) a in
        if a_can == a then a_can else norm0 x.rep a_can )

(* Extending the carrier ==================================================*)

let add_use_of sup sub use =
  Trm.Map.update sub use ~f:(fun u ->
      Some (Use.add sup (Option.value ~default:Use.empty u)) )

let add_uses_of a use =
  Iter.fold ~f:(add_use_of a) (Theory.solvable_trms a) use

(** [canon_extend_ a x] is [a', x'] where [x'] is [x] extended so that
    [a' = canon x' a]. This finds [a] in rep or recurses and then canonizes
    if absent, adding the canonized term [a'] and its subterms. *)
let rec canon_extend_ a x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ | %a" Trm.pp a pp_raw x)
    ~retn:(fun {pf} (a', x') ->
      pf "%a@ %a" Trm.pp a' pp_diff (x, x') ;
      assert (Trm.equal a' (canon x' a)) )
  @@ fun () ->
  match Theory.classify a with
  | InterpAtom -> (a, x)
  | NonInterpAtom -> (
    match Trm.Map.find_or_add a a x.rep with
    | Some a', _ -> (a', x)
    | None, rep -> (a, {x with rep}) )
  | InterpApp ->
      if Trm.Map.mem a x.rep then
        (* optimize: a already a rep so don't need to consider subterms *)
        (a, x)
      else Trm.fold_map ~f:canon_extend_ a x
  | UninterpApp -> (
    match Trm.Map.find a x.rep with
    | Some a' ->
        (* a already has rep a' *)
        (a', x)
    | None -> (
        (* norm wrt rep and add subterms *)
        let a_norm, x = Trm.fold_map ~f:canon_extend_ a x in
        match Trm.Map.find_or_add a_norm a_norm x.rep with
        | Some a', _ ->
            (* a_norm already equal to a' *)
            (a', x)
        | None, rep ->
            (* a_norm newly added *)
            let use = add_uses_of a_norm x.use in
            let x = {x with rep; use} in
            (a_norm, x) ) )

(** normalize and add a term to the carrier *)
let canon_extend a x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ | %a" Trm.pp a pp_raw x)
    ~retn:(fun {pf} (a', x') -> pf "%a@ %a" Trm.pp a' pp_diff (x, x'))
  @@ fun () -> canon_extend_ a x

(* Propagation ============================================================*)

let move_cls_to_rep a_cls a' rep =
  Cls.fold a_cls rep ~f:(fun e rep -> Trm.Map.add ~key:e ~data:a' rep)

let find_and_move_cls noninterp ~of_:u ~to_:u' cls =
  let u_cls, cls = Trm.Map.find_and_remove u cls in
  let u_cls = Option.value u_cls ~default:Cls.empty in
  let u_cls = if noninterp then Cls.add u u_cls else u_cls in
  let cls =
    Trm.Map.update u' cls ~f:(fun u'_cls ->
        Some (Cls.union u_cls (Option.value u'_cls ~default:Cls.empty)) )
  in
  (u_cls, cls)

let move_uses ~rem:f ~add:t use =
  let f_trms = Theory.solvable_trms f |> Trm.Set.of_iter in
  let t_trms = Theory.solvable_trms t |> Trm.Set.of_iter in
  let f_trms, ft_trms, t_trms = Trm.Set.diff_inter_diff f_trms t_trms in
  (* remove f from use of each of its subterms not shared with t *)
  let use =
    Trm.Set.fold f_trms use ~f:(fun e use ->
        Trm.Map.update e use ~f:(function
          | Some e_use ->
              let e_use' = Use.remove f e_use in
              if Use.is_empty e_use' then None else Some e_use'
          | None -> assert false ) )
  in
  (* move each subterm of both f and t from a use of f to a use of t *)
  let use =
    Trm.Set.fold ft_trms use ~f:(fun e use ->
        Trm.Map.update e use ~f:(function
          | Some e_use -> Some (Use.add t (Use.remove f e_use))
          | None -> assert false ) )
  in
  (* add t to use of each of its subterms not shared with f *)
  let use =
    Trm.Set.fold t_trms use ~f:(fun e use ->
        Trm.Map.update e use ~f:(fun e_use ->
            Some (Use.add t (Option.value e_use ~default:Use.empty)) ) )
  in
  use

let update_rep noninterp ~from:r ~to_:r' x =
  [%trace]
    ~call:(fun {pf} -> pf "@ @[%a ↦ %a@]@ %a" Trm.pp r Trm.pp r' pp_raw x)
    ~retn:(fun {pf} x' -> pf "%a" pp_diff (x, x'))
  @@ fun () ->
  let r_cls, cls = find_and_move_cls noninterp ~of_:r ~to_:r' x.cls in
  let rep = move_cls_to_rep r_cls r' x.rep in
  let use =
    if Trm.Map.mem r rep then add_uses_of r' x.use
    else move_uses ~rem:r ~add:r' x.use
  in
  {x with rep; cls; use}

(** add v ↦ t to x *)
let propagate1 {Theory.var= v; rep= t} x =
  [%trace]
    ~call:(fun {pf} ->
      pf "@ @[%a ↦ %a@]@ %a" Trm.pp v Trm.pp t pp_raw x ;
      (* v should be a solvable term that is a representative or absent *)
      assert (Theory.is_noninterpreted v) ;
      assert (
        match Trm.Map.find v x.rep with
        | Some v' -> Trm.equal v v'
        | None -> true ) ;
      (* while t may be an interpreted term and may not be in the carrier,
         it should already be normalized *)
      assert (Trm.equal t (norm1 x.rep t)) )
    ~retn:(fun {pf} -> pf "%a" pp_raw)
  @@ fun () ->
  let s = Trm.Map.singleton v t in
  let x = update_rep true ~from:v ~to_:t x in
  Use.fold (use_of v x) x ~f:(fun u x ->
      let w = norm1 s u in
      let x = {x with pnd= (u, w) :: x.pnd} in
      if Theory.is_noninterpreted u then
        if in_car w x then x else {x with use= add_uses_of w x.use}
      else update_rep false ~from:u ~to_:w x )

let solve ~wrt ~xs d e pending =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Trm.pp d Trm.pp e)
    ~retn:(fun {pf} -> pf "%a" Theory.pp)
  @@ fun () ->
  Theory.solve d e
    {wrt; no_fresh= false; fresh= xs; solved= Some []; pending}

let rec propagate ~wrt x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a" pp_raw x)
    ~retn:(fun {pf} x' -> pf "%a" pp_diff (x, x'))
  @@ fun () ->
  match x.pnd with
  | (a, b) :: pnd -> (
      let a' = Subst.norm x.rep a in
      let b' = Subst.norm x.rep b in
      if Trm.equal a' b' then propagate ~wrt {x with pnd}
      else
        match solve ~wrt ~xs:x.xs a' b' pnd with
        | {solved= Some solved; wrt; fresh; pending} ->
            let xs = Var.Set.union x.xs fresh in
            let x = {x with xs; pnd= pending} in
            propagate ~wrt (List.fold ~f:propagate1 solved x)
        | {solved= None} -> {x with sat= false; pnd= []} )
  | [] -> x

(* Core operations ========================================================*)

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
    let a', x = canon_extend a x in
    let b', x = canon_extend b x in
    if Trm.equal a' b' then x else merge ~wrt a' b' x

let extract_xs r = (r.xs, {r with xs= Var.Set.empty})

(* Exposed interface ======================================================*)

let is_empty {sat; rep} =
  sat && Subst.for_alli rep ~f:(fun ~key:a ~data:a' -> Trm.equal a a')

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

(* class including the representative of a term not assumed to be a rep *)
let rep_cls_of r e =
  let e' = Subst.apply r.rep e in
  Cls.add e' (cls_of e' r)

let class_of r e =
  match Term.get_trm (normalize r e) with
  | Some e' ->
      Iter.to_list (Iter.map ~f:Term.of_trm (Cls.to_iter (rep_cls_of r e')))
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
        let rep' = Subst.apply_rec s rep in
        Cls.fold cls r ~f:(fun trm r ->
            let trm' = Subst.apply_rec s trm in
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
      if Subst.length s.rep <= Subst.length r.rep then (s, r) else (r, s)
    in
    Subst.fold s.rep r ~f:(fun ~key:e ~data:e' r -> and_eq ~wrt e e' r) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let inter wrt r s =
  [%Trace.call fun {pf} -> pf "@ @[<hv 1>   %a@ @<2>∨ %a@]" pp r pp s]
  ;
  ( if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems rs r s =
      Trm.Map.fold s.cls rs ~f:(fun ~key:rep ~data:cls rs ->
          Cls.fold cls
            ([rep], rs)
            ~f:(fun exp (reps, rs) ->
              match
                List.find ~f:(fun rep -> implies r (Fml.eq exp rep)) reps
              with
              | Some rep -> (reps, and_eq ~wrt exp rep rs)
              | None -> (exp :: reps, rs) )
          |> snd )
    in
    let rs = empty in
    let rs = merge_mems rs r s in
    let rs = merge_mems rs s r in
    rs )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let interN us rs =
  match rs with
  | [] -> (us, unsat)
  | r :: rs -> List.fold ~f:(fun r (us, s) -> inter us s r) rs (us, r)

let rec add_ wrt b r =
  match (b : Fml.t) with
  | Tt -> r
  | Not Tt -> unsat
  | And {pos; neg} -> Fml.fold_pos_neg ~f:(add_ wrt) ~pos ~neg r
  | Eq (d, e) -> and_eq ~wrt d e r
  | Eq0 e -> and_eq ~wrt Trm.zero e r
  | Pos _ | Not _ | Or _ | Iff _ | Cond _ | Lit _ -> r

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

let trivial vs r =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Var.Set.pp_xs vs pp_raw r)
    ~retn:(fun {pf} ks -> pf "%a" Var.Set.pp_xs ks)
  @@ fun () ->
  Var.Set.fold vs Var.Set.empty ~f:(fun v ks ->
      let x = Trm.var v in
      match Subst.find x r.rep with
      | None -> Var.Set.add v ks
      | Some x' when Trm.equal x x' && Use.is_empty (use_of x r) ->
          Var.Set.add v ks
      | _ -> ks )

let trim ks x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Var.Set.pp_xs ks pp_raw x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_raw x' ;
      invariant x' ;
      assert (Var.Set.disjoint ks (fv x')) )
  @@ fun () ->
  (* expand classes to include reps *)
  let reps =
    Subst.fold x.rep Trm.Set.empty ~f:(fun ~key:_ ~data:rep reps ->
        Trm.Set.add rep reps )
  in
  let clss =
    Trm.Set.fold reps x.cls ~f:(fun rep clss ->
        Trm.Map.update rep clss ~f:(fun cls0 ->
            Some (Cls.add rep (Option.value cls0 ~default:Cls.empty)) ) )
  in
  (* enumerate expanded classes and update solution subst *)
  Trm.Map.fold clss x ~f:(fun ~key:a' ~data:ecls x ->
      (* remove mappings for non-rep class elements to kill *)
      let keep, drop =
        Trm.Set.diff_inter (Cls.to_set ecls) (ks : Var.Set.t :> Trm.Set.t)
      in
      if Trm.Set.is_empty drop then x
      else
        let rep = Trm.Set.fold ~f:Subst.remove drop x.rep in
        let x = {x with rep} in
        (* new class is keepers without rep *)
        let keep' = Trm.Set.remove a' keep in
        let ecls = Cls.of_set keep' in
        if keep' != keep then
          (* a' is to be kept: continue to use it as rep *)
          let cls =
            if Cls.is_empty ecls then Trm.Map.remove a' x.cls
            else Trm.Map.add ~key:a' ~data:ecls x.cls
          in
          {x with cls}
        else
          (* a' is to be removed: choose new rep from the keepers *)
          let cls = Trm.Map.remove a' x.cls in
          let x = {x with cls} in
          match
            Trm.Set.reduce keep ~f:(fun x y ->
                if Theory.prefer x y < 0 then x else y )
          with
          | Some b' ->
              (* add mappings from each keeper to the new representative *)
              let rep =
                Trm.Set.fold keep x.rep ~f:(fun elt rep ->
                    Subst.add ~key:elt ~data:b' rep )
              in
              (* add trimmed class to new rep *)
              let cls =
                if Cls.is_empty ecls then x.cls
                else Trm.Map.add ~key:b' ~data:ecls x.cls
              in
              {x with rep; cls}
          | None ->
              (* entire class removed *)
              x )

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
      let ks = trivial xs r in
      let r = trim ks r in
      (zs, r, ks)

(* Existential Witnessing and Elimination =================================*)

let subst_invariant us s0 s =
  assert (s0 == s || not (Subst.equal s0 s)) ;
  assert (
    Subst.iteri s ~f:(fun ~key ~data ->
        (* dom of new entries not ito us *)
        assert (
          Option.for_all ~f:(Trm.equal data) (Subst.find key s0)
          || not (Var.Set.subset (Trm.fv key) ~of_:us) ) ;
        (* rep not ito us implies trm not ito us *)
        assert (
          Var.Set.subset (Trm.fv data) ~of_:us
          || not (Var.Set.subset (Trm.fv key) ~of_:us) ) ) ;
    true )

(** try to solve [p = q] such that [fv (p - q) ⊆ us ∪ xs] and [p - q]
    has at most one maximal solvable subterm, [kill], where
    [fv kill ⊈ us]; solve [p = q] for [kill]; extend subst mapping [kill]
    to the solution *)
let solve_poly_eq us p' q' subst =
  [%Trace.call fun {pf} -> pf "@ %a = %a" Trm.pp p' Trm.pp q']
  ;
  let diff = Trm.sub p' q' in
  let max_solvables_not_ito_us =
    Iter.fold (Theory.solvables diff) Zero ~f:(fun solvable_subterm ->
      function
      | Many -> Many
      | zom when Var.Set.subset (Trm.fv solvable_subterm) ~of_:us -> zom
      | One _ -> Many
      | Zero -> One solvable_subterm )
  in
  ( match max_solvables_not_ito_us with
  | One kill ->
      let+ kill, keep = Trm.Arith.solve_zero_eq diff ~for_:kill in
      Subst.compose1 ~key:(Trm.arith kill) ~data:(Trm.arith keep) subst
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
            (List.fold solved soln ~f:(fun {var; rep} soln ->
                 Subst.compose1 ~key:var ~data:rep soln ))
      | {solved= None} -> None )
  | [] -> Some soln

let solve_seq_eq us e' f' subst =
  [%Trace.call fun {pf} -> pf "@ %a = %a" Trm.pp e' Trm.pp f']
  ;
  let x_ito_us x u =
    (not (Var.Set.subset (Trm.fv x) ~of_:us))
    && Var.Set.subset (Trm.fv u) ~of_:us
  in
  let solve_concat ms n a =
    let a, n =
      match Trm.seq_size a with
      | Some n -> (a, n)
      | None -> (Trm.sized ~siz:n ~seq:a, n)
    in
    solve_pending
      (Theory.solve_concat ms a n
         { wrt= Var.Set.empty
         ; no_fresh= true
         ; fresh= Var.Set.empty
         ; solved= Some []
         ; pending= [] })
      subst
  in
  ( match ((e' : Trm.t), (f' : Trm.t)) with
  | (Concat ms as c), a when x_ito_us c a ->
      solve_concat ms (Trm.seq_size_exn c) a
  | a, (Concat ms as c) when x_ito_us c a ->
      solve_concat ms (Trm.seq_size_exn c) a
  | (Sized {seq= Var _ as v} as m), u when x_ito_us m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
  | u, (Sized {seq= Var _ as v} as m) when x_ito_us m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
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
    [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else
    [fv u ⊆ us ∪ xs] *)
let rec solve_interp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "@ cls: @[%a@]@ subst: @[%a@]" Cls.pp cls Subst.pp subst]
  ;
  let rec solve_interp_eqs_ cls' (cls, subst) =
    match Cls.pop cls with
    | None -> (cls', subst)
    | Some (trm, cls) ->
        let trm' = Subst.norm subst trm in
        if Theory.is_interpreted trm' then
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

let dom_trm e =
  match (e : Trm.t) with
  | Sized {seq= Var _ as v} -> Some v
  | _ when Theory.is_noninterpreted e -> Some e
  | _ -> None

(** move equations from [cls] (which is assumed to be normalized by [subst])
    to [subst] which can be expressed as [x ↦ u] where [x] is
    noninterpreted [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else
    [fv u ⊆ us ∪ xs] *)
let solve_uninterp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "@ cls: @[%a@]@ subst: @[%a@]" Cls.pp cls Subst.pp subst]
  ;
  let compare e f =
    [%compare: Theory.kind * Trm.t]
      (Theory.classify e, e)
      (Theory.classify f, f)
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
          | Some rep -> (
              if compare rep trm <= 0 then
                match dom_trm trm with
                | Some trm -> {s with cls_xs= Cls.add trm cls_xs}
                | None -> {s with cls_us= Cls.add trm cls_us}
              else
                match dom_trm rep with
                | Some rep ->
                    {s with rep_xs= Some trm; cls_xs= Cls.add rep cls_xs}
                | None ->
                    {s with rep_xs= Some trm; cls_us= Cls.add rep cls_us} )
          | None -> {s with rep_xs= Some trm} )
  in
  ( match rep_us with
  | Some rep_us ->
      let cls = Cls.add rep_us cls_us in
      let cls, cls_xs =
        match rep_xs with
        | Some rep -> (
          match dom_trm rep with
          | Some rep -> (cls, Cls.add rep cls_xs)
          | None -> (Cls.add rep cls, cls_xs) )
        | None -> (cls, cls_xs)
      in
      let subst =
        Cls.fold cls_xs subst ~f:(fun trm_xs subst ->
            let trm_xs = Subst.apply_rec subst trm_xs in
            let rep_us = Subst.apply_rec subst rep_us in
            Subst.compose1 ~key:trm_xs ~data:rep_us subst )
      in
      (cls, subst)
  | None -> (
    match rep_xs with
    | Some rep_xs ->
        let cls = Cls.add rep_xs cls_us in
        let subst =
          Cls.fold cls_xs subst ~f:(fun trm_xs subst ->
              let trm_xs = Subst.apply_rec subst trm_xs in
              let rep_xs = Subst.apply_rec subst rep_xs in
              Subst.compose1 ~key:trm_xs ~data:rep_xs subst )
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
  (* find terms of form [Extract {_=Sized {_=x}}] *)
  let extract_uses =
    Use.flat_map (use_of x r) ~f:(function
      | Sized _ as m ->
          Use.filter (use_of m r) ~f:(function
            | Extract _ -> true
            | _ -> false )
      | _ -> Use.empty )
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
        | Extract {seq= Sized {siz= n}; off= o; len= l} ->
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
              Cls.fold (rep_cls_of r e) None ~f:(fun trm rep_ito_us ->
                  match rep_ito_us with
                  | Some rep when Trm.compare rep trm <= 0 -> rep_ito_us
                  | _ when Var.Set.subset (Trm.fv trm) ~of_:us -> Some trm
                  | _ -> rep_ito_us )
            in
            Trm.sized ~siz:(Trm.seq_size_exn e) ~seq:rep_ito_us :: suffix ) )
    |> Iter.of_list
    |> Iter.min ~lt:(fun xs ys -> [%compare: Trm.t list] xs ys < 0)
  with
  | Some extracts ->
      let concat = Trm.concat (Array.of_list extracts) in
      let subst = Subst.compose1 ~key:x ~data:concat subst in
      (classes, subst, us_xs)
  | None -> (classes, subst, us_xs)

let solve_for_xs r us xs =
  Var.Set.fold xs ~f:(fun x (classes, subst, us_xs) ->
      let x = Trm.var x in
      if Subst.mem x subst then (classes, subst, us_xs)
      else solve_concat_extracts r us x (classes, subst, us_xs) )

(** move equations from [classes] to [subst] which can be expressed, after
    normalizing with [subst], as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us]
    and [fv u ⊆ us] or else [fv u ⊆ us ∪ xs]. *)
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

(** enumerate variable contexts vᵢ in [v₁;…] and accumulate a solution
    subst with entries [x ↦ u] where [r] entails [x = u] and
    [⋃ⱼ₌₁ⁱ vⱼ ⊇ fv x ⊈ ⋃ⱼ₌₁ⁱ⁻¹ vⱼ] and
    [fv u ⊆ ⋃ⱼ₌₁ⁱ⁻¹ vⱼ] if possible and otherwise
    [fv u ⊆ ⋃ⱼ₌₁ⁱ vⱼ] *)
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
    Subst.iteri subst ~f:(fun ~key ~data ->
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

(* Replay debugging =======================================================*)

type call =
  | Add of Var.Set.t * Formula.t * t
  | Union of Var.Set.t * t * t
  | Inter of Var.Set.t * t * t
  | InterN of Var.Set.t * t list
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
  | Inter (us, r, s) -> inter us r s |> ignore
  | InterN (us, rs) -> interN us rs |> ignore
  | Dnf f -> dnf f |> ignore
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
let inter_tmr = Timer.create "inter" ~at_exit:report
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

let inter us r s =
  wrap inter_tmr (fun () -> inter us r s) (fun () -> Inter (us, r, s))

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
