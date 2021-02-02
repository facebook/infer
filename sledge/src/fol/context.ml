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
  val fold : t -> 's -> f:(key:Trm.t -> data:Trm.t -> 's -> 's) -> 's
  val fold_eqs : t -> 's -> f:(Fml.t -> 's -> 's) -> 's
  val iteri : t -> f:(key:Trm.t -> data:Trm.t -> unit) -> unit
  val for_alli : t -> f:(key:Trm.t -> data:Trm.t -> bool) -> bool
  val apply : t -> Trm.t -> Trm.t
  val subst_ : t -> Trm.t -> Trm.t
  val subst : t -> Term.t -> Term.t
  val norm : t -> Trm.t -> Trm.t
  val compose : t -> t -> t
  val compose1 : key:Trm.t -> data:Trm.t -> t -> t
  val extend : Trm.t -> t -> t option
  val map_entries : f:(Trm.t -> Trm.t) -> t -> t
  val to_iter : t -> (Trm.t * Trm.t) iter
  val to_list : t -> (Trm.t * Trm.t) list
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
  let find = Trm.Map.find
  let fold = Trm.Map.fold

  let fold_eqs s z ~f =
    Trm.Map.fold ~f:(fun ~key ~data -> f (Fml.eq key data)) s z

  let iteri = Trm.Map.iteri
  let for_alli = Trm.Map.for_alli
  let to_iter = Trm.Map.to_iter
  let to_list = Trm.Map.to_list

  (** look up a term in a substitution *)
  let apply s a = Trm.Map.find a s |> Option.value ~default:a

  let rec subst_ s a = apply s (Trm.map ~f:(subst_ s) a)
  let subst s e = Term.map_trms ~f:(subst_ s) e

  (** apply a substitution to maximal non-interpreted subterms *)
  let norm s a =
    [%trace]
      ~call:(fun {pf} -> pf "@ %a" Trm.pp a)
      ~retn:(fun {pf} -> pf "%a" Trm.pp)
    @@ fun () -> Theory.map_solvables ~f:(apply s) a

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

  (** add an identity entry if the term is not already present *)
  let extend e s =
    let exception Found in
    match
      Trm.Map.update e s ~f:(function
        | Some _ -> raise_notrace Found
        | None -> e )
    with
    | exception Found -> None
    | s -> Some s

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
    let is_var_in xs e =
      Option.exists ~f:(fun x -> Var.Set.mem x xs) (Var.of_trm e)
    in
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
              let t = Trm.Map.add ~key ~data t
              and ks =
                Var.Set.diff ks (Var.Set.union (Trm.fv key) (Trm.fv data))
              and s = Trm.Map.remove key s in
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

  val empty : t
  val add : Trm.t -> t -> t
  val remove : Trm.t -> t -> t
  val union : t -> t -> t
  val is_empty : t -> bool
  val pop : t -> (Trm.t * t) option
  val filter : t -> f:(Trm.t -> bool) -> t
  val partition : t -> f:(Trm.t -> bool) -> t * t
  val fold : t -> 's -> f:(Trm.t -> 's -> 's) -> 's
  val map : t -> f:(Trm.t -> Trm.t) -> t
  val to_iter : t -> Trm.t iter
  val to_set : t -> Trm.Set.t
  val of_set : Trm.Set.t -> t
  val ppx : Trm.Var.strength -> t pp
  val pp : t pp
  val pp_raw : t pp
  val pp_diff : (t * t) pp
end = struct
  type t = Trm.t list [@@deriving compare, equal, sexp]

  let empty = []
  let add = List.cons
  let remove = List.remove ~eq:Trm.equal
  let union = List.rev_append
  let is_empty = List.is_empty
  let pop = function [] -> None | x :: xs -> Some (x, xs)
  let filter = List.filter
  let partition = List.partition
  let fold = List.fold
  let map = List.map_endo
  let to_iter = List.to_iter
  let to_set = Trm.Set.of_list
  let of_set s = Iter.to_list (Trm.Set.to_iter s)

  let ppx x fs es =
    List.pp "@ = " (Trm.ppx x) fs (List.sort_uniq ~cmp:Trm.compare es)

  let pp = ppx (fun _ -> None)

  let pp_raw fs es =
    Trm.Set.pp_full ~pre:"{@[" ~suf:"@]}" ~sep:",@ " Trm.pp fs (to_set es)

  let pp_diff = List.pp_diff ~cmp:Trm.compare "@ = " Trm.pp
end

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
  ; pnd: (Trm.t * Trm.t) list
        (** pending equations to add (once invariants are reestablished) *)
  }
[@@deriving compare, equal, sexp]

(* Pretty-printing ========================================================*)

let pp_eq fs (e, f) = Format.fprintf fs "@[%a = %a@]" Trm.pp e Trm.pp f

let pp_raw fs {sat; rep; cls; pnd} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v (k, v)
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_trm_v fs (k, v) = if not (Trm.equal k v) then Trm.pp fs v in
  let pp_cls_v fs (_, cls) = Cls.pp_raw fs cls in
  let pp_pnd fs pnd =
    if not (List.is_empty pnd) then
      Format.fprintf fs ";@ pnd= @[%a@]" (List.pp ";@ " pp_eq) pnd
  in
  Format.fprintf fs "@[{@[<hv>sat= %b;@ rep= %a;@ cls= %a%a@]}@]" sat
    (pp_alist Trm.pp pp_trm_v)
    (Subst.to_list rep)
    (pp_alist Trm.pp pp_cls_v)
    (Trm.Map.to_list cls) pp_pnd pnd

let pp_diff fs (r, s) =
  let pp_sat fs =
    if not (Bool.equal r.sat s.sat) then
      Format.fprintf fs "sat= @[-- %b@ ++ %b@];@ " r.sat s.sat
  in
  let pp_rep fs =
    if not (Subst.is_empty r.rep) then
      Format.fprintf fs "rep= %a;@ " Subst.pp_diff (r.rep, s.rep)
  in
  let pp_cls fs =
    if not (Trm.Map.equal Cls.equal r.cls s.cls) then
      Format.fprintf fs "cls= %a;@ "
        (Trm.Map.pp_diff ~eq:Cls.equal Trm.pp Cls.pp_raw Cls.pp_diff)
        (r.cls, s.cls)
  in
  let pp_pnd fs =
    List.pp_diff ~cmp:[%compare: Trm.t * Trm.t] ~pre:"pnd= @[" ~suf:"@]"
      ";@ " pp_eq fs (r.pnd, s.pnd)
  in
  Format.fprintf fs "@[{@[<hv>%t%t%t%t@]}@]" pp_sat pp_rep pp_cls pp_pnd

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

(** test membership in carrier *)
let in_car r e = Subst.mem e r.rep

(** congruent specialized to assume subterms of [a'] are [Subst.norm]alized
    wrt [r] (or canonized) *)
let semi_congruent r a' b = Trm.equal a' (Trm.map ~f:(Subst.norm r.rep) b)

(** terms are congruent if equal after normalizing subterms *)
let congruent r a b = semi_congruent r (Trm.map ~f:(Subst.norm r.rep) a) b

(* Invariant ==============================================================*)

let pre_invariant r =
  let@ () = Invariant.invariant [%here] r [%sexp_of: t] in
  Subst.iteri r.rep ~f:(fun ~key:trm ~data:rep ->
      (* no interpreted terms in carrier *)
      assert (
        (not (Theory.is_interpreted trm))
        || fail "non-interp %a" Trm.pp trm () ) ;
      (* carrier is closed under subterms *)
      Iter.iter (Trm.trms trm) ~f:(fun subtrm ->
          assert (
            Theory.is_interpreted subtrm
            || (match subtrm with Z _ | Q _ -> true | _ -> false)
            || in_car r subtrm
            || fail "@[subterm %a@ of %a@ not in carrier of@ %a@]" Trm.pp
                 subtrm Trm.pp trm pp r () ) ) ;
      (* rep is idempotent *)
      assert (
        let rep' = Subst.norm r.rep rep in
        Trm.equal rep rep'
        || fail "not idempotent: %a != %a in@ %a" Trm.pp rep Trm.pp rep'
             Subst.pp r.rep () ) ;
      (* every term is in the class of its rep *)
      assert (
        Trm.equal trm rep
        || Trm.Set.mem trm
             (Cls.to_set
                (Trm.Map.find rep r.cls |> Option.value ~default:Cls.empty))
        || fail "%a not in cls of %a = {%a}@ %a" Trm.pp trm Trm.pp rep
             Cls.pp
             (Trm.Map.find rep r.cls |> Option.value ~default:Cls.empty)
             pp_raw r () ) ) ;
  Trm.Map.iteri r.cls ~f:(fun ~key:rep ~data:cls ->
      (* each class does not include its rep *)
      assert (not (Trm.Set.mem rep (Cls.to_set cls))) ;
      (* representative of every element of [rep]'s class is [rep] *)
      Iter.iter (Cls.to_iter cls) ~f:(fun elt ->
          assert (Option.exists ~f:(Trm.equal rep) (Subst.find elt r.rep)) ) )

let invariant r =
  let@ () = Invariant.invariant [%here] r [%sexp_of: t] in
  assert (List.is_empty r.pnd) ;
  pre_invariant r ;
  assert (
    (not r.sat)
    || Subst.for_alli r.rep ~f:(fun ~key:a ~data:a' ->
           Subst.for_alli r.rep ~f:(fun ~key:b ~data:b' ->
               Trm.compare a b >= 0
               || (not (congruent r a b))
               || Trm.equal a' b'
               || fail "not congruent %a@ %a@ in@ %a" Trm.pp a Trm.pp b pp r
                    () ) ) )

(* Extending the carrier ==================================================*)

let rec extend_ a s =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Trm.pp a Subst.pp s)
    ~retn:(fun {pf} s' -> pf "%a" Subst.pp_diff (s, s'))
  @@ fun () ->
  match Theory.classify a with
  | InterpAtom -> s
  | InterpApp -> Iter.fold ~f:extend_ (Trm.trms a) s
  | NonInterpAtom ->
      (* add uninterpreted terms if missing *)
      Trm.Map.change a s ~f:(function None -> Some a | a' -> a')
  | UninterpApp -> (
    (* add uninterpreted terms if missing *)
    match Subst.extend a s with
    | None -> s
    (* and their subterms if newly added *)
    | Some s -> Iter.fold ~f:extend_ (Trm.trms a) s )

(** add a term to the carrier *)
let extend a x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Trm.pp a pp x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      pre_invariant x' )
  @@ fun () ->
  let rep = extend_ a x.rep in
  if rep == x.rep then x else {x with rep}

(* Propagation ============================================================*)

(** add a=a' to x using a' as the representative *)
let propagate1 (a, a') x =
  [%trace]
    ~call:(fun {pf} -> pf "@ @[%a ↦ %a@]@ %a" Trm.pp a Trm.pp a' pp_raw x)
    ~retn:(fun {pf} -> pf "%a" pp_raw)
  @@ fun () ->
  (* pending equations need not be between terms in the carrier *)
  let x = extend a (extend a' x) in
  let s = Trm.Map.singleton a a' in
  Trm.Map.fold x.rep x ~f:(fun ~key:_ ~data:b0' x ->
      let b' = Subst.norm s b0' in
      if b' == b0' then x
      else
        let b0'_cls, cls =
          Trm.Map.find_and_remove b0' x.cls
          |> Option.value ~default:(Cls.empty, x.cls)
        in
        let b0'_cls, pnd =
          if Theory.is_noninterpreted b0' then (Cls.add b0' b0'_cls, x.pnd)
          else (b0'_cls, (b0', b') :: x.pnd)
        in
        let rep =
          Cls.fold b0'_cls x.rep ~f:(fun c rep ->
              Trm.Map.add ~key:c ~data:b' rep )
        in
        let cls =
          Trm.Map.update b' cls ~f:(fun b'_cls ->
              Cls.union b0'_cls (Option.value b'_cls ~default:Cls.empty) )
        in
        {x with rep; cls; pnd} )

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
    ~retn:(fun {pf} -> pf "%a" pp_raw)
  @@ fun () ->
  match x.pnd with
  | (a, b) :: pnd -> (
      let a' = Subst.norm x.rep a in
      let b' = Subst.norm x.rep b in
      match solve ~wrt ~xs:x.xs a' b' pnd with
      | {solved= Some solved; wrt; fresh; pending} ->
          let xs = Var.Set.union x.xs fresh in
          let x = {x with xs; pnd= pending} in
          propagate ~wrt (List.fold ~f:propagate1 solved x)
      | {solved= None} -> {x with sat= false; pnd= []} )
  | [] -> x

(* Core operations ========================================================*)

let empty =
  let rep = Subst.empty in
  {xs= Var.Set.empty; sat= true; rep; cls= Trm.Map.empty; pnd= []}
  |> check invariant

let unsat = {empty with sat= false}

(** [lookup r a] is [b'] if [a ~ b = b'] for some equation [b = b'] in rep *)
let lookup r a =
  ([%Trace.call fun {pf} -> pf "@ %a" Trm.pp a]
  ;
  Iter.find_map (Subst.to_iter r.rep) ~f:(fun (b, b') ->
      Option.return_if (semi_congruent r a b) b' )
  |> Option.value ~default:a)
  |>
  [%Trace.retn fun {pf} -> pf "%a" Trm.pp]

(** rewrite a term into canonical form using rep and, for non-interpreted
    terms, congruence composed with rep *)
let rec canon r a =
  [%Trace.call fun {pf} -> pf "@ %a" Trm.pp a]
  ;
  ( match Theory.classify a with
  | InterpAtom -> a
  | NonInterpAtom -> Subst.apply r.rep a
  | InterpApp -> Trm.map ~f:(canon r) a
  | UninterpApp -> (
      let a' = Trm.map ~f:(canon r) a in
      match Theory.classify a' with
      | InterpAtom | InterpApp -> a'
      | NonInterpAtom -> Subst.apply r.rep a'
      | UninterpApp -> lookup r a' ) )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Trm.pp]

let canon_f r b =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Fml.pp b pp_raw r)
    ~retn:(fun {pf} -> pf "%a" Fml.pp)
  @@ fun () -> Fml.map_trms ~f:(canon r) b

let merge ~wrt a b x =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a@ %a" Trm.pp a Trm.pp b pp x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      pre_invariant x' )
  @@ fun () ->
  let x = {x with pnd= (a, b) :: x.pnd} in
  propagate ~wrt x

(** find an unproved equation between congruent terms *)
let find_missing r =
  Iter.find_map (Subst.to_iter r.rep) ~f:(fun (a, a') ->
      let a_subnorm = Trm.map ~f:(Subst.norm r.rep) a in
      Iter.find_map (Subst.to_iter r.rep) ~f:(fun (b, b') ->
          (* need to equate a' and b'? *)
          let need_a'_eq_b' =
            (* optimize: do not consider both a = b and b = a *)
            Trm.compare a b < 0
            (* a and b are not already equal *)
            && (not (Trm.equal a' b'))
            (* a and b are congruent *)
            && semi_congruent r a_subnorm b
          in
          Option.return_if need_a'_eq_b' (a', b') ) )

let rec close ~wrt x =
  if not x.sat then x
  else
    match find_missing x with
    | Some (a', b') -> close ~wrt (merge ~wrt a' b' x)
    | None -> x

let close ~wrt r =
  [%Trace.call fun {pf} -> pf "@ %a" pp r]
  ;
  close ~wrt r
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let and_eq_ ~wrt a b x =
  [%trace]
    ~call:(fun {pf} -> pf "@ @[%a = %a@]@ %a" Trm.pp a Trm.pp b pp x)
    ~retn:(fun {pf} x' ->
      pf "%a" pp_diff (x, x') ;
      invariant x' )
  @@ fun () ->
  if not x.sat then x
  else
    let x0 = x in
    let a' = canon x a in
    let b' = canon x b in
    if Trm.equal a' b' then extend a' (extend b' x)
    else
      let x = merge ~wrt a' b' x in
      match (a, b) with
      | (Var _ as v), _ when not (in_car x0 v) -> x
      | _, (Var _ as v) when not (in_car x0 v) -> x
      | _ -> close ~wrt x

let extract_xs r = (r.xs, {r with xs= Var.Set.empty})

(* Exposed interface ======================================================*)

let is_empty {sat; rep} =
  sat && Subst.for_alli rep ~f:(fun ~key:a ~data:a' -> Trm.equal a a')

let is_unsat {sat} = not sat

let implies r b =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Fml.pp b pp r]
  ;
  Fml.equal Fml.tt (canon_f r b)
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

let refutes r b = Fml.equal Fml.ff (canon_f r b)
let normalize r e = Term.map_trms ~f:(canon r) e

let cls_of r e =
  let e' = Subst.apply r.rep e in
  Cls.add e' (Trm.Map.find e' r.cls |> Option.value ~default:Cls.empty)

let class_of r e =
  match Term.get_trm (normalize r e) with
  | Some e' ->
      Iter.to_list (Iter.map ~f:Term.of_trm (Cls.to_iter (cls_of r e')))
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

let fold_uses_of r t s ~f =
  let rec fold_ e s ~f =
    let s =
      Iter.fold (Trm.trms e) s ~f:(fun sub s ->
          fold_ ~f sub (if Trm.equal t sub then f e s else s) )
    in
    if Theory.is_interpreted e then Iter.fold ~f:(fold_ ~f) (Trm.trms e) s
    else s
  in
  Subst.fold r.rep s ~f:(fun ~key:trm ~data:rep s ->
      fold_ ~f trm (fold_ ~f rep s) )

let iter_uses_of t r ~f = fold_uses_of r t () ~f:(fun use () -> f use)
let uses_of t r = Iter.from_labelled_iter (iter_uses_of t r)

let apply_subst wrt s r =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Subst.pp s pp r]
  ;
  ( if Subst.is_empty s then r
  else
    Trm.Map.fold r.cls {r with rep= Subst.empty; cls= Trm.Map.empty}
      ~f:(fun ~key:rep ~data:cls r ->
        let rep' = Subst.subst_ s rep in
        Cls.fold cls r ~f:(fun trm r ->
            let trm' = Subst.subst_ s trm in
            and_eq_ ~wrt trm' rep' r ) ) )
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
    Subst.fold s.rep r ~f:(fun ~key:e ~data:e' r -> and_eq_ ~wrt e e' r) )
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
              | Some rep -> (reps, and_eq_ ~wrt exp rep rs)
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
  | Eq (d, e) -> and_eq_ ~wrt d e r
  | Eq0 e -> and_eq_ ~wrt Trm.zero e r
  | Pos _ | Not _ | Or _ | Iff _ | Cond _ | Lit _ -> r

let add us b r =
  [%Trace.call fun {pf} -> pf "@ %a@ %a" Fml.pp b pp r]
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
  let join1 = Iter.cons in
  let top = (Var.Set.empty, Fml.tt, empty) in
  let bot = Iter.empty in
  Fml.fold_dnf ~meet1 ~join1 ~top ~bot f

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
  if rep == x.rep && cls == x.cls then x else {x with rep; cls}

let trivial vs r =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a@ %a" Var.Set.pp_xs vs pp_raw r)
    ~retn:(fun {pf} ks -> pf "%a" Var.Set.pp_xs ks)
  @@ fun () ->
  Var.Set.fold vs Var.Set.empty ~f:(fun v ks ->
      let x = Trm.var v in
      match Subst.find x r.rep with
      | None -> Var.Set.add v ks
      | Some x' when Trm.equal x x' && Iter.is_empty (uses_of x r) ->
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
            Cls.add rep (Option.value cls0 ~default:Cls.empty) ) )
  in
  (* enumerate expanded classes and update solution subst *)
  let kills = Trm.Set.of_vars ks in
  Trm.Map.fold clss x ~f:(fun ~key:a' ~data:ecls x ->
      (* remove mappings for non-rep class elements to kill *)
      let keep, drop = Trm.Set.diff_inter (Cls.to_set ecls) kills in
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

type 'a zom = Zero | One of 'a | Many

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
            (List.fold solved soln ~f:(fun (trm, rep) soln ->
                 Subst.compose1 ~key:trm ~data:rep soln ))
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
    non-interpreted [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else
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
            let trm_xs = Subst.subst_ subst trm_xs in
            let rep_us = Subst.subst_ subst rep_us in
            Subst.compose1 ~key:trm_xs ~data:rep_us subst )
      in
      (cls, subst)
  | None -> (
    match rep_xs with
    | Some rep_xs ->
        let cls = Cls.add rep_xs cls_us in
        let subst =
          Cls.fold cls_xs subst ~f:(fun trm_xs subst ->
              let trm_xs = Subst.subst_ subst trm_xs in
              let rep_xs = Subst.subst_ subst rep_xs in
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
  let uses =
    fold_uses_of r x [] ~f:(fun use uses ->
        match use with
        | Sized _ as m ->
            fold_uses_of r m uses ~f:(fun use uses ->
                match use with Extract _ as e -> e :: uses | _ -> uses )
        | _ -> uses )
  in
  let find_extracts_at_off off =
    List.filter uses ~f:(function
      | Extract {off= o} -> implies r (Fml.eq o off)
      | _ -> false )
  in
  let rec find_extracts full_rev_extracts rev_prefix off =
    List.fold (find_extracts_at_off off) full_rev_extracts
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
              Cls.fold (cls_of r e) None ~f:(fun trm rep_ito_us ->
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

let wrap tmr f call =
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
