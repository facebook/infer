(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Equality over uninterpreted functions and linear rational arithmetic *)

(** Classification of Terms by Theory *)

type kind = Interpreted | Simplified | Atomic | Uninterpreted
[@@deriving compare, equal]

let classify e =
  match (e : Term.t) with
  | Add _ | Mul _ -> Interpreted
  | Ap2 ((Eq | Dq), _, _) | Ap2 (Memory, _, _) | ApN (Concat, _) ->
      Simplified
  | Ap1 _ | Ap2 _ | Ap3 _ | ApN _ -> Uninterpreted
  | RecN _ | Var _ | Integer _ | Float _ | Nondet _ | Label _ -> Atomic

let rec fold_max_solvables e ~init ~f =
  match classify e with
  | Interpreted ->
      Term.fold e ~init ~f:(fun d s -> fold_max_solvables ~f d ~init:s)
  | _ -> f e init

let rec iter_max_solvables e ~f =
  match classify e with
  | Interpreted -> Term.iter ~f:(iter_max_solvables ~f) e
  | _ -> f e

(** Solution Substitutions *)
module Subst : sig
  type t [@@deriving compare, equal, sexp]

  val pp : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val is_empty : t -> bool
  val length : t -> int
  val mem : t -> Term.t -> bool
  val fold : t -> init:'a -> f:(key:Term.t -> data:Term.t -> 'a -> 'a) -> 'a
  val iteri : t -> f:(key:Term.t -> data:Term.t -> unit) -> unit
  val for_alli : t -> f:(key:Term.t -> data:Term.t -> bool) -> bool
  val apply : t -> Term.t -> Term.t
  val norm : t -> Term.t -> Term.t
  val compose : t -> t -> t
  val compose1 : key:Term.t -> data:Term.t -> t -> t
  val extend : Term.t -> t -> t option
  val map_entries : f:(Term.t -> Term.t) -> t -> t
  val to_alist : t -> (Term.t * Term.t) list
end = struct
  type t = Term.t Term.Map.t [@@deriving compare, equal, sexp]

  let pp = Map.pp Term.pp Term.pp

  let pp_diff =
    Map.pp_diff ~data_equal:Term.equal Term.pp Term.pp Term.pp_diff

  let empty = Term.Map.empty
  let is_empty = Map.is_empty
  let length = Map.length
  let mem = Map.mem
  let fold = Map.fold
  let iteri = Map.iteri
  let for_alli = Map.for_alli
  let to_alist = Map.to_alist ~key_order:`Increasing

  (** look up a term in a substitution *)
  let apply s a = Map.find s a |> Option.value ~default:a

  (** apply a substitution to maximal non-interpreted subterms *)
  let rec norm s a =
    match classify a with
    | Interpreted -> Term.map ~f:(norm s) a
    | Simplified -> apply s (Term.map ~f:(norm s) a)
    | Atomic | Uninterpreted -> apply s a

  (** compose two substitutions *)
  let compose r s =
    let r' = Map.map ~f:(norm s) r in
    Map.merge_skewed r' s ~combine:(fun ~key v1 v2 ->
        if Term.equal v1 v2 then v1
        else fail "domains intersect: %a" Term.pp key () )

  (** compose a substitution with a mapping *)
  let compose1 ~key ~data s =
    if Term.equal key data then s
    else compose s (Map.set Term.Map.empty ~key ~data)

  (** add an identity entry if the term is not already present *)
  let extend e s =
    let exception Found in
    match
      Map.update s e ~f:(function
        | Some _ -> Exn.raise_without_backtrace Found
        | None -> e )
    with
    | exception Found -> None
    | s -> Some s

  (** map over a subst, applying [f] to both domain and range, requires that
      [f] is injective and for any set of terms [E], [f\[E\]] is disjoint
      from [E] *)
  let map_entries ~f s =
    Map.fold s ~init:s ~f:(fun ~key ~data s ->
        let key' = f key in
        let data' = f data in
        if Term.equal key' key then
          if Term.equal data' data then s else Map.set s ~key ~data:data'
        else Map.remove s key |> Map.add_exn ~key:key' ~data:data' )
end

(** Theory Solver *)

let rec is_constant e =
  match (e : Term.t) with
  | Var _ | Nondet _ -> false
  | Ap1 (_, x) -> is_constant x
  | Ap2 (_, x, y) -> is_constant x && is_constant y
  | Ap3 (_, x, y, z) -> is_constant x && is_constant y && is_constant z
  | ApN (_, xs) | RecN (_, xs) -> Vector.for_all ~f:is_constant xs
  | Add args | Mul args ->
      Qset.for_all ~f:(fun arg _ -> is_constant arg) args
  | Label _ | Float _ | Integer _ -> true

let solve e f =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp e Term.pp f]
  ;
  let rec solve_ e f s =
    let solve_uninterp e f =
      match ((e : Term.t), (f : Term.t)) with
      | Integer {data= m}, Integer {data= n} when not (Z.equal m n) -> None
      | _ -> (
        match (is_constant e, is_constant f) with
        (* orient equation to discretionarily prefer term that is constant
           or compares smaller as class representative *)
        | true, false -> Some (Subst.compose1 ~key:f ~data:e s)
        | false, true -> Some (Subst.compose1 ~key:e ~data:f s)
        | _ ->
            let key, data =
              if Term.compare e f > 0 then (e, f) else (f, e)
            in
            Some (Subst.compose1 ~key ~data s) )
    in
    let concat_size args =
      Vector.fold_until args ~init:Term.zero
        ~f:(fun sum m ->
          match (m : Term.t) with
          | Ap2 (Memory, siz, _) -> Continue (Term.add siz sum)
          | _ -> Stop None )
        ~finish:(fun _ -> None)
    in
    match ((e : Term.t), (f : Term.t)) with
    | (Add _ | Mul _ | Integer _), _ | _, (Add _ | Mul _ | Integer _) -> (
        let e_f = Term.sub e f in
        match Term.solve_zero_eq e_f with
        | Some (key, data) -> Some (Subst.compose1 ~key ~data s)
        | None -> solve_uninterp e_f Term.zero )
    | ApN (Concat, ms), ApN (Concat, ns) -> (
      match (concat_size ms, concat_size ns) with
      | Some p, Some q -> solve_uninterp e f >>= solve_ p q
      | _ -> solve_uninterp e f )
    | Ap2 (Memory, m, _), ApN (Concat, ns)
     |ApN (Concat, ns), Ap2 (Memory, m, _) -> (
      match concat_size ns with
      | Some p -> solve_uninterp e f >>= solve_ p m
      | _ -> solve_uninterp e f )
    | _ -> solve_uninterp e f
  in
  solve_ e f Subst.empty
  |>
  [%Trace.retn fun {pf} ->
    function Some s -> pf "%a" Subst.pp s | None -> pf "false"]

(** Equality Relations *)

(** see also [invariant] *)
type t =
  { sat: bool  (** [false] only if constraints are inconsistent *)
  ; rep: Subst.t
        (** functional set of oriented equations: map [a] to [a'],
            indicating that [a = a'] holds, and that [a'] is the
            'rep(resentative)' of [a] *) }
[@@deriving compare, equal, sexp]

let classes r =
  let add key data cls =
    if Term.equal key data then cls
    else Map.add_multi cls ~key:data ~data:key
  in
  Subst.fold r.rep ~init:Term.Map.empty ~f:(fun ~key ~data cls ->
      match classify key with
      | Interpreted | Atomic -> add key data cls
      | Simplified | Uninterpreted ->
          add (Term.map ~f:(Subst.apply r.rep) key) data cls )

(** Pretty-printing *)

let pp fs {sat; rep} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v (k, v)
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_term_v fs (k, v) = if not (Term.equal k v) then Term.pp fs v in
  Format.fprintf fs "@[{@[<hv>sat= %b;@ rep= %a@]}@]" sat
    (pp_alist Term.pp pp_term_v)
    (Subst.to_alist rep)

let pp_diff fs (r, s) =
  let pp_sat fs =
    if not (Bool.equal r.sat s.sat) then
      Format.fprintf fs "sat= @[-- %b@ ++ %b@];@ " r.sat s.sat
  in
  let pp_rep fs =
    if not (Subst.is_empty r.rep) then
      Format.fprintf fs "rep= %a" Subst.pp_diff (r.rep, s.rep)
  in
  Format.fprintf fs "@[{@[<hv>%t%t@]}@]" pp_sat pp_rep

let ppx_cls x = List.pp "@ = " (Term.ppx x)
let pp_cls = ppx_cls (fun _ -> None)
let pp_diff_cls = List.pp_diff ~compare:Term.compare "@ = " Term.pp

let ppx_clss x fs cs =
  List.pp "@ @<2>∧ "
    (fun fs (key, data) ->
      Format.fprintf fs "@[%a@ = %a@]" (Term.ppx x) key (ppx_cls x)
        (List.sort ~compare:Term.compare data) )
    fs (Map.to_alist cs)

let pp_clss fs cs = ppx_clss (fun _ -> None) fs cs

let pp_diff_clss =
  Map.pp_diff ~data_equal:(List.equal Term.equal) Term.pp pp_cls pp_diff_cls

(** Invariant *)

(** test membership in carrier *)
let in_car r e = Subst.mem r.rep e

let invariant r =
  Invariant.invariant [%here] r [%sexp_of: t]
  @@ fun () ->
  Subst.iteri r.rep ~f:(fun ~key:a ~data:_ ->
      (* no interpreted terms in carrier *)
      assert (Poly.(classify a <> Interpreted)) ;
      (* carrier is closed under subterms *)
      iter_max_solvables a ~f:(fun b ->
          assert (
            in_car r b
            || fail "@[subterm %a of %a not in carrier of@ %a@]" Term.pp b
                 Term.pp a pp r () ) ) )

(** Core operations *)

let true_ = {sat= true; rep= Subst.empty} |> check invariant

(** terms are congruent if equal after normalizing subterms *)
let congruent r a b =
  Term.equal
    (Term.map ~f:(Subst.norm r.rep) a)
    (Term.map ~f:(Subst.norm r.rep) b)

(** [lookup r a] is [b'] if [a ~ b = b'] for some equation [b = b'] in rep *)
let lookup r a =
  With_return.with_return
  @@ fun {return} ->
  (* congruent specialized to assume [a] canonized and [b] non-interpreted *)
  let semi_congruent r a b =
    Term.equal a (Term.map ~f:(Subst.apply r.rep) b)
  in
  Subst.iteri r.rep ~f:(fun ~key ~data ->
      if semi_congruent r a key then return data ) ;
  a

(** rewrite a term into canonical form using rep and, for uninterpreted
    terms, congruence composed with rep *)
let rec canon r a =
  match classify a with
  | Interpreted -> Term.map ~f:(canon r) a
  | Simplified | Uninterpreted -> lookup r (Term.map ~f:(canon r) a)
  | Atomic -> Subst.apply r.rep a

(** add a term to the carrier *)
let rec extend a r =
  match classify a with
  | Interpreted -> Term.fold ~f:extend a ~init:r
  | Simplified | Uninterpreted -> (
    match Subst.extend a r.rep with
    | Some rep -> Term.fold ~f:extend a ~init:{r with rep}
    | None -> r )
  | Atomic -> r

let extend a r = extend a r |> check invariant

let merge a b r =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Term.pp a Term.pp b pp r]
  ;
  ( match solve a b with
  | Some s -> {r with rep= Subst.compose r.rep s}
  | None -> {r with sat= false} )
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

(** find an unproved equation between congruent terms *)
let find_missing r =
  With_return.with_return
  @@ fun {return} ->
  Subst.iteri r.rep ~f:(fun ~key:a ~data:a' ->
      Subst.iteri r.rep ~f:(fun ~key:b ~data:b' ->
          if
            Term.compare a b < 0
            && (not (Term.equal a' b'))
            && congruent r a b
          then return (Some (a', b')) ) ) ;
  None

let rec close r =
  if not r.sat then r
  else
    match find_missing r with
    | Some (a', b') -> close (merge a' b' r)
    | None -> r

let close r =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  close r
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

(** Exposed interface *)

let and_eq a b r =
  [%Trace.call fun {pf} -> pf "%a = %a@ %a" Term.pp a Term.pp b pp r]
  ;
  ( if not r.sat then r
  else
    let a' = canon r a in
    let b' = canon r b in
    let r = extend a' r in
    let r = extend b' r in
    if Term.equal a' b' then r else close (merge a' b' r) )
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let is_true {sat; rep} =
  sat && Subst.for_alli rep ~f:(fun ~key:a ~data:a' -> Term.equal a a')

let is_false {sat} = not sat
let entails_eq r d e = Term.equal (canon r d) (canon r e)

let entails r s =
  Subst.for_alli s.rep ~f:(fun ~key:e ~data:e' -> entails_eq r e e')

let normalize = canon

let class_of r e =
  let e' = normalize r e in
  e' :: Map.find_multi (classes r) e'

let difference r a b =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Term.pp a Term.pp b pp r]
  ;
  let a = canon r a in
  let b = canon r b in
  ( if Term.equal a b then Some Z.zero
  else
    match normalize r (Term.sub a b) with
    | Integer {data} -> Some data
    | _ -> None )
  |>
  [%Trace.retn fun {pf} ->
    function Some d -> pf "%a" Z.pp_print d | None -> pf ""]

let and_ r s =
  if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Subst.length s.rep <= Subst.length r.rep then (s, r) else (r, s)
    in
    Subst.fold s.rep ~init:r ~f:(fun ~key:e ~data:e' r -> and_eq e e' r)

let or_ r s =
  [%Trace.call fun {pf} -> pf "@[<hv 1>   %a@ @<2>∨ %a@]" pp r pp s]
  ;
  ( if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems rs r s =
      Map.fold (classes s) ~init:rs ~f:(fun ~key:rep ~data:cls rs ->
          List.fold cls
            ~init:([rep], rs)
            ~f:(fun (reps, rs) exp ->
              match List.find ~f:(entails_eq r exp) reps with
              | Some rep -> (reps, and_eq exp rep rs)
              | None -> (exp :: reps, rs) )
          |> snd )
    in
    let rs = true_ in
    let rs = merge_mems rs r s in
    let rs = merge_mems rs s r in
    rs )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

let rename r sub =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  let rep = Subst.map_entries ~f:(Term.rename sub) r.rep in
  (if rep == r.rep then r else {r with rep})
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let fold_terms r ~init ~f =
  Subst.fold r.rep ~f:(fun ~key ~data z -> f (f z data) key) ~init

let fold_vars r ~init ~f =
  fold_terms r ~init ~f:(fun init -> Term.fold_vars ~f ~init)

let fv e = fold_vars e ~f:Set.add ~init:Var.Set.empty
let pp_classes fs r = pp_clss fs (classes r)
let ppx_classes x fs r = ppx_clss x fs (classes r)

let ppx_classes_diff x fs (r, s) =
  let clss = classes s in
  let clss =
    Map.filter_mapi clss ~f:(fun ~key:rep ~data:cls ->
        match
          List.filter cls ~f:(fun exp -> not (entails_eq r rep exp))
        with
        | [] -> None
        | cls -> Some cls )
  in
  List.pp "@ @<2>∧ "
    (fun fs (rep, cls) ->
      Format.fprintf fs "@[%a@ = %a@]" (Term.ppx x) rep
        (List.pp "@ = " (Term.ppx x))
        (List.sort ~compare:Term.compare cls) )
    fs (Map.to_alist clss)

(** Existential Witnessing and Elimination *)

type 'a zom = Zero | One of 'a | Many

(* try to find a [term] in [cls] such that [fv (poly - term) ⊆ us ∪ xs] and
   [poly - term] has at most one maximal solvable subterm, [kill], where [fv
   kill ⊈ us]; solve [poly = term] for [kill]; extend subst mapping [kill]
   to the solution *)
let solve_interp_eq us us_xs poly (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "poly: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Term.pp poly pp_cls cls
      Subst.pp subst]
  ;
  ( if not (Set.is_subset (Term.fv poly) ~of_:us_xs) then None
  else
    List.find_map cls ~f:(fun trm ->
        if not (Set.is_subset (Term.fv trm) ~of_:us_xs) then None
        else
          let diff = Subst.norm subst (Term.sub poly trm) in
          let max_solvables_not_ito_us =
            fold_max_solvables diff ~init:Zero ~f:(fun solvable_subterm ->
              function
              | Many -> Many
              | zom when Set.is_subset (Term.fv solvable_subterm) ~of_:us ->
                  zom
              | One _ -> Many
              | Zero -> One solvable_subterm )
          in
          match max_solvables_not_ito_us with
          | One kill ->
              let+ kill, keep = Term.solve_zero_eq diff ~for_:kill in
              Subst.compose1 ~key:kill ~data:keep subst
          | Many | Zero -> None ) )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst) ;
    Option.iter subst' ~f:(fun subst' ->
        Subst.iteri subst' ~f:(fun ~key ~data ->
            assert (Set.is_subset (Term.fv key) ~of_:us_xs) ;
            assert (
              Subst.mem subst key
              || not (Set.is_subset (Term.fv key) ~of_:us) ) ;
            assert (Set.is_subset (Term.fv data) ~of_:us) ) )]

(* move equations from [cls] to [subst] which are between [Interpreted]
   terms and can be expressed, after normalizing with [subst], as [x ↦ u]
   where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let rec solve_interp_eqs us us_xs (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_cls cls Subst.pp subst]
  ;
  let rec solve_interp_eqs_ cls' (cls, subst) =
    match cls with
    | [] -> (cls', subst)
    | trm :: cls -> (
        let trm' = Subst.norm subst trm in
        match classify trm' with
        | Interpreted -> (
          match solve_interp_eq us us_xs trm' (cls', subst) with
          | None -> (
            match solve_interp_eq us us_xs trm' (cls, subst) with
            | None -> solve_interp_eqs_ (trm' :: cls') (cls, subst)
            | Some subst -> solve_interp_eqs_ cls' (cls, subst) )
          | Some subst -> solve_interp_eqs_ cls' (cls, subst) )
        | _ -> solve_interp_eqs_ (trm' :: cls') (cls, subst) )
  in
  let cls', subst' = solve_interp_eqs_ [] (cls, subst) in
  ( if subst' != subst then solve_interp_eqs us us_xs (cls', subst')
  else (cls', subst') )
  |>
  [%Trace.retn fun {pf} (cls', subst') ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_diff_cls (cls, cls') Subst.pp_diff
      (subst, subst')]

(* move equations from [cls] (which is assumed to be normalized by [subst])
   to [subst] which are between non-[Interpreted] terms and can be expressed
   as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let solve_uninterp_eqs us us_xs (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_cls cls Subst.pp subst]
  ;
  let rep_ito_us, cls_not_ito_us, cls_delay =
    List.fold cls ~init:(None, [], [])
      ~f:(fun (rep_ito_us, cls_not_ito_us, cls_delay) trm ->
        if not (equal_kind (classify trm) Interpreted) then
          let fv_trm = Term.fv trm in
          if Set.is_subset fv_trm ~of_:us then
            match rep_ito_us with
            | Some rep when Term.compare rep trm <= 0 ->
                (rep_ito_us, cls_not_ito_us, trm :: cls_delay)
            | Some rep -> (Some trm, cls_not_ito_us, rep :: cls_delay)
            | None -> (Some trm, cls_not_ito_us, cls_delay)
          else if Set.is_subset fv_trm ~of_:us_xs then
            (rep_ito_us, trm :: cls_not_ito_us, cls_delay)
          else (rep_ito_us, cls_not_ito_us, trm :: cls_delay)
        else (rep_ito_us, cls_not_ito_us, trm :: cls_delay) )
  in
  ( match rep_ito_us with
  | None -> (cls, subst)
  | Some rep_ito_us ->
      let cls =
        if List.is_empty cls_delay then [] else rep_ito_us :: cls_delay
      in
      let subst =
        List.fold cls_not_ito_us ~init:subst ~f:(fun subst trm_not_ito_us ->
            Subst.compose1 ~key:trm_not_ito_us ~data:rep_ito_us subst )
      in
      (cls, subst) )
  |>
  [%Trace.retn fun {pf} (cls', subst') ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_diff_cls (cls, cls') Subst.pp_diff
      (subst, subst') ;
    Subst.iteri subst' ~f:(fun ~key ~data ->
        assert (Set.is_subset (Term.fv key) ~of_:us_xs) ;
        assert (
          Subst.mem subst key || not (Set.is_subset (Term.fv key) ~of_:us)
        ) ;
        assert (Set.is_subset (Term.fv data) ~of_:us) )]

(* move equations between terms in [rep]'s class [cls] from [classes] to
   [subst] which can be expressed, after normalizing with [subst], as [x ↦
   u] where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let solve_class us us_xs ~key:rep ~data:cls (classes, subst) =
  let classes0 = classes in
  [%Trace.call fun {pf} ->
    pf "rep: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Term.pp rep pp_cls cls
      Subst.pp subst]
  ;
  let cls, subst = solve_interp_eqs us us_xs (rep :: cls, subst) in
  let cls, subst = solve_uninterp_eqs us us_xs (cls, subst) in
  let cls =
    List.remove ~equal:Term.equal cls (Subst.norm subst rep)
    |> Option.value ~default:cls
  in
  let classes =
    if List.is_empty cls then Map.remove classes rep
    else Map.set classes ~key:rep ~data:cls
  in
  (classes, subst)
  |>
  [%Trace.retn fun {pf} (classes', subst') ->
    pf "subst: @[%a@]@ classes: @[%a@]" Subst.pp_diff (subst, subst')
      pp_diff_clss (classes0, classes')]

(* move equations from [classes] to [subst] which can be expressed, after
   normalizing with [subst], as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let solve_classes (classes, subst, us) xs =
  [%Trace.call fun {pf} -> pf "xs: {@[%a@]}" Var.Set.pp xs]
  ;
  let rec solve_classes_ (classes0, subst0, us_xs) =
    let classes, subst =
      Map.fold ~f:(solve_class us us_xs) classes0 ~init:(classes0, subst0)
    in
    if subst != subst0 then solve_classes_ (classes, subst, us_xs)
    else (classes, subst, us_xs)
  in
  solve_classes_ (classes, subst, Set.union us xs)
  |>
  [%Trace.retn fun {pf} (classes', subst', _) ->
    pf "subst: @[%a@]@ classes: @[%a@]" Subst.pp_diff (subst, subst')
      pp_diff_clss (classes, classes')]

let pp_vss fs vss =
  Format.fprintf fs "[@[%a@]]"
    (List.pp ";@ " (fun fs vs -> Format.fprintf fs "{@[%a@]}" Var.Set.pp vs))
    vss

(* enumerate variable contexts vᵢ in [v₁;…] and accumulate a solution subst
   with entries [x ↦ u] where [r] entails [x = u] and [⋃ⱼ₌₁ⁱ vⱼ ⊇ fv x ⊈
   ⋃ⱼ₌₁ⁱ⁻¹ vⱼ ⊇ fv u] *)
let solve_for_vars vss r =
  [%Trace.call fun {pf} -> pf "%a@ @[%a@]" pp_vss vss pp_classes r]
  ;
  List.fold ~f:solve_classes
    ~init:(classes r, Subst.empty, Var.Set.empty)
    vss
  |> snd3
  |>
  [%Trace.retn fun {pf} subst ->
    pf "%a" Subst.pp subst ;
    Subst.iteri subst ~f:(fun ~key ~data ->
        assert (entails_eq r key data) ;
        assert (
          List.exists vss ~f:(fun vs ->
              match
                ( Set.is_subset (Term.fv key) ~of_:vs
                , Set.is_subset (Term.fv data) ~of_:vs )
              with
              | false, true -> true
              | true, false -> assert false
              | _ -> false ) ) )]
