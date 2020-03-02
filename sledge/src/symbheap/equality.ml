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
  | Ap2 (Memory, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _) ->
      Interpreted
  | Ap2 ((Eq | Dq), _, _) -> Simplified
  | Ap1 _ | Ap2 _ | Ap3 _ | ApN _ -> Uninterpreted
  | RecN _ | Var _ | Integer _ | Float _ | Nondet _ | Label _ -> Atomic

let interpreted e = equal_kind (classify e) Interpreted
let non_interpreted e = not (interpreted e)

let rec fold_max_solvables e ~init ~f =
  if interpreted e then
    Term.fold e ~init ~f:(fun d s -> fold_max_solvables ~f d ~init:s)
  else f e init

let rec iter_max_solvables e ~f =
  if interpreted e then Term.iter ~f:(iter_max_solvables ~f) e else f e

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
  val trim : bound:Var.Set.t -> Var.Set.t -> t -> t
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

  (** [trim bound kills subst] is [subst] without mappings that mention
      [kills] or [bound ∩ fv x] for removed entries [x ↦ u] *)
  let rec trim ~bound ks s =
    let ks', s' =
      Map.fold s ~init:(ks, s) ~f:(fun ~key ~data (ks, s) ->
          let fv_key = Term.fv key in
          let fv_data = Term.fv data in
          if Set.disjoint ks (Set.union fv_key fv_data) then (ks, s)
          else
            let ks = Set.union ks (Set.inter bound fv_key) in
            let s = Map.remove s key in
            (ks, s) )
    in
    if s' != s then trim ~bound ks' s' else s'
end

(** Theory Solver *)

(* orient equations s.t. Var < Memory < Extract < Concat < others, then
   using height of aggregate nesting, and then using Term.compare *)
let orient e f =
  let compare e f =
    let rank e =
      match (e : Term.t) with
      | Var _ -> 0
      | Ap2 (Memory, _, _) -> 1
      | Ap3 (Extract, _, _, _) -> 2
      | ApN (Concat, _) -> 3
      | _ -> 4
    in
    let rec height e =
      match (e : Term.t) with
      | Ap2 (Memory, _, x) -> 1 + height x
      | Ap3 (Extract, x, _, _) -> 1 + height x
      | ApN (Concat, xs) ->
          1 + Vector.fold ~init:0 ~f:(fun h x -> max h (height x)) xs
      | _ -> 0
    in
    let o = compare (rank e) (rank f) in
    if o <> 0 then o
    else
      let o = compare (height e) (height f) in
      if o <> 0 then o else Term.compare e f
  in
  match Ordering.of_int (compare e f) with
  | Less -> Some (e, f)
  | Equal -> None
  | Greater -> Some (f, e)

let norm (_, _, s) e = Subst.norm s e

let extend ~var ~rep (us, xs, s) =
  Some (us, xs, Subst.compose1 ~key:var ~data:rep s)

let fresh name (us, xs, s) =
  let x, us = Var.fresh name ~wrt:us in
  let xs = Set.add xs x in
  (Term.var x, (us, xs, s))

let solve_poly p q s =
  match Term.sub p q with
  | Integer {data} -> if Z.equal Z.zero data then Some s else None
  | Var _ as var -> extend ~var ~rep:Term.zero s
  | p_q -> (
    match Term.solve_zero_eq p_q with
    | Some (var, rep) -> extend ~var ~rep s
    | None -> extend ~var:p_q ~rep:Term.zero s )

(* α[o,l) = β ==> l = |β| ∧ α = (⟨n,c⟩[0,o) ^ β ^ ⟨n,c⟩[o+l,n-o-l)) where n
   = |α| and c fresh *)
let rec solve_extract a o l b s =
  let n = Term.agg_size_exn a in
  let c, s = fresh "c" s in
  let n_c = Term.memory ~siz:n ~arr:c in
  let o_l = Term.add o l in
  let n_o_l = Term.sub n o_l in
  let c0 = Term.extract ~agg:n_c ~off:Term.zero ~len:o in
  let c1 = Term.extract ~agg:n_c ~off:o_l ~len:n_o_l in
  let b, s =
    match Term.agg_size b with
    | None -> (Term.memory ~siz:l ~arr:b, Some s)
    | Some m -> (b, solve_ l m s)
  in
  s >>= solve_ a (Term.concat [|c0; b; c1|])

(* α₀^…^αᵢ^αⱼ^…^αᵥ = β ==> |α₀^…^αᵥ| = |β| ∧ … ∧ αⱼ = β[n₀+…+nᵢ,nⱼ) ∧ …
   where nₓ ≡ |αₓ| and m = |β| *)
and solve_concat a0V b m s =
  Vector.fold_until a0V ~init:(s, Term.zero)
    ~f:(fun (s, oI) aJ ->
      let nJ = Term.agg_size_exn aJ in
      let oJ = Term.add oI nJ in
      match solve_ aJ (Term.extract ~agg:b ~off:oI ~len:nJ) s with
      | Some s -> Continue (s, oJ)
      | None -> Stop None )
    ~finish:(fun (s, n0V) -> solve_ n0V m s)

and solve_ e f s =
  [%Trace.call fun {pf} ->
    pf "%a@[%a@ %a@ %a@]" Var.Set.pp_xs (snd3 s) Term.pp e Term.pp f
      Subst.pp (trd3 s)]
  ;
  ( match orient (norm s e) (norm s f) with
  (* e' = f' ==> true when e' ≡ f' *)
  | None -> Some s
  (* i = j ==> false when i ≠ j *)
  | Some (Integer _, Integer _) -> None
  (* ⟨0,a⟩ = β ==> a = β = ⟨⟩ *)
  | Some (Ap2 (Memory, n, a), b) when Term.equal n Term.zero ->
      s |> solve_ a (Term.concat [||]) >>= solve_ b (Term.concat [||])
  | Some (b, Ap2 (Memory, n, a)) when Term.equal n Term.zero ->
      s |> solve_ a (Term.concat [||]) >>= solve_ b (Term.concat [||])
  (* v = ⟨n,a⟩ ==> v = a *)
  | Some ((Var _ as v), Ap2 (Memory, _, a)) -> s |> solve_ v a
  (* ⟨n,a⟩ = ⟨m,b⟩ ==> n = m ∧ a = β *)
  | Some (Ap2 (Memory, n, a), Ap2 (Memory, m, b)) ->
      s |> solve_ n m >>= solve_ a b
  (* ⟨n,a⟩ = β ==> n = |β| ∧ a = β *)
  | Some (Ap2 (Memory, n, a), b) ->
      (match Term.agg_size b with None -> Some s | Some m -> solve_ n m s)
      >>= solve_ a b
  | Some ((Var _ as v), (Ap3 (Extract, _, _, l) as e)) ->
      if not (Set.mem (Term.fv e) (Var.of_ v)) then
        (* v = α[o,l) ==> v ↦ α[o,l) when v ∉ fv(α[o,l)) *)
        extend ~var:v ~rep:e s
      else
        (* v = α[o,l) ==> α[o,l) ↦ ⟨l,v⟩ when v ∈ fv(α[o,l)) *)
        extend ~var:e ~rep:(Term.memory ~siz:l ~arr:v) s
  | Some ((Var _ as v), (ApN (Concat, a0V) as c)) ->
      if not (Set.mem (Term.fv c) (Var.of_ v)) then
        (* v = α₀^…^αᵥ ==> v ↦ α₀^…^αᵥ when v ∉ fv(α₀^…^αᵥ) *)
        extend ~var:v ~rep:c s
      else
        (* v = α₀^…^αᵥ ==> ⟨|α₀^…^αᵥ|,v⟩ = α₀^…^αᵥ when v ∈ fv(α₀^…^αᵥ) *)
        let m = Term.agg_size_exn c in
        solve_concat a0V (Term.memory ~siz:m ~arr:v) m s
  | Some ((Ap3 (Extract, _, _, l) as e), ApN (Concat, a0V)) ->
      solve_concat a0V e l s
  | Some (ApN (Concat, a0V), (ApN (Concat, _) as c)) ->
      solve_concat a0V c (Term.agg_size_exn c) s
  | Some (Ap3 (Extract, a, o, l), e) -> solve_extract a o l e s
  (* p = q ==> p-q = 0 *)
  | Some
      ( ((Add _ | Mul _ | Integer _) as p), q
      | q, ((Add _ | Mul _ | Integer _) as p) ) ->
      solve_poly p q s
  | Some (rep, var) ->
      assert (non_interpreted var) ;
      assert (non_interpreted rep) ;
      extend ~var ~rep s )
  |>
  [%Trace.retn fun {pf} ->
    function
    | Some (_, xs, s) -> pf "%a%a" Var.Set.pp_xs xs Subst.pp s
    | None -> pf "false"]

let solve ~us ~xs e f =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp e Term.pp f]
  ;
  (solve_ e f (us, xs, Subst.empty) >>| fun (_, xs, s) -> (xs, s))
  |>
  [%Trace.retn fun {pf} ->
    function
    | Some (xs, s) -> pf "%a%a" Var.Set.pp_xs xs Subst.pp s
    | None -> pf "false"]

(** Equality Relations *)

(** see also [invariant] *)
type t =
  { xs: Var.Set.t
        (** existential variables that did not appear in input equations *)
  ; sat: bool  (** [false] only if constraints are inconsistent *)
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

let cls_of r e =
  let e' = Subst.apply r.rep e in
  Map.find (classes r) e' |> Option.value ~default:[e']

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
      assert (non_interpreted a) ;
      (* carrier is closed under subterms *)
      iter_max_solvables a ~f:(fun b ->
          assert (
            in_car r b
            || fail "@[subterm %a of %a not in carrier of@ %a@]" Term.pp b
                 Term.pp a pp r () ) ) )

(** Core operations *)

let true_ =
  {xs= Var.Set.empty; sat= true; rep= Subst.empty} |> check invariant

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
  | Interpreted | Simplified -> Term.fold ~f:extend a ~init:r
  | Uninterpreted -> (
    match Subst.extend a r.rep with
    | Some rep -> Term.fold ~f:extend a ~init:{r with rep}
    | None -> r )
  | Atomic -> r

let extend a r = extend a r |> check invariant

let merge us a b r =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Term.pp a Term.pp b pp r]
  ;
  ( match solve ~us ~xs:r.xs a b with
  | Some (xs, s) ->
      {r with xs= Set.union r.xs xs; rep= Subst.compose r.rep s}
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

let rec close us r =
  if not r.sat then r
  else
    match find_missing r with
    | Some (a', b') -> close us (merge us a' b' r)
    | None -> r

let close us r =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  close us r
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let and_eq us a b r =
  if not r.sat then r
  else
    let a' = canon r a in
    let b' = canon r b in
    let r = extend a' r in
    let r = extend b' r in
    if Term.equal a' b' then r else close us (merge us a' b' r)

let extract_xs r = (r.xs, {r with xs= Var.Set.empty})

(** Exposed interface *)

let is_true {sat; rep} =
  sat && Subst.for_alli rep ~f:(fun ~key:a ~data:a' -> Term.equal a a')

let is_false {sat} = not sat
let entails_eq r d e = Term.is_true (canon r (Term.eq d e))

let entails r s =
  Subst.for_alli s.rep ~f:(fun ~key:e ~data:e' -> entails_eq r e e')

let normalize = canon

let class_of r e =
  let e' = normalize r e in
  e' :: Map.find_multi (classes r) e'

let fold_uses_of r t ~init ~f =
  let rec fold_ e ~init:s ~f =
    let s =
      Term.fold e ~init:s ~f:(fun sub s ->
          if Term.equal t sub then f s e else s )
    in
    if interpreted e then
      Term.fold e ~init:s ~f:(fun d s -> fold_ ~f d ~init:s)
    else s
  in
  Subst.fold r.rep ~init ~f:(fun ~key:trm ~data:rep s ->
      let f trm s = fold_ trm ~init:s ~f in
      f trm (f rep s) )

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

let and_ us r s =
  ( if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Subst.length s.rep <= Subst.length r.rep then (s, r) else (r, s)
    in
    Subst.fold s.rep ~init:r ~f:(fun ~key:e ~data:e' r -> and_eq us e e' r)
  )
  |> extract_xs

let or_ us r s =
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
              | Some rep -> (reps, and_eq us exp rep rs)
              | None -> (exp :: reps, rs) )
          |> snd )
    in
    let rs = true_ in
    let rs = merge_mems rs r s in
    let rs = merge_mems rs s r in
    rs )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r) -> pf "%a" pp r]

let and_eq us a b r =
  [%Trace.call fun {pf} -> pf "%a = %a@ %a" Term.pp a Term.pp b pp r]
  ;
  and_eq us a b r |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

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

(* try to solve [p = q] such that [fv (p - q) ⊆ us ∪ xs] and [p - q] has at
   most one maximal solvable subterm, [kill], where [fv kill ⊈ us]; solve [p
   = q] for [kill]; extend subst mapping [kill] to the solution *)
let solve_poly_eq us p' q' subst =
  let diff = Term.sub p' q' in
  let max_solvables_not_ito_us =
    fold_max_solvables diff ~init:Zero ~f:(fun solvable_subterm -> function
      | Many -> Many
      | zom when Set.is_subset (Term.fv solvable_subterm) ~of_:us -> zom
      | One _ -> Many
      | Zero -> One solvable_subterm )
  in
  match max_solvables_not_ito_us with
  | One kill ->
      let+ kill, keep = Term.solve_zero_eq diff ~for_:kill in
      Subst.compose1 ~key:kill ~data:keep subst
  | Many | Zero -> None

let solve_memory_eq us e' f' subst =
  match ((e' : Term.t), (f' : Term.t)) with
  | (Ap2 (Memory, _, (Var _ as v)) as m), u
   |u, (Ap2 (Memory, _, (Var _ as v)) as m) ->
      if
        (not (Set.is_subset (Term.fv m) ~of_:us))
        && Set.is_subset (Term.fv u) ~of_:us
      then Some (Subst.compose1 ~key:v ~data:u subst)
      else None
  | _ -> None

let solve_interp_eq us e' (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "trm: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Term.pp e' pp_cls cls
      Subst.pp subst]
  ;
  List.find_map cls ~f:(fun f ->
      let f' = Subst.norm subst f in
      match solve_memory_eq us e' f' subst with
      | Some subst -> Some subst
      | None -> solve_poly_eq us e' f' subst )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst) ;
    Option.iter subst' ~f:(fun subst' ->
        Subst.iteri subst' ~f:(fun ~key ~data ->
            assert (
              Subst.mem subst key
              || not (Set.is_subset (Term.fv key) ~of_:us) ) ;
            assert (Set.is_subset (Term.fv data) ~of_:us) ) )]

(* move equations from [cls] to [subst] which are between [Interpreted]
   terms and can be expressed, after normalizing with [subst], as [x ↦ u]
   where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let rec solve_interp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_cls cls Subst.pp subst]
  ;
  let rec solve_interp_eqs_ cls' (cls, subst) =
    match cls with
    | [] -> (cls', subst)
    | trm :: cls ->
        let trm' = Subst.norm subst trm in
        if interpreted trm' then
          match solve_interp_eq us trm' (cls, subst) with
          | Some subst -> solve_interp_eqs_ cls' (cls, subst)
          | None -> solve_interp_eqs_ (trm' :: cls') (cls, subst)
        else solve_interp_eqs_ (trm' :: cls') (cls, subst)
  in
  let cls', subst' = solve_interp_eqs_ [] (cls, subst) in
  ( if subst' != subst then solve_interp_eqs us (cls', subst')
  else (cls', subst') )
  |>
  [%Trace.retn fun {pf} (cls', subst') ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_diff_cls (cls, cls') Subst.pp_diff
      (subst, subst')]

(* move equations from [cls] (which is assumed to be normalized by [subst])
   to [subst] which are between non-[Interpreted] terms and can be expressed
   as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let solve_uninterp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_cls cls Subst.pp subst]
  ;
  let rep_ito_us, cls_not_ito_us, cls_delay =
    List.fold cls ~init:(None, [], [])
      ~f:(fun (rep_ito_us, cls_not_ito_us, cls_delay) trm ->
        if non_interpreted trm then
          if Set.is_subset (Term.fv trm) ~of_:us then
            match rep_ito_us with
            | Some rep when Term.compare rep trm <= 0 ->
                (rep_ito_us, cls_not_ito_us, trm :: cls_delay)
            | Some rep -> (Some trm, cls_not_ito_us, rep :: cls_delay)
            | None -> (Some trm, cls_not_ito_us, cls_delay)
          else (rep_ito_us, trm :: cls_not_ito_us, cls_delay)
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
  let cls, cls_not_ito_us_xs =
    List.partition_tf ~f:(fun e -> Set.is_subset (Term.fv e) ~of_:us_xs) cls
  in
  let cls, subst = solve_interp_eqs us (rep :: cls, subst) in
  let cls, subst = solve_uninterp_eqs us (cls, subst) in
  let cls = List.rev_append cls_not_ito_us_xs cls in
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

let solve_concat_extracts_eq r x =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp x pp r]
  ;
  let uses =
    fold_uses_of r x ~init:[] ~f:(fun uses -> function
      | Ap2 (Memory, _, _) as m ->
          fold_uses_of r m ~init:uses ~f:(fun uses -> function
            | Ap3 (Extract, _, _, _) as e -> e :: uses | _ -> uses )
      | _ -> uses )
  in
  let find_extracts_at_off off =
    List.filter uses ~f:(fun use ->
        match (use : Term.t) with
        | Ap3 (Extract, _, o, _) -> entails_eq r o off
        | _ -> false )
  in
  let rec find_extracts full_rev_extracts rev_prefix off =
    List.fold (find_extracts_at_off off) ~init:full_rev_extracts
      ~f:(fun full_rev_extracts e ->
        match e with
        | Ap3 (Extract, Ap2 (Memory, n, _), o, l) ->
            let o_l = Term.add o l in
            if entails_eq r n o_l then
              (e :: rev_prefix) :: full_rev_extracts
            else find_extracts full_rev_extracts (e :: rev_prefix) o_l
        | _ -> full_rev_extracts )
  in
  find_extracts [] [] Term.zero
  |>
  [%Trace.retn fun {pf} ->
    pf "@[[%a]@]" (List.pp ";@ " (List.pp ",@ " Term.pp))]

let solve_concat_extracts r us x (classes, subst, us_xs) =
  match
    List.filter_map (solve_concat_extracts_eq r x) ~f:(fun rev_extracts ->
        List.fold_option rev_extracts ~init:[] ~f:(fun suffix e ->
            let+ rep_ito_us =
              List.fold (cls_of r e) ~init:None ~f:(fun rep_ito_us trm ->
                  match rep_ito_us with
                  | Some rep when Term.compare rep trm <= 0 -> rep_ito_us
                  | _ when Set.is_subset (Term.fv trm) ~of_:us -> Some trm
                  | _ -> rep_ito_us )
            in
            Term.memory ~siz:(Term.agg_size_exn e) ~arr:rep_ito_us :: suffix
        ) )
    |> List.min_elt ~compare:[%compare: Term.t list]
  with
  | Some extracts ->
      let concat = Term.concat (Array.of_list extracts) in
      let subst = Subst.compose1 ~key:x ~data:concat subst in
      (classes, subst, us_xs)
  | None -> (classes, subst, us_xs)

let solve_for_xs r us xs (classes, subst, us_xs) =
  Set.fold xs ~init:(classes, subst, us_xs)
    ~f:(fun (classes, subst, us_xs) x ->
      let x = Term.var x in
      if Subst.mem subst x then (classes, subst, us_xs)
      else solve_concat_extracts r us x (classes, subst, us_xs) )

(* move equations from [classes] to [subst] which can be expressed, after
   normalizing with [subst], as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us ⊇ fv u] *)
let solve_classes r (classes, subst, us) xs =
  [%Trace.call fun {pf} -> pf "xs: {@[%a@]}" Var.Set.pp xs]
  ;
  let rec solve_classes_ (classes0, subst0, us_xs) =
    let classes, subst =
      Map.fold ~f:(solve_class us us_xs) classes0 ~init:(classes0, subst0)
    in
    if subst != subst0 then solve_classes_ (classes, subst, us_xs)
    else (classes, subst, us_xs)
  in
  (classes, subst, Set.union us xs)
  |> solve_classes_ |> solve_for_xs r us xs
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
  List.fold ~f:(solve_classes r)
    ~init:(classes r, Subst.empty, Var.Set.empty)
    vss
  |> snd3
  |>
  [%Trace.retn fun {pf} subst ->
    pf "%a" Subst.pp subst ;
    Subst.iteri subst ~f:(fun ~key ~data ->
        assert (
          entails_eq r key data
          || fail "@[%a = %a not entailed by@ %a@]" Term.pp key Term.pp data
               pp_classes r () ) ;
        assert (
          List.exists vss ~f:(fun vs ->
              match
                ( Set.is_subset (Term.fv key) ~of_:vs
                , Set.is_subset (Term.fv data) ~of_:vs )
              with
              | false, true -> true
              | true, false -> assert false
              | _ -> false ) ) )]
