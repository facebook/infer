(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Equality over uninterpreted functions and linear rational arithmetic *)

(** Classification of Terms by Theory *)

type kind = Interpreted | Atomic | Uninterpreted
[@@deriving compare, equal]

let classify e =
  match (e : Term.t) with
  | Add _ | Ap2 (Sized, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _) ->
      Interpreted
  | Mul _ | Ap1 _ | Ap2 _ | Ap3 _ | ApN _ | Apply _ | PosLit _ | NegLit _
   |And _ | Or _ ->
      Uninterpreted
  | Var _ | Integer _ | Rational _ | Label _ | RecRecord _ -> Atomic

let interpreted e = equal_kind (classify e) Interpreted
let non_interpreted e = not (interpreted e)
let uninterpreted e = equal_kind (classify e) Uninterpreted

let rec fold_max_solvables e ~init ~f =
  if non_interpreted e then f e init
  else Term.fold e ~init ~f:(fun d s -> fold_max_solvables ~f d ~init:s)

(** Solution Substitutions *)
module Subst : sig
  type t [@@deriving compare, equal, sexp]

  val pp : t pp
  val pp_diff : (t * t) pp
  val empty : t
  val is_empty : t -> bool
  val length : t -> int
  val mem : t -> Term.t -> bool
  val find : t -> Term.t -> Term.t option
  val fold : t -> init:'a -> f:(key:Term.t -> data:Term.t -> 'a -> 'a) -> 'a
  val iteri : t -> f:(key:Term.t -> data:Term.t -> unit) -> unit
  val for_alli : t -> f:(key:Term.t -> data:Term.t -> bool) -> bool
  val apply : t -> Term.t -> Term.t
  val subst : t -> Term.t -> Term.t
  val norm : t -> Term.t -> Term.t
  val compose : t -> t -> t
  val compose1 : key:Term.t -> data:Term.t -> t -> t
  val extend : Term.t -> t -> t option
  val remove : Var.Set.t -> t -> t
  val map_entries : f:(Term.t -> Term.t) -> t -> t
  val to_alist : t -> (Term.t * Term.t) list
  val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
end = struct
  type t = Term.t Term.Map.t [@@deriving compare, equal, sexp_of]

  let t_of_sexp = Term.Map.t_of_sexp Term.t_of_sexp
  let pp = Term.Map.pp Term.pp Term.pp

  let pp_diff =
    Term.Map.pp_diff ~data_equal:Term.equal Term.pp Term.pp Term.pp_diff

  let empty = Term.Map.empty
  let is_empty = Term.Map.is_empty
  let length = Term.Map.length
  let mem = Term.Map.mem
  let find = Term.Map.find
  let fold = Term.Map.fold
  let iteri = Term.Map.iteri
  let for_alli = Term.Map.for_alli
  let to_alist = Term.Map.to_alist ~key_order:`Increasing

  (** look up a term in a substitution *)
  let apply s a = Term.Map.find s a |> Option.value ~default:a

  let rec subst s a = apply s (Term.map ~f:(subst s) a)

  (** apply a substitution to maximal non-interpreted subterms *)
  let rec norm s a =
    if interpreted a then Term.map ~f:(norm s) a else apply s a

  (** compose two substitutions *)
  let compose r s =
    [%Trace.call fun {pf} -> pf "%a@ %a" pp r pp s]
    ;
    let r' = Term.Map.map_endo ~f:(norm s) r in
    Term.Map.merge_endo r' s ~f:(fun ~key -> function
      | `Both (data_r, data_s) ->
          assert (
            Term.equal data_s data_r
            || fail "domains intersect: %a" Term.pp key () ) ;
          Some data_r
      | `Left data | `Right data -> Some data )
    |>
    [%Trace.retn fun {pf} r' ->
      pf "%a" pp_diff (r, r') ;
      assert (r' != r ==> not (equal r' r))]

  (** compose a substitution with a mapping *)
  let compose1 ~key ~data s =
    match (key : Term.t) with
    | Integer _ | Rational _ | Label _ -> s
    | _ ->
        if Term.equal key data then s
        else compose s (Term.Map.singleton key data)

  (** add an identity entry if the term is not already present *)
  let extend e s =
    let exception Found in
    match
      Term.Map.update s e ~f:(function
        | Some _ -> raise_notrace Found
        | None -> e )
    with
    | exception Found -> None
    | s -> Some s

  (** remove entries for vars *)
  let remove xs s =
    Var.Set.fold ~f:(fun s x -> Term.Map.remove s (Term.var x)) ~init:s xs

  (** map over a subst, applying [f] to both domain and range, requires that
      [f] is injective and for any set of terms [E], [f\[E\]] is disjoint
      from [E] *)
  let map_entries ~f s =
    Term.Map.fold s ~init:s ~f:(fun ~key ~data s ->
        let key' = f key in
        let data' = f data in
        if Term.equal key' key then
          if Term.equal data' data then s
          else Term.Map.set s ~key ~data:data'
        else
          let s = Term.Map.remove s key in
          match (key : Term.t) with
          | Integer _ | Rational _ | Label _ -> s
          | _ -> Term.Map.add_exn ~key:key' ~data:data' s )

  (** Holds only if [true ⊢ ∃xs. e=f]. Clients assume
      [not (is_valid_eq xs e f)] implies [not (is_valid_eq ys e f)] for
      [ys ⊆ xs]. *)
  let is_valid_eq xs e f =
    let is_var_in xs e =
      Option.exists ~f:(Var.Set.mem xs) (Var.of_term e)
    in
    ( is_var_in xs e
    || is_var_in xs f
    || (uninterpreted e && Term.exists ~f:(is_var_in xs) e)
    || (uninterpreted f && Term.exists ~f:(is_var_in xs) f) )
    $> fun b ->
    [%Trace.info
      "is_valid_eq %a%a=%a = %b" Var.Set.pp_xs xs Term.pp e Term.pp f b]

  (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks
      and ν are maximal where ∃ks. ν is universally valid, xs ⊇ ks and
      ks ∩ fv(τ) = ∅. *)
  let partition_valid xs s =
    (* Move equations e=f from s to t when ∃ks.e=f fails to be provably
       valid. When moving an equation, reduce ks by fv(e=f) to maintain ks ∩
       fv(t) = ∅. This reduction may cause equations in s to no longer be
       valid, so loop until no change. *)
    let rec partition_valid_ t ks s =
      let t', ks', s' =
        Term.Map.fold s ~init:(t, ks, s) ~f:(fun ~key ~data (t, ks, s) ->
            if is_valid_eq ks key data then (t, ks, s)
            else
              let t = Term.Map.set ~key ~data t
              and ks =
                Var.Set.diff ks (Var.Set.union (Term.fv key) (Term.fv data))
              and s = Term.Map.remove s key in
              (t, ks, s) )
      in
      if s' != s then partition_valid_ t' ks' s' else (t', ks', s')
    in
    partition_valid_ empty xs s
end

(** Theory Solver *)

(** prefer representative terms that are minimal in the order s.t. Var <
    Sized < Extract < Concat < others, then using height of sequence
    nesting, and then using Term.compare *)
let prefer e f =
  let rank e =
    match (e : Term.t) with
    | Var _ -> 0
    | Ap2 (Sized, _, _) -> 1
    | Ap3 (Extract, _, _, _) -> 2
    | ApN (Concat, _) -> 3
    | _ -> 4
  in
  let o = compare (rank e) (rank f) in
  if o <> 0 then o
  else
    let o = compare (Term.height e) (Term.height f) in
    if o <> 0 then o else Term.compare e f

(** orient equations based on representative preference *)
let orient e f =
  match Ordering.of_int (prefer e f) with
  | Less -> Some (e, f)
  | Equal -> None
  | Greater -> Some (f, e)

let norm (_, _, s) e = Subst.norm s e

let compose1 ?f ~var ~rep (us, xs, s) =
  let s =
    match f with
    | Some f when not (f var rep) -> s
    | _ -> Subst.compose1 ~key:var ~data:rep s
  in
  Some (us, xs, s)

let fresh name (us, xs, s) =
  let x, us = Var.fresh name ~wrt:us in
  let xs = Var.Set.add xs x in
  (Term.var x, (us, xs, s))

let solve_poly ?f p q s =
  match Term.sub p q with
  | Integer {data} -> if Z.equal Z.zero data then Some s else None
  | Var _ as var -> compose1 ?f ~var ~rep:Term.zero s
  | p_q -> (
    match Term.solve_zero_eq p_q with
    | Some (var, rep) -> compose1 ?f ~var ~rep s
    | None -> compose1 ?f ~var:p_q ~rep:Term.zero s )

(* α[o,l) = β ==> l = |β| ∧ α = (⟨n,c⟩[0,o) ^ β ^ ⟨n,c⟩[o+l,n-o-l)) where n
   = |α| and c fresh *)
let rec solve_extract ?f a o l b s =
  let n = Term.seq_size_exn a in
  let c, s = fresh "c" s in
  let n_c = Term.sized ~siz:n ~seq:c in
  let o_l = Term.add o l in
  let n_o_l = Term.sub n o_l in
  let c0 = Term.extract ~seq:n_c ~off:Term.zero ~len:o in
  let c1 = Term.extract ~seq:n_c ~off:o_l ~len:n_o_l in
  let b, s =
    match Term.seq_size b with
    | None -> (Term.sized ~siz:l ~seq:b, Some s)
    | Some m -> (b, solve_ ?f l m s)
  in
  s >>= solve_ ?f a (Term.concat [|c0; b; c1|])

(* α₀^…^αᵢ^αⱼ^…^αᵥ = β ==> |α₀^…^αᵥ| = |β| ∧ … ∧ αⱼ = β[n₀+…+nᵢ,nⱼ) ∧ …
   where nₓ ≡ |αₓ| and m = |β| *)
and solve_concat ?f a0V b m s =
  IArray.fold_until a0V ~init:(s, Term.zero)
    ~f:(fun (s, oI) aJ ->
      let nJ = Term.seq_size_exn aJ in
      let oJ = Term.add oI nJ in
      match solve_ ?f aJ (Term.extract ~seq:b ~off:oI ~len:nJ) s with
      | Some s -> Continue (s, oJ)
      | None -> Stop None )
    ~finish:(fun (s, n0V) -> solve_ ?f n0V m s)

and solve_ ?f d e s =
  [%Trace.call fun {pf} ->
    pf "%a@[%a@ %a@ %a@]" Var.Set.pp_xs (snd3 s) Term.pp d Term.pp e
      Subst.pp (trd3 s)]
  ;
  ( match orient (norm s d) (norm s e) with
  (* e' = f' ==> true when e' ≡ f' *)
  | None -> Some s
  (* i = j ==> false when i ≠ j *)
  | Some (Integer _, Integer _) | Some (Rational _, Rational _) -> None
  (* ⟨0,a⟩ = β ==> a = β = ⟨⟩ *)
  | Some (Ap2 (Sized, n, a), b) when Term.equal n Term.zero ->
      s |> solve_ ?f a (Term.concat [||]) >>= solve_ ?f b (Term.concat [||])
  | Some (b, Ap2 (Sized, n, a)) when Term.equal n Term.zero ->
      s |> solve_ ?f a (Term.concat [||]) >>= solve_ ?f b (Term.concat [||])
  (* v = ⟨n,a⟩ ==> v = a *)
  | Some ((Var _ as v), Ap2 (Sized, _, a)) -> s |> solve_ ?f v a
  (* ⟨n,a⟩ = ⟨m,b⟩ ==> n = m ∧ a = β *)
  | Some (Ap2 (Sized, n, a), Ap2 (Sized, m, b)) ->
      s |> solve_ ?f n m >>= solve_ ?f a b
  (* ⟨n,a⟩ = β ==> n = |β| ∧ a = β *)
  | Some (Ap2 (Sized, n, a), b) ->
      ( match Term.seq_size b with
      | None -> Some s
      | Some m -> solve_ ?f n m s )
      >>= solve_ ?f a b
  | Some ((Var _ as v), (Ap3 (Extract, _, _, l) as e)) ->
      if not (Var.Set.mem (Term.fv e) (Var.of_ v)) then
        (* v = α[o,l) ==> v ↦ α[o,l) when v ∉ fv(α[o,l)) *)
        compose1 ?f ~var:v ~rep:e s
      else
        (* v = α[o,l) ==> α[o,l) ↦ ⟨l,v⟩ when v ∈ fv(α[o,l)) *)
        compose1 ?f ~var:e ~rep:(Term.sized ~siz:l ~seq:v) s
  | Some ((Var _ as v), (ApN (Concat, a0V) as c)) ->
      if not (Var.Set.mem (Term.fv c) (Var.of_ v)) then
        (* v = α₀^…^αᵥ ==> v ↦ α₀^…^αᵥ when v ∉ fv(α₀^…^αᵥ) *)
        compose1 ?f ~var:v ~rep:c s
      else
        (* v = α₀^…^αᵥ ==> ⟨|α₀^…^αᵥ|,v⟩ = α₀^…^αᵥ when v ∈ fv(α₀^…^αᵥ) *)
        let m = Term.seq_size_exn c in
        solve_concat ?f a0V (Term.sized ~siz:m ~seq:v) m s
  | Some ((Ap3 (Extract, _, _, l) as e), ApN (Concat, a0V)) ->
      solve_concat ?f a0V e l s
  | Some (ApN (Concat, a0V), (ApN (Concat, _) as c)) ->
      solve_concat ?f a0V c (Term.seq_size_exn c) s
  | Some (Ap3 (Extract, a, o, l), e) -> solve_extract ?f a o l e s
  (* p = q ==> p-q = 0 *)
  | Some
      ( ((Add _ | Mul _ | Integer _ | Rational _) as p), q
      | q, ((Add _ | Mul _ | Integer _ | Rational _) as p) ) ->
      solve_poly ?f p q s
  | Some (rep, var) ->
      assert (non_interpreted var) ;
      assert (non_interpreted rep) ;
      compose1 ?f ~var ~rep s )
  |>
  [%Trace.retn fun {pf} ->
    function
    | Some (_, xs, s) -> pf "%a%a" Var.Set.pp_xs xs Subst.pp s
    | None -> pf "false"]

let solve ?f ~us ~xs d e =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp d Term.pp e]
  ;
  (solve_ ?f d e (us, xs, Subst.empty) >>| fun (_, xs, s) -> (xs, s))
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

type classes = Term.t list Term.Map.t

let classes r =
  let add key data cls =
    if Term.equal key data then cls
    else Term.Map.add_multi cls ~key:data ~data:key
  in
  Subst.fold r.rep ~init:Term.Map.empty ~f:(fun ~key ~data cls ->
      match classify key with
      | Interpreted | Atomic -> add key data cls
      | Uninterpreted -> add (Term.map ~f:(Subst.apply r.rep) key) data cls )

let cls_of r e =
  let e' = Subst.apply r.rep e in
  Term.Map.find (classes r) e' |> Option.value ~default:[e']

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

let ppx_classes x fs clss =
  List.pp "@ @<2>∧ "
    (fun fs (rep, cls) ->
      Format.fprintf fs "@[%a@ = %a@]" (Term.ppx x) rep (ppx_cls x)
        (List.sort ~compare:Term.compare cls) )
    fs (Term.Map.to_alist clss)

let pp_classes fs r = ppx_classes (fun _ -> None) fs (classes r)

let pp_diff_clss =
  Term.Map.pp_diff ~data_equal:(List.equal Term.equal) Term.pp pp_cls
    pp_diff_cls

(** Basic queries *)

(** test membership in carrier *)
let in_car r e = Subst.mem r.rep e

(** congruent specialized to assume subterms of [a'] are [Subst.norm]alized
    wrt [r] (or canonized) *)
let semi_congruent r a' b = Term.equal a' (Term.map ~f:(Subst.norm r.rep) b)

(** terms are congruent if equal after normalizing subterms *)
let congruent r a b = semi_congruent r (Term.map ~f:(Subst.norm r.rep) a) b

(** Invariant *)

let pre_invariant r =
  let@ () = Invariant.invariant [%here] r [%sexp_of: t] in
  Subst.iteri r.rep ~f:(fun ~key:trm ~data:_ ->
      (* no interpreted terms in carrier *)
      assert (non_interpreted trm || fail "non-interp %a" Term.pp trm ()) ;
      (* carrier is closed under subterms *)
      Term.iter trm ~f:(fun subtrm ->
          assert (
            non_interpreted subtrm
            ==> (Term.is_constant subtrm || in_car r subtrm)
            || fail "@[subterm %a@ of %a@ not in carrier of@ %a@]" Term.pp
                 subtrm Term.pp trm pp r () ) ) )

let invariant r =
  let@ () = Invariant.invariant [%here] r [%sexp_of: t] in
  pre_invariant r ;
  assert (
    (not r.sat)
    || Subst.for_alli r.rep ~f:(fun ~key:a ~data:a' ->
           Subst.for_alli r.rep ~f:(fun ~key:b ~data:b' ->
               Term.compare a b >= 0
               || congruent r a b ==> Term.equal a' b'
               || fail "not congruent %a@ %a@ in@ %a" Term.pp a Term.pp b pp
                    r () ) ) )

(** Core operations *)

let true_ =
  let rep = Subst.empty in
  let rep = Option.value_exn (Subst.extend Term.true_ rep) in
  let rep = Option.value_exn (Subst.extend Term.false_ rep) in
  {xs= Var.Set.empty; sat= true; rep} |> check invariant

let false_ = {true_ with sat= false}

(** [lookup r a] is [b'] if [a ~ b = b'] for some equation [b = b'] in rep *)
let lookup r a =
  ([%Trace.call fun {pf} -> pf "%a" Term.pp a]
  ;
  let@ {return} = with_return in
  Subst.iteri r.rep ~f:(fun ~key:b ~data:b' ->
      if semi_congruent r a b then return b' ) ;
  a)
  |>
  [%Trace.retn fun {pf} -> pf "%a" Term.pp]

(** rewrite a term into canonical form using rep and, for non-interpreted
    terms, congruence composed with rep *)
let rec canon r a =
  [%Trace.call fun {pf} -> pf "%a" Term.pp a]
  ;
  ( match classify a with
  | Atomic -> Subst.apply r.rep a
  | Interpreted -> Term.map ~f:(canon r) a
  | Uninterpreted -> (
      let a' = Term.map ~f:(canon r) a in
      match classify a' with
      | Atomic -> Subst.apply r.rep a'
      | Interpreted -> a'
      | Uninterpreted -> lookup r a' ) )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Term.pp]

let rec extend_ a r =
  match (a : Term.t) with
  | Integer _ | Rational _ | Label _ -> r
  | _ -> (
      if interpreted a then Term.fold ~f:extend_ a ~init:r
      else
        (* add uninterpreted terms *)
        match Subst.extend a r with
        (* and their subterms if newly added *)
        | Some r -> Term.fold ~f:extend_ a ~init:r
        | None -> r )

(** add a term to the carrier *)
let extend a r =
  let rep = extend_ a r.rep in
  if rep == r.rep then r else {r with rep} |> check pre_invariant

let merge us a b r =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Term.pp a Term.pp b pp r]
  ;
  ( match solve ~us ~xs:r.xs a b with
  | Some (xs, s) ->
      {r with xs= Var.Set.union r.xs xs; rep= Subst.compose r.rep s}
  | None -> {r with sat= false} )
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    pre_invariant r']

(** find an unproved equation between congruent terms *)
let find_missing r =
  let@ {return} = with_return in
  Subst.iteri r.rep ~f:(fun ~key:a ~data:a' ->
      let a_subnorm = Term.map ~f:(Subst.norm r.rep) a in
      Subst.iteri r.rep ~f:(fun ~key:b ~data:b' ->
          if
            (* optimize: do not consider both a = b and b = a *)
            Term.compare a b < 0
            (* a and b are not already equal *)
            && (not (Term.equal a' b'))
            (* a and b are congruent *)
            && semi_congruent r a_subnorm b
          then (* need to equate a' and b' *)
            return (Some (a', b')) ) ) ;
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

let and_eq_ us a b r =
  if not r.sat then r
  else
    let r0 = r in
    let a' = canon r a in
    let b' = canon r b in
    let r = extend a' r in
    let r = extend b' r in
    if Term.equal a' b' then r
    else
      let r = merge us a' b' r in
      match (a, b) with
      | (Var _ as v), _ when not (in_car r0 v) -> r
      | _, (Var _ as v) when not (in_car r0 v) -> r
      | _ -> close us r

let extract_xs r = (r.xs, {r with xs= Var.Set.empty})

(** Exposed interface *)

let is_true {sat; rep} =
  sat && Subst.for_alli rep ~f:(fun ~key:a ~data:a' -> Term.equal a a')

let is_false {sat} = not sat

let implies r b =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp b pp r]
  ;
  Term.is_true (canon r b)
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

let normalize = canon

let class_of r e =
  let e' = normalize r e in
  e' :: Term.Map.find_multi (classes r) e'

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

let apply_subst us s r =
  [%Trace.call fun {pf} -> pf "%a@ %a" Subst.pp s pp r]
  ;
  Term.Map.fold (classes r) ~init:true_ ~f:(fun ~key:rep ~data:cls r ->
      let rep' = Subst.subst s rep in
      List.fold cls ~init:r ~f:(fun r trm ->
          let trm' = Subst.subst s trm in
          and_eq_ us trm' rep' r ) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (xs, r') ->
    pf "%a%a" Var.Set.pp_xs xs pp_diff (r, r') ;
    invariant r']

let and_ us r s =
  [%Trace.call fun {pf} -> pf "@[<hv 1>   %a@ @<2>∧ %a@]" pp r pp s]
  ;
  ( if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Subst.length s.rep <= Subst.length r.rep then (s, r) else (r, s)
    in
    Subst.fold s.rep ~init:r ~f:(fun ~key:e ~data:e' r -> and_eq_ us e e' r)
  )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let or_ us r s =
  [%Trace.call fun {pf} -> pf "@[<hv 1>   %a@ @<2>∨ %a@]" pp r pp s]
  ;
  ( if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems rs r s =
      Term.Map.fold (classes s) ~init:rs ~f:(fun ~key:rep ~data:cls rs ->
          List.fold cls
            ~init:([rep], rs)
            ~f:(fun (reps, rs) exp ->
              match
                List.find ~f:(fun rep -> implies r (Term.eq exp rep)) reps
              with
              | Some rep -> (reps, and_eq_ us exp rep rs)
              | None -> (exp :: reps, rs) )
          |> snd )
    in
    let rs = true_ in
    let rs = merge_mems rs r s in
    let rs = merge_mems rs s r in
    rs )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let orN us rs =
  match rs with
  | [] -> (us, false_)
  | r :: rs -> List.fold ~f:(fun (us, s) r -> or_ us s r) ~init:(us, r) rs

let rec and_term_ us e r =
  let eq_false b r = and_eq_ us b Term.false_ r in
  match (e : Term.t) with
  | Integer {data} -> if Z.is_false data then false_ else r
  | And cs -> Term.Set.fold cs ~init:r ~f:(fun r e -> and_term_ us e r)
  | Ap2 (Eq, a, b) -> and_eq_ us a b r
  | Ap2 (Xor, Integer {data}, a) when Z.is_true data -> eq_false a r
  | Ap2 (Xor, a, Integer {data}) when Z.is_true data -> eq_false a r
  | _ -> r

let and_term us e r =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp e pp r]
  ;
  and_term_ us e r |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let and_eq us a b r =
  [%Trace.call fun {pf} -> pf "%a = %a@ %a" Term.pp a Term.pp b pp r]
  ;
  and_eq_ us a b r |> extract_xs
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

(** Existential Witnessing and Elimination *)

let subst_invariant us s0 s =
  assert (s0 == s || not (Subst.equal s0 s)) ;
  assert (
    Subst.iteri s ~f:(fun ~key ~data ->
        (* dom of new entries not ito us *)
        assert (
          Option.for_all ~f:(Term.equal data) (Subst.find s0 key)
          || not (Var.Set.is_subset (Term.fv key) ~of_:us) ) ;
        (* rep not ito us implies trm not ito us *)
        assert (
          Var.Set.is_subset (Term.fv data) ~of_:us
          || not (Var.Set.is_subset (Term.fv key) ~of_:us) ) ) ;
    true )

type 'a zom = Zero | One of 'a | Many

(** try to solve [p = q] such that [fv (p - q) ⊆ us ∪ xs] and [p - q]
    has at most one maximal solvable subterm, [kill], where
    [fv kill ⊈ us]; solve [p = q] for [kill]; extend subst mapping [kill]
    to the solution *)
let solve_poly_eq us p' q' subst =
  [%Trace.call fun {pf} -> pf "%a = %a" Term.pp p' Term.pp q']
  ;
  let diff = Term.sub p' q' in
  let max_solvables_not_ito_us =
    fold_max_solvables diff ~init:Zero ~f:(fun solvable_subterm -> function
      | Many -> Many
      | zom when Var.Set.is_subset (Term.fv solvable_subterm) ~of_:us -> zom
      | One _ -> Many
      | Zero -> One solvable_subterm )
  in
  ( match max_solvables_not_ito_us with
  | One kill ->
      let+ kill, keep = Term.solve_zero_eq diff ~for_:kill in
      Subst.compose1 ~key:kill ~data:keep subst
  | Many | Zero -> None )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst)]

let solve_seq_eq us e' f' subst =
  [%Trace.call fun {pf} -> pf "%a = %a" Term.pp e' Term.pp f']
  ;
  let f x u =
    (not (Var.Set.is_subset (Term.fv x) ~of_:us))
    && Var.Set.is_subset (Term.fv u) ~of_:us
  in
  let solve_concat ms n a =
    let a, n =
      match Term.seq_size a with
      | Some n -> (a, n)
      | None -> (Term.sized ~siz:n ~seq:a, n)
    in
    let+ _, xs, s = solve_concat ~f ms a n (us, Var.Set.empty, subst) in
    assert (Var.Set.is_empty xs) ;
    s
  in
  ( match ((e' : Term.t), (f' : Term.t)) with
  | (ApN (Concat, ms) as c), a when f c a ->
      solve_concat ms (Term.seq_size_exn c) a
  | a, (ApN (Concat, ms) as c) when f c a ->
      solve_concat ms (Term.seq_size_exn c) a
  | (Ap2 (Sized, _, (Var _ as v)) as m), u when f m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
  | u, (Ap2 (Sized, _, (Var _ as v)) as m) when f m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
  | _ -> None )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst)]

let solve_interp_eq us e' (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "trm: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Term.pp e' pp_cls cls
      Subst.pp subst]
  ;
  List.find_map cls ~f:(fun f ->
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

type cls_solve_state =
  { rep_us: Term.t option  (** rep, that is ito us, for class *)
  ; cls_us: Term.t list  (** cls that is ito us, or interpreted *)
  ; rep_xs: Term.t option  (** rep, that is *not* ito us, for class *)
  ; cls_xs: Term.t list  (** cls that is *not* ito us *) }

let dom_trm e =
  match (e : Term.t) with
  | Ap2 (Sized, _, (Var _ as v)) -> Some v
  | _ when non_interpreted e -> Some e
  | _ -> None

(** move equations from [cls] (which is assumed to be normalized by [subst])
    to [subst] which can be expressed as [x ↦ u] where [x] is
    non-interpreted [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else
    [fv u ⊆ us ∪ xs] *)
let solve_uninterp_eqs us (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_cls cls Subst.pp subst]
  ;
  let compare e f =
    [%compare: kind * Term.t] (classify e, e) (classify f, f)
  in
  let {rep_us; cls_us; rep_xs; cls_xs} =
    List.fold cls ~init:{rep_us= None; cls_us= []; rep_xs= None; cls_xs= []}
      ~f:(fun ({rep_us; cls_us; rep_xs; cls_xs} as s) trm ->
        if Var.Set.is_subset (Term.fv trm) ~of_:us then
          match rep_us with
          | Some rep when compare rep trm <= 0 ->
              {s with cls_us= trm :: cls_us}
          | Some rep -> {s with rep_us= Some trm; cls_us= rep :: cls_us}
          | None -> {s with rep_us= Some trm}
        else
          match rep_xs with
          | Some rep -> (
              if compare rep trm <= 0 then
                match dom_trm trm with
                | Some trm -> {s with cls_xs= trm :: cls_xs}
                | None -> {s with cls_us= trm :: cls_us}
              else
                match dom_trm rep with
                | Some rep ->
                    {s with rep_xs= Some trm; cls_xs= rep :: cls_xs}
                | None -> {s with rep_xs= Some trm; cls_us= rep :: cls_us} )
          | None -> {s with rep_xs= Some trm} )
  in
  ( match rep_us with
  | Some rep_us ->
      let cls = rep_us :: cls_us in
      let cls, cls_xs =
        match rep_xs with
        | Some rep -> (
          match dom_trm rep with
          | Some rep -> (cls, rep :: cls_xs)
          | None -> (rep :: cls, cls_xs) )
        | None -> (cls, cls_xs)
      in
      let subst =
        List.fold cls_xs ~init:subst ~f:(fun subst trm_xs ->
            Subst.compose1 ~key:trm_xs ~data:rep_us subst )
      in
      (cls, subst)
  | None -> (
    match rep_xs with
    | Some rep_xs ->
        let cls = rep_xs :: cls_us in
        let subst =
          List.fold cls_xs ~init:subst ~f:(fun subst trm_xs ->
              Subst.compose1 ~key:trm_xs ~data:rep_xs subst )
        in
        (cls, subst)
    | None -> (cls, subst) ) )
  |>
  [%Trace.retn fun {pf} (cls', subst') ->
    pf "cls: @[%a@]@ subst: @[%a@]" pp_diff_cls (cls, cls') Subst.pp_diff
      (subst, subst') ;
    subst_invariant us subst subst']

(** move equations between terms in [rep]'s class [cls] from [classes] to
    [subst] which can be expressed, after normalizing with [subst], as
    [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us] and [fv u ⊆ us] or else
    [fv u ⊆ us ∪ xs] *)
let solve_class us us_xs ~key:rep ~data:cls (classes, subst) =
  let classes0 = classes in
  [%Trace.call fun {pf} ->
    pf "rep: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Term.pp rep pp_cls cls
      Subst.pp subst]
  ;
  let cls, cls_not_ito_us_xs =
    List.partition_tf
      ~f:(fun e -> Var.Set.is_subset (Term.fv e) ~of_:us_xs)
      (rep :: cls)
  in
  let cls, subst = solve_interp_eqs us (cls, subst) in
  let cls, subst = solve_uninterp_eqs us (cls, subst) in
  let cls = List.rev_append cls_not_ito_us_xs cls in
  let cls =
    List.remove ~equal:Term.equal cls (Subst.norm subst rep)
    |> Option.value ~default:cls
  in
  let classes =
    if List.is_empty cls then Term.Map.remove classes rep
    else Term.Map.set classes ~key:rep ~data:cls
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
      | Ap2 (Sized, _, _) as m ->
          fold_uses_of r m ~init:uses ~f:(fun uses -> function
            | Ap3 (Extract, _, _, _) as e -> e :: uses | _ -> uses )
      | _ -> uses )
  in
  let find_extracts_at_off off =
    List.filter uses ~f:(fun use ->
        match (use : Term.t) with
        | Ap3 (Extract, _, o, _) -> implies r (Term.eq o off)
        | _ -> false )
  in
  let rec find_extracts full_rev_extracts rev_prefix off =
    List.fold (find_extracts_at_off off) ~init:full_rev_extracts
      ~f:(fun full_rev_extracts e ->
        match e with
        | Ap3 (Extract, Ap2 (Sized, n, _), o, l) ->
            let o_l = Term.add o l in
            if implies r (Term.eq n o_l) then
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
                  | _ when Var.Set.is_subset (Term.fv trm) ~of_:us ->
                      Some trm
                  | _ -> rep_ito_us )
            in
            Term.sized ~siz:(Term.seq_size_exn e) ~seq:rep_ito_us :: suffix ) )
    |> List.min_elt ~compare:[%compare: Term.t list]
  with
  | Some extracts ->
      let concat = Term.concat (Array.of_list extracts) in
      let subst = Subst.compose1 ~key:x ~data:concat subst in
      (classes, subst, us_xs)
  | None -> (classes, subst, us_xs)

let solve_for_xs r us xs (classes, subst, us_xs) =
  Var.Set.fold xs ~init:(classes, subst, us_xs)
    ~f:(fun (classes, subst, us_xs) x ->
      let x = Term.var x in
      if Subst.mem subst x then (classes, subst, us_xs)
      else solve_concat_extracts r us x (classes, subst, us_xs) )

(** move equations from [classes] to [subst] which can be expressed, after
    normalizing with [subst], as [x ↦ u] where [us ∪ xs ⊇ fv x ⊈ us]
    and [fv u ⊆ us] or else [fv u ⊆ us ∪ xs]. *)
let solve_classes r (classes, subst, us) xs =
  [%Trace.call fun {pf} ->
    pf "us: {@[%a@]}@ xs: {@[%a@]}" Var.Set.pp us Var.Set.pp xs]
  ;
  let rec solve_classes_ (classes0, subst0, us_xs) =
    let classes, subst =
      Term.Map.fold ~f:(solve_class us us_xs) classes0
        ~init:(classes0, subst0)
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
      pp_diff_clss (classes, classes')]

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
    pf "%a@ @[%a@]@ @[%a@]" pp_vss vss pp_classes r pp r ;
    invariant r]
  ;
  let us, vss =
    match vss with us :: vss -> (us, vss) | [] -> (Var.Set.empty, vss)
  in
  List.fold ~f:(solve_classes r) ~init:(classes r, Subst.empty, us) vss
  |> snd3
  |>
  [%Trace.retn fun {pf} subst ->
    pf "%a" Subst.pp subst ;
    Subst.iteri subst ~f:(fun ~key ~data ->
        assert (
          implies r (Term.eq key data)
          || fail "@[%a@ = %a@ not entailed by@ @[%a@]@]" Term.pp key
               Term.pp data pp_classes r () ) ;
        assert (
          List.fold_until vss ~init:us
            ~f:(fun us xs ->
              let us_xs = Var.Set.union us xs in
              let ks = Term.fv key in
              let ds = Term.fv data in
              if
                Var.Set.is_subset ks ~of_:us_xs
                && Var.Set.is_subset ds ~of_:us_xs
                && ( Var.Set.is_subset ds ~of_:us
                   || not (Var.Set.is_subset ks ~of_:us) )
              then Stop true
              else Continue us_xs )
            ~finish:(fun _ -> false) ) )]

let elim xs r = {r with rep= Subst.remove xs r.rep}
