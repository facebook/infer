(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Equality over uninterpreted functions and linear rational arithmetic *)

open Exp

(** Classification of Terms by Theory *)

type kind = Interpreted | Atomic | Uninterpreted
[@@deriving compare, equal]

let classify e =
  match (e : Trm.t) with
  | Arith _ | Sized _ | Extract _ | Concat _ -> Interpreted
  | Var _ | Z _ | Q _ | Ancestor _ -> Atomic
  | Splat _ | Select _ | Update _ | Record _ | Apply _ -> Uninterpreted

let interpreted e = equal_kind (classify e) Interpreted
let non_interpreted e = not (interpreted e)
let uninterpreted e = equal_kind (classify e) Uninterpreted

let rec max_solvables e =
  if non_interpreted e then Iter.return e
  else Iter.flat_map ~f:max_solvables (Trm.subtrms e)

let fold_max_solvables e s ~f = Iter.fold ~f (max_solvables e) s

(** Solution Substitutions *)
module Subst : sig
  type t [@@deriving compare, equal, sexp]

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
  val remove : Var.Set.t -> t -> t
  val map_entries : f:(Trm.t -> Trm.t) -> t -> t
  val to_iter : t -> (Trm.t * Trm.t) iter
  val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
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

  (** look up a term in a substitution *)
  let apply s a = Trm.Map.find a s |> Option.value ~default:a

  let rec subst_ s a = apply s (Trm.map ~f:(subst_ s) a)
  let subst s e = Term.map_trms ~f:(subst_ s) e

  (** apply a substitution to maximal non-interpreted subterms *)
  let rec norm s a =
    [%trace]
      ~call:(fun {pf} -> pf "%a" Trm.pp a)
      ~retn:(fun {pf} -> pf "%a" Trm.pp)
    @@ fun () -> if interpreted a then Trm.map ~f:(norm s) a else apply s a

  (** compose two substitutions *)
  let compose r s =
    [%Trace.call fun {pf} -> pf "%a@ %a" pp r pp s]
    ;
    let r' = Trm.Map.map_endo ~f:(norm s) r in
    Trm.Map.merge_endo r' s ~f:(fun key -> function
      | `Both (data_r, data_s) ->
          assert (
            Trm.equal data_s data_r
            || fail "domains intersect: %a" Trm.pp key () ) ;
          Some data_r
      | `Left data | `Right data -> Some data )
    |>
    [%Trace.retn fun {pf} r' ->
      pf "%a" pp_diff (r, r') ;
      assert (r' == r || not (equal r' r))]

  (** compose a substitution with a mapping *)
  let compose1 ~key ~data s =
    match (key : Trm.t) with
    | Z _ | Q _ -> s
    | _ ->
        if Trm.equal key data then s
        else compose s (Trm.Map.singleton key data)

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

  (** remove entries for vars *)
  let remove xs s =
    Var.Set.fold ~f:(fun x -> Trm.Map.remove (Trm.var x)) xs s

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
          match (key : Trm.t) with
          | Z _ | Q _ -> s
          | _ -> Trm.Map.add_exn ~key:key' ~data:data' s )

  (** Holds only if [true ⊢ ∃xs. e=f]. Clients assume
      [not (is_valid_eq xs e f)] implies [not (is_valid_eq ys e f)] for
      [ys ⊆ xs]. *)
  let is_valid_eq xs e f =
    let is_var_in xs e =
      Option.exists ~f:(fun x -> Var.Set.mem x xs) (Var.of_trm e)
    in
    ( is_var_in xs e
    || is_var_in xs f
    || (uninterpreted e && Iter.exists ~f:(is_var_in xs) (Trm.subtrms e))
    || (uninterpreted f && Iter.exists ~f:(is_var_in xs) (Trm.subtrms f)) )
    $> fun b ->
    [%Trace.info
      "is_valid_eq %a%a=%a = %b" Var.Set.pp_xs xs Trm.pp e Trm.pp f b]

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
    partition_valid_ empty xs s
end

(** Theory Solver *)

(** prefer representative terms that are minimal in the order s.t. Var <
    Sized < Extract < Concat < others, then using height of sequence
    nesting, and then using Trm.compare *)
let prefer e f =
  let rank e =
    match (e : Trm.t) with
    | Var _ -> 0
    | Sized _ -> 1
    | Extract _ -> 2
    | Concat _ -> 3
    | _ -> 4
  in
  let o = compare (rank e) (rank f) in
  if o <> 0 then o
  else
    let o = compare (Trm.height e) (Trm.height f) in
    if o <> 0 then o else Trm.compare e f

(** orient equations based on representative preference *)
let orient e f =
  match Sign.of_int (prefer e f) with
  | Neg -> Some (e, f)
  | Zero -> None
  | Pos -> Some (f, e)

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
  let xs = Var.Set.add x xs in
  (Trm.var x, (us, xs, s))

let solve_poly ?f p q s =
  [%trace]
    ~call:(fun {pf} -> pf "%a = %a" Trm.pp p Trm.pp q)
    ~retn:(fun {pf} -> function
      | Some (_, xs, s) -> pf "%a%a" Var.Set.pp_xs xs Subst.pp s
      | None -> pf "false" )
  @@ fun () ->
  match Trm.sub p q with
  | Z z -> if Z.equal Z.zero z then Some s else None
  | Var _ as var -> compose1 ?f ~var ~rep:Trm.zero s
  | p_q -> (
    match Trm.Arith.solve_zero_eq p_q with
    | Some (var, rep) ->
        compose1 ?f ~var:(Trm.arith var) ~rep:(Trm.arith rep) s
    | None -> compose1 ?f ~var:p_q ~rep:Trm.zero s )

(* α[o,l) = β ==> l = |β| ∧ α = (⟨n,c⟩[0,o) ^ β ^ ⟨n,c⟩[o+l,n-o-l)) where n
   = |α| and c fresh *)
let rec solve_extract ?f a o l b s =
  let n = Trm.seq_size_exn a in
  let c, s = fresh "c" s in
  let n_c = Trm.sized ~siz:n ~seq:c in
  let o_l = Trm.add o l in
  let n_o_l = Trm.sub n o_l in
  let c0 = Trm.extract ~seq:n_c ~off:Trm.zero ~len:o in
  let c1 = Trm.extract ~seq:n_c ~off:o_l ~len:n_o_l in
  let b, s =
    match Trm.seq_size b with
    | None -> (Trm.sized ~siz:l ~seq:b, Some s)
    | Some m -> (b, solve_ ?f l m s)
  in
  s >>= solve_ ?f a (Trm.concat [|c0; b; c1|])

(* α₀^…^αᵢ^αⱼ^…^αᵥ = β ==> |α₀^…^αᵥ| = |β| ∧ … ∧ αⱼ = β[n₀+…+nᵢ,nⱼ) ∧ …
   where nₓ ≡ |αₓ| and m = |β| *)
and solve_concat ?f a0V b m s =
  Iter.fold_until (Array.to_iter a0V) (s, Trm.zero)
    ~f:(fun aJ (s, oI) ->
      let nJ = Trm.seq_size_exn aJ in
      let oJ = Trm.add oI nJ in
      match solve_ ?f aJ (Trm.extract ~seq:b ~off:oI ~len:nJ) s with
      | Some s -> `Continue (s, oJ)
      | None -> `Stop None )
    ~finish:(fun (s, n0V) -> solve_ ?f n0V m s)

and solve_ ?f d e s =
  [%Trace.call fun {pf} ->
    pf "%a@[%a@ %a@ %a@]" Var.Set.pp_xs (snd3 s) Trm.pp d Trm.pp e Subst.pp
      (trd3 s)]
  ;
  ( match orient (norm s d) (norm s e) with
  (* e' = f' ==> true when e' ≡ f' *)
  | None -> Some s
  (* i = j ==> false when i ≠ j *)
  | Some (Z _, Z _) | Some (Q _, Q _) -> None
  (* ⟨0,a⟩ = β ==> a = β = ⟨⟩ *)
  | Some (Sized {siz= n; seq= a}, b) when Trm.equal n Trm.zero ->
      s |> solve_ ?f a (Trm.concat [||]) >>= solve_ ?f b (Trm.concat [||])
  | Some (b, Sized {siz= n; seq= a}) when Trm.equal n Trm.zero ->
      s |> solve_ ?f a (Trm.concat [||]) >>= solve_ ?f b (Trm.concat [||])
  (* v = ⟨n,a⟩ ==> v = a *)
  | Some ((Var _ as v), Sized {seq= a}) -> s |> solve_ ?f v a
  (* ⟨n,a⟩ = ⟨m,b⟩ ==> n = m ∧ a = β *)
  | Some (Sized {siz= n; seq= a}, Sized {siz= m; seq= b}) ->
      s |> solve_ ?f n m >>= solve_ ?f a b
  (* ⟨n,a⟩ = β ==> n = |β| ∧ a = β *)
  | Some (Sized {siz= n; seq= a}, b) ->
      ( match Trm.seq_size b with
      | None -> Some s
      | Some m -> solve_ ?f n m s )
      >>= solve_ ?f a b
  | Some ((Var _ as v), (Extract {len= l} as e)) ->
      if not (Var.Set.mem (Var.of_ v) (Trm.fv e)) then
        (* v = α[o,l) ==> v ↦ α[o,l) when v ∉ fv(α[o,l)) *)
        compose1 ?f ~var:v ~rep:e s
      else
        (* v = α[o,l) ==> α[o,l) ↦ ⟨l,v⟩ when v ∈ fv(α[o,l)) *)
        compose1 ?f ~var:e ~rep:(Trm.sized ~siz:l ~seq:v) s
  | Some ((Var _ as v), (Concat a0V as c)) ->
      if not (Var.Set.mem (Var.of_ v) (Trm.fv c)) then
        (* v = α₀^…^αᵥ ==> v ↦ α₀^…^αᵥ when v ∉ fv(α₀^…^αᵥ) *)
        compose1 ?f ~var:v ~rep:c s
      else
        (* v = α₀^…^αᵥ ==> ⟨|α₀^…^αᵥ|,v⟩ = α₀^…^αᵥ when v ∈ fv(α₀^…^αᵥ) *)
        let m = Trm.seq_size_exn c in
        solve_concat ?f a0V (Trm.sized ~siz:m ~seq:v) m s
  | Some ((Extract {len= l} as e), Concat a0V) -> solve_concat ?f a0V e l s
  | Some (Concat a0V, (Concat _ as c)) ->
      solve_concat ?f a0V c (Trm.seq_size_exn c) s
  | Some (Extract {seq= a; off= o; len= l}, e) -> solve_extract ?f a o l e s
  (* p = q ==> p-q = 0 *)
  | Some (((Arith _ | Z _ | Q _) as p), q | q, ((Arith _ | Z _ | Q _) as p))
    ->
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
  [%Trace.call fun {pf} -> pf "%a@ %a" Trm.pp d Trm.pp e]
  ;
  (solve_ ?f d e (us, xs, Subst.empty) |>= fun (_, xs, s) -> (xs, s))
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
  let add elt rep cls =
    if Trm.equal elt rep then cls
    else Trm.Map.add_multi ~key:rep ~data:elt cls
  in
  Subst.fold r.rep Trm.Map.empty ~f:(fun ~key:elt ~data:rep cls ->
      match classify elt with
      | Interpreted | Atomic -> add elt rep cls
      | Uninterpreted -> add (Trm.map ~f:(Subst.apply r.rep) elt) rep cls )

let cls_of r e =
  let e' = Subst.apply r.rep e in
  Trm.Map.find e' (classes r) |> Option.value ~default:[e']

(** Pretty-printing *)

let pp_raw fs {sat; rep} =
  let pp_alist pp_k pp_v fs alist =
    let pp_assoc fs (k, v) =
      Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_k k pp_v (k, v)
    in
    Format.fprintf fs "[@[<hv>%a@]]" (List.pp ";@ " pp_assoc) alist
  in
  let pp_term_v fs (k, v) = if not (Trm.equal k v) then Trm.pp fs v in
  Format.fprintf fs "@[{@[<hv>sat= %b;@ rep= %a@]}@]" sat
    (pp_alist Trm.pp pp_term_v)
    (Iter.to_list (Subst.to_iter rep))

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

let ppx_cls x = List.pp "@ = " (Trm.ppx x)
let pp_cls = ppx_cls (fun _ -> None)
let pp_diff_cls = List.pp_diff ~cmp:Trm.compare "@ = " Trm.pp

let ppx_classes x fs clss =
  List.pp "@ @<2>∧ "
    (fun fs (rep, cls) ->
      Format.fprintf fs "@[%a@ = %a@]" (Trm.ppx x) rep (ppx_cls x)
        (List.sort ~cmp:Trm.compare cls) )
    fs
    (Iter.to_list (Trm.Map.to_iter clss))

let pp_classes fs r = ppx_classes (fun _ -> None) fs (classes r)

let pp_diff_clss =
  Trm.Map.pp_diff ~eq:(List.equal Trm.equal) Trm.pp pp_cls pp_diff_cls

let pp fs r = ppx_classes (fun _ -> None) fs (classes r)

let ppx var_strength fs clss noneqs =
  let first = Trm.Map.is_empty clss in
  if not first then Format.fprintf fs "  " ;
  ppx_classes var_strength fs clss ;
  List.pp
    ~pre:(if first then "@[  " else "@ @[@<2>∧ ")
    "@ @<2>∧ " (Fml.ppx var_strength) fs noneqs ~suf:"@]" ;
  first && List.is_empty noneqs

(** Basic queries *)

(** test membership in carrier *)
let in_car r e = Subst.mem e r.rep

(** congruent specialized to assume subterms of [a'] are [Subst.norm]alized
    wrt [r] (or canonized) *)
let semi_congruent r a' b = Trm.equal a' (Trm.map ~f:(Subst.norm r.rep) b)

(** terms are congruent if equal after normalizing subterms *)
let congruent r a b = semi_congruent r (Trm.map ~f:(Subst.norm r.rep) a) b

(** Invariant *)

let pre_invariant r =
  let@ () = Invariant.invariant [%here] r [%sexp_of: t] in
  Subst.iteri r.rep ~f:(fun ~key:trm ~data:_ ->
      (* no interpreted terms in carrier *)
      assert (non_interpreted trm || fail "non-interp %a" Trm.pp trm ()) ;
      (* carrier is closed under subterms *)
      Iter.iter (Trm.subtrms trm) ~f:(fun subtrm ->
          assert (
            (not (non_interpreted subtrm))
            || (match subtrm with Z _ | Q _ -> true | _ -> false)
            || in_car r subtrm
            || fail "@[subterm %a@ of %a@ not in carrier of@ %a@]" Trm.pp
                 subtrm Trm.pp trm pp r () ) ) )

let invariant r =
  let@ () = Invariant.invariant [%here] r [%sexp_of: t] in
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

(** Core operations *)

let empty =
  let rep = Subst.empty in
  (* let rep = Option.get_exn (Subst.extend Trm.true_ rep) in
   * let rep = Option.get_exn (Subst.extend Trm.false_ rep) in *)
  {xs= Var.Set.empty; sat= true; rep} |> check invariant

let unsat = {empty with sat= false}

(** [lookup r a] is [b'] if [a ~ b = b'] for some equation [b = b'] in rep *)
let lookup r a =
  ([%Trace.call fun {pf} -> pf "%a" Trm.pp a]
  ;
  Iter.find_map (Subst.to_iter r.rep) ~f:(fun (b, b') ->
      Option.return_if (semi_congruent r a b) b' )
  |> Option.value ~default:a)
  |>
  [%Trace.retn fun {pf} -> pf "%a" Trm.pp]

(** rewrite a term into canonical form using rep and, for non-interpreted
    terms, congruence composed with rep *)
let rec canon r a =
  [%Trace.call fun {pf} -> pf "%a" Trm.pp a]
  ;
  ( match classify a with
  | Atomic -> Subst.apply r.rep a
  | Interpreted -> Trm.map ~f:(canon r) a
  | Uninterpreted -> (
      let a' = Trm.map ~f:(canon r) a in
      match classify a' with
      | Atomic -> Subst.apply r.rep a'
      | Interpreted -> a'
      | Uninterpreted -> lookup r a' ) )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Trm.pp]

let canon_f r b =
  [%trace]
    ~call:(fun {pf} -> pf "%a@ %a" Fml.pp b pp r)
    ~retn:(fun {pf} -> pf "%a" Fml.pp)
  @@ fun () -> Fml.map_trms ~f:(canon r) b

let rec extend_ a r =
  match (a : Trm.t) with
  | Z _ | Q _ -> r
  | _ -> (
      if interpreted a then Iter.fold ~f:extend_ (Trm.subtrms a) r
      else
        (* add uninterpreted terms *)
        match Subst.extend a r with
        (* and their subterms if newly added *)
        | Some r -> Iter.fold ~f:extend_ (Trm.subtrms a) r
        | None -> r )

(** add a term to the carrier *)
let extend a r =
  let rep = extend_ a r.rep in
  if rep == r.rep then r else {r with rep} |> check pre_invariant

let merge us a b r =
  [%Trace.call fun {pf} -> pf "%a@ %a@ %a" Trm.pp a Trm.pp b pp r]
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
    if Trm.equal a' b' then r
    else
      let r = merge us a' b' r in
      match (a, b) with
      | (Var _ as v), _ when not (in_car r0 v) -> r
      | _, (Var _ as v) when not (in_car r0 v) -> r
      | _ -> close us r

let extract_xs r = (r.xs, {r with xs= Var.Set.empty})

(** Exposed interface *)

let is_empty {sat; rep} =
  sat && Subst.for_alli rep ~f:(fun ~key:a ~data:a' -> Trm.equal a a')

let is_unsat {sat} = not sat

let implies r b =
  [%Trace.call fun {pf} -> pf "%a@ %a" Fml.pp b pp r]
  ;
  Fml.equal Fml.tt (canon_f r b)
  |>
  [%Trace.retn fun {pf} -> pf "%b"]

let refutes r b = Fml.equal Fml.ff (canon_f r b)
let normalize r e = Term.map_trms ~f:(canon r) e

let class_of r e =
  match Term.get_trm (normalize r e) with
  | Some e' ->
      List.map ~f:Term.of_trm (e' :: Trm.Map.find_multi e' (classes r))
  | None -> []

let diff_classes r s =
  Trm.Map.filter_mapi (classes r) ~f:(fun ~key:rep ~data:cls ->
      match
        List.filter cls ~f:(fun exp -> not (implies s (Fml.eq rep exp)))
      with
      | [] -> None
      | cls -> Some cls )

let ppx_diff var_strength fs parent_ctx fml ctx =
  let fml' = canon_f ctx fml in
  ppx var_strength fs
    (diff_classes ctx parent_ctx)
    (if Fml.(equal tt fml') then [] else [fml'])

let fold_uses_of r t s ~f =
  let rec fold_ e s ~f =
    let s =
      Iter.fold (Trm.subtrms e) s ~f:(fun sub s ->
          if Trm.equal t sub then f s e else s )
    in
    if interpreted e then Iter.fold ~f:(fold_ ~f) (Trm.subtrms e) s else s
  in
  Subst.fold r.rep s ~f:(fun ~key:trm ~data:rep s ->
      fold_ ~f trm (fold_ ~f rep s) )

let apply_subst us s r =
  [%Trace.call fun {pf} -> pf "%a@ %a" Subst.pp s pp r]
  ;
  Trm.Map.fold (classes r) empty ~f:(fun ~key:rep ~data:cls r ->
      let rep' = Subst.subst_ s rep in
      List.fold cls r ~f:(fun trm r ->
          let trm' = Subst.subst_ s trm in
          and_eq_ us trm' rep' r ) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (xs, r') ->
    pf "%a%a" Var.Set.pp_xs xs pp_diff (r, r') ;
    invariant r']

let union us r s =
  [%Trace.call fun {pf} -> pf "@[<hv 1>   %a@ @<2>∧ %a@]" pp r pp s]
  ;
  ( if not r.sat then r
  else if not s.sat then s
  else
    let s, r =
      if Subst.length s.rep <= Subst.length r.rep then (s, r) else (r, s)
    in
    Subst.fold s.rep r ~f:(fun ~key:e ~data:e' r -> and_eq_ us e e' r) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (_, r') ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let inter us r s =
  [%Trace.call fun {pf} -> pf "@[<hv 1>   %a@ @<2>∨ %a@]" pp r pp s]
  ;
  ( if not s.sat then r
  else if not r.sat then s
  else
    let merge_mems rs r s =
      Trm.Map.fold (classes s) rs ~f:(fun ~key:rep ~data:cls rs ->
          List.fold cls
            ([rep], rs)
            ~f:(fun exp (reps, rs) ->
              match
                List.find ~f:(fun rep -> implies r (Fml.eq exp rep)) reps
              with
              | Some rep -> (reps, and_eq_ us exp rep rs)
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

let rec add_ us b r =
  match (b : Fml.t) with
  | Tt -> r
  | Not Tt -> unsat
  | And {pos; neg} -> Fml.fold_pos_neg ~f:(add_ us) ~pos ~neg r
  | Eq (d, e) -> and_eq_ us d e r
  | Eq0 e -> and_eq_ us Trm.zero e r
  | Pos _ | Not _ | Or _ | Iff _ | Cond _ | Lit _ -> r

let add us b r =
  [%Trace.call fun {pf} -> pf "%a@ %a" Fml.pp b pp r]
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

let rename r sub =
  [%Trace.call fun {pf} -> pf "%a" pp r]
  ;
  let rep =
    Subst.map_entries ~f:(Trm.map_vars ~f:(Var.Subst.apply sub)) r.rep
  in
  (if rep == r.rep then r else {r with rep})
  |>
  [%Trace.retn fun {pf} r' ->
    pf "%a" pp_diff (r, r') ;
    invariant r']

let trms r =
  Iter.flat_map ~f:(fun (k, v) -> Iter.doubleton k v) (Subst.to_iter r.rep)

let vars r = Iter.flat_map ~f:Trm.vars (trms r)
let fv r = Var.Set.of_iter (vars r)

(** Existential Witnessing and Elimination *)

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
  [%Trace.call fun {pf} -> pf "%a = %a" Trm.pp p' Trm.pp q']
  ;
  let diff = Trm.sub p' q' in
  let max_solvables_not_ito_us =
    fold_max_solvables diff Zero ~f:(fun solvable_subterm -> function
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

let solve_seq_eq us e' f' subst =
  [%Trace.call fun {pf} -> pf "%a = %a" Trm.pp e' Trm.pp f']
  ;
  let f x u =
    (not (Var.Set.subset (Trm.fv x) ~of_:us))
    && Var.Set.subset (Trm.fv u) ~of_:us
  in
  let solve_concat ms n a =
    let a, n =
      match Trm.seq_size a with
      | Some n -> (a, n)
      | None -> (Trm.sized ~siz:n ~seq:a, n)
    in
    let+ _, xs, s = solve_concat ~f ms a n (us, Var.Set.empty, subst) in
    assert (Var.Set.is_empty xs) ;
    s
  in
  ( match ((e' : Trm.t), (f' : Trm.t)) with
  | (Concat ms as c), a when f c a -> solve_concat ms (Trm.seq_size_exn c) a
  | a, (Concat ms as c) when f c a -> solve_concat ms (Trm.seq_size_exn c) a
  | (Sized {seq= Var _ as v} as m), u when f m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
  | u, (Sized {seq= Var _ as v} as m) when f m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
  | _ -> None )
  |>
  [%Trace.retn fun {pf} subst' ->
    pf "@[%a@]" Subst.pp_diff (subst, Option.value subst' ~default:subst)]

let solve_interp_eq us e' (cls, subst) =
  [%Trace.call fun {pf} ->
    pf "trm: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Trm.pp e' pp_cls cls
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
  { rep_us: Trm.t option  (** rep, that is ito us, for class *)
  ; cls_us: Trm.t list  (** cls that is ito us, or interpreted *)
  ; rep_xs: Trm.t option  (** rep, that is *not* ito us, for class *)
  ; cls_xs: Trm.t list  (** cls that is *not* ito us *) }

let dom_trm e =
  match (e : Trm.t) with
  | Sized {seq= Var _ as v} -> Some v
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
    [%compare: kind * Trm.t] (classify e, e) (classify f, f)
  in
  let {rep_us; cls_us; rep_xs; cls_xs} =
    List.fold cls {rep_us= None; cls_us= []; rep_xs= None; cls_xs= []}
      ~f:(fun trm ({rep_us; cls_us; rep_xs; cls_xs} as s) ->
        if Var.Set.subset (Trm.fv trm) ~of_:us then
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
        List.fold cls_xs subst ~f:(fun trm_xs subst ->
            Subst.compose1 ~key:trm_xs ~data:rep_us subst )
      in
      (cls, subst)
  | None -> (
    match rep_xs with
    | Some rep_xs ->
        let cls = rep_xs :: cls_us in
        let subst =
          List.fold cls_xs subst ~f:(fun trm_xs subst ->
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
    pf "rep: @[%a@]@ cls: @[%a@]@ subst: @[%a@]" Trm.pp rep pp_cls cls
      Subst.pp subst]
  ;
  let cls, cls_not_ito_us_xs =
    List.partition
      ~f:(fun e -> Var.Set.subset (Trm.fv e) ~of_:us_xs)
      (rep :: cls)
  in
  let cls, subst = solve_interp_eqs us (cls, subst) in
  let cls, subst = solve_uninterp_eqs us (cls, subst) in
  let cls = List.rev_append cls_not_ito_us_xs cls in
  let cls = List.remove ~eq:Trm.equal (Subst.norm subst rep) cls in
  let classes =
    if List.is_empty cls then Trm.Map.remove rep classes
    else Trm.Map.add ~key:rep ~data:cls classes
  in
  (classes, subst)
  |>
  [%Trace.retn fun {pf} (classes', subst') ->
    pf "subst: @[%a@]@ classes: @[%a@]" Subst.pp_diff (subst, subst')
      pp_diff_clss (classes0, classes')]

let solve_concat_extracts_eq r x =
  [%Trace.call fun {pf} -> pf "%a@ %a" Trm.pp x pp r]
  ;
  let uses =
    fold_uses_of r x [] ~f:(fun uses -> function
      | Sized _ as m ->
          fold_uses_of r m uses ~f:(fun uses -> function
            | Extract _ as e -> e :: uses | _ -> uses )
      | _ -> uses )
  in
  let find_extracts_at_off off =
    List.filter uses ~f:(fun use ->
        match (use : Trm.t) with
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
              List.fold (cls_of r e) None ~f:(fun trm rep_ito_us ->
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
    pf "us: {@[%a@]}@ xs: {@[%a@]}" Var.Set.pp us Var.Set.pp xs]
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
  List.fold ~f:(solve_classes r) vss (classes r, Subst.empty, us) |> snd3
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

let elim xs r = {r with rep= Subst.remove xs r.rep}

(*
 * Replay debugging
 *)

type call =
  | Add of Var.Set.t * Formula.t * t
  | Union of Var.Set.t * t * t
  | Inter of Var.Set.t * t * t
  | InterN of Var.Set.t * t list
  | Rename of t * Var.Subst.t
  | Is_unsat of t
  | Implies of t * Formula.t
  | Refutes of t * Formula.t
  | Normalize of t * Term.t
  | Apply_subst of Var.Set.t * Subst.t * t
  | Solve_for_vars of Var.Set.t list * t
  | Elim of Var.Set.t * t
[@@deriving sexp]

let replay c =
  match call_of_sexp (Sexp.of_string c) with
  | Add (us, e, r) -> add us e r |> ignore
  | Union (us, r, s) -> union us r s |> ignore
  | Inter (us, r, s) -> inter us r s |> ignore
  | InterN (us, rs) -> interN us rs |> ignore
  | Rename (r, s) -> rename r s |> ignore
  | Is_unsat r -> is_unsat r |> ignore
  | Implies (r, f) -> implies r f |> ignore
  | Refutes (r, f) -> refutes r f |> ignore
  | Normalize (r, e) -> normalize r e |> ignore
  | Apply_subst (us, s, r) -> apply_subst us s r |> ignore
  | Solve_for_vars (vss, r) -> solve_for_vars vss r |> ignore
  | Elim (ks, r) -> elim ks r |> ignore

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
      let sexp = sexp_of_call (call ()) in
      raise (Replay (exn, bt, sexp))

let add_tmr = Timer.create "add" ~at_exit:report
let union_tmr = Timer.create "union" ~at_exit:report
let inter_tmr = Timer.create "inter" ~at_exit:report
let interN_tmr = Timer.create "interN" ~at_exit:report
let rename_tmr = Timer.create "rename" ~at_exit:report
let is_unsat_tmr = Timer.create "is_unsat" ~at_exit:report
let implies_tmr = Timer.create "implies" ~at_exit:report
let refutes_tmr = Timer.create "refutes" ~at_exit:report
let normalize_tmr = Timer.create "normalize" ~at_exit:report
let apply_subst_tmr = Timer.create "apply_subst" ~at_exit:report
let solve_for_vars_tmr = Timer.create "solve_for_vars" ~at_exit:report
let elim_tmr = Timer.create "elim" ~at_exit:report

let add us e r =
  wrap add_tmr (fun () -> add us e r) (fun () -> Add (us, e, r))

let union us r s =
  wrap union_tmr (fun () -> union us r s) (fun () -> Union (us, r, s))

let inter us r s =
  wrap inter_tmr (fun () -> inter us r s) (fun () -> Inter (us, r, s))

let interN us rs =
  wrap interN_tmr (fun () -> interN us rs) (fun () -> InterN (us, rs))

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

let elim ks r = wrap elim_tmr (fun () -> elim ks r) (fun () -> Elim (ks, r))
