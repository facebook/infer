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
let uninterpreted e = equal_kind (classify e) Uninterpreted

let rec fold_max_solvables e ~init ~f =
  if non_interpreted e then f e init
  else Term.fold e ~init ~f:(fun d s -> fold_max_solvables ~f d ~init:s)

let rec iter_max_solvables e ~f =
  if non_interpreted e then f e else Term.iter ~f:(iter_max_solvables ~f) e

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
    match classify a with
    | Interpreted -> Term.map ~f:(norm s) a
    | Simplified -> apply s (Term.map ~f:(norm s) a)
    | Atomic | Uninterpreted -> apply s a

  (** compose two substitutions *)
  let compose r s =
    let r' = Term.Map.map ~f:(norm s) r in
    Term.Map.merge_skewed r' s ~combine:(fun ~key v1 v2 ->
        if Term.equal v1 v2 then v1
        else fail "domains intersect: %a" Term.pp key () )

  (** compose a substitution with a mapping *)
  let compose1 ~key ~data s =
    if Term.equal key data then s
    else compose s (Term.Map.set Term.Map.empty ~key ~data)

  (** add an identity entry if the term is not already present *)
  let extend e s =
    let exception Found in
    match
      Term.Map.update s e ~f:(function
        | Some _ -> Exn.raise_without_backtrace Found
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
        else Term.Map.remove s key |> Term.Map.add_exn ~key:key' ~data:data'
    )

  (** Holds only if [true ⊢ ∃xs. e=f]. Clients assume
      [not (is_valid_eq xs e f)] implies [not (is_valid_eq ys e f)] for
      [ys ⊆ xs]. *)
  let is_valid_eq xs e f =
    let is_var_in xs e =
      Option.exists ~f:(Var.Set.mem xs) (Var.of_term e)
    in
    ( is_var_in xs e || is_var_in xs f
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

(** orient equations s.t. Var < Memory < Extract < Concat < others, then
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
          1 + IArray.fold ~init:0 ~f:(fun h x -> max h (height x)) xs
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

let extend ?f ~var ~rep (us, xs, s) =
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
  | Var _ as var -> extend ?f ~var ~rep:Term.zero s
  | p_q -> (
    match Term.solve_zero_eq p_q with
    | Some (var, rep) -> extend ?f ~var ~rep s
    | None -> extend ?f ~var:p_q ~rep:Term.zero s )

(* α[o,l) = β ==> l = |β| ∧ α = (⟨n,c⟩[0,o) ^ β ^ ⟨n,c⟩[o+l,n-o-l)) where n
   = |α| and c fresh *)
let rec solve_extract ?f a o l b s =
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
    | Some m -> (b, solve_ ?f l m s)
  in
  s >>= solve_ ?f a (Term.concat [|c0; b; c1|])

(* α₀^…^αᵢ^αⱼ^…^αᵥ = β ==> |α₀^…^αᵥ| = |β| ∧ … ∧ αⱼ = β[n₀+…+nᵢ,nⱼ) ∧ …
   where nₓ ≡ |αₓ| and m = |β| *)
and solve_concat ?f a0V b m s =
  IArray.fold_until a0V ~init:(s, Term.zero)
    ~f:(fun (s, oI) aJ ->
      let nJ = Term.agg_size_exn aJ in
      let oJ = Term.add oI nJ in
      match solve_ ?f aJ (Term.extract ~agg:b ~off:oI ~len:nJ) s with
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
  | Some (Integer _, Integer _) -> None
  (* ⟨0,a⟩ = β ==> a = β = ⟨⟩ *)
  | Some (Ap2 (Memory, n, a), b) when Term.equal n Term.zero ->
      s |> solve_ ?f a (Term.concat [||]) >>= solve_ ?f b (Term.concat [||])
  | Some (b, Ap2 (Memory, n, a)) when Term.equal n Term.zero ->
      s |> solve_ ?f a (Term.concat [||]) >>= solve_ ?f b (Term.concat [||])
  (* v = ⟨n,a⟩ ==> v = a *)
  | Some ((Var _ as v), Ap2 (Memory, _, a)) -> s |> solve_ ?f v a
  (* ⟨n,a⟩ = ⟨m,b⟩ ==> n = m ∧ a = β *)
  | Some (Ap2 (Memory, n, a), Ap2 (Memory, m, b)) ->
      s |> solve_ ?f n m >>= solve_ ?f a b
  (* ⟨n,a⟩ = β ==> n = |β| ∧ a = β *)
  | Some (Ap2 (Memory, n, a), b) ->
      ( match Term.agg_size b with
      | None -> Some s
      | Some m -> solve_ ?f n m s )
      >>= solve_ ?f a b
  | Some ((Var _ as v), (Ap3 (Extract, _, _, l) as e)) ->
      if not (Var.Set.mem (Term.fv e) (Var.of_ v)) then
        (* v = α[o,l) ==> v ↦ α[o,l) when v ∉ fv(α[o,l)) *)
        extend ?f ~var:v ~rep:e s
      else
        (* v = α[o,l) ==> α[o,l) ↦ ⟨l,v⟩ when v ∈ fv(α[o,l)) *)
        extend ?f ~var:e ~rep:(Term.memory ~siz:l ~arr:v) s
  | Some ((Var _ as v), (ApN (Concat, a0V) as c)) ->
      if not (Var.Set.mem (Term.fv c) (Var.of_ v)) then
        (* v = α₀^…^αᵥ ==> v ↦ α₀^…^αᵥ when v ∉ fv(α₀^…^αᵥ) *)
        extend ?f ~var:v ~rep:c s
      else
        (* v = α₀^…^αᵥ ==> ⟨|α₀^…^αᵥ|,v⟩ = α₀^…^αᵥ when v ∈ fv(α₀^…^αᵥ) *)
        let m = Term.agg_size_exn c in
        solve_concat ?f a0V (Term.memory ~siz:m ~arr:v) m s
  | Some ((Ap3 (Extract, _, _, l) as e), ApN (Concat, a0V)) ->
      solve_concat ?f a0V e l s
  | Some (ApN (Concat, a0V), (ApN (Concat, _) as c)) ->
      solve_concat ?f a0V c (Term.agg_size_exn c) s
  | Some (Ap3 (Extract, a, o, l), e) -> solve_extract ?f a o l e s
  (* p = q ==> p-q = 0 *)
  | Some
      ( ((Add _ | Mul _ | Integer _) as p), q
      | q, ((Add _ | Mul _ | Integer _) as p) ) ->
      solve_poly ?f p q s
  | Some (rep, var) ->
      assert (non_interpreted var) ;
      assert (non_interpreted rep) ;
      extend ?f ~var ~rep s )
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

let classes r =
  let add key data cls =
    if Term.equal key data then cls
    else Term.Map.add_multi cls ~key:data ~data:key
  in
  Subst.fold r.rep ~init:Term.Map.empty ~f:(fun ~key ~data cls ->
      match classify key with
      | Interpreted | Atomic -> add key data cls
      | Simplified | Uninterpreted ->
          add (Term.map ~f:(Subst.apply r.rep) key) data cls )

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

let ppx_clss x fs cs =
  List.pp "@ @<2>∧ "
    (fun fs (key, data) ->
      Format.fprintf fs "@[%a@ = %a@]" (Term.ppx x) key (ppx_cls x)
        (List.sort ~compare:Term.compare data) )
    fs (Term.Map.to_alist cs)

let pp_clss fs cs = ppx_clss (fun _ -> None) fs cs

let pp_diff_clss =
  Term.Map.pp_diff ~data_equal:(List.equal Term.equal) Term.pp pp_cls
    pp_diff_cls

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

let false_ = {true_ with sat= false}

(** terms are congruent if equal after normalizing subterms *)
let congruent r a b =
  Term.equal
    (Term.map ~f:(Subst.norm r.rep) a)
    (Term.map ~f:(Subst.norm r.rep) b)

(** [lookup r a] is [b'] if [a ~ b = b'] for some equation [b = b'] in rep *)
let lookup r a =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp a pp r]
  ;
  ( with_return
  @@ fun {return} ->
  (* congruent specialized to assume [a] canonized and [b] non-interpreted *)
  let semi_congruent r a b =
    Term.equal a (Term.map ~f:(Subst.apply r.rep) b)
  in
  Subst.iteri r.rep ~f:(fun ~key ~data ->
      if semi_congruent r a key then return data ) ;
  a )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Term.pp]

(** rewrite a term into canonical form using rep and, for non-interpreted
    terms, congruence composed with rep *)
let rec canon r a =
  [%Trace.call fun {pf} -> pf "%a@ %a" Term.pp a pp r]
  ;
  ( match classify a with
  | Atomic -> Subst.apply r.rep a
  | Interpreted -> Term.map ~f:(canon r) a
  | Simplified | Uninterpreted -> (
      let a' = Term.map ~f:(canon r) a in
      match classify a' with
      | Atomic -> Subst.apply r.rep a'
      | Interpreted -> Term.map ~f:(canon r) a'
      | Simplified | Uninterpreted -> lookup r a' ) )
  |>
  [%Trace.retn fun {pf} -> pf "%a" Term.pp]

let rec extend_ a r =
  match classify a with
  | Interpreted | Simplified -> Term.fold ~f:extend_ a ~init:r
  | Uninterpreted -> (
    match Subst.extend a r with
    | Some r -> Term.fold ~f:extend_ a ~init:r
    | None -> r )
  | Atomic -> r

(** add a term to the carrier *)
let extend a r =
  let rep = extend_ a r.rep in
  if rep == r.rep then r else {r with rep} |> check invariant

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
    invariant r']

(** find an unproved equation between congruent terms *)
let find_missing r =
  with_return
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

let apply_subst us s r =
  [%Trace.call fun {pf} -> pf "%a@ %a" Subst.pp s pp r]
  ;
  Term.Map.fold (classes r) ~init:true_ ~f:(fun ~key:rep ~data:cls r ->
      let rep' = Subst.subst s rep in
      List.fold cls ~init:r ~f:(fun r trm ->
          let trm' = Subst.subst s trm in
          and_eq us trm' rep' r ) )
  |> extract_xs
  |>
  [%Trace.retn fun {pf} (xs, r') -> pf "%a%a" Var.Set.pp_xs xs pp r']

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
      Term.Map.fold (classes s) ~init:rs ~f:(fun ~key:rep ~data:cls rs ->
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

let orN us rs =
  match rs with
  | [] -> (us, false_)
  | r :: rs -> List.fold ~f:(fun (us, s) r -> or_ us s r) ~init:(us, r) rs

let rec and_term_ us e r =
  let eq_false b r = and_eq us b Term.false_ r in
  match (e : Term.t) with
  | Integer {data} -> if Z.is_false data then false_ else r
  | Ap2 (And, a, b) -> and_term_ us a (and_term_ us b r)
  | Ap2 (Eq, a, b) -> and_eq us a b r
  | Ap2 (Xor, Integer {data}, a) when Z.is_true data -> eq_false a r
  | Ap2 (Xor, a, Integer {data}) when Z.is_true data -> eq_false a r
  | _ -> r

let and_term us e r = and_term_ us e r |> extract_xs

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

let fv e = fold_vars e ~f:Var.Set.add ~init:Var.Set.empty
let pp_classes fs r = pp_clss fs (classes r)
let ppx_classes x fs r = ppx_clss x fs (classes r)

let ppx_classes_diff x fs (r, s) =
  let clss = classes s in
  let clss =
    Term.Map.filter_mapi clss ~f:(fun ~key:rep ~data:cls ->
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
        (List.dedup_and_sort ~compare:Term.compare cls) )
    fs (Term.Map.to_alist clss)

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
  let diff = Term.sub p' q' in
  let max_solvables_not_ito_us =
    fold_max_solvables diff ~init:Zero ~f:(fun solvable_subterm -> function
      | Many -> Many
      | zom when Var.Set.is_subset (Term.fv solvable_subterm) ~of_:us -> zom
      | One _ -> Many
      | Zero -> One solvable_subterm )
  in
  match max_solvables_not_ito_us with
  | One kill ->
      let+ kill, keep = Term.solve_zero_eq diff ~for_:kill in
      Subst.compose1 ~key:kill ~data:keep subst
  | Many | Zero -> None

let solve_memory_eq us e' f' subst =
  [%Trace.call fun {pf} -> pf "%a = %a" Term.pp e' Term.pp f']
  ;
  let f x u =
    (not (Var.Set.is_subset (Term.fv x) ~of_:us))
    && Var.Set.is_subset (Term.fv u) ~of_:us
  in
  let solve_concat ms n a =
    let a, n =
      match Term.agg_size a with
      | Some n -> (a, n)
      | None -> (Term.memory ~siz:n ~arr:a, n)
    in
    let+ _, xs, s = solve_concat ~f ms a n (us, Var.Set.empty, subst) in
    assert (Var.Set.is_empty xs) ;
    s
  in
  ( match ((e' : Term.t), (f' : Term.t)) with
  | (ApN (Concat, ms) as c), a when f c a ->
      solve_concat ms (Term.agg_size_exn c) a
  | a, (ApN (Concat, ms) as c) when f c a ->
      solve_concat ms (Term.agg_size_exn c) a
  | (Ap2 (Memory, _, (Var _ as v)) as m), u when f m u ->
      Some (Subst.compose1 ~key:v ~data:u subst)
  | u, (Ap2 (Memory, _, (Var _ as v)) as m) when f m u ->
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
      match solve_memory_eq us e' f' subst with
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
  | Ap2 (Memory, _, (Var _ as v)) -> Some v
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
                  | _ when Var.Set.is_subset (Term.fv trm) ~of_:us ->
                      Some trm
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
  |> solve_classes_ |> solve_for_xs r us xs
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
  [%Trace.call fun {pf} -> pf "%a@ @[%a@]" pp_vss vss pp_classes r]
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
          entails_eq r key data
          || fail "@[%a = %a not entailed by@ %a@]" Term.pp key Term.pp data
               pp_classes r () ) ;
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

(* Replay debugging *)

type call =
  | Normalize of t * Term.t
  | And_eq of Var.Set.t * Term.t * Term.t * t
  | And_term of Var.Set.t * Term.t * t
  | And_ of Var.Set.t * t * t
  | Or_ of Var.Set.t * t * t
  | OrN of Var.Set.t * t list
  | Rename of t * Var.Subst.t
  | Apply_subst of Var.Set.t * Subst.t * t
  | Solve_for_vars of Var.Set.t list * t
[@@deriving sexp]

let replay c =
  match call_of_sexp (Sexp.of_string c) with
  | Normalize (r, e) -> normalize r e |> ignore
  | And_eq (us, a, b, r) -> and_eq us a b r |> ignore
  | And_term (us, e, r) -> and_term us e r |> ignore
  | And_ (us, r, s) -> and_ us r s |> ignore
  | Or_ (us, r, s) -> or_ us r s |> ignore
  | OrN (us, rs) -> orN us rs |> ignore
  | Rename (r, s) -> rename r s |> ignore
  | Apply_subst (us, s, r) -> apply_subst us s r |> ignore
  | Solve_for_vars (vss, r) -> solve_for_vars vss r |> ignore

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
          Format.eprintf "@\n%a@\n@." Sexp.pp_hum (sexp_of_call (call ())) )
    ) ;
    r
  in
  if not [%debug] then f ()
  else
    try f () with exn -> raise_s ([%sexp_of: exn * call] (exn, call ()))

let normalize_tmr = Timer.create "normalize" ~at_exit:report
let and_eq_tmr = Timer.create "and_eq" ~at_exit:report
let and_term_tmr = Timer.create "and_term" ~at_exit:report
let and_tmr = Timer.create "and_" ~at_exit:report
let or_tmr = Timer.create "or_" ~at_exit:report
let orN_tmr = Timer.create "orN" ~at_exit:report
let rename_tmr = Timer.create "rename" ~at_exit:report
let apply_subst_tmr = Timer.create "apply_subst" ~at_exit:report
let solve_for_vars_tmr = Timer.create "solve_for_vars" ~at_exit:report

let normalize r e =
  wrap normalize_tmr (fun () -> normalize r e) (fun () -> Normalize (r, e))

let and_eq us a b r =
  wrap and_eq_tmr
    (fun () -> and_eq us a b r)
    (fun () -> And_eq (us, a, b, r))

let and_term us e r =
  wrap and_term_tmr
    (fun () -> and_term us e r)
    (fun () -> And_term (us, e, r))

let and_ us r s =
  wrap and_tmr (fun () -> and_ us r s) (fun () -> And_ (us, r, s))

let or_ us r s =
  wrap or_tmr (fun () -> or_ us r s) (fun () -> Or_ (us, r, s))

let orN us rs = wrap orN_tmr (fun () -> orN us rs) (fun () -> OrN (us, rs))

let rename r s =
  wrap rename_tmr (fun () -> rename r s) (fun () -> Rename (r, s))

let apply_subst us s r =
  wrap apply_subst_tmr
    (fun () -> apply_subst us s r)
    (fun () -> Apply_subst (us, s, r))

let solve_for_vars vss r =
  wrap solve_for_vars_tmr
    (fun () -> solve_for_vars vss r)
    (fun () -> Solve_for_vars (vss, r))
