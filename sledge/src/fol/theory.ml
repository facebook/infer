(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Theory Solver *)

(* Theory equation solver state ===========================================*)

type oriented_equality = {var: Trm.t; rep: Trm.t}

type t =
  { wrt: Var.Set.t
  ; no_fresh: bool
  ; fresh: Var.Set.t
  ; solved: oriented_equality list option
  ; pending: (Trm.t * Trm.t) list }

let pp ppf = function
  | {solved= None} -> Format.fprintf ppf "unsat"
  | {solved= Some solved; fresh; pending} ->
      Format.fprintf ppf "%a%a : %a" Var.Set.pp_xs fresh
        (List.pp ";@ " (fun ppf {var; rep} ->
             Format.fprintf ppf "@[%a ↦ %a@]" Trm.pp var Trm.pp rep ))
        solved
        (List.pp ";@ " (fun ppf (a, b) ->
             Format.fprintf ppf "@[%a = %a@]" Trm.pp a Trm.pp b ))
        pending

(* Classification of terms ================================================*)

type kind = InterpApp | NonInterpAtom | InterpAtom | UninterpApp
[@@deriving compare, equal]

let classify e =
  match (e : Trm.t) with
  | Var _ -> NonInterpAtom
  | Z _ | Q _ -> InterpAtom
  | Arith a ->
      if Trm.Arith.is_uninterpreted a then UninterpApp
      else (
        assert (
          match Trm.Arith.classify a with
          | Trm _ | Const _ -> violates Trm.invariant e
          | Interpreted -> true
          | Uninterpreted -> false ) ;
        InterpApp )
  | Concat [||] -> InterpAtom
  | Splat _ | Sized _ | Extract _ | Concat _ -> InterpApp
  | Apply (_, [||]) -> NonInterpAtom
  | Apply _ -> UninterpApp

let is_interpreted e = equal_kind (classify e) InterpApp
let is_uninterpreted e = equal_kind (classify e) UninterpApp

let is_noninterpreted e =
  match classify e with
  | InterpAtom | InterpApp -> false
  | NonInterpAtom | UninterpApp -> true

let rec solvables e =
  match classify e with
  | InterpAtom -> Iter.empty
  | InterpApp -> solvable_trms e
  | NonInterpAtom | UninterpApp -> Iter.return e

and solvable_trms e = Iter.flat_map ~f:solvables (Trm.trms e)

let rec map_solvables e ~f =
  match classify e with
  | InterpAtom -> e
  | NonInterpAtom | UninterpApp -> f e
  | InterpApp -> Trm.map ~f:(map_solvables ~f) e

(* Solving equations ======================================================*)

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

let add_solved ~var ~rep s =
  match s with
  | {solved= None} -> s
  | {solved= Some solved} -> {s with solved= Some ({var; rep} :: solved)}

let add_pending a b s = {s with pending= (a, b) :: s.pending}

let fresh name s =
  if s.no_fresh then None
  else
    let x, wrt = Var.fresh name ~wrt:s.wrt in
    let fresh = Var.Set.add x s.fresh in
    Some (Trm.var x, {s with wrt; fresh})

let solve_poly p q s =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp p Trm.pp q)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  match Trm.sub p q with
  | Z z -> if Z.equal Z.zero z then s else {s with solved= None}
  | Var _ as var -> add_solved ~var ~rep:Trm.zero s
  | p_q -> (
    match Trm.Arith.solve_zero_eq p_q with
    | Some (var, rep) ->
        add_solved ~var:(Trm.arith var) ~rep:(Trm.arith rep) s
    | None -> add_solved ~var:p_q ~rep:Trm.zero s )

(* α[o,l) = β ==> l = |β| ∧ α = (⟨n,c⟩[0,o) ^ β ^ ⟨n,c⟩[o+l,n-o-l)) where n
   = |α| and c fresh *)
let solve_extract a o l b s =
  [%trace]
    ~call:(fun {pf} ->
      pf "@ %a = %a" Trm.pp (Trm.extract ~seq:a ~off:o ~len:l) Trm.pp b )
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  match fresh "c" s with
  | None -> s
  | Some (c, s) ->
      let n = Trm.seq_size_exn a in
      let n_c = Trm.sized ~siz:n ~seq:c in
      let o_l = Trm.add o l in
      let n_o_l = Trm.sub n o_l in
      let c0 = Trm.extract ~seq:n_c ~off:Trm.zero ~len:o in
      let c1 = Trm.extract ~seq:n_c ~off:o_l ~len:n_o_l in
      let b, s =
        match Trm.seq_size b with
        | None -> (Trm.sized ~siz:l ~seq:b, s)
        | Some m -> (b, add_pending l m s)
      in
      add_pending a (Trm.concat [|c0; b; c1|]) s

(* α₀^…^αᵢ^αⱼ^…^αᵥ = β ==> |α₀^…^αᵥ| = |β| ∧ … ∧ αⱼ = β[n₀+…+nᵢ,nⱼ) ∧ …
   where nₓ ≡ |αₓ| and m = |β| *)
let solve_concat a0V b m s =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp (Trm.concat a0V) Trm.pp b)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  let s, n0V =
    Iter.fold (Array.to_iter a0V) (s, Trm.zero) ~f:(fun aJ (s, oI) ->
        let nJ = Trm.seq_size_exn aJ in
        let oJ = Trm.add oI nJ in
        let s = add_pending aJ (Trm.extract ~seq:b ~off:oI ~len:nJ) s in
        (s, oJ) )
  in
  add_pending n0V m s

let solve d e s =
  [%trace]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp d Trm.pp e)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  match orient d e with
  (* e' = f' ==> true when e' ≡ f' *)
  | None -> s
  (* i = j ==> false when i ≠ j *)
  | Some (Z _, Z _) | Some (Q _, Q _) -> {s with solved= None}
  (*
   * Concat
   *)
  (* ⟨0,a⟩ = β ==> a = β = ⟨⟩ *)
  | Some (Sized {siz= n; seq= a}, b) when n == Trm.zero ->
      s
      |> add_pending a (Trm.concat [||])
      |> add_pending b (Trm.concat [||])
  | Some (b, Sized {siz= n; seq= a}) when n == Trm.zero ->
      s
      |> add_pending a (Trm.concat [||])
      |> add_pending b (Trm.concat [||])
  (* ⟨n,0⟩ = α₀^…^αᵥ ==> … ∧ αⱼ = ⟨n,0⟩[n₀+…+nᵢ,nⱼ) ∧ … *)
  | Some ((Sized {siz= n; seq} as b), Concat a0V) when seq == Trm.zero ->
      solve_concat a0V b n s
  (* ⟨n,e^⟩ = α₀^…^αᵥ ==> … ∧ αⱼ = ⟨n,e^⟩[n₀+…+nᵢ,nⱼ) ∧ … *)
  | Some ((Sized {siz= n; seq= Splat _} as b), Concat a0V) ->
      solve_concat a0V b n s
  | Some ((Var _ as v), (Concat a0V as c)) ->
      if not (Trm.Set.mem v (Trm.fv c :> Trm.Set.t)) then
        (* v = α₀^…^αᵥ ==> v ↦ α₀^…^αᵥ when v ∉ fv(α₀^…^αᵥ) *)
        add_solved ~var:v ~rep:c s
      else
        (* v = α₀^…^αᵥ ==> ⟨|α₀^…^αᵥ|,v⟩ = α₀^…^αᵥ when v ∈ fv(α₀^…^αᵥ) *)
        let m = Trm.seq_size_exn c in
        solve_concat a0V (Trm.sized ~siz:m ~seq:v) m s
  (* α₀^…^αᵥ = β₀^…^βᵤ ==> … ∧ αⱼ = (β₀^…^βᵤ)[n₀+…+nᵢ,nⱼ) ∧ … *)
  | Some (Concat a0V, (Concat _ as c)) ->
      solve_concat a0V c (Trm.seq_size_exn c) s
  (* α[o,l) = α₀^…^αᵥ ==> … ∧ αⱼ = α[o,l)[n₀+…+nᵢ,nⱼ) ∧ … *)
  | Some ((Extract {len= l} as e), Concat a0V) -> solve_concat a0V e l s
  (*
   * Extract
   *)
  | Some ((Var _ as v), (Extract {len= l} as e)) ->
      if not (Trm.Set.mem v (Trm.fv e :> Trm.Set.t)) then
        (* v = α[o,l) ==> v ↦ α[o,l) when v ∉ fv(α[o,l)) *)
        add_solved ~var:v ~rep:e s
      else
        (* v = α[o,l) ==> α[o,l) ↦ ⟨l,v⟩ when v ∈ fv(α[o,l)) *)
        add_solved ~var:e ~rep:(Trm.sized ~siz:l ~seq:v) s
  (* α[o,l) = β ==> … ∧ α = _^β^_ *)
  | Some (Extract {seq= a; off= o; len= l}, e) -> solve_extract a o l e s
  (*
   * Sized
   *)
  (* v = ⟨n,a⟩ ==> v = a *)
  | Some ((Var _ as v), Sized {seq= a}) -> s |> add_pending v a
  (* ⟨n,a⟩ = ⟨m,b⟩ ==> n = m ∧ a = β *)
  | Some (Sized {siz= n; seq= a}, Sized {siz= m; seq= b}) ->
      s |> add_pending n m |> add_pending a b
  (* ⟨n,a⟩ = β ==> n = |β| ∧ a = β *)
  | Some (Sized {siz= n; seq= a}, b) ->
      s
      |> Option.fold ~f:(add_pending n) (Trm.seq_size b)
      |> add_pending a b
  (*
   * Splat
   *)
  (* a^ = b^ ==> a = b *)
  | Some (Splat a, Splat b) -> s |> add_pending a b
  (*
   * Arithmetic
   *)
  (* p = q ==> p-q = 0 *)
  | Some (((Arith _ | Z _ | Q _) as p), q | q, ((Arith _ | Z _ | Q _) as p))
    ->
      solve_poly p q s
  (*
   * Uninterpreted
   *)
  (* r = v ==> v ↦ r *)
  | Some (rep, var) ->
      assert (is_noninterpreted var) ;
      assert (is_noninterpreted rep) ;
      add_solved ~var ~rep s
