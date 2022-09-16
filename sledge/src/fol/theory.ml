(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Theory Solver *)

open Var.Fresh.Import

(* Theory equation solver state ======================================== *)

type oriented_equality = {var: Trm.t; rep: Trm.t}

type t =
  { no_fresh: bool
  ; solved: oriented_equality list option
  ; pending: (Trm.t * Trm.t) list }

let pp ppf = function
  | {solved= None} -> Format.fprintf ppf "unsat"
  | {solved= Some solved; pending} ->
      Format.fprintf ppf "%a : %a"
        (List.pp ";@ " (fun ppf {var; rep} ->
             Format.fprintf ppf "@[%a ↦ %a@]" Trm.pp var Trm.pp rep ) )
        solved
        (List.pp ";@ " (fun ppf (a, b) ->
             Format.fprintf ppf "@[%a = %a@]" Trm.pp a Trm.pp b ) )
        pending

(* Solving equations ==================================================== *)

(** prefer representative terms that are minimal in the order s.t. Var <
    Extract < Concat < others, then using height of sequence nesting, and
    then using Trm.compare *)
let prefer e f =
  let rank e =
    match (e : Trm.t) with
    | Var _ -> 0
    | Extract _ -> 1
    | Concat _ -> 2
    | _ -> 3
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

let solve_poly p q s =
  [%dbg]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp p Trm.pp q)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  match Trm.sub p q with
  | Z z -> if Z.equal Z.zero z then s else {s with solved= None}
  | Q _ -> {s with solved= None}
  | Var _ as var -> add_solved ~var ~rep:Trm.zero s
  | p_q -> (
    match Trm.Arith.solve_zero_eq p_q with
    | Some (var, rep) ->
        let var = Trm.arith var in
        let rep = Trm.arith rep in
        if Trm.non_interpreted var then add_solved ~var ~rep s
        else add_pending var rep s
    | None -> add_solved ~var:p_q ~rep:Trm.zero s )

(* ⟨n,a⟩[o,l) = β ==> l = |β| ∧ a = (⟨n,c⟩[0,o) ^ β ^ ⟨n,c⟩[o+l,n-o-l))
   where c fresh *)
let solve_extract a n o l b s =
  [%dbgs]
    ~call:(fun {pf} ->
      pf "@ %a = %a" Trm.pp
        (Trm.extract ~seq:a ~siz:n ~off:o ~len:l)
        Trm.pp b )
    ~retn:(fun {pf} (s', vxd) -> pf "%a%a" Var.Context.pp_diff vxd pp s')
  @@ fun vx ->
  if s.no_fresh then s
  else
    let c = Var.Fresh.var "c" vx in
    let c = Trm.var c in
    let o_l = Trm.add o l in
    let n_o_l = Trm.sub n o_l in
    let x0 = Trm.extract ~seq:c ~siz:n ~off:Trm.zero ~len:o in
    let c0 = {Trm.seq= x0; siz= o} in
    let x1 = Trm.extract ~seq:c ~siz:n ~off:o_l ~len:n_o_l in
    let c1 = {Trm.seq= x1; siz= n_o_l} in
    let b, s =
      match Trm.seq_size b with
      | None -> ({Trm.seq= b; siz= l}, s)
      | Some m -> ({Trm.seq= b; siz= m}, add_pending l m s)
    in
    add_pending a (Trm.concat [|c0; b; c1|]) s

(* α₀^…^αᵢ^αⱼ^…^αᵥ = β ==> |α₀^…^αᵥ| = |β| ∧ … ∧ αⱼ = β[n₀+…+nᵢ,nⱼ) ∧ …
   where nₓ ≡ |αₓ| and m = |β| *)
let solve_concat a0V b m s =
  [%dbg]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp (Trm.concat a0V) Trm.pp b)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () ->
  let s, n0V =
    Array.fold a0V (s, Trm.zero) ~f:(fun {Trm.seq= aJ; siz= nJ} (s, oI) ->
        let oJ = Trm.add oI nJ in
        let xJ = Trm.extract ~seq:b ~siz:m ~off:oI ~len:nJ in
        let s = add_pending aJ xJ s in
        (s, oJ) )
  in
  add_pending n0V m s

(* α₀^…^αᵢ^αⱼ^…^αᵥ = e^ ==> … ∧ αⱼ = e^ ∧ … *)
let solve_concat_splat a0V b s =
  [%dbg]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp (Trm.concat a0V) Trm.pp b)
    ~retn:(fun {pf} -> pf "%a" pp)
  @@ fun () -> Array.fold a0V s ~f:(fun {Trm.seq} s -> add_pending seq b s)

let solve d e s =
  [%dbgs]
    ~call:(fun {pf} -> pf "@ %a = %a" Trm.pp d Trm.pp e)
    ~retn:(fun {pf} (s', vxd) -> pf "%a%a" Var.Context.pp_diff vxd pp s')
  @@ fun vx ->
  match orient d e with
  (* e' = f' ==> true when e' ≡ f' *)
  | None -> s
  (* i = j ==> false when i ≠ j *)
  | Some (Z _, Z _) | Some (Q _, Q _) -> {s with solved= None}
  (*
   * Concat
   *)
  (* 0 = α₀^…^αᵥ ==> … ∧ αⱼ = 0 ∧ … *)
  | Some (Concat a0V, z) when z == Trm.zero -> solve_concat_splat a0V z s
  (* e^ = α₀^…^αᵥ ==> … ∧ αⱼ = e^ ∧ … *)
  | Some (Concat a0V, (Splat _ as b)) -> solve_concat_splat a0V b s
  | Some ((Var _ as v), (Concat a0V as c)) ->
      if not (Trm.Set.mem v (Trm.fv c :> Trm.Set.t)) then
        (* v = α₀^…^αᵥ ==> v ↦ α₀^…^αᵥ when v ∉ fv(α₀^…^αᵥ) *)
        add_solved ~var:v ~rep:c s
      else
        (* v = α₀^…^αᵥ ==> … ∧ αⱼ = ⟨|αⱼ|,v⟩[n₀+…+nᵢ,nⱼ) ∧ … when v ∈
           fv(α₀^…^αᵥ) *)
        solve_concat a0V v (Trm.seq_size_exn c) s
  (* α₀^…^αᵥ = β₀^…^βᵤ ==> … ∧ αⱼ = (β₀^…^βᵤ)[n₀+…+nᵢ,nⱼ) ∧ … *)
  | Some (Concat a0V, (Concat _ as c)) ->
      solve_concat a0V c (Trm.seq_size_exn c) s
  (* ⟨n,a⟩[o,l) = α₀^…^αᵥ ==> … ∧ αⱼ = ⟨n,a⟩[o,l)[n₀+…+nᵢ,nⱼ) ∧ … *)
  | Some ((Extract {len= l} as e), Concat a0V) -> solve_concat a0V e l s
  (*
   * Extract
   *)
  | Some ((Var _ as v), (Extract _ as e)) ->
      if not (Trm.Set.mem v (Trm.fv e :> Trm.Set.t)) then
        (* v = ⟨n,a⟩[o,l) ==> v ↦ ⟨n,a⟩[o,l) when v ∉ fv(⟨n,a⟩[o,l)) *)
        add_solved ~var:v ~rep:e s
      else
        (* v = ⟨n,a⟩[o,l) ==> ⟨n,a⟩[o,l) ↦ v when v ∈ fv(⟨n,a⟩[o,l)) *)
        add_solved ~var:e ~rep:v s
  (* ⟨n,a⟩[o,l) = β ==> … ∧ ⟨n,a⟩ = _^β^_ *)
  | Some (Extract {seq= a; siz= n; off= o; len= l}, e) ->
      solve_extract a n o l e s vx
  (*
   * Splat
   *)
  (* a^ = b^ ==> a = b *)
  | Some (Splat a, Splat b) -> add_pending a b s
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
      assert (Trm.non_interpreted var) ;
      assert (Trm.non_interpreted rep) ;
      add_solved ~var ~rep s
