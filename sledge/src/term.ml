(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

[@@@warning "+9"]

type op1 =
  | Signed of {bits: int}
  | Unsigned of {bits: int}
  | Convert of {src: Typ.t; dst: Typ.t}
  | Splat
  | Select of int
[@@deriving compare, equal, hash, sexp]

type op2 =
  | Eq
  | Dq
  | Lt
  | Le
  | Ord
  | Uno
  | Div
  | Rem
  | Xor
  | Shl
  | Lshr
  | Ashr
  | Memory
  | Update of int
[@@deriving compare, equal, hash, sexp]

type op3 = Conditional | Extract [@@deriving compare, equal, hash, sexp]
type opN = Concat | Record [@@deriving compare, equal, hash, sexp]
type recN = Record [@@deriving compare, equal, hash, sexp]

module rec Set : sig
  include Import.Set.S with type elt := T.t

  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
end = struct
  include Import.Set.Make (T)

  let hash_fold_t = hash_fold_t T.hash_fold_t
  let hash = Hash.of_fold hash_fold_t

  include Provide_of_sexp (T)
end

and Qset : sig
  include Import.Qset.S with type elt := T.t

  val hash : t -> int
  val hash_fold_t : t Hash.folder
  val t_of_sexp : Sexp.t -> t
end = struct
  include Import.Qset.Make (T)

  let hash_fold_t = hash_fold_t T.hash_fold_t
  let hash = Hash.of_fold hash_fold_t
  let t_of_sexp = t_of_sexp T.t_of_sexp
end

and T : sig
  type set = Set.t [@@deriving compare, equal, hash, sexp]
  type qset = Qset.t [@@deriving compare, equal, hash, sexp]

  type t =
    | Var of {id: int; name: string}
    | Ap1 of op1 * t
    | Ap2 of op2 * t * t
    | Ap3 of op3 * t * t * t
    | ApN of opN * t iarray
    | RecN of recN * t iarray  (** NOTE: cyclic *)
    | And of set
    | Or of set
    | Add of qset
    | Mul of qset
    | Label of {parent: string; name: string}
    | Nondet of {msg: string}
    | Float of {data: string}
    | Integer of {data: Z.t}
    | Rational of {data: Q.t}
  [@@deriving compare, equal, hash, sexp]
end = struct
  type set = Set.t [@@deriving compare, equal, hash, sexp]
  type qset = Qset.t [@@deriving compare, equal, hash, sexp]

  type t =
    | Var of {id: int; name: string}
    | Ap1 of op1 * t
    | Ap2 of op2 * t * t
    | Ap3 of op3 * t * t * t
    | ApN of opN * t iarray
    | RecN of recN * t iarray  (** NOTE: cyclic *)
    | And of set
    | Or of set
    | Add of qset
    | Mul of qset
    | Label of {parent: string; name: string}
    | Nondet of {msg: string}
    | Float of {data: string}
    | Integer of {data: Z.t}
    | Rational of {data: Q.t}
  [@@deriving compare, equal, hash, sexp]

  (* Note: solve (and invariant) requires Qset.min_elt to return a
     non-coefficient, so Integer and Rational terms must compare higher than
     any valid monomial *)
  let compare x y =
    if x == y then 0
    else
      match (x, y) with
      | Var {id= i; name= _}, Var {id= j; name= _} when i > 0 && j > 0 ->
          Int.compare i j
      | _ -> compare x y

  let equal x y =
    x == y
    ||
    match (x, y) with
    | Var {id= i; name= _}, Var {id= j; name= _} when i > 0 && j > 0 ->
        Int.equal i j
    | _ -> equal x y
end

include T
module Map = struct include Map.Make (T) include Provide_of_sexp (T) end

let fix (f : (t -> 'a as 'f) -> 'f) (bot : 'f) (e : t) : 'a =
  let rec fix_f seen e =
    match e with
    | RecN _ ->
        if List.mem ~equal:( == ) seen e then f bot e
        else f (fix_f (e :: seen)) e
    | _ -> f (fix_f seen) e
  in
  let rec fix_f_seen_nil e =
    match e with RecN _ -> f (fix_f [e]) e | _ -> f fix_f_seen_nil e
  in
  fix_f_seen_nil e

let fix_flip (f : ('z -> t -> 'a as 'f) -> 'f) (bot : 'f) (z : 'z) (e : t) =
  fix (fun f' e z -> f (fun z e -> f' e z) z e) (fun e z -> bot z e) e z

let rec ppx strength fs term =
  let pp_ pp fs term =
    let pf fmt =
      Format.pp_open_box fs 2 ;
      Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
    in
    match term with
    | Var {name; id= -1} -> Trace.pp_styled `Bold "%@%s" fs name
    | Var {name; id= 0} -> Trace.pp_styled `Bold "%%%s" fs name
    | Var {name; id} -> (
      match strength term with
      | None -> pf "%%%s_%d" name id
      | Some `Universal -> Trace.pp_styled `Bold "%%%s_%d" fs name id
      | Some `Existential -> Trace.pp_styled `Cyan "%%%s_%d" fs name id
      | Some `Anonymous -> Trace.pp_styled `Cyan "_" fs )
    | Integer {data} -> Trace.pp_styled `Magenta "%a" fs Z.pp data
    | Rational {data} -> Trace.pp_styled `Magenta "%a" fs Q.pp data
    | Float {data} -> pf "%s" data
    | Nondet {msg} -> pf "nondet \"%s\"" msg
    | Label {name} -> pf "%s" name
    | Ap1 (Signed {bits}, arg) -> pf "((s%i)@ %a)" bits pp arg
    | Ap1 (Unsigned {bits}, arg) -> pf "((u%i)@ %a)" bits pp arg
    | Ap1 (Convert {src; dst}, arg) ->
        pf "((%a)(%a)@ %a)" Typ.pp dst Typ.pp src pp arg
    | Ap2 (Eq, x, y) -> pf "(%a@ = %a)" pp x pp y
    | Ap2 (Dq, x, y) -> pf "(%a@ @<2>≠ %a)" pp x pp y
    | Ap2 (Lt, x, y) -> pf "(%a@ < %a)" pp x pp y
    | Ap2 (Le, x, y) -> pf "(%a@ @<2>≤ %a)" pp x pp y
    | Ap2 (Ord, x, y) -> pf "(%a@ ord %a)" pp x pp y
    | Ap2 (Uno, x, y) -> pf "(%a@ uno %a)" pp x pp y
    | Add args ->
        let pp_poly_term fs (monomial, coefficient) =
          match monomial with
          | Integer {data} when Z.equal Z.one data -> Q.pp fs coefficient
          | _ when Q.equal Q.one coefficient -> pp fs monomial
          | _ ->
              Format.fprintf fs "%a @<1>× %a" Q.pp coefficient pp monomial
        in
        pf "(%a)" (Qset.pp "@ + " pp_poly_term) args
    | Mul args ->
        let pp_mono_term fs (factor, exponent) =
          if Q.equal Q.one exponent then pp fs factor
          else Format.fprintf fs "%a^%a" pp factor Q.pp exponent
        in
        pf "(%a)" (Qset.pp "@ @<2>× " pp_mono_term) args
    | Ap2 (Div, x, y) -> pf "(%a@ / %a)" pp x pp y
    | Ap2 (Rem, x, y) -> pf "(%a@ rem %a)" pp x pp y
    | And xs -> pf "(@[%a@])" (Set.pp ~sep:" &&@ " pp) xs
    | Or xs -> pf "(@[%a@])" (Set.pp ~sep:" ||@ " pp) xs
    | Ap2 (Xor, x, Integer {data}) when Z.is_true data -> pf "¬%a" pp x
    | Ap2 (Xor, Integer {data}, x) when Z.is_true data -> pf "¬%a" pp x
    | Ap2 (Xor, x, y) -> pf "(%a@ xor %a)" pp x pp y
    | Ap2 (Shl, x, y) -> pf "(%a@ shl %a)" pp x pp y
    | Ap2 (Lshr, x, y) -> pf "(%a@ lshr %a)" pp x pp y
    | Ap2 (Ashr, x, y) -> pf "(%a@ ashr %a)" pp x pp y
    | Ap3 (Conditional, cnd, thn, els) ->
        pf "(%a@ ? %a@ : %a)" pp cnd pp thn pp els
    | Ap3 (Extract, agg, off, len) -> pf "%a[%a,%a)" pp agg pp off pp len
    | Ap1 (Splat, byt) -> pf "%a^" pp byt
    | Ap2 (Memory, siz, arr) -> pf "@<1>⟨%a,%a@<1>⟩" pp siz pp arr
    | ApN (Concat, args) when IArray.is_empty args -> pf "@<2>⟨⟩"
    | ApN (Concat, args) -> pf "(%a)" (IArray.pp "@,^" pp) args
    | ApN (Record, elts) -> pf "{%a}" (pp_record strength) elts
    | RecN (Record, elts) -> pf "{|%a|}" (IArray.pp ",@ " pp) elts
    | Ap1 (Select idx, rcd) -> pf "%a[%i]" pp rcd idx
    | Ap2 (Update idx, rcd, elt) ->
        pf "[%a@ @[| %i → %a@]]" pp rcd idx pp elt
  in
  fix_flip pp_ (fun _ _ -> ()) fs term
  [@@warning "-9"]

and pp_record strength fs elts =
  [%Trace.fprintf
    fs "%a"
      (fun fs elts ->
        match
          String.init (IArray.length elts) ~f:(fun i ->
              match IArray.get elts i with
              | Integer {data} -> Char.of_int_exn (Z.to_int data)
              | _ -> raise (Invalid_argument "not a string") )
        with
        | s -> Format.fprintf fs "@[<h>%s@]" (String.escaped s)
        | exception _ ->
            Format.fprintf fs "@[<h>%a@]"
              (IArray.pp ",@ " (ppx strength))
              elts )
      elts]

let pp = ppx (fun _ -> None)
let pp_t = pp
let pp_diff fs (x, y) = Format.fprintf fs "-- %a ++ %a" pp x pp y

(** Invariant *)

let assert_conjunction = function
  | And cs ->
      Set.iter cs ~f:(fun c ->
          assert (match c with And _ -> false | _ -> true) )
  | _ -> assert false

let assert_disjunction = function
  | Or cs ->
      Set.iter cs ~f:(fun c ->
          assert (match c with Or _ -> false | _ -> true) )
  | _ -> assert false

(* an indeterminate (factor of a monomial) is any
   non-Add/Mul/Integer/Rational term *)
let assert_indeterminate = function
  | Integer _ | Rational _ | Add _ | Mul _ -> assert false
  | _ -> assert true

(* a monomial is a power product of factors, e.g.
 *     ∏ᵢ xᵢ^nᵢ
 * for (non-constant) indeterminants xᵢ and positive integer exponents nᵢ
 *)
let assert_monomial mono =
  match mono with
  | Mul args ->
      Qset.iter args ~f:(fun factor exponent ->
          assert (Q.is_real exponent) ;
          assert (Q.sign exponent > 0) ;
          assert_indeterminate factor |> Fn.id )
  | _ -> assert_indeterminate mono |> Fn.id

(* a polynomial term is a monomial multiplied by a non-zero coefficient
 *     c × ∏ᵢ xᵢ
 *)
let assert_poly_term mono coeff =
  assert (Q.is_real coeff) ;
  assert (Q.sign coeff <> 0) ;
  match mono with
  | Integer {data} -> assert (Z.equal Z.one data)
  | Mul args ->
      ( match Qset.min_elt args with
      | None | Some ((Integer _ | Rational _), _) -> assert false
      | Some (_, n) -> assert (Qset.length args > 1 || not (Q.equal Q.one n))
      ) ;
      assert_monomial mono |> Fn.id
  | _ -> assert_monomial mono |> Fn.id

(* a polynomial is a linear combination of monomials, e.g.
 *     ∑ᵢ cᵢ × ∏ⱼ xᵢⱼ
 * for non-zero constant coefficients cᵢ
 * and monomials ∏ⱼ xᵢⱼ, one of which may be the empty product 1
 *)
let assert_polynomial poly =
  match poly with
  | Add args ->
      ( match Qset.min_elt args with
      | None | Some ((Integer _ | Rational _), _) -> assert false
      | Some (_, k) -> assert (Qset.length args > 1 || not (Q.equal Q.one k))
      ) ;
      Qset.iter args ~f:(fun m c -> assert_poly_term m c |> Fn.id)
  | _ -> assert false

(* aggregate args of Extract and Concat must be aggregate terms, in
   particular, not variables *)
let rec assert_aggregate = function
  | Ap2 (Memory, _, _) -> ()
  | Ap3 (Extract, a, _, _) -> assert_aggregate a
  | ApN (Concat, a0N) ->
      assert (IArray.length a0N <> 1) ;
      IArray.iter ~f:assert_aggregate a0N
  | _ -> assert false

let invariant e =
  Invariant.invariant [%here] e [%sexp_of: t]
  @@ fun () ->
  match e with
  | And _ -> assert_conjunction e |> Fn.id
  | Or _ -> assert_disjunction e |> Fn.id
  | Add _ -> assert_polynomial e |> Fn.id
  | Mul _ -> assert_monomial e |> Fn.id
  | Ap2 (Memory, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _) ->
      assert_aggregate e
  | ApN (Record, elts) | RecN (Record, elts) ->
      assert (not (IArray.is_empty elts))
  | Ap1 (Convert {src= Integer _; dst= Integer _}, _) -> assert false
  | Ap1 (Convert {src; dst}, _) ->
      assert (Typ.convertible src dst) ;
      assert (
        not (Typ.equivalent src dst) (* avoid redundant representations *)
      )
  | Rational {data} ->
      assert (Q.is_real data) ;
      assert (not (Z.equal Z.one (Q.den data)))
  | _ -> ()
  [@@warning "-9"]

(** Variables are the terms constructed by [Var] *)
module Var = struct
  include T

  let pp = pp

  type strength = t -> [`Universal | `Existential | `Anonymous] option

  module Map = Map

  module Set = struct
    include Set

    let pp vs = Set.pp pp_t vs
    let ppx strength vs = Set.pp (ppx strength) vs

    let pp_xs fs xs =
      if not (is_empty xs) then
        Format.fprintf fs "@<2>∃ @[%a@] .@;<1 2>" pp xs
  end

  let invariant x =
    Invariant.invariant [%here] x [%sexp_of: t]
    @@ fun () -> match x with Var _ -> invariant x | _ -> assert false

  let id = function Var v -> v.id | x -> violates invariant x
  let name = function Var v -> v.name | x -> violates invariant x
  let is_global = function Var v -> v.id = -1 | x -> violates invariant x
  let of_ = function Var _ as v -> v | _ -> invalid_arg "Var.of_"

  let of_term = function
    | Var _ as v -> Some (v |> check invariant)
    | _ -> None

  let program ?global name =
    Var {name; id= (if Option.is_some global then -1 else 0)}

  let fresh name ~wrt =
    let max = match Set.max_elt wrt with None -> 0 | Some max -> id max in
    let x' = Var {name; id= max + 1} in
    (x', Set.add wrt x')

  let identified ~name ~id = Var {name; id}

  (** Variable renaming substitutions *)
  module Subst = struct
    type t = T.t Map.t [@@deriving compare, equal, sexp_of]

    let t_of_sexp = Map.t_of_sexp T.t_of_sexp

    let invariant s =
      Invariant.invariant [%here] s [%sexp_of: t]
      @@ fun () ->
      let domain, range =
        Map.fold s ~init:(Set.empty, Set.empty)
          ~f:(fun ~key ~data (domain, range) ->
            assert (not (Set.mem range data)) ;
            (Set.add domain key, Set.add range data) )
      in
      assert (Set.disjoint domain range)

    let pp = Map.pp pp_t pp_t
    let empty = Map.empty
    let is_empty = Map.is_empty

    let freshen vs ~wrt =
      let xs = Set.inter wrt vs in
      ( if Set.is_empty xs then empty
      else
        let wrt = Set.union wrt vs in
        Set.fold xs ~init:(empty, wrt) ~f:(fun (sub, wrt) x ->
            let x', wrt = fresh (name x) ~wrt in
            let sub = Map.add_exn sub ~key:x ~data:x' in
            (sub, wrt) )
        |> fst )
      |> check invariant

    let fold sub ~init ~f =
      Map.fold sub ~init ~f:(fun ~key ~data s -> f key data s)

    let invert sub =
      Map.fold sub ~init:empty ~f:(fun ~key ~data sub' ->
          Map.add_exn sub' ~key:data ~data:key )
      |> check invariant

    let restrict sub vs =
      Map.filter_keys ~f:(Set.mem vs) sub |> check invariant

    let domain sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key ~data:_ domain ->
          Set.add domain key )

    let range sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key:_ ~data range ->
          Set.add range data )

    let apply sub v = Map.find sub v |> Option.value ~default:v

    let apply_set sub vs =
      Map.fold sub ~init:vs ~f:(fun ~key ~data vs ->
          let vs' = Set.remove vs key in
          if vs' == vs then vs
          else (
            assert (not (Set.equal vs' vs)) ;
            Set.add vs' data ) )
      |> check (fun vs' ->
             assert (Set.disjoint (domain sub) vs') ;
             assert (Set.is_subset (range sub) ~of_:vs') )
  end
end

(** Construct *)

(* variables *)

let var x = x

(* constants *)

let integer data = Integer {data} |> check invariant

let rational data =
  ( if Z.equal Z.one (Q.den data) then Integer {data= Q.num data}
  else Rational {data} )
  |> check invariant

let null = integer Z.zero
let zero = integer Z.zero
let one = integer Z.one
let minus_one = integer Z.minus_one
let bool b = integer (Z.of_bool b)
let true_ = bool true
let false_ = bool false
let float data = Float {data} |> check invariant
let nondet msg = Nondet {msg} |> check invariant
let label ~parent ~name = Label {parent; name} |> check invariant

(* type conversions *)

let simp_signed bits arg =
  match arg with
  | Integer {data} -> integer (Z.signed_extract data 0 bits)
  | _ -> Ap1 (Signed {bits}, arg)

let simp_unsigned bits arg =
  match arg with
  | Integer {data} -> integer (Z.extract data 0 bits)
  | _ -> Ap1 (Unsigned {bits}, arg)

let simp_convert src dst arg = Ap1 (Convert {src; dst}, arg)

(* arithmetic *)

(* Sums of polynomial terms represented by multisets. A sum ∑ᵢ cᵢ × Xᵢ of
   monomials Xᵢ with coefficients cᵢ is represented by a multiset where the
   elements are Xᵢ with multiplicities cᵢ. A constant is treated as the
   coefficient of the empty monomial, which is the unit of multiplication 1. *)
module Sum = struct
  let empty = Qset.empty

  let add coeff term sum =
    assert (not (Q.equal Q.zero coeff)) ;
    match term with
    | Integer {data} when Z.equal Z.zero data -> sum
    | Integer {data} -> Qset.add sum one Q.(coeff * of_z data)
    | Rational {data} -> Qset.add sum one Q.(coeff * data)
    | _ -> Qset.add sum term coeff

  let of_ ?(coeff = Q.one) term = add coeff term empty

  let map sum ~f =
    Qset.fold sum ~init:empty ~f:(fun e c sum -> add c (f e) sum)

  let mul_const const sum =
    assert (not (Q.equal Q.zero const)) ;
    if Q.equal Q.one const then sum
    else Qset.map_counts ~f:(fun _ -> Q.mul const) sum

  let to_term sum =
    match Qset.classify sum with
    | `Zero -> zero
    | `One (arg, q) -> (
      match arg with
      | Integer {data} ->
          assert (Z.equal Z.one data) ;
          rational q
      | _ when Q.equal Q.one q -> arg
      | _ -> Add sum )
    | `Many -> Add sum
end

(* Products of indeterminants represented by multisets. A product ∏ᵢ xᵢ^nᵢ
   of indeterminates xᵢ is represented by a multiset where the elements are
   xᵢ and the multiplicities are the exponents nᵢ. *)
module Prod = struct
  let empty = Qset.empty

  let add term prod =
    assert (match term with Integer _ | Rational _ -> false | _ -> true) ;
    Qset.add prod term Q.one

  let of_ term = add term empty
  let union = Qset.union

  let to_term prod =
    match Qset.pop prod with
    | None -> one
    | Some (factor, power, prod')
      when Qset.is_empty prod' && Q.equal Q.one power ->
        factor
    | _ -> Mul prod
end

let rec simp_add_ es poly =
  (* (coeff × term) + poly *)
  let f term coeff poly =
    match (term, poly) with
    (* (0 × e) + s ==> 0 (optim) *)
    | _ when Q.equal Q.zero coeff -> poly
    (* (c × 0) + s ==> s (optim) *)
    | Integer {data}, _ when Z.equal Z.zero data -> poly
    (* (c × cᵢ) + cⱼ ==> c×cᵢ+cⱼ *)
    | Integer {data= i}, Integer {data= j} ->
        rational Q.((coeff * of_z i) + of_z j)
    | Rational {data= i}, Rational {data= j} -> rational Q.((coeff * i) + j)
    (* (c × ∑ᵢ cᵢ × Xᵢ) + s ==> (∑ᵢ (c × cᵢ) × Xᵢ) + s *)
    | Add args, _ -> simp_add_ (Sum.mul_const coeff args) poly
    (* (c₀ × X₀) + (∑ᵢ₌₁ⁿ cᵢ × Xᵢ) ==> ∑ᵢ₌₀ⁿ cᵢ × Xᵢ *)
    | _, Add args -> Sum.to_term (Sum.add coeff term args)
    (* (c₁ × X₁) + X₂ ==> ∑ᵢ₌₁² cᵢ × Xᵢ for c₂ = 1 *)
    | _ -> Sum.to_term (Sum.add coeff term (Sum.of_ poly))
  in
  Qset.fold ~f es ~init:poly

and simp_mul2 e f =
  match (e, f) with
  (* c₁ × c₂ ==> c₁×c₂ *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.mul i j)
  | Rational {data= i}, Rational {data= j} -> rational (Q.mul i j)
  (* 0 × f ==> 0 *)
  | Integer {data}, _ when Z.equal Z.zero data -> e
  (* e × 0 ==> 0 *)
  | _, Integer {data} when Z.equal Z.zero data -> f
  (* c × (∑ᵤ cᵤ × ∏ⱼ yᵤⱼ) ==> ∑ᵤ c × cᵤ × ∏ⱼ yᵤⱼ *)
  | Integer {data}, Add args | Add args, Integer {data} ->
      Sum.to_term (Sum.mul_const (Q.of_z data) args)
  | Rational {data}, Add args | Add args, Rational {data} ->
      Sum.to_term (Sum.mul_const data args)
  (* c₁ × x₁ ==> ∑ᵢ₌₁ cᵢ × xᵢ *)
  | Integer {data= c}, x | x, Integer {data= c} ->
      Sum.to_term (Sum.of_ ~coeff:(Q.of_z c) x)
  | Rational {data= c}, x | x, Rational {data= c} ->
      Sum.to_term (Sum.of_ ~coeff:c x)
  (* (∏ᵤ₌₀ⁱ xᵤ) × (∏ᵥ₌ᵢ₊₁ⁿ xᵥ) ==> ∏ⱼ₌₀ⁿ xⱼ *)
  | Mul xs1, Mul xs2 -> Prod.to_term (Prod.union xs1 xs2)
  (* (∏ᵢ xᵢ) × (∑ᵤ cᵤ × ∏ⱼ yᵤⱼ) ==> ∑ᵤ cᵤ × ∏ᵢ xᵢ × ∏ⱼ yᵤⱼ *)
  | (Mul prod as m), Add sum | Add sum, (Mul prod as m) ->
      Sum.to_term
        (Sum.map sum ~f:(function
          | Mul args -> Prod.to_term (Prod.union prod args)
          | (Integer _ | Rational _) as c -> simp_mul2 c m
          | mono -> Prod.to_term (Prod.add mono prod) ))
  (* x₀ × (∏ᵢ₌₁ⁿ xᵢ) ==> ∏ᵢ₌₀ⁿ xᵢ *)
  | Mul xs1, x | x, Mul xs1 -> Prod.to_term (Prod.add x xs1)
  (* e × (∑ᵤ cᵤ × ∏ⱼ yᵤⱼ) ==> ∑ᵤ e × cᵤ × ∏ⱼ yᵤⱼ *)
  | Add args, e | e, Add args ->
      simp_add_ (Sum.map ~f:(fun m -> simp_mul2 e m) args) zero
  (* x₁ × x₂ ==> ∏ᵢ₌₁² xᵢ *)
  | _ -> Prod.to_term (Prod.add e (Prod.of_ f))

let simp_div x y =
  match (x, y) with
  (* e / 0 ==> e / 0 *)
  | _, Integer {data} when Z.equal Z.zero data -> Ap2 (Div, x, y)
  (* e / 1 ==> e *)
  | e, Integer {data} when Z.equal Z.one data -> e
  (* e / -1 ==> -1×e *)
  | e, (Integer {data} as c) when Z.equal Z.minus_one data -> simp_mul2 e c
  (* i / j ==> i/j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.div i j)
  | Rational {data= i}, Rational {data= j} -> rational (Q.div i j)
  (* (∑ᵢ cᵢ × Xᵢ) / z ==> ∑ᵢ cᵢ/z × Xᵢ *)
  | Add args, Integer {data} ->
      Sum.to_term (Sum.mul_const Q.(inv (of_z data)) args)
  | Add args, Rational {data} ->
      Sum.to_term (Sum.mul_const Q.(inv data) args)
  (* x / n ==> 1/n × x *)
  | _, Integer {data} -> Sum.to_term (Sum.of_ ~coeff:Q.(inv (of_z data)) x)
  | _, Rational {data} -> Sum.to_term (Sum.of_ ~coeff:Q.(inv data) x)
  (* x / y *)
  | _ -> Ap2 (Div, x, y)

let simp_rem x y =
  match (x, y) with
  (* i % j *)
  | Integer {data= i}, Integer {data= j} when not (Z.equal Z.zero j) ->
      integer (Z.rem i j)
  (* (n/d) % i ==> (n / d) % i *)
  | Rational {data= q}, Integer {data= i} when not (Z.equal Z.zero i) ->
      integer (Z.rem (Z.div q.num q.den) i)
  (* e % 1 ==> 0 *)
  | _, Integer {data} when Z.equal Z.one data -> zero
  | _ -> Ap2 (Rem, x, y)

let simp_add es = simp_add_ es zero
let simp_add2 e f = simp_add_ (Sum.of_ e) f
let simp_negate x = simp_mul2 minus_one x

let simp_sub x y =
  match (x, y) with
  (* i - j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.sub i j)
  | Rational {data= i}, Rational {data= j} -> rational (Q.sub i j)
  (* x - y ==> x + (-1 * y) *)
  | _ -> simp_add2 x (simp_negate y)

let simp_mul es =
  (* (bas ^ pwr) × term *)
  let rec mul_pwr bas pwr term =
    if Q.equal Q.zero pwr then term
    else mul_pwr bas Q.(pwr - one) (simp_mul2 bas term)
  in
  Qset.fold es ~init:one ~f:(fun bas pwr term ->
      if Q.sign pwr >= 0 then mul_pwr bas pwr term
      else simp_div term (mul_pwr bas (Q.neg pwr) one) )

(* if-then-else *)

let simp_cond cnd thn els =
  match cnd with
  (* ¬(true ? t : e) ==> t *)
  | Integer {data} when Z.is_true data -> thn
  (* ¬(false ? t : e) ==> e *)
  | Integer {data} when Z.is_false data -> els
  | _ -> Ap3 (Conditional, cnd, thn, els)

(* boolean / bitwise *)

let rec is_boolean = function
  | Ap1 ((Unsigned {bits= 1} | Convert {dst= Integer {bits= 1; _}; _}), _)
   |Ap2 ((Eq | Dq | Lt | Le), _, _) ->
      true
  | Ap2 ((Div | Rem | Xor | Shl | Lshr | Ashr), x, y)
   |Ap3 (Conditional, _, x, y) ->
      is_boolean x || is_boolean y
  | And xs | Or xs -> Set.for_all ~f:is_boolean xs
  | _ -> false

let rec simp_and2 x y =
  match (x, y) with
  (* i && j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logand i j)
  (* e && true ==> e *)
  | (Integer {data}, e | e, Integer {data}) when Z.is_true data -> e
  (* e && false ==> false *)
  | ((Integer {data} as f), _ | _, (Integer {data} as f))
    when Z.is_false data ->
      f
  (* e && (c ? t : f) ==> (c ? e && t : e && f) *)
  | e, Ap3 (Conditional, c, t, f) | Ap3 (Conditional, c, t, f), e ->
      simp_cond c (simp_and2 e t) (simp_and2 e f)
  (* e && e ==> e *)
  | _ when equal x y -> x
  | _ ->
      let add s = function And cs -> Set.union s cs | c -> Set.add s c in
      And (add (add Set.empty x) y)

let simp_and xs = Set.fold xs ~init:true_ ~f:simp_and2

let rec simp_or2 x y =
  match (x, y) with
  (* i || j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logor i j)
  (* e || true ==> true *)
  | ((Integer {data} as t), _ | _, (Integer {data} as t))
    when Z.is_true data ->
      t
  (* e || false ==> e *)
  | (Integer {data}, e | e, Integer {data}) when Z.is_false data -> e
  (* e || (c ? t : f) ==> (c ? e || t : e || f) *)
  | e, Ap3 (Conditional, c, t, f) | Ap3 (Conditional, c, t, f), e ->
      simp_cond c (simp_or2 e t) (simp_or2 e f)
  (* e || e ==> e *)
  | _ when equal x y -> x
  | _ ->
      let add s = function Or cs -> Set.union s cs | c -> Set.add s c in
      Or (add (add Set.empty x) y)

let simp_or xs = Set.fold xs ~init:false_ ~f:simp_or2

(* aggregate sizes *)

let rec agg_size_exn = function
  | Ap2 (Memory, n, _) | Ap3 (Extract, _, _, n) -> n
  | ApN (Concat, a0U) ->
      IArray.fold a0U ~init:zero ~f:(fun a0I aJ ->
          simp_add2 a0I (agg_size_exn aJ) )
  | _ -> invalid_arg "agg_size_exn"

let agg_size e = try Some (agg_size_exn e) with Invalid_argument _ -> None

(* memory *)

let empty_agg = ApN (Concat, IArray.of_array [||])
let simp_splat byt = Ap1 (Splat, byt)

let simp_memory siz arr =
  (* ⟨n,α⟩ ==> α when n ≡ |α| *)
  match agg_size arr with
  | Some n when equal siz n -> arr
  | _ -> Ap2 (Memory, siz, arr)

type pcmp = Lt | Eq | Gt | Unknown

let partial_compare x y : pcmp =
  match simp_sub x y with
  | Integer {data} -> (
    match Int.sign (Z.sign data) with Neg -> Lt | Zero -> Eq | Pos -> Gt )
  | Rational {data} -> (
    match Int.sign (Q.sign data) with Neg -> Lt | Zero -> Eq | Pos -> Gt )
  | _ -> Unknown

let partial_ge x y =
  match partial_compare x y with Gt | Eq -> true | Lt | Unknown -> false

let rec simp_extract agg off len =
  [%Trace.call fun {pf} -> pf "%a" pp (Ap3 (Extract, agg, off, len))]
  ;
  (* _[_,0) ==> ⟨⟩ *)
  ( if equal len zero then empty_agg
  else
    let o_l = simp_add2 off len in
    match agg with
    (* α[m,k)[o,l) ==> α[m+o,l) when k ≥ o+l *)
    | Ap3 (Extract, a, m, k) when partial_ge k o_l ->
        simp_extract a (simp_add2 m off) len
    (* ⟨n,E^⟩[o,l) ==> ⟨l,E^⟩ when n ≥ o+l *)
    | Ap2 (Memory, n, (Ap1 (Splat, _) as e)) when partial_ge n o_l ->
        simp_memory len e
    (* ⟨n,a⟩[0,n) ==> ⟨n,a⟩ *)
    | Ap2 (Memory, n, _) when equal off zero && equal n len -> agg
    (* For (α₀^α₁)[o,l) there are 3 cases:
     *
     * ⟨...⟩^⟨...⟩
     *  [,)
     * o < o+l ≤ |α₀| : (α₀^α₁)[o,l) ==> α₀[o,l) ^ α₁[0,0)
     *
     * ⟨...⟩^⟨...⟩
     *   [  ,  )
     * o ≤ |α₀| < o+l : (α₀^α₁)[o,l) ==> α₀[o,|α₀|-o) ^ α₁[0,l-(|α₀|-o))
     *
     * ⟨...⟩^⟨...⟩
     *        [,)
     * |α₀| ≤ o : (α₀^α₁)[o,l) ==> α₀[o,0) ^ α₁[o-|α₀|,l)
     *
     * So in general:
     *
     * (α₀^α₁)[o,l) ==> α₀[o,l₀) ^ α₁[o₁,l-l₀)
     * where l₀ = max 0 (min l |α₀|-o)
     *       o₁ = max 0 o-|α₀|
     *)
    | ApN (Concat, na1N) -> (
      match len with
      | Integer {data= l} ->
          IArray.fold_map_until na1N ~init:(l, off)
            ~f:(fun (l, oI) naI ->
              let nI = agg_size_exn naI in
              if Z.equal Z.zero l then
                Continue ((l, oI), simp_extract naI oI zero)
              else
                let oI_nI = simp_sub oI nI in
                match oI_nI with
                | Integer {data} ->
                    let oJ = if Z.sign data <= 0 then zero else oI_nI in
                    let lI = Z.(max zero (min l (neg data))) in
                    let l = Z.(l - lI) in
                    Continue ((l, oJ), simp_extract naI oI (integer lI))
                | _ -> Stop (Ap3 (Extract, agg, off, len)) )
            ~finish:(fun (_, e1N) -> simp_concat e1N)
      | _ -> Ap3 (Extract, agg, off, len) )
    (* α[o,l) *)
    | _ -> Ap3 (Extract, agg, off, len) )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

and simp_concat xs =
  [%Trace.call fun {pf} -> pf "%a" pp (ApN (Concat, xs))]
  ;
  (* (α^(β^γ)^δ) ==> (α^β^γ^δ) *)
  let flatten xs =
    let exists_sub_Concat =
      IArray.exists ~f:(function ApN (Concat, _) -> true | _ -> false)
    in
    let concat_sub_Concat xs =
      IArray.concat
        (IArray.fold_right xs ~init:[] ~f:(fun x s ->
             match x with
             | ApN (Concat, ys) -> ys :: s
             | x -> IArray.of_array [|x|] :: s ))
    in
    if exists_sub_Concat xs then concat_sub_Concat xs else xs
  in
  let simp_adjacent e f =
    match (e, f) with
    (* ⟨n,a⟩[o,k)^⟨n,a⟩[o+k,l) ==> ⟨n,a⟩[o,k+l) when n ≥ o+k+l *)
    | ( Ap3 (Extract, (Ap2 (Memory, n, _) as na), o, k)
      , Ap3 (Extract, na', o_k, l) )
      when equal na na'
           && equal o_k (simp_add2 o k)
           && partial_ge n (simp_add2 o_k l) ->
        Some (simp_extract na o (simp_add2 k l))
    (* ⟨m,E^⟩^⟨n,E^⟩ ==> ⟨m+n,E^⟩ *)
    | Ap2 (Memory, m, (Ap1 (Splat, _) as a)), Ap2 (Memory, n, a')
      when equal a a' ->
        Some (simp_memory (simp_add2 m n) a)
    | _ -> None
  in
  let xs = flatten xs in
  let xs = IArray.combine_adjacent ~f:simp_adjacent xs in
  (if IArray.length xs = 1 then IArray.get xs 0 else ApN (Concat, xs))
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp]

(* comparison *)

let simp_lt x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j} -> bool (Z.lt i j)
  | Rational {data= i}, Rational {data= j} -> bool (Q.lt i j)
  | _ -> Ap2 (Lt, x, y)

let simp_le x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j} -> bool (Z.leq i j)
  | Rational {data= i}, Rational {data= j} -> bool (Q.leq i j)
  | _ -> Ap2 (Le, x, y)

let simp_ord x y = Ap2 (Ord, x, y)
let simp_uno x y = Ap2 (Uno, x, y)

let rec simp_eq x y =
  match
    match Ordering.of_int (compare x y) with
    | Equal -> None
    | Less -> Some (x, y)
    | Greater -> Some (y, x)
  with
  (* e = e ==> true *)
  | None -> bool true
  | Some (x, y) -> (
    match (x, y) with
    (* i = j ==> false when i ≠ j *)
    | Integer _, Integer _ | Rational _, Rational _ -> bool false
    (* b = false ==> ¬b *)
    | b, Integer {data} when Z.is_false data && is_boolean b -> simp_not b
    (* b = true ==> b *)
    | b, Integer {data} when Z.is_true data && is_boolean b -> b
    (* e = (c ? t : f) ==> (c ? e = t : e = f) *)
    | e, Ap3 (Conditional, c, t, f) | Ap3 (Conditional, c, t, f), e ->
        simp_cond c (simp_eq e t) (simp_eq e f)
    (* α^β^δ = α^γ^δ ==> β = γ *)
    | ApN (Concat, a), ApN (Concat, b) ->
        let m = IArray.length a in
        let n = IArray.length b in
        let length_common_prefix =
          let rec find_lcp i =
            if equal (IArray.get a i) (IArray.get b i) then find_lcp (i + 1)
            else i
          in
          find_lcp 0
        in
        let length_common_suffix =
          let rec find_lcs i =
            if equal (IArray.get a (m - 1 - i)) (IArray.get b (n - 1 - i))
            then find_lcs (i + 1)
            else i
          in
          find_lcs 0
        in
        let length_common = length_common_prefix + length_common_suffix in
        if length_common = 0 then Ap2 (Eq, x, y)
        else
          let pos = length_common_prefix in
          let a = IArray.sub ~pos ~len:(m - length_common) a in
          let b = IArray.sub ~pos ~len:(n - length_common) b in
          simp_eq (simp_concat a) (simp_concat b)
    | ( (Ap2 (Memory, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _))
      , (Ap2 (Memory, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _)) ) ->
        Ap2 (Eq, x, y)
    (* x = α ==> ⟨x,|α|⟩ = α *)
    | ( x
      , ( (Ap2 (Memory, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _)) as
        a ) )
     |( ( (Ap2 (Memory, _, _) | Ap3 (Extract, _, _, _) | ApN (Concat, _)) as
        a )
      , x ) ->
        simp_eq (Ap2 (Memory, agg_size_exn a, x)) a
    | x, y -> Ap2 (Eq, x, y) )

and simp_dq x y =
  match (x, y) with
  (* e ≠ (c ? t : f) ==> (c ? e ≠ t : e ≠ f) *)
  | e, Ap3 (Conditional, c, t, f) | Ap3 (Conditional, c, t, f), e ->
      simp_cond c (simp_dq e t) (simp_dq e f)
  | _ -> (
    match simp_eq x y with
    | Ap2 (Eq, x, y) -> Ap2 (Dq, x, y)
    | b -> simp_not b )

(* negation-normal form *)
and simp_not term =
  match term with
  (* ¬(x = y) ==> x ≠ y *)
  | Ap2 (Eq, x, y) -> simp_dq x y
  (* ¬(x ≠ y) ==> x = y *)
  | Ap2 (Dq, x, y) -> simp_eq x y
  (* ¬(x < y) ==> y <= x *)
  | Ap2 (Lt, x, y) -> simp_le y x
  (* ¬(x <= y) ==> y < x *)
  | Ap2 (Le, x, y) -> simp_lt y x
  (* ¬(x ≠ nan ∧ y ≠ nan) ==> x = nan ∨ y = nan *)
  | Ap2 (Ord, x, y) -> simp_uno x y
  (* ¬(x = nan ∨ y = nan) ==> x ≠ nan ∧ y ≠ nan *)
  | Ap2 (Uno, x, y) -> simp_ord x y
  (* ¬(a ∧ b) ==> ¬a ∨ ¬b *)
  | And xs -> simp_or (Set.map ~f:simp_not xs)
  (* ¬(a ∨ b) ==> ¬a ∧ ¬b *)
  | Or xs -> simp_and (Set.map ~f:simp_not xs)
  (* ¬¬e ==> e *)
  | Ap2 (Xor, Integer {data}, e) when Z.is_true data -> e
  | Ap2 (Xor, e, Integer {data}) when Z.is_true data -> e
  (* ¬(c ? t : e) ==> c ? ¬t : ¬e *)
  | Ap3 (Conditional, cnd, thn, els) ->
      simp_cond cnd (simp_not thn) (simp_not els)
  (* ¬i ==> -i-1 *)
  | Integer {data} -> integer (Z.lognot data)
  (* ¬e ==> true xor e *)
  | e -> Ap2 (Xor, true_, e)

(* bitwise *)

let simp_xor x y =
  match (x, y) with
  (* i xor j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logxor i j)
  (* true xor b ==> ¬b *)
  | Integer {data}, b when Z.is_true data && is_boolean b -> simp_not b
  | b, Integer {data} when Z.is_true data && is_boolean b -> simp_not b
  (* e xor e ==> 0 *)
  | _ when equal x y -> zero
  | _ -> Ap2 (Xor, x, y)

let simp_shl x y =
  match (x, y) with
  (* i shl j *)
  | Integer {data= i}, Integer {data= j} when Z.sign j >= 0 ->
      integer (Z.shift_left i (Z.to_int j))
  (* e shl 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> Ap2 (Shl, x, y)

let simp_lshr x y =
  match (x, y) with
  (* i lshr j *)
  | Integer {data= i}, Integer {data= j} when Z.sign j >= 0 ->
      integer (Z.shift_right_trunc i (Z.to_int j))
  (* e lshr 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> Ap2 (Lshr, x, y)

let simp_ashr x y =
  match (x, y) with
  (* i ashr j *)
  | Integer {data= i}, Integer {data= j} when Z.sign j >= 0 ->
      integer (Z.shift_right i (Z.to_int j))
  (* e ashr 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> Ap2 (Ashr, x, y)

(* records *)

let simp_record elts = ApN (Record, elts)
let simp_select idx rcd = Ap1 (Select idx, rcd)
let simp_update idx rcd elt = Ap2 (Update idx, rcd, elt)

let rec_app key =
  let memo_id = Hashtbl.create key in
  let dummy = null in
  Staged.stage
  @@ fun ~id op elt_thks ->
  match Hashtbl.find memo_id id with
  | None ->
      (* Add placeholder to prevent computing [elts] in calls to [rec_app]
         from [elt_thks] for recursive occurrences of [id]. *)
      let elta = Array.create ~len:(IArray.length elt_thks) dummy in
      let elts = IArray.of_array elta in
      Hashtbl.set memo_id ~key:id ~data:elts ;
      IArray.iteri elt_thks ~f:(fun i (lazy elt) -> elta.(i) <- elt) ;
      RecN (op, elts) |> check invariant
  | Some elts ->
      (* Do not check invariant as invariant will be checked above after the
         thunks are forced, before which invariant-checking may spuriously
         fail. Note that it is important that the value constructed here
         shares the array in the memo table, so that the update after
         forcing the recursive thunks also updates this value. *)
      RecN (op, elts)

(* dispatching for normalization and invariant checking *)

let norm1 op x =
  ( match op with
  | Signed {bits} -> simp_signed bits x
  | Unsigned {bits} -> simp_unsigned bits x
  | Convert {src; dst} -> simp_convert src dst x
  | Splat -> simp_splat x
  | Select idx -> simp_select idx x )
  |> check invariant

let norm2 op x y =
  ( match op with
  | Memory -> simp_memory x y
  | Eq -> simp_eq x y
  | Dq -> simp_dq x y
  | Lt -> simp_lt x y
  | Le -> simp_le x y
  | Ord -> simp_ord x y
  | Uno -> simp_uno x y
  | Div -> simp_div x y
  | Rem -> simp_rem x y
  | Xor -> simp_xor x y
  | Shl -> simp_shl x y
  | Lshr -> simp_lshr x y
  | Ashr -> simp_ashr x y
  | Update idx -> simp_update idx x y )
  |> check invariant

let norm3 op x y z =
  ( match op with
  | Conditional -> simp_cond x y z
  | Extract -> simp_extract x y z )
  |> check invariant

let normN op xs =
  (match op with Concat -> simp_concat xs | Record -> simp_record xs)
  |> check invariant

(* exposed interface *)

let signed bits term = norm1 (Signed {bits}) term
let unsigned bits term = norm1 (Unsigned {bits}) term

let convert src ~to_:dst arg =
  if Typ.equivalent src dst then arg else norm1 (Convert {src; dst}) arg

let eq = norm2 Eq
let dq = norm2 Dq
let lt = norm2 Lt
let le = norm2 Le
let ord = norm2 Ord
let uno = norm2 Uno
let neg e = simp_negate e |> check invariant
let add e f = simp_add2 e f |> check invariant
let addN args = simp_add args |> check invariant
let sub e f = simp_sub e f |> check invariant
let mul e f = simp_mul2 e f |> check invariant
let mulN args = simp_mul args |> check invariant
let div = norm2 Div
let rem = norm2 Rem
let and_ e f = simp_and2 e f |> check invariant
let or_ e f = simp_or2 e f |> check invariant
let andN es = simp_and es |> check invariant
let orN es = simp_or es |> check invariant
let not_ e = simp_not e |> check invariant
let xor = norm2 Xor
let shl = norm2 Shl
let lshr = norm2 Lshr
let ashr = norm2 Ashr
let conditional ~cnd ~thn ~els = norm3 Conditional cnd thn els
let splat byt = norm1 Splat byt
let memory ~siz ~arr = norm2 Memory siz arr
let extract ~agg ~off ~len = norm3 Extract agg off len
let concat xs = normN Concat (IArray.of_array xs)
let record elts = normN Record elts
let select ~rcd ~idx = norm1 (Select idx) rcd
let update ~rcd ~idx ~elt = norm2 (Update idx) rcd elt
let size_of t = integer (Z.of_int (Typ.size_of t))

let eq_concat (siz, arr) ms =
  eq (memory ~siz ~arr)
    (concat (Array.map ~f:(fun (siz, arr) -> memory ~siz ~arr) ms))

(** Transform *)

let map e ~f =
  let map1 op ~f x =
    let x' = f x in
    if x' == x then e else norm1 op x'
  in
  let map2 op ~f x y =
    let x' = f x in
    let y' = f y in
    if x' == x && y' == y then e else norm2 op x' y'
  in
  let map3 op ~f x y z =
    let x' = f x in
    let y' = f y in
    let z' = f z in
    if x' == x && y' == y && z' == z then e else norm3 op x' y' z'
  in
  let mapN op ~f xs =
    let xs' = IArray.map_endo ~f xs in
    if xs' == xs then e else normN op xs'
  in
  let map_set mk ~f args =
    let args' = Set.map ~f args in
    if args' == args then e else mk args'
  in
  let map_qset mk ~f args =
    let args' = Qset.map ~f:(fun arg q -> (f arg, q)) args in
    if args' == args then e else mk args'
  in
  match e with
  | And args -> map_set andN ~f args
  | Or args -> map_set orN ~f args
  | Add args -> map_qset addN ~f args
  | Mul args -> map_qset mulN ~f args
  | Ap1 (op, x) -> map1 op ~f x
  | Ap2 (op, x, y) -> map2 op ~f x y
  | Ap3 (op, x, y, z) -> map3 op ~f x y z
  | ApN (op, xs) -> mapN op ~f xs
  | RecN (_, xs) ->
      assert (
        xs == IArray.map_endo ~f xs
        || fail "Term.map does not support updating subterms of RecN." () ) ;
      e
  | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> e

let fold_map e ~init ~f =
  let s = ref init in
  let f x =
    let s', x' = f !s x in
    s := s' ;
    x'
  in
  let e' = map e ~f in
  (!s, e')

let map_rec_pre e ~f =
  let rec map_rec_pre_f memo e =
    match f e with
    | Some e' -> e'
    | None -> (
      match e with
      | RecN (op, xs) -> (
        match List.Assoc.find ~equal:( == ) memo e with
        | None ->
            let xs' = IArray.to_array xs in
            let e' = RecN (op, IArray.of_array xs') in
            let memo = List.Assoc.add ~equal:( == ) memo e e' in
            let changed = ref false in
            Array.map_inplace xs' ~f:(fun x ->
                let x' = map_rec_pre_f memo x in
                if x' != x then changed := true ;
                x' ) ;
            if !changed then e' else e
        | Some e' -> e' )
      | _ -> map ~f:(map_rec_pre_f memo) e )
  in
  map_rec_pre_f [] e

let fold_map_rec_pre e ~init ~f =
  let rec fold_map_rec_pre_f memo s e =
    match f s e with
    | Some (s, e') -> (s, e')
    | None -> (
      match e with
      | RecN (op, xs) -> (
        match List.Assoc.find ~equal:( == ) memo e with
        | None ->
            let xs' = IArray.to_array xs in
            let e' = RecN (op, IArray.of_array xs') in
            let memo = List.Assoc.add ~equal:( == ) memo e e' in
            let changed = ref false in
            let s =
              Array.fold_map_inplace ~init:s xs' ~f:(fun s x ->
                  let s, x' = fold_map_rec_pre_f memo s x in
                  if x' != x then changed := true ;
                  (s, x') )
            in
            if !changed then (s, e') else (s, e)
        | Some e' -> (s, e') )
      | _ -> fold_map ~f:(fold_map_rec_pre_f memo) ~init:s e )
  in
  fold_map_rec_pre_f [] init e

let rename sub e =
  map_rec_pre e ~f:(function
    | Var _ as v -> Some (Var.Subst.apply sub v)
    | _ -> None )

(** Traverse *)

let iter e ~f =
  match e with
  | Ap1 (_, x) -> f x
  | Ap2 (_, x, y) -> f x ; f y
  | Ap3 (_, x, y, z) -> f x ; f y ; f z
  | ApN (_, xs) | RecN (_, xs) -> IArray.iter ~f xs
  | And args | Or args -> Set.iter ~f args
  | Add args | Mul args -> Qset.iter ~f:(fun arg _ -> f arg) args
  | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> ()

let exists e ~f =
  match e with
  | Ap1 (_, x) -> f x
  | Ap2 (_, x, y) -> f x || f y
  | Ap3 (_, x, y, z) -> f x || f y || f z
  | ApN (_, xs) | RecN (_, xs) -> IArray.exists ~f xs
  | And args | Or args -> Set.exists ~f args
  | Add args | Mul args -> Qset.exists ~f:(fun arg _ -> f arg) args
  | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> false

let for_all e ~f =
  match e with
  | Ap1 (_, x) -> f x
  | Ap2 (_, x, y) -> f x && f y
  | Ap3 (_, x, y, z) -> f x && f y && f z
  | ApN (_, xs) | RecN (_, xs) -> IArray.for_all ~f xs
  | And args | Or args -> Set.for_all ~f args
  | Add args | Mul args -> Qset.for_all ~f:(fun arg _ -> f arg) args
  | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> true

let fold e ~init:s ~f =
  match e with
  | Ap1 (_, x) -> f x s
  | Ap2 (_, x, y) -> f y (f x s)
  | Ap3 (_, x, y, z) -> f z (f y (f x s))
  | ApN (_, xs) | RecN (_, xs) ->
      IArray.fold ~f:(fun s x -> f x s) xs ~init:s
  | And args | Or args -> Set.fold ~f:(fun s e -> f e s) args ~init:s
  | Add args | Mul args -> Qset.fold ~f:(fun e _ s -> f e s) args ~init:s
  | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> s

let iter_terms e ~f =
  let iter_terms_ iter_terms_ e =
    ( match e with
    | Ap1 (_, x) -> iter_terms_ x
    | Ap2 (_, x, y) -> iter_terms_ x ; iter_terms_ y
    | Ap3 (_, x, y, z) -> iter_terms_ x ; iter_terms_ y ; iter_terms_ z
    | ApN (_, xs) | RecN (_, xs) -> IArray.iter ~f:iter_terms_ xs
    | And args | Or args -> Set.iter args ~f:iter_terms_
    | Add args | Mul args ->
        Qset.iter args ~f:(fun arg _ -> iter_terms_ arg)
    | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> () ) ;
    f e
  in
  fix iter_terms_ (fun _ -> ()) e

let fold_terms e ~init ~f =
  let fold_terms_ fold_terms_ e s =
    let s =
      match e with
      | Ap1 (_, x) -> fold_terms_ x s
      | Ap2 (_, x, y) -> fold_terms_ y (fold_terms_ x s)
      | Ap3 (_, x, y, z) -> fold_terms_ z (fold_terms_ y (fold_terms_ x s))
      | ApN (_, xs) | RecN (_, xs) ->
          IArray.fold ~f:(fun s x -> fold_terms_ x s) xs ~init:s
      | And args | Or args ->
          Set.fold args ~init:s ~f:(fun s x -> fold_terms_ x s)
      | Add args | Mul args ->
          Qset.fold args ~init:s ~f:(fun arg _ s -> fold_terms_ arg s)
      | Var _ | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> s
    in
    f s e
  in
  fix fold_terms_ (fun _ s -> s) e init

let iter_vars e ~f =
  iter_terms e ~f:(function Var _ as v -> f (v :> Var.t) | _ -> ())

let exists_vars e ~f =
  with_return (fun {return} ->
      iter_vars e ~f:(fun v -> if f v then return true) ;
      false )

let fold_vars e ~init ~f =
  fold_terms e ~init ~f:(fun s -> function
    | Var _ as v -> f s (v :> Var.t) | _ -> s )

(** Query *)

let fv e = fold_vars e ~f:Set.add ~init:Var.Set.empty
let is_true = function Integer {data} -> Z.is_true data | _ -> false
let is_false = function Integer {data} -> Z.is_false data | _ -> false

let rec is_constant = function
  | Var _ -> false
  | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> true
  | a -> for_all ~f:is_constant a

let height e =
  let height_ height_ = function
    | Var _ -> 0
    | Ap1 (_, a) -> 1 + height_ a
    | Ap2 (_, a, b) -> 1 + max (height_ a) (height_ b)
    | Ap3 (_, a, b, c) -> 1 + max (height_ a) (max (height_ b) (height_ c))
    | ApN (_, v) | RecN (_, v) ->
        1 + IArray.fold v ~init:0 ~f:(fun m a -> max m (height_ a))
    | And bs | Or bs ->
        1 + Set.fold bs ~init:0 ~f:(fun m a -> max m (height_ a))
    | Add qs | Mul qs ->
        1 + Qset.fold qs ~init:0 ~f:(fun a _ m -> max m (height_ a))
    | Label _ | Nondet _ | Float _ | Integer _ | Rational _ -> 0
  in
  fix height_ (fun _ -> 0) e

(** Solve *)

let exists_fv_in vs qset =
  Qset.exists qset ~f:(fun e _ -> exists_vars e ~f:(Var.Set.mem vs))

(* solve [0 = rejected_sum + (coeff × mono) + sum] *)
let solve_for_mono rejected_sum coeff mono sum =
  match mono with
  | Integer _ -> None
  | _ ->
      if exists_fv_in (fv mono) sum then None
      else
        Some
          ( mono
          , Sum.to_term
              (Sum.mul_const
                 (Q.inv (Q.neg coeff))
                 (Qset.union rejected_sum sum)) )

(* solve [0 = rejected + sum] *)
let rec solve_sum rejected_sum sum =
  let* mono, coeff, sum = Qset.pop_min_elt sum in
  solve_for_mono rejected_sum coeff mono sum
  |> Option.or_else ~f:(fun () ->
         solve_sum (Qset.add rejected_sum mono coeff) sum )

(* solve [0 = e] *)
let solve_zero_eq ?for_ e =
  [%Trace.call fun {pf} -> pf "0 = %a%a" pp e (Option.pp " for %a" pp) for_]
  ;
  ( match e with
  | Add sum -> (
    match for_ with
    | None -> solve_sum Qset.empty sum
    | Some mono ->
        let* coeff, sum = Qset.find_and_remove sum mono in
        solve_for_mono Qset.empty coeff mono sum )
  | _ -> None )
  |>
  [%Trace.retn fun {pf} s ->
    pf "%a"
      (Option.pp "%a" (fun fs (c, r) ->
           Format.fprintf fs "%a ↦ %a" pp c pp r ))
      s ;
    match (for_, s) with
    | Some f, Some (c, _) -> assert (equal f c)
    | _ -> ()]
