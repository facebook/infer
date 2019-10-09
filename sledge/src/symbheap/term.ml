(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Terms *)

module Z = struct
  include Z

  (** Interpret as a bounded integer with specified signedness and width. *)
  let extract ?(unsigned = false) bits z =
    if unsigned then Z.extract z 0 bits else Z.signed_extract z 0 bits
end

module rec T : sig
  type qset = Qset.M(T).t [@@deriving compare, equal, hash, sexp]

  type t =
    (* nary: arithmetic, numeric and pointer *)
    | Add of {args: qset}
    | Mul of {args: qset}
    (* pointer and memory constants and operations *)
    | Splat of {byt: t; siz: t}
    | Memory of {siz: t; arr: t}
    | Concat of {args: t vector}
    (* nullary *)
    | Var of {id: int; name: string}
    | Nondet of {msg: string}
    | Label of {parent: string; name: string}
    (* curried application *)
    | App of {op: t; arg: t}
    (* binary: comparison *)
    | Eq
    | Dq
    | Gt
    | Ge
    | Lt
    | Le
    | Ord
    | Uno
    (* binary: arithmetic, numeric and pointer *)
    | Div
    | Rem
    (* binary: boolean / bitwise *)
    | And
    | Or
    | Xor
    | Shl
    | Lshr
    | Ashr
    (* ternary: conditional *)
    | Conditional
    (* array/struct constants and operations *)
    | Record
    | Select
    | Update
    | Struct_rec of {elts: t vector}  (** NOTE: may be cyclic *)
    (* unary: conversion *)
    | Extract of {unsigned: bool; bits: int}
    | Convert of {unsigned: bool; dst: Typ.t; src: Typ.t}
    (* numeric constants *)
    | Integer of {data: Z.t}
    | Float of {data: string}
  [@@deriving compare, equal, hash, sexp]

  (* Note: solve (and invariant) requires Qset.min_elt to return a
     non-coefficient, so Integer terms must compare higher than any valid
     monomial *)

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end = struct
  include T0 include Comparator.Make (T0)
end

(* auxiliary definition for safe recursive module initialization *)
and T0 : sig
  type qset = Qset.M(T).t [@@deriving compare, equal, hash, sexp]

  type t =
    | Add of {args: qset}
    | Mul of {args: qset}
    | Splat of {byt: t; siz: t}
    | Memory of {siz: t; arr: t}
    | Concat of {args: t vector}
    | Var of {id: int; name: string}
    | Nondet of {msg: string}
    | Label of {parent: string; name: string}
    | App of {op: t; arg: t}
    | Eq
    | Dq
    | Gt
    | Ge
    | Lt
    | Le
    | Ord
    | Uno
    | Div
    | Rem
    | And
    | Or
    | Xor
    | Shl
    | Lshr
    | Ashr
    | Conditional
    | Record
    | Select
    | Update
    | Struct_rec of {elts: t vector}
    | Extract of {unsigned: bool; bits: int}
    | Convert of {unsigned: bool; dst: Typ.t; src: Typ.t}
    | Integer of {data: Z.t}
    | Float of {data: string}
  [@@deriving compare, equal, hash, sexp]
end = struct
  type qset = Qset.M(T).t [@@deriving compare, equal, hash, sexp]

  type t =
    | Add of {args: qset}
    | Mul of {args: qset}
    | Splat of {byt: t; siz: t}
    | Memory of {siz: t; arr: t}
    | Concat of {args: t vector}
    | Var of {id: int; name: string}
    | Nondet of {msg: string}
    | Label of {parent: string; name: string}
    | App of {op: t; arg: t}
    | Eq
    | Dq
    | Gt
    | Ge
    | Lt
    | Le
    | Ord
    | Uno
    | Div
    | Rem
    | And
    | Or
    | Xor
    | Shl
    | Lshr
    | Ashr
    | Conditional
    | Record
    | Select
    | Update
    | Struct_rec of {elts: t vector}
    | Extract of {unsigned: bool; bits: int}
    | Convert of {unsigned: bool; dst: Typ.t; src: Typ.t}
    | Integer of {data: Z.t}
    | Float of {data: string}
  [@@deriving compare, equal, hash, sexp]
end

(* suppress spurious "Warning 60: unused module T0." *)
type _t = T0.t

include T

let empty_map = Map.empty (module T)
let empty_qset = Qset.empty (module T)

let fix (f : (t -> 'a as 'f) -> 'f) (bot : 'f) (e : t) : 'a =
  let rec fix_f seen e =
    match e with
    | Struct_rec _ ->
        if List.mem ~equal:( == ) seen e then f bot e
        else f (fix_f (e :: seen)) e
    | _ -> f (fix_f seen) e
  in
  let rec fix_f_seen_nil e =
    match e with Struct_rec _ -> f (fix_f [e]) e | _ -> f fix_f_seen_nil e
  in
  fix_f_seen_nil e

let fix_flip (f : ('z -> t -> 'a as 'f) -> 'f) (bot : 'f) (z : 'z) (e : t) =
  fix (fun f' e z -> f (fun z e -> f' e z) z e) (fun e z -> bot z e) e z

let uncurry =
  let rec uncurry_ acc_args = function
    | App {op; arg} -> uncurry_ (arg :: acc_args) op
    | op -> (op, acc_args)
  in
  uncurry_ []

let rec pp ?is_x fs term =
  let get_var_style var =
    match is_x with
    | None -> `None
    | Some is_x -> if not (is_x var) then `Bold else `Cyan
  in
  let pp_ pp fs term =
    let pf fmt =
      Format.pp_open_box fs 2 ;
      Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
    in
    match term with
    | Var {name; id= 0} as var ->
        Trace.pp_styled (get_var_style var) "%%%s" fs name
    | Var {name; id} as var ->
        Trace.pp_styled (get_var_style var) "%%%s_%d" fs name id
    | Nondet {msg} -> pf "nondet \"%s\"" msg
    | Label {name} -> pf "%s" name
    | Splat {byt; siz} -> pf "%a^%a" pp byt pp siz
    | Memory {siz; arr} -> pf "@<1>⟨%a,%a@<1>⟩" pp siz pp arr
    | Concat {args} -> pf "%a" (Vector.pp "@,^" pp) args
    | Integer {data} -> Trace.pp_styled `Magenta "%a" fs Z.pp data
    | Float {data} -> pf "%s" data
    | Eq -> pf "="
    | Dq -> pf "@<1>≠"
    | Gt -> pf ">"
    | Ge -> pf "@<1>≥"
    | Lt -> pf "<"
    | Le -> pf "@<1>≤"
    | Ord -> pf "ord"
    | Uno -> pf "uno"
    | Add {args} ->
        let pp_poly_term fs (monomial, coefficient) =
          match monomial with
          | Integer {data} when Z.equal Z.one data -> Q.pp fs coefficient
          | _ when Q.equal Q.one coefficient -> pp fs monomial
          | _ ->
              Format.fprintf fs "%a @<1>× %a" Q.pp coefficient pp monomial
        in
        pf "(%a)" (Qset.pp "@ + " pp_poly_term) args
    | Mul {args} ->
        let pp_mono_term fs (factor, exponent) =
          if Q.equal Q.one exponent then pp fs factor
          else Format.fprintf fs "%a^%a" pp factor Q.pp exponent
        in
        pf "(%a)" (Qset.pp "@ @<2>× " pp_mono_term) args
    | Div -> pf "/"
    | Rem -> pf "rem"
    | And -> pf "&&"
    | Or -> pf "||"
    | Xor -> pf "xor"
    | App {op= App {op= Xor; arg}; arg= Integer {data}} when Z.is_true data
      ->
        pf "¬%a" pp arg
    | App {op= App {op= Xor; arg= Integer {data}}; arg} when Z.is_true data
      ->
        pf "¬%a" pp arg
    | Shl -> pf "shl"
    | Lshr -> pf "lshr"
    | Ashr -> pf "ashr"
    | Conditional -> pf "(_?_:_)"
    | App {op= Conditional; arg= cnd} -> pf "(%a@ ? _:_)" pp cnd
    | App {op= App {op= Conditional; arg= cnd}; arg= thn} ->
        pf "(%a@ ? %a@ : _)" pp cnd pp thn
    | App {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}
      ->
        pf "(%a@ ? %a@ : %a)" pp cnd pp thn pp els
    | Select -> pf "_[_]"
    | App {op= Select; arg= rcd} -> pf "%a[_]" pp rcd
    | App {op= App {op= Select; arg= rcd}; arg= idx} ->
        pf "%a[%a]" pp rcd pp idx
    | Update -> pf "[_|_→_]"
    | App {op= Update; arg= rcd} -> pf "[%a@ @[| _→_@]]" pp rcd
    | App {op= App {op= Update; arg= rcd}; arg= elt} ->
        pf "[%a@ @[| _→ %a@]]" pp rcd pp elt
    | App {op= App {op= App {op= Update; arg= rcd}; arg= elt}; arg= idx} ->
        pf "[%a@ @[| %a → %a@]]" pp rcd pp idx pp elt
    | Record -> pf "{_}"
    | App {op; arg} -> (
      match uncurry term with
      | Record, elts -> pf "{%a}" pp_record elts
      | op, [x; y] -> pf "(%a@ %a %a)" pp x pp op pp y
      | _ -> pf "(%a@ %a)" pp op pp arg )
    | Struct_rec {elts} -> pf "{|%a|}" (Vector.pp ",@ " pp) elts
    | Extract {unsigned; bits} ->
        pf "(%s%i)" (if unsigned then "u" else "i") bits
    | Convert {unsigned= true; dst; src= Integer {bits}} ->
        pf "(%a)(u%i)" Typ.pp dst bits
    | Convert {unsigned= true; dst= Integer {bits}; src} ->
        pf "(u%i)(%a)" bits Typ.pp src
    | Convert {dst; src} -> pf "(%a)(%a)" Typ.pp dst Typ.pp src
  in
  fix_flip pp_ (fun _ _ -> ()) fs term

and pp_record fs elts =
  [%Trace.fprintf
    fs "%a"
      (fun fs elts ->
        let elta = Array.of_list elts in
        match
          String.init (Array.length elta) ~f:(fun i ->
              match elta.(i) with
              | Integer {data} -> Char.of_int_exn (Z.to_int data)
              | _ -> raise (Invalid_argument "not a string") )
        with
        | s -> Format.fprintf fs "@[<h>%s@]" (String.escaped s)
        | exception _ ->
            Format.fprintf fs "@[<h>%a@]" (List.pp ",@ " pp) elts )
      elts]

type term = t

let pp_t = pp ?is_x:None
let pp_full = pp
let pp = pp_t

(** Invariant *)

(* an indeterminate (factor of a monomial) is any non-Add/Mul/Integer term *)
let rec assert_indeterminate = function
  | App {op} -> assert_indeterminate op
  | Integer _ | Add _ | Mul _ -> assert false
  | _ -> assert true

(* a monomial is a power product of factors, e.g.
 *     ∏ᵢ xᵢ^nᵢ
 * for (non-constant) indeterminants xᵢ and positive integer exponents nᵢ
 *)
let assert_monomial mono =
  match mono with
  | Mul {args} ->
      Qset.iter args ~f:(fun factor exponent ->
          assert (Q.sign exponent > 0) ;
          assert_indeterminate factor |> Fn.id )
  | _ -> assert_indeterminate mono |> Fn.id

(* a polynomial term is a monomial multiplied by a non-zero coefficient
 *     c × ∏ᵢ xᵢ
 *)
let assert_poly_term mono coeff =
  assert (not (Q.equal Q.zero coeff)) ;
  match mono with
  | Integer {data} -> assert (Z.equal Z.one data)
  | Mul {args} ->
      ( match Qset.min_elt args with
      | None | Some (Integer _, _) -> assert false
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
  | Add {args} ->
      ( match Qset.min_elt args with
      | None | Some (Integer _, _) -> assert false
      | Some (_, k) -> assert (Qset.length args > 1 || not (Q.equal Q.one k))
      ) ;
      Qset.iter args ~f:(fun m c -> assert_poly_term m c |> Fn.id)
  | _ -> assert false

let invariant ?(partial = false) e =
  Invariant.invariant [%here] e [%sexp_of: t]
  @@ fun () ->
  let op, args = uncurry e in
  let assert_arity arity =
    let nargs = List.length args in
    assert (nargs = arity || (partial && nargs < arity))
  in
  match op with
  | App _ -> fail "uncurry cannot return App" ()
  | Integer _ -> assert_arity 0
  | Var _ | Nondet _ | Label _ | Float _ -> assert_arity 0
  | Extract _ -> assert_arity 1
  | Convert {dst; src} ->
      assert_arity 1 ;
      assert (Typ.convertible src dst)
  | Add _ -> assert_polynomial e |> Fn.id
  | Mul _ -> assert_monomial e |> Fn.id
  | Eq | Dq | Gt | Ge | Lt | Le | Div | Rem | And | Or | Xor | Shl | Lshr
   |Ashr ->
      assert_arity 2
  | Concat {args} -> assert (Vector.length args <> 1)
  | Splat {siz} -> (
    match siz with
    | Integer {data} -> assert (not (Z.equal Z.zero data))
    | _ -> () )
  | Memory _ -> assert true
  | Ord | Uno | Select -> assert_arity 2
  | Conditional | Update -> assert_arity 3
  | Record -> assert (partial || not (List.is_empty args))
  | Struct_rec {elts} ->
      assert (not (Vector.is_empty elts)) ;
      assert_arity 0

(** Variables are the terms constructed by [Var] *)
module Var = struct
  include T

  let pp = pp

  type var = t

  let of_reg (r : Reg.t) =
    match (r :> Exp.t) with
    | Reg {name} -> Var {id= 0; name}
    | _ -> violates Reg.invariant r

  module Set = struct
    include (
      Set :
        module type of Set with type ('elt, 'cmp) t := ('elt, 'cmp) Set.t )

    type t = Set.M(T).t [@@deriving compare, equal, sexp]

    let pp_full ?is_x vs = Set.pp (pp_full ?is_x) vs
    let pp = pp_full ?is_x:None
    let empty = Set.empty (module T)
    let of_option = Option.fold ~f:Set.add ~init:empty
    let of_list = Set.of_list (module T)
    let of_vector = Set.of_vector (module T)
    let of_regs = Set.fold ~init:empty ~f:(fun s r -> add s (of_reg r))
  end

  module Map = struct
    include (
      Map :
        module type of Map
        with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t )

    type 'v t = 'v Map.M(T).t [@@deriving compare, equal, sexp]
  end

  let invariant x =
    Invariant.invariant [%here] x [%sexp_of: t]
    @@ fun () -> match x with Var _ -> invariant x | _ -> assert false

  let id = function Var {id} -> id | x -> violates invariant x
  let name = function Var {name} -> name | x -> violates invariant x

  let of_term = function
    | Var _ as v -> Some (v |> check invariant)
    | _ -> None

  let fresh name ~(wrt : Set.t) =
    let max = match Set.max_elt wrt with None -> 0 | Some max -> id max in
    let x' = Var {name; id= max + 1} in
    (x', Set.add wrt x')

  (** Variable renaming substitutions *)
  module Subst = struct
    type t = T.t Map.M(T).t [@@deriving compare, equal, sexp]

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

    let pp fs s =
      Format.fprintf fs "@[<1>[%a]@]"
        (List.pp ",@ " (fun fs (k, v) ->
             Format.fprintf fs "@[[%a ↦ %a]@]" pp_t k pp_t v ))
        (Map.to_alist s)

    let empty = empty_map
    let is_empty = Map.is_empty

    let freshen vs ~wrt =
      let xs = Set.inter wrt vs in
      let wrt = Set.union wrt vs in
      Set.fold xs ~init:(empty, wrt) ~f:(fun (sub, wrt) x ->
          let x', wrt = fresh (name x) ~wrt in
          let sub = Map.add_exn sub ~key:x ~data:x' in
          (sub, wrt) )
      |> fst |> check invariant

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

    let apply sub v = try Map.find_exn sub v with Caml.Not_found -> v

    let apply_set sub vs =
      Map.fold sub ~init:vs ~f:(fun ~key ~data vs ->
          let vs' = Set.remove vs key in
          if Set.to_tree vs' == Set.to_tree vs then vs
          else (
            assert (not (Set.equal vs' vs)) ;
            Set.add vs' data ) )
  end
end

let fold_terms e ~init ~f =
  let fold_terms_ fold_terms_ e z =
    let z =
      match e with
      | App {op= x; arg= y}
       |Splat {byt= x; siz= y}
       |Memory {siz= x; arr= y} ->
          fold_terms_ y (fold_terms_ x z)
      | Add {args} | Mul {args} ->
          Qset.fold args ~init:z ~f:(fun arg _ z -> fold_terms_ arg z)
      | Concat {args} | Struct_rec {elts= args} ->
          Vector.fold args ~init:z ~f:(fun z elt -> fold_terms_ elt z)
      | _ -> z
    in
    f z e
  in
  fix fold_terms_ (fun _ z -> z) e init

let iter_terms e ~f = fold_terms e ~init:() ~f:(fun () e -> f e)

let fold_vars e ~init ~f =
  fold_terms e ~init ~f:(fun z -> function
    | Var _ as v -> f z (v :> Var.t) | _ -> z )

let fv e = fold_vars e ~f:Set.add ~init:Var.Set.empty

(** Construct *)

let var x = x
let nondet msg = Nondet {msg} |> check invariant
let label ~parent ~name = Label {parent; name} |> check invariant
let integer data = Integer {data} |> check invariant
let null = integer Z.zero
let zero = integer Z.zero
let one = integer Z.one
let minus_one = integer Z.minus_one
let bool b = integer (Z.of_bool b)
let true_ = bool true
let false_ = bool false
let float data = Float {data} |> check invariant

let simp_extract ~unsigned bits arg =
  match arg with
  | Integer {data} -> integer (Z.extract ~unsigned bits data)
  | _ -> App {op= Extract {unsigned; bits}; arg}

let simp_convert ~unsigned dst src arg =
  if (not unsigned) && Typ.castable dst src then arg
  else
    match (dst, src, arg) with
    | Integer {bits= m}, Integer {bits= n}, Integer {data} ->
        integer (Z.extract ~unsigned (min m n) data)
    | _ -> App {op= Convert {unsigned; dst; src}; arg}

let simp_gt x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j} -> bool (Z.gt i j)
  | _ -> App {op= App {op= Gt; arg= x}; arg= y}

let simp_ge x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j} -> bool (Z.geq i j)
  | _ -> App {op= App {op= Ge; arg= x}; arg= y}

let simp_lt x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j} -> bool (Z.lt i j)
  | _ -> App {op= App {op= Lt; arg= x}; arg= y}

let simp_le x y =
  match (x, y) with
  | Integer {data= i}, Integer {data= j} -> bool (Z.leq i j)
  | _ -> App {op= App {op= Le; arg= x}; arg= y}

let simp_ord x y = App {op= App {op= Ord; arg= x}; arg= y}
let simp_uno x y = App {op= App {op= Uno; arg= x}; arg= y}

let sum_mul_const const sum =
  assert (not (Q.equal Q.zero const)) ;
  if Q.equal Q.one const then sum
  else Qset.map_counts ~f:(fun _ -> Q.mul const) sum

let rec sum_to_term sum =
  match Qset.length sum with
  | 0 -> zero
  | 1 -> (
    match Qset.min_elt sum with
    | Some (Integer _, q) -> rational q
    | Some (arg, q) when Q.equal Q.one q -> arg
    | _ -> Add {args= sum} )
  | _ -> Add {args= sum}

and rational Q.{num; den} = simp_div (integer num) (integer den)

and simp_div x y =
  match (x, y) with
  (* i / j *)
  | Integer {data= i}, Integer {data= j} when not (Z.equal Z.zero j) ->
      integer (Z.div i j)
  (* e / 1 ==> e *)
  | e, Integer {data} when Z.equal Z.one data -> e
  (* (∑ᵢ cᵢ × Xᵢ) / z ==> ∑ᵢ cᵢ/z × Xᵢ *)
  | Add {args}, Integer {data} ->
      sum_to_term (sum_mul_const Q.(inv (of_z data)) args)
  | _ -> App {op= App {op= Div; arg= x}; arg= y}

let simp_rem x y =
  match (x, y) with
  (* i % j *)
  | Integer {data= i}, Integer {data= j} when not (Z.equal Z.zero j) ->
      integer (Z.rem i j)
  (* e % 1 ==> 0 *)
  | _, Integer {data} when Z.equal Z.one data -> zero
  | _ -> App {op= App {op= Rem; arg= x}; arg= y}

(* Sums of polynomial terms represented by multisets. A sum ∑ᵢ cᵢ × Xᵢ of
   monomials Xᵢ with coefficients cᵢ is represented by a multiset where the
   elements are Xᵢ with multiplicities cᵢ. A constant is treated as the
   coefficient of the empty monomial, which is the unit of multiplication 1. *)
module Sum = struct
  let empty = empty_qset

  let add coeff term sum =
    assert (not (Q.equal Q.zero coeff)) ;
    match term with
    | Integer {data} when Z.equal Z.zero data -> sum
    | Integer {data} -> Qset.add sum one Q.(coeff * of_z data)
    | _ -> Qset.add sum term coeff

  let singleton ?(coeff = Q.one) term = add coeff term empty

  let map sum ~f =
    Qset.fold sum ~init:empty ~f:(fun e c sum -> add c (f e) sum)

  let mul_const = sum_mul_const
  let to_term = sum_to_term
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
    (* (c × ∑ᵢ cᵢ × Xᵢ) + s ==> (∑ᵢ (c × cᵢ) × Xᵢ) + s *)
    | Add {args}, _ -> simp_add_ (Sum.mul_const coeff args) poly
    (* (c₀ × X₀) + (∑ᵢ₌₁ⁿ cᵢ × Xᵢ) ==> ∑ᵢ₌₀ⁿ cᵢ × Xᵢ *)
    | _, Add {args} -> Sum.to_term (Sum.add coeff term args)
    (* (c₁ × X₁) + X₂ ==> ∑ᵢ₌₁² cᵢ × Xᵢ for c₂ = 1 *)
    | _ -> Sum.to_term (Sum.add coeff term (Sum.singleton poly))
  in
  Qset.fold ~f es ~init:poly

let simp_add es = simp_add_ es zero
let simp_add2 e f = simp_add_ (Sum.singleton e) f

(* Products of indeterminants represented by multisets. A product ∏ᵢ xᵢ^nᵢ
   of indeterminates xᵢ is represented by a multiset where the elements are
   xᵢ and the multiplicities are the exponents nᵢ. *)
module Prod = struct
  let empty = empty_qset

  let add term prod =
    assert (match term with Integer _ -> false | _ -> true) ;
    Qset.add prod term Q.one

  let singleton term = add term empty
  let union = Qset.union
end

let rec simp_mul2 e f =
  match (e, f) with
  (* c₁ × c₂ ==> c₁×c₂ *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.mul i j)
  (* 0 × f ==> 0 *)
  | Integer {data}, _ when Z.equal Z.zero data -> e
  (* e × 0 ==> 0 *)
  | _, Integer {data} when Z.equal Z.zero data -> f
  (* c × (∑ᵤ cᵤ × ∏ⱼ yᵤⱼ) ==> ∑ᵤ c × cᵤ × ∏ⱼ yᵤⱼ *)
  | Integer {data}, Add {args} | Add {args}, Integer {data} ->
      Sum.to_term (Sum.mul_const (Q.of_z data) args)
  (* c₁ × x₁ ==> ∑ᵢ₌₁ cᵢ × xᵢ *)
  | Integer {data= c}, x | x, Integer {data= c} ->
      Sum.to_term (Sum.singleton ~coeff:(Q.of_z c) x)
  (* (∏ᵤ₌₀ⁱ xᵤ) × (∏ᵥ₌ᵢ₊₁ⁿ xᵥ) ==> ∏ⱼ₌₀ⁿ xⱼ *)
  | Mul {args= xs1}, Mul {args= xs2} -> Mul {args= Prod.union xs1 xs2}
  (* (∏ᵢ xᵢ) × (∑ᵤ cᵤ × ∏ⱼ yᵤⱼ) ==> ∑ᵤ cᵤ × ∏ᵢ xᵢ × ∏ⱼ yᵤⱼ *)
  | (Mul {args= prod} as m), Add {args= sum}
   |Add {args= sum}, (Mul {args= prod} as m) ->
      Sum.to_term
        (Sum.map sum ~f:(function
          | Mul {args} -> Mul {args= Prod.union prod args}
          | Integer _ as c -> simp_mul2 c m
          | mono -> Mul {args= Prod.add mono prod} ))
  (* x₀ × (∏ᵢ₌₁ⁿ xᵢ) ==> ∏ᵢ₌₀ⁿ xᵢ *)
  | Mul {args= xs1}, x | x, Mul {args= xs1} -> Mul {args= Prod.add x xs1}
  (* e × (∑ᵤ cᵤ × ∏ⱼ yᵤⱼ) ==> ∑ᵤ e × cᵤ × ∏ⱼ yᵤⱼ *)
  | Add {args}, e | e, Add {args} ->
      simp_add (Sum.map ~f:(fun m -> simp_mul2 e m) args)
  (* x₁ × x₂ ==> ∏ᵢ₌₁² xᵢ *)
  | _ -> Mul {args= Prod.add e (Prod.singleton f)}

let simp_mul es =
  (* (bas ^ pwr) × term *)
  let rec mul_pwr bas pwr term =
    if Q.equal Q.zero pwr then term
    else mul_pwr bas Q.(pwr - one) (simp_mul2 bas term)
  in
  Qset.fold es ~init:one ~f:(fun bas pwr term ->
      if Q.sign pwr >= 0 then mul_pwr bas pwr term
      else simp_div term (mul_pwr bas (Q.neg pwr) one) )

let simp_negate x = simp_mul2 minus_one x

let simp_sub x y =
  match (x, y) with
  (* i - j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.sub i j)
  (* x - y ==> x + (-1 * y) *)
  | _ -> simp_add2 x (simp_negate y)

let simp_cond cnd thn els =
  match cnd with
  (* ¬(true ? t : e) ==> t *)
  | Integer {data} when Z.is_true data -> thn
  (* ¬(false ? t : e) ==> e *)
  | Integer {data} when Z.is_false data -> els
  | _ ->
      App {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}

let rec simp_and x y =
  match (x, y) with
  (* i && j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logand i j)
  (* e && true ==> e *)
  | (Integer {data}, e | e, Integer {data}) when Z.is_true data -> e
  (* e && false ==> 0 *)
  | ((Integer {data} as f), _ | _, (Integer {data} as f))
    when Z.is_false data ->
      f
  (* e && (c ? t : f) ==> (c ? e && t : e && f) *)
  | e, App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}
   |App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}, e ->
      simp_cond c (simp_and e t) (simp_and e f)
  (* e && e ==> e *)
  | _ when equal x y -> x
  | _ -> App {op= App {op= And; arg= x}; arg= y}

let rec simp_or x y =
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
  | e, App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}
   |App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}, e ->
      simp_cond c (simp_or e t) (simp_or e f)
  (* e || e ==> e *)
  | _ when equal x y -> x
  | _ -> App {op= App {op= Or; arg= x}; arg= y}

let rec is_boolean = function
  | App {op= App {op= Eq | Dq | Gt | Ge | Lt | Le}}
   |App {op= Convert {dst= Integer {bits= 1}}} ->
      true
  | App
      { op= App {op= Div | Rem | And | Or | Xor | Shl | Lshr | Ashr; arg= x}
      ; arg= y }
   |App {op= App {op= App {op= Conditional}; arg= x}; arg= y} ->
      is_boolean x || is_boolean y
  | _ -> false

let rec simp_not term =
  match term with
  (* ¬(x = y) ==> x ≠ y *)
  | App {op= App {op= Eq; arg= x}; arg= y} -> simp_dq x y
  (* ¬(x ≠ y) ==> x = y *)
  | App {op= App {op= Dq; arg= x}; arg= y} -> simp_eq x y
  (* ¬(x > y) ==> x <= y *)
  | App {op= App {op= Gt; arg= x}; arg= y} -> simp_le x y
  (* ¬(x >= y) ==> x < y *)
  | App {op= App {op= Ge; arg= x}; arg= y} -> simp_lt x y
  (* ¬(x < y) ==> x >= y *)
  | App {op= App {op= Lt; arg= x}; arg= y} -> simp_ge x y
  (* ¬(x <= y) ==> x > y *)
  | App {op= App {op= Le; arg= x}; arg= y} -> simp_gt x y
  (* ¬(x ≠ nan ∧ y ≠ nan) ==> x = nan ∨ y = nan *)
  | App {op= App {op= Ord; arg= x}; arg= y} -> simp_uno x y
  (* ¬(x = nan ∨ y = nan) ==> x ≠ nan ∧ y ≠ nan *)
  | App {op= App {op= Uno; arg= x}; arg= y} -> simp_ord x y
  (* ¬(a ∧ b) ==> ¬a ∨ ¬b *)
  | App {op= App {op= And; arg= x}; arg= y} ->
      simp_or (simp_not x) (simp_not y)
  (* ¬(a ∨ b) ==> ¬a ∧ ¬b *)
  | App {op= App {op= Or; arg= x}; arg= y} ->
      simp_and (simp_not x) (simp_not y)
  (* ¬(c ? t : e) ==> c ? ¬t : ¬e *)
  | App {op= App {op= App {op= Conditional; arg= cnd}; arg= thn}; arg= els}
    ->
      simp_cond cnd (simp_not thn) (simp_not els)
  (* ¬i ==> -i-1 *)
  | Integer {data} -> integer (Z.lognot data)
  (* ¬e ==> true xor e *)
  | e -> App {op= App {op= Xor; arg= true_}; arg= e}

and simp_eq x y =
  match (x, y) with
  (* i = j *)
  | Integer {data= i}, Integer {data= j} -> bool (Z.equal i j)
  (* b = false ==> ¬b *)
  | b, Integer {data} when Z.is_false data && is_boolean b -> simp_not b
  | Integer {data}, b when Z.is_false data && is_boolean b -> simp_not b
  (* b = true ==> b *)
  | b, Integer {data} when Z.is_true data && is_boolean b -> b
  | Integer {data}, b when Z.is_true data && is_boolean b -> b
  (* e = (c ? t : f) ==> (c ? e = t : e = f) *)
  | e, App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}
   |App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}, e ->
      simp_cond c (simp_eq e t) (simp_eq e f)
  (* e = e ==> true *)
  | x, y when equal x y -> bool true
  | x, y -> App {op= App {op= Eq; arg= x}; arg= y}

and simp_dq x y =
  match (x, y) with
  (* e ≠ (c ? t : f) ==> (c ? e ≠ t : e ≠ f) *)
  | e, App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}
   |App {op= App {op= App {op= Conditional; arg= c}; arg= t}; arg= f}, e ->
      simp_cond c (simp_dq e t) (simp_dq e f)
  | _ -> (
    match simp_eq x y with
    | App {op= App {op= Eq; arg= x}; arg= y} ->
        App {op= App {op= Dq; arg= x}; arg= y}
    | b -> simp_not b )

let simp_xor x y =
  match (x, y) with
  (* i xor j *)
  | Integer {data= i}, Integer {data= j} -> integer (Z.logxor i j)
  (* true xor b ==> ¬b *)
  | Integer {data}, b when Z.is_true data && is_boolean b -> simp_not b
  | b, Integer {data} when Z.is_true data && is_boolean b -> simp_not b
  | _ -> App {op= App {op= Xor; arg= x}; arg= y}

let simp_shl x y =
  match (x, y) with
  (* i shl j *)
  | Integer {data= i}, Integer {data= j} when Z.sign j >= 0 ->
      integer (Z.shift_left i (Z.to_int j))
  (* e shl 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Shl; arg= x}; arg= y}

let simp_lshr x y =
  match (x, y) with
  (* i lshr j *)
  | Integer {data= i}, Integer {data= j} when Z.sign j >= 0 ->
      integer (Z.shift_right_trunc i (Z.to_int j))
  (* e lshr 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Lshr; arg= x}; arg= y}

let simp_ashr x y =
  match (x, y) with
  (* i ashr j *)
  | Integer {data= i}, Integer {data= j} when Z.sign j >= 0 ->
      integer (Z.shift_right i (Z.to_int j))
  (* e ashr 0 ==> e *)
  | e, Integer {data} when Z.equal Z.zero data -> e
  | _ -> App {op= App {op= Ashr; arg= x}; arg= y}

(** Access *)

let iter e ~f =
  match e with
  | App {op= x; arg= y} | Splat {byt= x; siz= y} | Memory {siz= x; arr= y}
    ->
      f x ; f y
  | Add {args} | Mul {args} -> Qset.iter ~f:(fun arg _ -> f arg) args
  | Concat {args} | Struct_rec {elts= args} -> Vector.iter ~f args
  | _ -> ()

let fold e ~init:s ~f =
  match e with
  | App {op= x; arg= y} | Splat {byt= x; siz= y} | Memory {siz= x; arr= y}
    ->
      f y (f x s)
  | Add {args} | Mul {args} ->
      Qset.fold ~f:(fun e _ s -> f e s) args ~init:s
  | Concat {args} | Struct_rec {elts= args} ->
      Vector.fold ~f:(fun s e -> f e s) args ~init:s
  | _ -> s

let is_subterm ~sub ~sup =
  With_return.with_return
  @@ fun {return} ->
  iter_terms sup ~f:(fun e -> if equal sub e then return true) ;
  false

let app1 ?(partial = false) op arg =
  ( match (op, arg) with
  | App {op= Eq; arg= x}, y -> simp_eq x y
  | App {op= Dq; arg= x}, y -> simp_dq x y
  | App {op= Gt; arg= x}, y -> simp_gt x y
  | App {op= Ge; arg= x}, y -> simp_ge x y
  | App {op= Lt; arg= x}, y -> simp_lt x y
  | App {op= Le; arg= x}, y -> simp_le x y
  | App {op= Ord; arg= x}, y -> simp_ord x y
  | App {op= Uno; arg= x}, y -> simp_uno x y
  | App {op= Div; arg= x}, y -> simp_div x y
  | App {op= Rem; arg= x}, y -> simp_rem x y
  | App {op= And; arg= x}, y -> simp_and x y
  | App {op= Or; arg= x}, y -> simp_or x y
  | App {op= Xor; arg= x}, y -> simp_xor x y
  | App {op= Shl; arg= x}, y -> simp_shl x y
  | App {op= Lshr; arg= x}, y -> simp_lshr x y
  | App {op= Ashr; arg= x}, y -> simp_ashr x y
  | App {op= App {op= Conditional; arg= x}; arg= y}, z -> simp_cond x y z
  | Extract {unsigned; bits}, x -> simp_extract ~unsigned bits x
  | Convert {unsigned; dst; src}, x -> simp_convert ~unsigned dst src x
  | _ -> App {op; arg} )
  |> check (invariant ~partial)
  |> check (fun e ->
         (* every App subterm of output appears in input *)
         match op with
         | App {op= Eq | Dq | Xor} -> ()
         | _ -> (
           match e with
           | App {op= App {op= App {op= Conditional}}} -> ()
           | _ ->
               iter e ~f:(function
                 | App {op= Eq | Dq} -> ()
                 | App _ as a ->
                     assert (
                       is_subterm ~sub:a ~sup:op
                       || is_subterm ~sub:a ~sup:arg
                       || Trace.fail
                            "simplifying %a %a@ yields %a@ with new \
                             subterm %a"
                            pp op pp arg pp e pp a )
                 | _ -> () ) ) )

let app2 op x y = app1 (app1 ~partial:true op x) y
let app3 op x y z = app1 (app1 ~partial:true (app1 ~partial:true op x) y) z
let addN args = simp_add args |> check invariant
let mulN args = simp_mul args |> check invariant
let simp_memory siz arr = Memory {siz; arr}
let memory ~siz ~arr = simp_memory siz arr |> check invariant

let simp_concat xs =
  if Vector.length xs = 1 then Vector.get xs 0
  else
    let args =
      if Vector.for_all xs ~f:(function Concat _ -> false | _ -> true)
      then xs
      else
        Vector.concat
          (Vector.fold_right xs ~init:[] ~f:(fun x s ->
               match x with
               | Concat {args} -> args :: s
               | x -> Vector.of_array [|x|] :: s ))
    in
    Concat {args}

let concat xs = simp_concat (Vector.of_array xs) |> check invariant

let simp_splat byt siz =
  match siz with
  | Integer {data} when Z.equal Z.zero data -> concat [||]
  | _ -> Splat {byt; siz}

let splat ~byt ~siz = simp_splat byt siz |> check invariant
let eq = app2 Eq
let dq = app2 Dq
let gt = app2 Gt
let ge = app2 Ge
let lt = app2 Lt
let le = app2 Le
let ord = app2 Ord
let uno = app2 Uno
let neg = simp_negate
let add = simp_add2
let sub = simp_sub
let mul = simp_mul2
let div = app2 Div
let rem = app2 Rem
let and_ = app2 And
let or_ = app2 Or
let xor = app2 Xor
let not_ = simp_not
let shl = app2 Shl
let lshr = app2 Lshr
let ashr = app2 Ashr
let conditional ~cnd ~thn ~els = app3 Conditional cnd thn els
let record elts = List.fold ~f:app1 ~init:Record elts
let select ~rcd ~idx = app2 Select rcd idx
let update ~rcd ~elt ~idx = app3 Update rcd elt idx

let struct_rec key =
  let memo_id = Hashtbl.create key in
  let dummy = null in
  Staged.stage
  @@ fun ~id elt_thks ->
  match Hashtbl.find memo_id id with
  | None ->
      (* Add placeholder to prevent computing [elts] in calls to
         [struct_rec] from [elt_thks] for recursive occurrences of [id]. *)
      let elta = Array.create ~len:(Vector.length elt_thks) dummy in
      let elts = Vector.of_array elta in
      Hashtbl.set memo_id ~key:id ~data:elts ;
      Vector.iteri elt_thks ~f:(fun i (lazy elt) -> elta.(i) <- elt) ;
      Struct_rec {elts} |> check invariant
  | Some elts ->
      (* Do not check invariant as invariant will be checked above after the
         thunks are forced, before which invariant-checking may spuriously
         fail. Note that it is important that the value constructed here
         shares the array in the memo table, so that the update after
         forcing the recursive thunks also updates this value. *)
      Struct_rec {elts}

let extract ?(unsigned = false) ~bits term =
  app1 (Extract {unsigned; bits}) term

let convert ?(unsigned = false) ~dst ~src term =
  app1 (Convert {unsigned; dst; src}) term

let size_of t =
  Option.bind (Typ.prim_bit_size_of t) ~f:(fun n ->
      if n % 8 = 0 then Some (integer (Z.of_int (n / 8))) else None )

let rec of_exp (e : Exp.t) =
  let unsigned op typ x y =
    match Typ.prim_bit_size_of typ with
    | Some bits ->
        op
          (extract ~unsigned:true ~bits (of_exp x))
          (extract ~unsigned:true ~bits (of_exp y))
    | None -> violates Exp.invariant e
  in
  match e with
  | Reg {name} -> var (Var {id= 0; name})
  | Nondet {msg} -> nondet msg
  | Label {parent; name} -> label ~parent ~name
  | Integer {data} -> integer data
  | Float {data} -> float data
  | Ap1 (Convert {unsigned; dst}, src, arg) ->
      convert ~unsigned ~dst ~src (of_exp arg)
  | Ap1 (Select idx, _, arg) ->
      select ~rcd:(of_exp arg) ~idx:(integer (Z.of_int idx))
  | Ap2 (Eq, _, x, y) -> eq (of_exp x) (of_exp y)
  | Ap2 (Dq, _, x, y) -> dq (of_exp x) (of_exp y)
  | Ap2 (Gt, _, x, y) -> gt (of_exp x) (of_exp y)
  | Ap2 (Ge, _, x, y) -> ge (of_exp x) (of_exp y)
  | Ap2 (Lt, _, x, y) -> lt (of_exp x) (of_exp y)
  | Ap2 (Le, _, x, y) -> le (of_exp x) (of_exp y)
  | Ap2 (Ugt, typ, x, y) -> unsigned gt typ x y
  | Ap2 (Uge, typ, x, y) -> unsigned ge typ x y
  | Ap2 (Ult, typ, x, y) -> unsigned lt typ x y
  | Ap2 (Ule, typ, x, y) -> unsigned le typ x y
  | Ap2 (Ord, _, x, y) -> ord (of_exp x) (of_exp y)
  | Ap2 (Uno, _, x, y) -> uno (of_exp x) (of_exp y)
  | Ap2 (Add, _, x, y) -> add (of_exp x) (of_exp y)
  | Ap2 (Sub, _, x, y) -> sub (of_exp x) (of_exp y)
  | Ap2 (Mul, _, x, y) -> mul (of_exp x) (of_exp y)
  | Ap2 (Div, _, x, y) -> div (of_exp x) (of_exp y)
  | Ap2 (Rem, _, x, y) -> rem (of_exp x) (of_exp y)
  | Ap2 (Udiv, typ, x, y) -> unsigned div typ x y
  | Ap2 (Urem, typ, x, y) -> unsigned rem typ x y
  | Ap2 (And, _, x, y) -> and_ (of_exp x) (of_exp y)
  | Ap2 (Or, _, x, y) -> or_ (of_exp x) (of_exp y)
  | Ap2 (Xor, _, x, y) -> xor (of_exp x) (of_exp y)
  | Ap2 (Shl, _, x, y) -> shl (of_exp x) (of_exp y)
  | Ap2 (Lshr, _, x, y) -> lshr (of_exp x) (of_exp y)
  | Ap2 (Ashr, _, x, y) -> ashr (of_exp x) (of_exp y)
  | Ap2 (Update idx, _, rcd, elt) ->
      update ~rcd:(of_exp rcd) ~elt:(of_exp elt)
        ~idx:(integer (Z.of_int idx))
  | Ap3 (Conditional, _, cnd, thn, els) ->
      conditional ~cnd:(of_exp cnd) ~thn:(of_exp thn) ~els:(of_exp els)
  | ApN (Record, _, elts) ->
      record (Vector.to_list (Vector.map ~f:of_exp elts))
  | ApN (Struct_rec, _, elts) ->
      Staged.unstage
        (struct_rec (module Exp))
        ~id:e
        (Vector.map elts ~f:(fun e -> lazy (of_exp e)))

(** Transform *)

let map e ~f =
  let map_ : (t -> t) -> t -> t =
   fun map_ e ->
    let map_bin mk ~f x y =
      let x' = f x in
      let y' = f y in
      if x' == x && y' == y then e else mk x' y'
    in
    let map_vector mk ~f args =
      let args' = Vector.map_preserving_phys_equal ~f args in
      if args' == args then e else mk args'
    in
    let map_qset mk ~f args =
      let args' = Qset.map ~f:(fun arg q -> (f arg, q)) args in
      if args' == args then e else mk args'
    in
    match e with
    | App {op; arg} -> map_bin (app1 ~partial:true) ~f op arg
    | Add {args} -> map_qset addN ~f args
    | Mul {args} -> map_qset mulN ~f args
    | Splat {byt; siz} -> map_bin simp_splat ~f byt siz
    | Memory {siz; arr} -> map_bin simp_memory ~f siz arr
    | Concat {args} -> map_vector simp_concat ~f args
    | Struct_rec {elts= args} -> Struct_rec {elts= Vector.map args ~f:map_}
    | _ -> e
  in
  fix map_ (fun e -> e) e

let rename sub e =
  let rec rename_ sub e =
    match e with
    | Var _ -> Var.Subst.apply sub e
    | _ -> map ~f:(rename_ sub) e
  in
  rename_ sub e |> check (invariant ~partial:true)

(** Query *)

let is_true = function Integer {data} -> Z.is_true data | _ -> false
let is_false = function Integer {data} -> Z.is_false data | _ -> false

let rec is_constant e =
  let is_constant_bin x y = is_constant x && is_constant y in
  match e with
  | Var _ | Nondet _ -> false
  | App {op= x; arg= y} | Splat {byt= x; siz= y} | Memory {siz= x; arr= y}
    ->
      is_constant_bin x y
  | Add {args} | Mul {args} ->
      Qset.for_all ~f:(fun arg _ -> is_constant arg) args
  | Concat {args} | Struct_rec {elts= args} ->
      Vector.for_all ~f:is_constant args
  | _ -> true

let classify = function
  | Add _ | Mul _ -> `Interpreted
  | App {op= Eq | Dq | App {op= Eq | Dq}} -> `Simplified
  | App _ -> `Uninterpreted
  | _ -> `Atomic

let solve e f =
  [%Trace.call fun {pf} -> pf "%a@ %a" pp e pp f]
  ;
  let rec solve_ e f s =
    let solve_uninterp e f =
      let ord = compare e f in
      match (is_constant e, is_constant f) with
      | true, true when ord <> 0 -> None
      (* orient equation to discretionarily prefer term that is constant or
         compares smaller as class representative *)
      | true, false -> Some (Map.add_exn s ~key:f ~data:e)
      | false, true -> Some (Map.add_exn s ~key:e ~data:f)
      | _ ->
          let key, data = if ord > 0 then (e, f) else (f, e) in
          Some (Map.add_exn s ~key ~data)
    in
    let concat_size args =
      Vector.fold_until args ~init:zero
        ~f:(fun sum -> function Memory {siz} -> Continue (add siz sum)
          | _ -> Stop None )
        ~finish:(fun _ -> None)
    in
    match (e, f) with
    | (Add _ | Mul _ | Integer _), _ | _, (Add _ | Mul _ | Integer _) -> (
      match sub e f with
      | Add {args} ->
          let c, q = Qset.min_elt_exn args in
          let n = Sum.to_term (Qset.remove args c) in
          let d = rational (Q.neg q) in
          let r = div n d in
          Some (Map.add_exn s ~key:c ~data:r)
      | e_f -> solve_uninterp e_f zero )
    | Concat {args= ms}, Concat {args= ns} -> (
      match (concat_size ms, concat_size ns) with
      | Some p, Some q -> solve_uninterp e f >>= solve_ p q
      | _ -> solve_uninterp e f )
    | Memory {siz= m}, Concat {args= ns} | Concat {args= ns}, Memory {siz= m}
      -> (
      match concat_size ns with
      | Some p -> solve_uninterp e f >>= solve_ p m
      | _ -> solve_uninterp e f )
    | _ -> solve_uninterp e f
  in
  solve_ e f empty_map
  |>
  [%Trace.retn fun {pf} ->
    function Some s -> pf "%a" Var.Subst.pp s | None -> pf "false"]
