(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging
module Bound = Bounds.Bound
module Counter = Counter

module Boolean = struct
  type t = Bottom | False | True | Top [@@deriving compare]

  let top = Top

  let true_ = True

  let equal = [%compare.equal: t]

  let is_false = function False -> true | _ -> false

  let is_true = function True -> true | _ -> false
end

open Ints
module SymbolPath = Symb.SymbolPath
module SymbolTable = Symb.SymbolTable
module SymbolSet = Symb.SymbolSet

(** A NonNegativeBound is a Bound that is either non-negative or symbolic but will be evaluated to a non-negative value once instantiated *)
module NonNegativeBound = struct
  type t = Bound.t [@@deriving compare]

  let pp = Bound.pp

  let zero = Bound.zero

  let of_bound b = if Bound.le b zero then zero else b

  let int_lb b =
    Bound.big_int_lb b
    |> Option.bind ~f:NonNegativeInt.of_big_int
    |> Option.value ~default:NonNegativeInt.zero


  let int_ub b = Bound.big_int_ub b |> Option.map ~f:NonNegativeInt.of_big_int_exn

  let classify = function
    | Bound.PInf ->
        Bounds.ValTop
    | Bound.MInf ->
        assert false
    | b -> (
      match Bound.is_const b with
      | None ->
          Bounds.Symbolic b
      | Some c ->
          Bounds.Constant (NonNegativeInt.of_big_int_exn c) )


  let subst b map =
    match Bound.subst_ub b map with
    | Bottom ->
        Bounds.Constant NonNegativeInt.zero
    | NonBottom b ->
        of_bound b |> classify
end

module type NonNegativeSymbol = sig
  type t [@@deriving compare]

  val int_lb : t -> NonNegativeInt.t

  val int_ub : t -> NonNegativeInt.t option

  val subst : t -> (Symb.Symbol.t -> t bottom_lifted) -> (NonNegativeInt.t, t) Bounds.valclass

  val pp : F.formatter -> t -> unit
end

module MakePolynomial (S : NonNegativeSymbol) = struct
  module M = struct
    include Caml.Map.Make (S)

    let increasing_union ~f m1 m2 = union (fun _ v1 v2 -> Some (f v1 v2)) m1 m2

    let zip m1 m2 = merge (fun _ opt1 opt2 -> Some (opt1, opt2)) m1 m2

    let fold_no_key m ~init ~f =
      let f _k v acc = f acc v in
      fold f m init


    let le ~le_elt m1 m2 =
      match
        merge
          (fun _ v1_opt v2_opt ->
            match (v1_opt, v2_opt) with
            | Some _, None ->
                raise Exit
            | Some lhs, Some rhs when not (le_elt ~lhs ~rhs) ->
                raise Exit
            | _ ->
                None )
          m1 m2
      with
      | _ ->
          true
      | exception Exit ->
          false


    let xcompare ~xcompare_elt ~lhs ~rhs =
      (* TODO: avoid creating zipped map *)
      zip lhs rhs
      |> PartialOrder.container ~fold:fold_no_key ~xcompare_elt:(PartialOrder.of_opt ~xcompare_elt)
  end

  (** If x < y < z then
    2 + 3 * x + 4 * x ^ 2 + x * y + 7 * y ^ 2 * z
    is represented by
    {const= 2; terms= {
      x -> {const= 3; terms= {
        x -> {const= 4; terms={}},
        y -> {const= 1; terms={}}
      }},
      y -> {const= 0; terms= {
        y -> {const= 0; terms= {
          z -> {const= 7; terms={}}
        }}
      }}
    }}

    The representation is a tree, each edge from a node to a child (terms) represents a multiplication by a symbol. If a node has a non-zero const, it represents the multiplication (of the path) by this constant.
    In the example above, we have the following paths:
    2
    x * 3
    x * x * 4
    x * y * 1
    y * y * z * 7

    Invariants:
      - except for the root, terms <> {} \/ const <> 0
      - symbols children of a term are 'smaller' than its self symbol
      - contents of terms are not zero
      - symbols in terms are only symbolic values
  *)
  type t = {const: NonNegativeInt.t; terms: t M.t}

  type astate = t

  let of_non_negative_int : NonNegativeInt.t -> t = fun const -> {const; terms= M.empty}

  let zero = of_non_negative_int NonNegativeInt.zero

  let one = of_non_negative_int NonNegativeInt.one

  let of_int_exn : int -> t = fun i -> i |> NonNegativeInt.of_int_exn |> of_non_negative_int

  let of_valclass : (NonNegativeInt.t, S.t) Bounds.valclass -> t top_lifted = function
    | ValTop ->
        Top
    | Constant i ->
        NonTop (of_non_negative_int i)
    | Symbolic s ->
        NonTop {const= NonNegativeInt.zero; terms= M.singleton s one}


  let is_zero : t -> bool = fun {const; terms} -> NonNegativeInt.is_zero const && M.is_empty terms

  let is_one : t -> bool = fun {const; terms} -> NonNegativeInt.is_one const && M.is_empty terms

  let is_constant : t -> bool = fun {terms} -> M.is_empty terms

  let is_symbolic : t -> bool = fun p -> not (is_constant p)

  let rec plus : t -> t -> t =
   fun p1 p2 ->
    { const= NonNegativeInt.(p1.const + p2.const)
    ; terms= M.increasing_union ~f:plus p1.terms p2.terms }


  let rec mult_const_positive : t -> PositiveInt.t -> t =
   fun {const; terms} c ->
    { const= NonNegativeInt.(const * (c :> NonNegativeInt.t))
    ; terms= M.map (fun p -> mult_const_positive p c) terms }


  let mult_const : t -> NonNegativeInt.t -> t =
   fun p c ->
    match PositiveInt.of_big_int (c :> Z.t) with None -> zero | Some c -> mult_const_positive p c


  (* (c + r * R + s * S + t * T) x s
    = 0 + r * (R x s) + s * (c + s * S + t * T) *)
  let rec mult_symb : t -> S.t -> t =
   fun {const; terms} s ->
    let less_than_s, equal_s_opt, greater_than_s = M.split s terms in
    let less_than_s = M.map (fun p -> mult_symb p s) less_than_s in
    let s_term =
      let terms =
        match equal_s_opt with
        | None ->
            greater_than_s
        | Some equal_s_p ->
            M.add s equal_s_p greater_than_s
      in
      {const; terms}
    in
    let terms = if is_zero s_term then less_than_s else M.add s s_term less_than_s in
    {const= NonNegativeInt.zero; terms}


  let rec mult : t -> t -> t =
   fun p1 p2 ->
    if is_zero p1 || is_zero p2 then zero
    else if is_one p1 then p2
    else if is_one p2 then p1
    else
      mult_const p1 p2.const |> M.fold (fun s p acc -> plus (mult_symb (mult p p1) s) acc) p2.terms


  let rec int_lb {const; terms} =
    M.fold
      (fun symbol polynomial acc ->
        let s_lb = S.int_lb symbol in
        let p_lb = int_lb polynomial in
        NonNegativeInt.((s_lb * p_lb) + acc) )
      terms const


  let rec int_ub {const; terms} =
    M.fold
      (fun symbol polynomial acc ->
        Option.bind acc ~f:(fun acc ->
            Option.bind (S.int_ub symbol) ~f:(fun s_ub ->
                Option.map (int_ub polynomial) ~f:(fun p_ub -> NonNegativeInt.((s_ub * p_ub) + acc))
            ) ) )
      terms (Some const)


  (* assumes symbols are not comparable *)
  let rec ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs ~rhs ->
    phys_equal lhs rhs
    || NonNegativeInt.( <= ) ~lhs:lhs.const ~rhs:rhs.const
       && M.le ~le_elt:( <= ) lhs.terms rhs.terms
    || Option.exists (int_ub lhs) ~f:(fun lhs_ub ->
           NonNegativeInt.( <= ) ~lhs:lhs_ub ~rhs:(int_lb rhs) )


  let rec xcompare ~lhs ~rhs =
    let cmp_const =
      PartialOrder.of_compare ~compare:NonNegativeInt.compare ~lhs:lhs.const ~rhs:rhs.const
    in
    let cmp_terms = M.xcompare ~xcompare_elt:xcompare ~lhs:lhs.terms ~rhs:rhs.terms in
    PartialOrder.join cmp_const cmp_terms


  (* Possible optimization for later: x join x^2 = x^2 instead of x + x^2 *)
  let rec join : t -> t -> t =
   fun p1 p2 ->
    if phys_equal p1 p2 then p1
    else
      { const= NonNegativeInt.max p1.const p2.const
      ; terms= M.increasing_union ~f:join p1.terms p2.terms }


  (* assumes symbols are not comparable *)
  (* TODO: improve this for comparable symbols *)
  let min_default_left : t -> t -> t =
   fun p1 p2 ->
    match xcompare ~lhs:p1 ~rhs:p2 with
    | `Equal | `LeftSmallerThanRight ->
        p1
    | `RightSmallerThanLeft ->
        p2
    | `NotComparable ->
        if is_constant p1 then p1 else if is_constant p2 then p2 else p1


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:_ ~next:_ ~num_iters:_ -> assert false


  let subst =
    let exception ReturnTop in
    (* avoids top-lifting everything *)
    let rec subst {const; terms} eval_sym =
      M.fold
        (fun s p acc ->
          match S.subst s eval_sym with
          | Constant c -> (
            match PositiveInt.of_big_int (c :> Z.t) with
            | None ->
                acc
            | Some c ->
                let p = subst p eval_sym in
                mult_const_positive p c |> plus acc )
          | ValTop ->
              let p = subst p eval_sym in
              if is_zero p then acc else raise ReturnTop
          | Symbolic s ->
              let p = subst p eval_sym in
              mult_symb p s |> plus acc )
        terms (of_non_negative_int const)
    in
    fun p eval_sym -> match subst p eval_sym with p -> NonTop p | exception ReturnTop -> Top


  (** Emit a pair (d,t) where d is the degree of the polynomial and t is the first term with such degree *)
  let rec degree_with_term {terms} =
    M.fold
      (fun t p acc ->
        let d, p' = degree_with_term p in
        max acc (d + 1, mult_symb p' t) )
      terms (0, one)


  let degree p = fst (degree_with_term p)

  let degree_term p = snd (degree_with_term p)

  let pp : F.formatter -> t -> unit =
    let add_symb s (((last_s, last_occ) as last), others) =
      if Int.equal 0 (S.compare s last_s) then ((last_s, PositiveInt.succ last_occ), others)
      else ((s, PositiveInt.one), last :: others)
    in
    let pp_coeff fmt (c : NonNegativeInt.t) =
      if Z.((c :> Z.t) > one) then F.fprintf fmt "%a * " NonNegativeInt.pp c
    in
    let pp_exp fmt (e : PositiveInt.t) =
      if Z.((e :> Z.t) > one) then F.fprintf fmt "^%a" PositiveInt.pp e
    in
    let pp_magic_parentheses pp fmt x =
      let s = F.asprintf "%a" pp x in
      if String.contains s ' ' then F.fprintf fmt "(%s)" s else F.pp_print_string fmt s
    in
    let pp_symb fmt symb = pp_magic_parentheses S.pp fmt symb in
    let pp_symb_exp fmt (symb, exp) = F.fprintf fmt "%a%a" pp_symb symb pp_exp exp in
    let pp_symbs fmt (last, others) =
      List.rev_append others [last] |> Pp.seq ~sep:" * " pp_symb_exp fmt
    in
    let rec pp_sub ~print_plus symbs fmt {const; terms} =
      if not (NonNegativeInt.is_zero const) then (
        if print_plus then F.pp_print_string fmt " + " ;
        F.fprintf fmt "%a%a" pp_coeff const pp_symbs symbs ) ;
      M.iter (fun s p -> pp_sub ~print_plus:true (add_symb s symbs) fmt p) terms
    in
    fun fmt {const; terms} ->
      let const_not_zero = not (NonNegativeInt.is_zero const) in
      if const_not_zero || M.is_empty terms then NonNegativeInt.pp fmt const ;
      M.fold
        (fun s p print_plus ->
          pp_sub ~print_plus ((s, PositiveInt.one), []) fmt p ;
          true )
        terms const_not_zero
      |> ignore
end

module NonNegativePolynomial = struct
  module NonNegativeNonTopPolynomial = MakePolynomial (NonNegativeBound)
  include AbstractDomain.TopLifted (NonNegativeNonTopPolynomial)

  let zero = NonTop NonNegativeNonTopPolynomial.zero

  let one = NonTop NonNegativeNonTopPolynomial.one

  let of_int_exn i = NonTop (NonNegativeNonTopPolynomial.of_int_exn i)

  let of_non_negative_bound b =
    b |> Bounds.NonNegativeBound.classify |> NonNegativeNonTopPolynomial.of_valclass


  let is_symbolic = function Top -> false | NonTop p -> NonNegativeNonTopPolynomial.is_symbolic p

  let is_top = function Top -> true | _ -> false

  let is_zero = function NonTop p when NonNegativeNonTopPolynomial.is_zero p -> true | _ -> false

  let top_lifted_increasing ~f p1 p2 =
    match (p1, p2) with Top, _ | _, Top -> Top | NonTop p1, NonTop p2 -> NonTop (f p1 p2)


  let plus = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.plus

  let mult = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.mult

  let min_default_left p1 p2 =
    match (p1, p2) with
    | Top, x | x, Top ->
        x
    | NonTop p1, NonTop p2 ->
        NonTop (NonNegativeNonTopPolynomial.min_default_left p1 p2)


  let widen ~prev ~next ~num_iters:_ = if ( <= ) ~lhs:next ~rhs:prev then prev else Top

  let subst p eval_sym =
    match p with Top -> Top | NonTop p -> NonNegativeNonTopPolynomial.subst p eval_sym


  let degree p =
    match p with Top -> None | NonTop p -> Some (NonNegativeNonTopPolynomial.degree p)


  let compare_by_degree p1 p2 =
    match (p1, p2) with
    | Top, Top ->
        0
    | Top, NonTop _ ->
        1
    | NonTop _, Top ->
        -1
    | NonTop p1, NonTop p2 ->
        NonNegativeNonTopPolynomial.degree p1 - NonNegativeNonTopPolynomial.degree p2


  let pp_degree fmt p =
    match p with
    | Top ->
        Format.pp_print_string fmt "Top"
    | NonTop p ->
        Format.pp_print_int fmt (NonNegativeNonTopPolynomial.degree p)


  let pp_degree_hum fmt p =
    match p with
    | Top ->
        Format.pp_print_string fmt "Top"
    | NonTop p ->
        Format.fprintf fmt "O(%a)" NonNegativeNonTopPolynomial.pp
          (NonNegativeNonTopPolynomial.degree_term p)


  let encode astate = Marshal.to_string astate [] |> B64.encode

  let decode enc_str = Marshal.from_string (B64.decode enc_str) 0
end

module ItvRange = struct
  type t = Bounds.NonNegativeBound.t

  let zero : t = Bounds.NonNegativeBound.zero

  let of_bounds : lb:Bound.t -> ub:Bound.t -> t =
   fun ~lb ~ub ->
    Bound.plus_u ub Bound.one
    |> Bound.plus_u (Bound.neg lb)
    |> Bound.simplify_bound_ends_from_paths |> Bounds.NonNegativeBound.of_bound


  let to_top_lifted_polynomial : t -> NonNegativePolynomial.astate =
   fun r -> NonNegativePolynomial.of_non_negative_bound r
end

module ItvPure = struct
  (** (l, u) represents the closed interval [l; u] (of course infinite bounds are open) *)
  type astate = Bound.t * Bound.t [@@deriving compare]

  type t = astate

  let lb : t -> Bound.t = fst

  let ub : t -> Bound.t = snd

  let is_lb_infty : t -> bool = function MInf, _ -> true | _ -> false

  let is_finite : t -> bool =
   fun (l, u) ->
    match (Bound.is_const l, Bound.is_const u) with Some _, Some _ -> true | _, _ -> false


  let have_similar_bounds (l1, u1) (l2, u2) = Bound.are_similar l1 l2 && Bound.are_similar u1 u2

  let has_infty = function Bound.MInf, _ | _, Bound.PInf -> true | _, _ -> false

  let ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs:(l1, u1) ~rhs:(l2, u2) -> Bound.le l2 l1 && Bound.le u1 u2


  let xcompare ~lhs:(l1, u1) ~rhs:(l2, u2) =
    let lcmp = Bound.xcompare ~lhs:l1 ~rhs:l2 in
    let ucmp = Bound.xcompare ~lhs:u1 ~rhs:u2 in
    match (lcmp, ucmp) with
    | `Equal, `Equal ->
        `Equal
    | `NotComparable, _ | _, `NotComparable -> (
      match (Bound.xcompare ~lhs:u1 ~rhs:l2, Bound.xcompare ~lhs:u2 ~rhs:l1) with
      | `Equal, `Equal ->
          `Equal (* weird, though *)
      | (`Equal | `LeftSmallerThanRight), _ ->
          `LeftSmallerThanRight
      | _, (`Equal | `LeftSmallerThanRight) ->
          `RightSmallerThanLeft
      | (`NotComparable | `RightSmallerThanLeft), (`NotComparable | `RightSmallerThanLeft) ->
          `NotComparable )
    | `Equal, `LeftSmallerThanRight
    | `RightSmallerThanLeft, `Equal
    | `RightSmallerThanLeft, `LeftSmallerThanRight ->
        `RightSubsumesLeft
    | `Equal, `RightSmallerThanLeft
    | `LeftSmallerThanRight, `Equal
    | `LeftSmallerThanRight, `RightSmallerThanLeft ->
        `LeftSubsumesRight
    | `LeftSmallerThanRight, `LeftSmallerThanRight ->
        `LeftSmallerThanRight
    | `RightSmallerThanLeft, `RightSmallerThanLeft ->
        `RightSmallerThanLeft


  let join : t -> t -> t =
   fun (l1, u1) (l2, u2) -> (Bound.underapprox_min l1 l2, Bound.overapprox_max u1 u2)


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:(l1, u1) ~next:(l2, u2) ~num_iters:_ -> (Bound.widen_l l1 l2, Bound.widen_u u1 u2)


  let pp : F.formatter -> t -> unit =
   fun fmt (l, u) ->
    if Bound.equal l u then Bound.pp fmt l
    else
      match Bound.is_same_symbol l u with
      | Some symbol ->
          Symb.SymbolPath.pp fmt symbol
      | None ->
          F.fprintf fmt "[%a, %a]" Bound.pp l Bound.pp u


  let of_bound bound = (bound, bound)

  let of_int n = of_bound (Bound.of_int n)

  let of_big_int n = of_bound (Bound.of_big_int n)

  let make_sym : unsigned:bool -> Typ.Procname.t -> SymbolTable.t -> SymbolPath.t -> Counter.t -> t
      =
   fun ~unsigned pname symbol_table path new_sym_num ->
    let lb, ub = Bounds.SymLinear.make ~unsigned pname symbol_table path new_sym_num in
    (Bound.of_sym lb, Bound.of_sym ub)


  let mone = of_bound Bound.mone

  let m1_255 = (Bound.minus_one, Bound._255)

  let nat = (Bound.zero, Bound.PInf)

  let one = of_bound Bound.one

  let pos = (Bound.one, Bound.PInf)

  let top = (Bound.MInf, Bound.PInf)

  let zero = of_bound Bound.zero

  let get_iterator_itv (_, u) = (Bound.zero, Bound.plus_u u Bound.mone)

  let true_sem = one

  let false_sem = zero

  let unknown_bool = join false_sem true_sem

  let is_top : t -> bool = function Bound.MInf, Bound.PInf -> true | _ -> false

  let is_nat : t -> bool = function l, Bound.PInf -> Bound.is_zero l | _ -> false

  let is_const : t -> Z.t option =
   fun (l, u) ->
    match (Bound.is_const l, Bound.is_const u) with
    | Some n, Some m when Z.equal n m ->
        Some n
    | _, _ ->
        None


  let is_zero : t -> bool = fun (l, u) -> Bound.is_zero l && Bound.is_zero u

  let is_one : t -> bool = fun (l, u) -> Bound.eq l Bound.one && Bound.eq u Bound.one

  let is_true : t -> bool = fun (l, u) -> Bound.le Bound.one l || Bound.le u Bound.mone

  let is_false : t -> bool = is_zero

  let is_symbolic : t -> bool = fun (lb, ub) -> Bound.is_symbolic lb || Bound.is_symbolic ub

  let is_ge_zero : t -> bool = fun (lb, _) -> Bound.le Bound.zero lb

  let is_le_zero : t -> bool = fun (_, ub) -> Bound.le ub Bound.zero

  let is_le_mone : t -> bool = fun (_, ub) -> Bound.le ub Bound.mone

  let range : t -> ItvRange.t = fun (lb, ub) -> ItvRange.of_bounds ~lb ~ub

  let neg : t -> t =
   fun (l, u) ->
    let l' = Bound.neg u in
    let u' = Bound.neg l in
    (l', u')


  let lnot : t -> Boolean.t =
   fun x -> if is_true x then Boolean.False else if is_false x then Boolean.True else Boolean.Top


  let plus : t -> t -> t = fun (l1, u1) (l2, u2) -> (Bound.plus_l l1 l2, Bound.plus_u u1 u2)

  let minus : t -> t -> t = fun i1 i2 -> plus i1 (neg i2)

  let mult_const : Z.t -> t -> t =
   fun n ((l, u) as itv) ->
    match NonZeroInt.of_big_int n with
    | None ->
        zero
    | Some n ->
        if NonZeroInt.is_one n then itv
        else if NonZeroInt.is_minus_one n then neg itv
        else if NonZeroInt.is_positive n then (Bound.mult_const_l n l, Bound.mult_const_u n u)
        else (Bound.mult_const_l n u, Bound.mult_const_u n l)


  (* Returns a precise value only when all coefficients are divided by
     n without remainder. *)
  let div_const : t -> Z.t -> t =
   fun ((l, u) as itv) n ->
    match NonZeroInt.of_big_int n with
    | None ->
        top
    | Some n ->
        if NonZeroInt.is_one n then itv
        else if NonZeroInt.is_minus_one n then neg itv
        else if NonZeroInt.is_positive n then
          let l' = Option.value ~default:Bound.MInf (Bound.div_const_l l n) in
          let u' = Option.value ~default:Bound.PInf (Bound.div_const_u u n) in
          (l', u')
        else
          let l' = Option.value ~default:Bound.MInf (Bound.div_const_l u n) in
          let u' = Option.value ~default:Bound.PInf (Bound.div_const_u l n) in
          (l', u')


  let mult : t -> t -> t =
   fun x y ->
    match (is_const x, is_const y) with
    | _, Some n ->
        mult_const n x
    | Some n, _ ->
        mult_const n y
    | None, None ->
        top


  let div : t -> t -> t = fun x y -> match is_const y with None -> top | Some n -> div_const x n

  let mod_sem : t -> t -> t =
   fun x y ->
    match is_const y with
    | None ->
        top
    | Some n when Z.(equal n zero) ->
        x (* x % [0,0] does nothing. *)
    | Some m -> (
      match is_const x with
      | Some n ->
          of_big_int Z.(n mod m)
      | None ->
          let abs_m = Z.abs m in
          if is_ge_zero x then (Bound.zero, Bound.of_big_int Z.(abs_m - one))
          else if is_le_zero x then (Bound.of_big_int Z.(one - abs_m), Bound.zero)
          else (Bound.of_big_int Z.(one - abs_m), Bound.of_big_int Z.(abs_m - one)) )


  (* x << [-1,-1] does nothing. *)
  let shiftlt : t -> t -> t =
   fun x y ->
    Option.value_map (is_const y) ~default:top ~f:(fun n ->
        match Z.to_int n with
        | n ->
            if n < 0 then x else mult_const Z.(one lsl n) x
        | exception Z.Overflow ->
            top )


  (* x >> [-1,-1] does nothing. *)
  let shiftrt : t -> t -> t =
   fun x y ->
    match is_const y with
    | Some n when Z.(leq n zero) ->
        x
    | Some n when Z.(n >= of_int 64) ->
        zero
    | Some n -> (
      match Z.to_int n with n -> div_const x Z.(one lsl n) | exception Z.Overflow -> top )
    | None ->
        top


  let band_sem : t -> t -> t =
   fun x y ->
    match (is_const x, is_const y) with
    | Some x', Some y' ->
        if Z.(equal x' y') then x else of_big_int Z.(x' land y')
    | _, _ ->
        if is_ge_zero x && is_ge_zero y then (Bound.zero, Bound.overapprox_min (ub x) (ub y))
        else if is_le_zero x && is_le_zero y then (Bound.MInf, Bound.overapprox_min (ub x) (ub y))
        else top


  let lt_sem : t -> t -> Boolean.t =
   fun (l1, u1) (l2, u2) ->
    if Bound.lt u1 l2 then Boolean.True else if Bound.le u2 l1 then Boolean.False else Boolean.Top


  let gt_sem : t -> t -> Boolean.t = fun x y -> lt_sem y x

  let le_sem : t -> t -> Boolean.t =
   fun (l1, u1) (l2, u2) ->
    if Bound.le u1 l2 then Boolean.True else if Bound.lt u2 l1 then Boolean.False else Boolean.Top


  let ge_sem : t -> t -> Boolean.t = fun x y -> le_sem y x

  let eq_sem : t -> t -> Boolean.t =
   fun (l1, u1) (l2, u2) ->
    if Bound.eq l1 u1 && Bound.eq u1 l2 && Bound.eq l2 u2 then Boolean.True
    else if Bound.lt u1 l2 || Bound.lt u2 l1 then Boolean.False
    else Boolean.Top


  let ne_sem : t -> t -> Boolean.t =
   fun (l1, u1) (l2, u2) ->
    if Bound.eq l1 u1 && Bound.eq u1 l2 && Bound.eq l2 u2 then Boolean.False
    else if Bound.lt u1 l2 || Bound.lt u2 l1 then Boolean.True
    else Boolean.Top


  let land_sem : t -> t -> Boolean.t =
   fun x y ->
    if is_true x && is_true y then Boolean.True
    else if is_false x || is_false y then Boolean.False
    else Boolean.Top


  let lor_sem : t -> t -> Boolean.t =
   fun x y ->
    if is_true x || is_true y then Boolean.True
    else if is_false x && is_false y then Boolean.False
    else Boolean.Top


  let min_sem : t -> t -> t =
   fun (l1, u1) (l2, u2) -> (Bound.underapprox_min l1 l2, Bound.overapprox_min u1 u2)


  let is_invalid : t -> bool = function
    | Bound.PInf, _ | _, Bound.MInf ->
        true
    | l, u ->
        Bound.lt u l


  let normalize : t -> t bottom_lifted = fun x -> if is_invalid x then Bottom else NonBottom x

  let subst : t -> (Symb.Symbol.t -> Bound.t bottom_lifted) -> t bottom_lifted =
   fun (l, u) eval_sym ->
    match (Bound.subst_lb l eval_sym, Bound.subst_ub u eval_sym) with
    | NonBottom l, NonBottom u ->
        normalize (l, u)
    | _ ->
        Bottom


  let prune_le : t -> t -> t = fun (l1, u1) (_, u2) -> (l1, Bound.overapprox_min u1 u2)

  let prune_ge : t -> t -> t = fun (l1, u1) (l2, _) -> (Bound.underapprox_max l1 l2, u1)

  let prune_lt : t -> t -> t = fun x y -> prune_le x (minus y one)

  let prune_gt : t -> t -> t = fun x y -> prune_ge x (plus y one)

  let prune_diff : t -> Bound.t -> t bottom_lifted =
   fun ((l, u) as itv) b ->
    if Bound.le b l then normalize (prune_gt itv (of_bound b))
    else if Bound.le u b then normalize (prune_lt itv (of_bound b))
    else NonBottom itv


  let prune_ne_zero : t -> t bottom_lifted = fun x -> prune_diff x Bound.zero

  let prune_comp : Binop.t -> t -> t -> t bottom_lifted =
   fun c x y ->
    if is_invalid y then NonBottom x
    else
      let x =
        match c with
        | Binop.Le ->
            prune_le x y
        | Binop.Ge ->
            prune_ge x y
        | Binop.Lt ->
            prune_lt x y
        | Binop.Gt ->
            prune_gt x y
        | _ ->
            assert false
      in
      normalize x


  let prune_eq : t -> t -> t bottom_lifted =
   fun x y ->
    match prune_comp Binop.Le x y with
    | Bottom ->
        Bottom
    | NonBottom x' ->
        prune_comp Binop.Ge x' y


  let prune_eq_zero : t -> t bottom_lifted =
   fun x ->
    let x' = prune_le x zero in
    prune_ge x' zero |> normalize


  let prune_ne : t -> t -> t bottom_lifted =
   fun x (l, u) ->
    if is_invalid (l, u) then NonBottom x else if Bound.eq l u then prune_diff x l else NonBottom x


  let get_symbols : t -> SymbolSet.t =
   fun (l, u) -> SymbolSet.union (Bound.get_symbols l) (Bound.get_symbols u)


  let make_positive : t -> t =
   fun ((l, u) as x) -> if Bound.lt l Bound.zero then (Bound.zero, u) else x
end

include AbstractDomain.BottomLifted (ItvPure)

type t = astate

let compare : t -> t -> int =
 fun x y ->
  match (x, y) with
  | Bottom, Bottom ->
      0
  | Bottom, _ ->
      -1
  | _, Bottom ->
      1
  | NonBottom x, NonBottom y ->
      ItvPure.compare_astate x y


let bot : t = Bottom

let top : t = NonBottom ItvPure.top

let lb : t -> Bound.t = function
  | NonBottom x ->
      ItvPure.lb x
  | Bottom ->
      L.(die InternalError) "lower bound of bottom"


let ub : t -> Bound.t = function
  | NonBottom x ->
      ItvPure.ub x
  | Bottom ->
      L.(die InternalError) "upper bound of bottom"


let false_sem = NonBottom ItvPure.false_sem

let m1_255 = NonBottom ItvPure.m1_255

let nat = NonBottom ItvPure.nat

let one = NonBottom ItvPure.one

let pos = NonBottom ItvPure.pos

let true_sem = NonBottom ItvPure.true_sem

let unknown_bool = NonBottom ItvPure.unknown_bool

let zero = NonBottom ItvPure.zero

let of_bool = function
  | Boolean.Bottom ->
      bot
  | Boolean.False ->
      false_sem
  | Boolean.True ->
      true_sem
  | Boolean.Top ->
      unknown_bool


let of_int : int -> astate = fun n -> NonBottom (ItvPure.of_int n)

let of_big_int : Z.t -> astate = fun n -> NonBottom (ItvPure.of_big_int n)

let of_int_lit : IntLit.t -> astate = fun n -> of_big_int (IntLit.to_big_int n)

let of_int64 : Int64.t -> astate = fun n -> of_big_int (Z.of_int64 n)

let is_false : t -> bool = function NonBottom x -> ItvPure.is_false x | Bottom -> false

let le : lhs:t -> rhs:t -> bool = ( <= )

let eq : t -> t -> bool = fun x y -> ( <= ) ~lhs:x ~rhs:y && ( <= ) ~lhs:y ~rhs:x

let range : t -> ItvRange.t = function
  | Bottom ->
      ItvRange.zero
  | NonBottom itv ->
      ItvPure.range itv


let lift1 : (ItvPure.t -> ItvPure.t) -> t -> t =
 fun f -> function Bottom -> Bottom | NonBottom x -> NonBottom (f x)


let bind1_gen : bot:'a -> (ItvPure.t -> 'a) -> t -> 'a =
 fun ~bot f x -> match x with Bottom -> bot | NonBottom x -> f x


let bind1 : (ItvPure.t -> t) -> t -> t = bind1_gen ~bot:Bottom

let bind1b : (ItvPure.t -> Boolean.t) -> t -> Boolean.t = bind1_gen ~bot:Boolean.Bottom

let lift2 : (ItvPure.t -> ItvPure.t -> ItvPure.t) -> t -> t -> t =
 fun f x y ->
  match (x, y) with
  | Bottom, _ | _, Bottom ->
      Bottom
  | NonBottom x, NonBottom y ->
      NonBottom (f x y)


let bind2_gen : bot:'a -> (ItvPure.t -> ItvPure.t -> 'a) -> t -> t -> 'a =
 fun ~bot f x y ->
  match (x, y) with Bottom, _ | _, Bottom -> bot | NonBottom x, NonBottom y -> f x y


let bind2 : (ItvPure.t -> ItvPure.t -> t) -> t -> t -> t = bind2_gen ~bot:Bottom

let bind2b : (ItvPure.t -> ItvPure.t -> Boolean.t) -> t -> t -> Boolean.t =
  bind2_gen ~bot:Boolean.Bottom


let plus : t -> t -> t = lift2 ItvPure.plus

let minus : t -> t -> t = lift2 ItvPure.minus

let get_iterator_itv : t -> t = lift1 ItvPure.get_iterator_itv

let make_sym : ?unsigned:bool -> Typ.Procname.t -> SymbolTable.t -> SymbolPath.t -> Counter.t -> t
    =
 fun ?(unsigned = false) pname symbol_table path new_sym_num ->
  NonBottom (ItvPure.make_sym ~unsigned pname symbol_table path new_sym_num)


let neg : t -> t = lift1 ItvPure.neg

let lnot : t -> Boolean.t = bind1b ItvPure.lnot

let mult : t -> t -> t = lift2 ItvPure.mult

let div : t -> t -> t = lift2 ItvPure.div

let mod_sem : t -> t -> t = lift2 ItvPure.mod_sem

let shiftlt : t -> t -> t = lift2 ItvPure.shiftlt

let shiftrt : t -> t -> t = lift2 ItvPure.shiftrt

let band_sem : t -> t -> t = lift2 ItvPure.band_sem

let lt_sem : t -> t -> Boolean.t = bind2b ItvPure.lt_sem

let gt_sem : t -> t -> Boolean.t = bind2b ItvPure.gt_sem

let le_sem : t -> t -> Boolean.t = bind2b ItvPure.le_sem

let ge_sem : t -> t -> Boolean.t = bind2b ItvPure.ge_sem

let eq_sem : t -> t -> Boolean.t = bind2b ItvPure.eq_sem

let ne_sem : t -> t -> Boolean.t = bind2b ItvPure.ne_sem

let land_sem : t -> t -> Boolean.t = bind2b ItvPure.land_sem

let lor_sem : t -> t -> Boolean.t = bind2b ItvPure.lor_sem

let min_sem : t -> t -> t = lift2 ItvPure.min_sem

let prune_eq_zero : t -> t = bind1 ItvPure.prune_eq_zero

let prune_ne_zero : t -> t = bind1 ItvPure.prune_ne_zero

let prune_comp : Binop.t -> t -> t -> t = fun comp -> bind2 (ItvPure.prune_comp comp)

let prune_eq : t -> t -> t = bind2 ItvPure.prune_eq

let prune_ne : t -> t -> t = bind2 ItvPure.prune_ne

let subst : t -> (Symb.Symbol.t -> Bound.t bottom_lifted) -> t =
 fun x eval_sym -> match x with NonBottom x' -> ItvPure.subst x' eval_sym | _ -> x


let get_symbols : t -> SymbolSet.t = function
  | Bottom ->
      SymbolSet.empty
  | NonBottom x ->
      ItvPure.get_symbols x


let normalize : t -> t = bind1 ItvPure.normalize
