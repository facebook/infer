(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module Bound = Bounds.Bound
open Ints

module DegreeKind = struct
  type t = Linear | Log [@@deriving compare]

  let compute d i =
    match d with
    | Linear ->
        i
    | Log -> (
      try NonNegativeInt.log2_ceil_exn i with Invalid_argument _ -> NonNegativeInt.zero )


  let pp_hole pp f d x = match d with Linear -> pp f x | Log -> Format.fprintf f "log(%a)" pp x
end

module Degree = struct
  type t = {linear: NonNegativeInt.t; log: NonNegativeInt.t} [@@deriving compare]

  let zero = {linear= NonNegativeInt.zero; log= NonNegativeInt.zero}

  let succ (k : DegreeKind.t) d =
    match k with
    | Linear ->
        {d with linear= NonNegativeInt.succ d.linear}
    | Log ->
        {d with log= NonNegativeInt.succ d.log}


  let encode_to_int d =
    (*
      Constructs an integer compatible with compare.
      We assume that degrees won't go exceed 100 ;-)
    *)
    (NonNegativeInt.to_int_exn d.linear * 100) + NonNegativeInt.to_int_exn d.log


  let is_zero d = NonNegativeInt.is_zero d.linear && NonNegativeInt.is_zero d.log

  let pp f d =
    NonNegativeInt.pp f d.linear ;
    if not (NonNegativeInt.is_zero d.log) then
      F.fprintf f " + %a%slog" NonNegativeInt.pp d.log SpecialChars.dot_operator
end

module type NonNegativeSymbol = sig
  type t [@@deriving compare]

  val classify : t -> (Ints.NonNegativeInt.t, t) Bounds.valclass

  val int_lb : t -> NonNegativeInt.t

  val int_ub : t -> NonNegativeInt.t option

  val subst : t -> Bound.eval_sym -> (NonNegativeInt.t, t) Bounds.valclass

  val pp : F.formatter -> t -> unit
end

module type NonNegativeSymbolWithDegreeKind = sig
  type t0

  include NonNegativeSymbol

  val make : DegreeKind.t -> t0 -> t

  val degree_kind : t -> DegreeKind.t
end

module MakeSymbolWithDegreeKind (S : NonNegativeSymbol) :
  NonNegativeSymbolWithDegreeKind with type t0 = S.t = struct
  type t0 = S.t [@@deriving compare]

  type t = {degree_kind: DegreeKind.t; symbol: t0} [@@deriving compare]

  let classify ({degree_kind; symbol} as self) =
    match S.classify symbol with
    | Constant c ->
        Bounds.Constant (DegreeKind.compute degree_kind c)
    | Symbolic _ ->
        Bounds.Symbolic self
    | ValTop ->
        Bounds.ValTop


  let make degree_kind symbol = {degree_kind; symbol}

  let int_lb {degree_kind; symbol} = S.int_lb symbol |> DegreeKind.compute degree_kind

  let int_ub {degree_kind; symbol} =
    S.int_ub symbol |> Option.map ~f:(DegreeKind.compute degree_kind)


  let subst {degree_kind; symbol} eval =
    match S.subst symbol eval with
    | Constant c ->
        Bounds.Constant (DegreeKind.compute degree_kind c)
    | Symbolic symbol ->
        Bounds.Symbolic {degree_kind; symbol}
    | ValTop ->
        Bounds.ValTop


  let pp f {degree_kind; symbol} = DegreeKind.pp_hole S.pp f degree_kind symbol

  let degree_kind {degree_kind} = degree_kind
end

module MakePolynomial (S : NonNegativeSymbolWithDegreeKind) = struct
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
        max acc (Degree.succ (S.degree_kind t) d, mult_symb p' t) )
      terms (Degree.zero, one)


  let degree p = fst (degree_with_term p)

  let degree_term p = snd (degree_with_term p)

  let multiplication_sep = F.sprintf " %s " SpecialChars.multiplication_sign

  let pp : F.formatter -> t -> unit =
    let add_symb s (((last_s, last_occ) as last), others) =
      if Int.equal 0 (S.compare s last_s) then ((last_s, PositiveInt.succ last_occ), others)
      else ((s, PositiveInt.one), last :: others)
    in
    let pp_coeff fmt (c : NonNegativeInt.t) =
      if Z.((c :> Z.t) > one) then
        F.fprintf fmt "%a %s " NonNegativeInt.pp c SpecialChars.dot_operator
    in
    let pp_exp fmt (e : PositiveInt.t) =
      if Z.((e :> Z.t) > one) then PositiveInt.pp_exponent fmt e
    in
    let pp_magic_parentheses pp fmt x =
      let s = F.asprintf "%a" pp x in
      if String.contains s ' ' then F.fprintf fmt "(%s)" s else F.pp_print_string fmt s
    in
    let pp_symb fmt symb = pp_magic_parentheses S.pp fmt symb in
    let pp_symb_exp fmt (symb, exp) = F.fprintf fmt "%a%a" pp_symb symb pp_exp exp in
    let pp_symbs fmt (last, others) =
      List.rev_append others [last] |> Pp.seq ~sep:multiplication_sep pp_symb_exp fmt
    in
    let rec pp_sub ~print_plus symbs fmt {const; terms} =
      let print_plus =
        if not (NonNegativeInt.is_zero const) then (
          if print_plus then F.pp_print_string fmt " + " ;
          F.fprintf fmt "%a%a" pp_coeff const pp_symbs symbs ;
          true )
        else print_plus
      in
      ( M.fold
          (fun s p print_plus ->
            pp_sub ~print_plus (add_symb s symbs) fmt p ;
            true )
          terms print_plus
        : bool )
      |> ignore
    in
    fun fmt {const; terms} ->
      let const_not_zero = not (NonNegativeInt.is_zero const) in
      if const_not_zero || M.is_empty terms then NonNegativeInt.pp fmt const ;
      ( M.fold
          (fun s p print_plus ->
            pp_sub ~print_plus ((s, PositiveInt.one), []) fmt p ;
            true )
          terms const_not_zero
        : bool )
      |> ignore
end

module NonNegativePolynomial = struct
  module NonNegativeBoundWithDegreeKind = MakeSymbolWithDegreeKind (Bounds.NonNegativeBound)
  module NonNegativeNonTopPolynomial = MakePolynomial (NonNegativeBoundWithDegreeKind)
  include AbstractDomain.TopLifted (NonNegativeNonTopPolynomial)

  let zero = NonTop NonNegativeNonTopPolynomial.zero

  let one = NonTop NonNegativeNonTopPolynomial.one

  let of_int_exn i = NonTop (NonNegativeNonTopPolynomial.of_int_exn i)

  let of_non_negative_bound ?(degree_kind = DegreeKind.Linear) b =
    b
    |> NonNegativeBoundWithDegreeKind.make degree_kind
    |> NonNegativeBoundWithDegreeKind.classify |> NonNegativeNonTopPolynomial.of_valclass


  let is_symbolic = function Top -> false | NonTop p -> NonNegativeNonTopPolynomial.is_symbolic p

  let is_top = function Top -> true | _ -> false

  let is_zero = function NonTop p when NonNegativeNonTopPolynomial.is_zero p -> true | _ -> false

  let is_one = function NonTop p when NonNegativeNonTopPolynomial.is_one p -> true | _ -> false

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
        Degree.compare
          (NonNegativeNonTopPolynomial.degree p1)
          (NonNegativeNonTopPolynomial.degree p2)


  let pp_degree fmt p =
    match p with
    | Top ->
        Format.pp_print_string fmt "Top"
    | NonTop p ->
        Degree.pp fmt (NonNegativeNonTopPolynomial.degree p)


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
