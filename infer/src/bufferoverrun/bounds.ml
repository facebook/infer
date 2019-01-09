(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging

exception Not_One_Symbol

open Ints

module SymLinear = struct
  module M = Symb.SymbolMap

  (**
     Map from symbols to integer coefficients.
     { x -> 2, y -> 5 } represents the value 2 * x + 5 * y
  *)
  type t = NonZeroInt.t M.t [@@deriving compare]

  let empty : t = M.empty

  let is_empty : t -> bool = fun x -> M.is_empty x

  let singleton_one : Symb.Symbol.t -> t = fun s -> M.singleton s NonZeroInt.one

  let singleton_minus_one : Symb.Symbol.t -> t = fun s -> M.singleton s NonZeroInt.minus_one

  let is_le_zero : t -> bool =
   fun x -> M.for_all (fun s v -> Symb.Symbol.is_unsigned s && NonZeroInt.is_negative v) x


  let is_ge_zero : t -> bool =
   fun x -> M.for_all (fun s v -> Symb.Symbol.is_unsigned s && NonZeroInt.is_positive v) x


  let le : t -> t -> bool =
   fun x y ->
    phys_equal x y
    ||
    let le_one_pair s v1_opt v2_opt =
      let v1 = NonZeroInt.opt_to_big_int v1_opt in
      let v2 = NonZeroInt.opt_to_big_int v2_opt in
      Z.(equal v1 v2) || (Symb.Symbol.is_unsigned s && v1 <= v2)
    in
    M.for_all2 ~f:le_one_pair x y


  let pp1 :
      markup:bool -> is_beginning:bool -> F.formatter -> Symb.Symbol.t -> NonZeroInt.t -> unit =
   fun ~markup ~is_beginning f s c ->
    let c = (c :> Z.t) in
    let c =
      if is_beginning then c
      else if Z.gt c Z.zero then ( F.pp_print_string f " + " ; c )
      else ( F.pp_print_string f " - " ; Z.neg c )
    in
    if Z.(equal c one) then (Symb.Symbol.pp_mark ~markup) f s
    else if Z.(equal c minus_one) then F.fprintf f "-%a" (Symb.Symbol.pp_mark ~markup) s
    else
      F.fprintf f "%a%s%a" Z.pp_print c SpecialChars.dot_operator (Symb.Symbol.pp_mark ~markup) s


  let pp : markup:bool -> is_beginning:bool -> F.formatter -> t -> unit =
   fun ~markup ~is_beginning f x ->
    if M.is_empty x then if is_beginning then F.pp_print_string f "0" else ()
    else
      ( M.fold (fun s c is_beginning -> pp1 ~markup ~is_beginning f s c ; false) x is_beginning
        : bool )
      |> ignore


  let zero : t = M.empty

  let is_zero : t -> bool = M.is_empty

  let neg : t -> t = fun x -> M.map NonZeroInt.( ~- ) x

  let plus : t -> t -> t =
   fun x y ->
    let plus_coeff _ c1 c2 = NonZeroInt.plus c1 c2 in
    PhysEqual.optim2 x y ~res:(M.union plus_coeff x y)


  let mult_const : NonZeroInt.t -> t -> t =
   fun n x -> if NonZeroInt.is_one n then x else M.map (NonZeroInt.( * ) n) x


  let exact_div_const_exn : t -> NonZeroInt.t -> t =
   fun x n -> if NonZeroInt.is_one n then x else M.map (fun c -> NonZeroInt.exact_div_exn c n) x


  (* Returns a symbol when the map contains only one symbol s with a
     given coefficient. *)
  let one_symbol_of_coeff : NonZeroInt.t -> t -> Symb.Symbol.t option =
   fun coeff x ->
    match M.is_singleton_or_more x with
    | IContainer.Singleton (k, v) when Z.equal (v :> Z.t) (coeff :> Z.t) ->
        Some k
    | _ ->
        None


  let fold m ~init ~f =
    let f s coeff acc = f acc s coeff in
    M.fold f m init


  let get_one_symbol_opt : t -> Symb.Symbol.t option = one_symbol_of_coeff NonZeroInt.one

  let get_mone_symbol_opt : t -> Symb.Symbol.t option = one_symbol_of_coeff NonZeroInt.minus_one

  let get_one_symbol : t -> Symb.Symbol.t =
   fun x -> match get_one_symbol_opt x with Some s -> s | None -> raise Not_One_Symbol


  let get_mone_symbol : t -> Symb.Symbol.t =
   fun x -> match get_mone_symbol_opt x with Some s -> s | None -> raise Not_One_Symbol


  let is_one_symbol : t -> bool =
   fun x -> match get_one_symbol_opt x with Some _ -> true | None -> false


  let is_mone_symbol : t -> bool =
   fun x -> match get_mone_symbol_opt x with Some _ -> true | None -> false


  let is_one_symbol_of : Symb.Symbol.t -> t -> bool =
   fun s x ->
    Option.value_map (get_one_symbol_opt x) ~default:false ~f:(fun s' -> Symb.Symbol.equal s s')


  let is_mone_symbol_of : Symb.Symbol.t -> t -> bool =
   fun s x ->
    Option.value_map (get_mone_symbol_opt x) ~default:false ~f:(fun s' -> Symb.Symbol.equal s s')


  let get_symbols : t -> Symb.SymbolSet.t =
   fun x -> M.fold (fun symbol _coeff acc -> Symb.SymbolSet.add symbol acc) x Symb.SymbolSet.empty


  (* we can give integer bounds (obviously 0) only when all symbols are unsigned *)

  let big_int_lb x = if is_ge_zero x then Some Z.zero else None

  let big_int_ub x = if is_le_zero x then Some Z.zero else None

  (** When two following symbols are from the same path, simplify what would lead to a zero sum. E.g. 2 * x.lb - x.ub = x.lb *)
  let simplify_bound_ends_from_paths : t -> t =
   fun x ->
    let f (prev_opt, to_add) symb coeff =
      match prev_opt with
      | Some (prev_coeff, prev_symb)
        when Symb.Symbol.paths_equal prev_symb symb
             && NonZeroInt.is_positive coeff <> NonZeroInt.is_positive prev_coeff ->
          let add_coeff =
            (if NonZeroInt.is_positive coeff then NonZeroInt.max else NonZeroInt.min)
              prev_coeff (NonZeroInt.( ~- ) coeff)
          in
          let to_add =
            to_add |> M.add symb add_coeff |> M.add prev_symb (NonZeroInt.( ~- ) add_coeff)
          in
          (None, to_add)
      | _ ->
          (Some (coeff, symb), to_add)
    in
    let _, to_add = fold x ~init:(None, zero) ~f in
    plus x to_add


  let is_same_symbol x1 x2 =
    match (get_one_symbol_opt x1, get_one_symbol_opt x2) with
    | Some s1, Some s2 when Symb.Symbol.paths_equal s1 s2 ->
        Some (Symb.Symbol.path s1)
    | _ ->
        None


  let exists_str ~f x = M.exists (fun k _ -> Symb.Symbol.exists_str ~f k) x
end

module Bound = struct
  type sign = Plus | Minus [@@deriving compare]

  module Sign = struct
    type t = sign [@@deriving compare]

    let neg = function Plus -> Minus | Minus -> Plus

    let eval_big_int x i1 i2 = match x with Plus -> Z.(i1 + i2) | Minus -> Z.(i1 - i2)

    let eval_neg_if_minus x i = match x with Plus -> i | Minus -> Z.neg i

    let pp ~need_plus : F.formatter -> t -> unit =
     fun fmt -> function
      | Plus ->
          if need_plus then F.pp_print_char fmt '+'
      | Minus ->
          F.pp_print_char fmt '-'
  end

  type min_max = Min | Max [@@deriving compare]

  module MinMax = struct
    type t = min_max [@@deriving compare]

    let neg = function Min -> Max | Max -> Min

    let eval_big_int x i1 i2 = match x with Min -> Z.min i1 i2 | Max -> Z.max i1 i2

    let pp : F.formatter -> t -> unit =
     fun fmt -> function Min -> F.pp_print_string fmt "min" | Max -> F.pp_print_string fmt "max"
  end

  (* MinMax constructs a bound that is in the "int [+|-] [min|max](int, Symb.Symbol)" format.
     e.g. `MinMax (1, Minus, Max, 2, s)` means "1 - max (2, s)". *)
  type t =
    | MInf
    | Linear of Z.t * SymLinear.t
    | MinMax of Z.t * Sign.t * MinMax.t * Z.t * Symb.Symbol.t
    | PInf
  [@@deriving compare]

  type eval_sym = t Symb.Symbol.eval

  let equal = [%compare.equal: t]

  let pp_mark : markup:bool -> F.formatter -> t -> unit =
   fun ~markup f -> function
    | MInf ->
        F.pp_print_string f "-oo"
    | PInf ->
        F.pp_print_string f "+oo"
    | Linear (c, x) ->
        if SymLinear.is_zero x then Z.pp_print f c
        else (
          SymLinear.pp ~markup ~is_beginning:true f x ;
          if not Z.(equal c zero) then
            if Z.gt c Z.zero then F.fprintf f " + %a" Z.pp_print c
            else F.fprintf f " - %a" Z.pp_print (Z.neg c) )
    | MinMax (c, sign, m, d, x) ->
        if Z.(equal c zero) then (Sign.pp ~need_plus:false) f sign
        else F.fprintf f "%a%a" Z.pp_print c (Sign.pp ~need_plus:true) sign ;
        F.fprintf f "%a(%a, %a)" MinMax.pp m Z.pp_print d (Symb.Symbol.pp_mark ~markup) x


  let pp = pp_mark ~markup:false

  let of_bound_end = function Symb.BoundEnd.LowerBound -> MInf | Symb.BoundEnd.UpperBound -> PInf

  let of_int : int -> t = fun n -> Linear (Z.of_int n, SymLinear.empty)

  let of_big_int : Z.t -> t = fun n -> Linear (n, SymLinear.empty)

  let minus_one = of_int (-1)

  let _255 = of_int 255

  let of_sym : SymLinear.t -> t = fun s -> Linear (Z.zero, s)

  let of_path path_of_partial make_symbol ~unsigned partial =
    let s = make_symbol ~unsigned (path_of_partial partial) in
    of_sym (SymLinear.singleton_one s)


  let of_normal_path = of_path Symb.SymbolPath.normal

  let of_offset_path = of_path Symb.SymbolPath.offset ~unsigned:false

  let of_length_path = of_path Symb.SymbolPath.length ~unsigned:true

  let is_symbolic : t -> bool = function
    | MInf | PInf ->
        false
    | Linear (_, se) ->
        not (SymLinear.is_empty se)
    | MinMax _ ->
        true


  let mk_MinMax (c, sign, m, d, s) =
    if Symb.Symbol.is_unsigned s && Z.(d <= zero) then
      match m with
      | Min ->
          of_big_int (Sign.eval_big_int sign c d)
      | Max -> (
        match sign with
        | Plus ->
            Linear (c, SymLinear.singleton_one s)
        | Minus ->
            Linear (c, SymLinear.singleton_minus_one s) )
    else MinMax (c, sign, m, d, s)


  let big_int_ub_of_minmax = function
    | MinMax (c, Plus, Min, d, _) ->
        Some Z.(c + d)
    | MinMax (c, Minus, Max, d, _) ->
        Some Z.(c - d)
    | MinMax (c, Minus, Min, _, s) when Symb.Symbol.is_unsigned s ->
        Some c
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let big_int_lb_of_minmax = function
    | MinMax (c, Plus, Max, d, _) ->
        Some Z.(c + d)
    | MinMax (c, Minus, Min, d, _) ->
        Some Z.(c - d)
    | MinMax (c, Plus, Min, _, s) when Symb.Symbol.is_unsigned s ->
        Some c
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let big_int_of_minmax = function
    | Symb.BoundEnd.LowerBound ->
        big_int_lb_of_minmax
    | Symb.BoundEnd.UpperBound ->
        big_int_ub_of_minmax


  let big_int_lb = function
    | MInf ->
        None
    | PInf ->
        assert false
    | MinMax _ as b ->
        big_int_lb_of_minmax b
    | Linear (c, se) ->
        SymLinear.big_int_lb se |> Option.map ~f:(Z.( + ) c)


  let big_int_ub = function
    | MInf ->
        assert false
    | PInf ->
        None
    | MinMax _ as b ->
        big_int_ub_of_minmax b
    | Linear (c, se) ->
        SymLinear.big_int_ub se |> Option.map ~f:(Z.( + ) c)


  let linear_ub_of_minmax = function
    | MinMax (c, Plus, Min, _, x) ->
        Some (Linear (c, SymLinear.singleton_one x))
    | MinMax (c, Minus, Max, _, x) ->
        Some (Linear (c, SymLinear.singleton_minus_one x))
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let linear_lb_of_minmax = function
    | MinMax (c, Plus, Max, _, x) ->
        Some (Linear (c, SymLinear.singleton_one x))
    | MinMax (c, Minus, Min, _, x) ->
        Some (Linear (c, SymLinear.singleton_minus_one x))
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let le_minmax_by_int x y =
    match (big_int_ub_of_minmax x, big_int_lb_of_minmax y) with
    | Some n, Some m ->
        n <= m
    | _, _ ->
        false


  let le_opt1 le opt_n m = Option.value_map opt_n ~default:false ~f:(fun n -> le n m)

  let le_opt2 le n opt_m = Option.value_map opt_m ~default:false ~f:(fun m -> le n m)

  let rec le : t -> t -> bool =
   fun x y ->
    match (x, y) with
    | MInf, _ | _, PInf ->
        true
    | _, MInf | PInf, _ ->
        false
    | Linear (c0, x0), Linear (c1, x1) ->
        c0 <= c1 && SymLinear.le x0 x1
    | MinMax _, MinMax _ when le_minmax_by_int x y ->
        true
    | MinMax (c1, (Plus as sign), Min, d1, s1), MinMax (c2, Plus, Min, d2, s2)
    | MinMax (c1, (Minus as sign), Min, d1, s1), MinMax (c2, Minus, Min, d2, s2)
    | MinMax (c1, (Plus as sign), Max, d1, s1), MinMax (c2, Plus, Max, d2, s2)
    | MinMax (c1, (Minus as sign), Max, d1, s1), MinMax (c2, Minus, Max, d2, s2)
      when Symb.Symbol.equal s1 s2 ->
        Z.leq c1 c2
        &&
        let v1 = Sign.eval_big_int sign c1 d1 in
        let v2 = Sign.eval_big_int sign c2 d2 in
        Z.leq v1 v2
    | MinMax (c1, Plus, Min, _, s1), MinMax (c2, Plus, Max, _, s2)
    | MinMax (c1, Minus, Max, _, s1), MinMax (c2, Minus, Min, _, s2)
      when Symb.Symbol.equal s1 s2 ->
        Z.leq c1 c2
    | MinMax _, MinMax _ ->
        false
    | MinMax _, Linear (c, se) ->
        (SymLinear.is_ge_zero se && le_opt1 Z.leq (big_int_ub_of_minmax x) c)
        || le_opt1 le (linear_ub_of_minmax x) y
    | Linear (c, se), MinMax _ ->
        (SymLinear.is_le_zero se && le_opt2 Z.leq c (big_int_lb_of_minmax y))
        || le_opt2 le x (linear_lb_of_minmax y)


  let lt : t -> t -> bool =
   fun x y ->
    match (x, y) with
    | MInf, Linear _ | MInf, MinMax _ | MInf, PInf | Linear _, PInf | MinMax _, PInf ->
        true
    | Linear (c, x), _ ->
        le (Linear (Z.succ c, x)) y
    | MinMax (c, sign, min_max, d, x), _ ->
        le (mk_MinMax (Z.succ c, sign, min_max, d, x)) y
    | _, _ ->
        false


  let gt : t -> t -> bool = fun x y -> lt y x

  let eq : t -> t -> bool = fun x y -> le x y && le y x

  let xcompare = PartialOrder.of_le ~le

  let is_const : t -> bool = function Linear (_, se) -> SymLinear.is_zero se | _ -> false

  let neg : t -> t = function
    | MInf ->
        PInf
    | PInf ->
        MInf
    | Linear (c, x) as b ->
        if Z.(equal c zero) && SymLinear.is_zero x then b else Linear (Z.neg c, SymLinear.neg x)
    | MinMax (c, sign, min_max, d, x) ->
        mk_MinMax (Z.neg c, Sign.neg sign, min_max, d, x)


  let exact_min : otherwise:(t -> t -> t) -> t -> t -> t =
   fun ~otherwise b1 b2 ->
    if le b1 b2 then b1
    else if le b2 b1 then b2
    else
      match (b1, b2) with
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_one_symbol x2 ->
          mk_MinMax (c2, Plus, Min, Z.(c1 - c2), SymLinear.get_one_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_one_symbol x1 && SymLinear.is_zero x2 ->
          mk_MinMax (c1, Plus, Min, Z.(c2 - c1), SymLinear.get_one_symbol x1)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2
        ->
          mk_MinMax (c2, Minus, Max, Z.(c2 - c1), SymLinear.get_mone_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2
        ->
          mk_MinMax (c1, Minus, Max, Z.(c1 - c2), SymLinear.get_mone_symbol x1)
      | MinMax (c1, (Plus as sign), (Min as minmax), _, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, (Plus as sign), (Min as minmax), _, s)
      | MinMax (c1, (Minus as sign), (Max as minmax), _, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, (Minus as sign), (Max as minmax), _, s)
        when SymLinear.is_zero se ->
          let d = Sign.eval_neg_if_minus sign Z.(c2 - c1) in
          mk_MinMax (c1, sign, minmax, d, s)
      | MinMax (c1, (Minus as sign), (Max as minmax), d1, s1), MinMax (c2, Minus, Max, d2, s2)
      | MinMax (c1, (Plus as sign), (Min as minmax), d1, s1), MinMax (c2, Plus, Min, d2, s2)
        when Symb.Symbol.equal s1 s2 ->
          let v1 = Sign.eval_big_int sign c1 d1 in
          let v2 = Sign.eval_big_int sign c2 d2 in
          let c = Z.min c1 c2 in
          let v = MinMax.eval_big_int minmax v1 v2 in
          let d = Sign.eval_neg_if_minus sign Z.(v - c) in
          mk_MinMax (c, sign, minmax, d, s1)
      | b1, b2 ->
          otherwise b1 b2


  let rec underapprox_min b1 b2 =
    exact_min b1 b2 ~otherwise:(fun b1 b2 ->
        match (b1, b2) with
        | MinMax (c1, sign, _, d1, _s), Linear (_c2, se)
        | Linear (_c2, se), MinMax (c1, sign, _, d1, _s)
          when SymLinear.is_zero se ->
            Linear (Sign.eval_big_int sign c1 d1, SymLinear.zero)
            (*
              There is no best abstraction, we could also use:
              For Plus, Max: mk_MinMax (c1, Plus, Min, Z.(c2 - c1), s)
              For Minus, Min: mk_MinMax (c1, Minus, Max, Z.(c1 - c2), s)
            *)
        | MinMax (_, Minus, Max, _, _), MinMax (_, Plus, Min, _, _)
        | MinMax (_, Plus, Min, _, _), MinMax (_, Minus, Max, _, _) ->
            fallback_underapprox_min b1 b2
        | MinMax (c1, (Plus as sign1), Max, d1, _), MinMax (c2, (Minus as sign2), Min, d2, _)
        | MinMax (c1, (Minus as sign1), Min, d1, _), MinMax (c2, (Plus as sign2), Max, d2, _) ->
            let v1 = Sign.eval_big_int sign1 c1 d1 in
            let v2 = Sign.eval_big_int sign2 c2 d2 in
            Linear (Z.min v1 v2, SymLinear.zero)
        | MinMax (c1, (Plus as sign), (Max as minmax), d1, s1), MinMax (c2, Plus, Max, d2, s2)
        | MinMax (c1, (Minus as sign), (Min as minmax), d1, s1), MinMax (c2, Minus, Min, d2, s2)
          when Symb.Symbol.equal s1 s2 ->
            let v1 = Sign.eval_big_int sign c1 d1 in
            let v2 = Sign.eval_big_int sign c2 d2 in
            let v = Z.min v1 v2 in
            let c = Z.min c1 c2 in
            let d = Sign.eval_neg_if_minus sign Z.(v - c) in
            mk_MinMax (c, sign, minmax, d, s1)
        | ( MinMax (c1, (Plus as sign1), (Min as minmax1), d1, s1)
          , MinMax (c2, (Plus as sign2), Max, d2, s2) )
        | ( MinMax (c2, (Plus as sign2), Max, d2, s2)
          , MinMax (c1, (Plus as sign1), (Min as minmax1), d1, s1) )
        | ( MinMax (c1, (Minus as sign1), (Max as minmax1), d1, s1)
          , MinMax (c2, (Minus as sign2), Min, d2, s2) )
        | ( MinMax (c2, (Minus as sign2), Min, d2, s2)
          , MinMax (c1, (Minus as sign1), (Max as minmax1), d1, s1) )
        | ( MinMax (c1, (Minus as sign1), (Max as minmax1), d1, s1)
          , MinMax (c2, (Plus as sign2), Max, d2, s2) )
        | ( MinMax (c2, (Plus as sign2), (Max as minmax1), d2, s2)
          , MinMax (c1, (Minus as sign1), Max, d1, s1) )
        | ( MinMax (c1, (Plus as sign1), (Min as minmax1), d1, s1)
          , MinMax (c2, (Minus as sign2), Min, d2, s2) )
        | ( MinMax (c2, (Minus as sign2), (Min as minmax1), d2, s2)
          , MinMax (c1, (Plus as sign1), Min, d1, s1) )
          when Symb.Symbol.equal s1 s2 ->
            let v1 = Sign.eval_big_int sign1 c1 d1 in
            let v2 = Sign.eval_big_int sign2 c2 d2 in
            let v = Z.min v1 v2 in
            let d = Sign.eval_neg_if_minus sign1 Z.(v - c1) in
            mk_MinMax (c1, sign1, minmax1, d, s1)
        | b1, b2 ->
            fallback_underapprox_min b1 b2 )


  and fallback_underapprox_min b1 b2 =
    match big_int_lb b2 with
    | Some v2 when not (is_const b2) ->
        underapprox_min b1 (Linear (v2, SymLinear.zero))
    | _ -> (
      match big_int_lb b1 with
      | Some v1 when not (is_const b1) ->
          underapprox_min (Linear (v1, SymLinear.zero)) b2
      | _ ->
          MInf )


  let overapprox_min original_b1 b2 =
    let rec overapprox_min b1 b2 =
      exact_min b1 b2 ~otherwise:(fun b1 b2 ->
          match (b1, b2) with
          | ( MinMax (c1, (Minus as sign1), (Max as minmax1), d1, s1)
            , MinMax (c2, (Plus as sign2), Min, d2, s2) )
          | ( MinMax (c1, (Plus as sign1), (Min as minmax1), d1, s1)
            , MinMax (c2, (Minus as sign2), Max, d2, s2) )
            when Symb.Symbol.equal s1 s2 ->
              let v1 = Sign.eval_big_int sign1 c1 d1 in
              let v2 = Sign.eval_big_int sign2 c2 d2 in
              let vmeet = Z.(shift_right (c1 + c2 + one) 1) in
              let v = Z.(min vmeet (min v1 v2)) in
              let d = Sign.eval_neg_if_minus sign1 Z.(v - c1) in
              mk_MinMax (c1, sign1, minmax1, d, s1)
          | MinMax (c1, (Minus as sign1), Max, d1, s1), MinMax (c2, (Plus as sign2), Min, d2, s2)
          | MinMax (c1, (Minus as sign1), Min, d1, s1), MinMax (c2, (Plus as sign2), Max, d2, s2)
          | MinMax (c1, (Plus as sign1), Min, d1, s1), MinMax (c2, (Minus as sign2), Max, d2, s2)
          | MinMax (c1, (Plus as sign1), Max, d1, s1), MinMax (c2, (Minus as sign2), Min, d2, s2)
            when Symb.Symbol.equal s1 s2 ->
              let v1 = Sign.eval_big_int sign1 c1 d1 in
              let v2 = Sign.eval_big_int sign2 c2 d2 in
              let vmeet = Z.(shift_right (c1 + c2 + one) 1) in
              Linear (Z.(max vmeet (max v1 v2)), SymLinear.zero)
          | (MinMax (_, Plus, Min, _, s1) as b), MinMax (_, Plus, Max, _, s2)
          | MinMax (_, Plus, Max, _, s2), (MinMax (_, Plus, Min, _, s1) as b)
          | (MinMax (_, Minus, Min, _, s1) as b), MinMax (_, Minus, Max, _, s2)
          | MinMax (_, Minus, Max, _, s2), (MinMax (_, Minus, Min, _, s1) as b)
            when Symb.Symbol.equal s1 s2 ->
              b
          | MinMax (c1, Plus, Max, _, s1), MinMax (c2, Plus, Max, _, s2)
          | MinMax (c1, Minus, Min, _, s1), MinMax (c2, Minus, Min, _, s2)
            when Symb.Symbol.equal s1 s2 ->
              if Z.leq c1 c2 then b1 else b2
          | ( MinMax (c1, (Minus as sign1), (Max as minmax1), d1, s1)
            , MinMax (c2, (Plus as sign2), Max, d2, s2) )
          | ( MinMax (c2, (Plus as sign2), (Max as minmax1), d2, s2)
            , MinMax (c1, (Minus as sign1), Max, d1, s1) )
          | ( MinMax (c1, (Plus as sign1), (Min as minmax1), d1, s1)
            , MinMax (c2, (Minus as sign2), Min, d2, s2) )
          | ( MinMax (c2, (Minus as sign2), (Min as minmax1), d2, s2)
            , MinMax (c1, (Plus as sign1), Min, d1, s1) )
            when Symb.Symbol.equal s1 s2 ->
              let v1 = Sign.eval_big_int sign1 c1 d1 in
              let v2 = Sign.eval_big_int sign2 c2 d2 in
              let vmin, vmax = if Z.leq v1 v2 then (v1, v2) else (v2, v1) in
              let vmeet = Z.(shift_right (c1 + c2 + one) 1) in
              let v = if Z.leq vmin vmeet && Z.leq vmeet vmax then vmeet else vmax in
              let d = Sign.eval_neg_if_minus sign1 Z.(v - c1) in
              mk_MinMax (c1, sign1, minmax1, d, s1)
          | _ -> (
            match big_int_ub b2 with
            | Some v2 when not (is_const b2) ->
                overapprox_min b1 (Linear (v2, SymLinear.zero))
            | _ -> (
              match big_int_ub b1 with
              | Some v1 when not (is_const b1) ->
                  overapprox_min (Linear (v1, SymLinear.zero)) b2
              | _ ->
                  (* When the result is not representable, our best effort is to return the first original argument. Any other deterministic heuristics would work too. *)
                  original_b1 ) ) )
    in
    overapprox_min original_b1 b2


  let underapprox_max b1 b2 =
    let res = neg (overapprox_min (neg b1) (neg b2)) in
    if equal res b1 then b1 else if equal res b2 then b2 else res


  let overapprox_max b1 b2 =
    let res = neg (underapprox_min (neg b1) (neg b2)) in
    if equal res b1 then b1 else if equal res b2 then b2 else res


  let approx_max = function
    | Symb.BoundEnd.LowerBound ->
        underapprox_max
    | Symb.BoundEnd.UpperBound ->
        overapprox_max


  let zero : t = Linear (Z.zero, SymLinear.zero)

  let widen_l : t -> t -> t =
   fun x y ->
    match (x, y) with
    | PInf, _ | _, PInf ->
        L.(die InternalError) "Lower bound cannot be +oo."
    | MinMax (n1, Plus, Max, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_one_symbol_of s1 s2 ->
        y
    | MinMax (n1, Minus, Min, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_mone_symbol_of s1 s2 ->
        y
    | _ ->
        if le x y then x else if le zero x && le zero y then zero else MInf


  let widen_u : t -> t -> t =
   fun x y ->
    match (x, y) with
    | MInf, _ | _, MInf ->
        L.(die InternalError) "Upper bound cannot be -oo."
    | MinMax (n1, Plus, Min, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_one_symbol_of s1 s2 ->
        y
    | MinMax (n1, Minus, Max, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_mone_symbol_of s1 s2 ->
        y
    | _ ->
        if le y x then x else if le x zero && le y zero then zero else PInf


  let one : t = Linear (Z.one, SymLinear.zero)

  let mone : t = Linear (Z.minus_one, SymLinear.zero)

  let is_some_const : Z.t -> t -> bool =
   fun c x -> match x with Linear (c', y) -> Z.equal c c' && SymLinear.is_zero y | _ -> false


  let is_zero : t -> bool = is_some_const Z.zero

  let is_const : t -> Z.t option =
   fun x -> match x with Linear (c, y) when SymLinear.is_zero y -> Some c | _ -> None


  let plus_common : f:(t -> t -> t) -> t -> t -> t =
   fun ~f x y ->
    if is_zero x then y
    else if is_zero y then x
    else
      match (x, y) with
      | Linear (c1, x1), Linear (c2, x2) ->
          Linear (Z.(c1 + c2), SymLinear.plus x1 x2)
      | MinMax (c1, sign, min_max, d1, x1), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, sign, min_max, d1, x1)
        when SymLinear.is_zero x2 ->
          mk_MinMax (Z.(c1 + c2), sign, min_max, d1, x1)
      | _ ->
          f x y


  let plus_l : t -> t -> t =
    plus_common ~f:(fun x y ->
        match (x, y) with
        | MinMax (c1, Plus, Max, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Plus, Max, d1, _) ->
            Linear (Z.(c1 + d1 + c2), x2)
        | MinMax (c1, Minus, Min, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Minus, Min, d1, _) ->
            Linear (Z.(c1 - d1 + c2), x2)
        | _, _ ->
            MInf )


  let plus_u : t -> t -> t =
    plus_common ~f:(fun x y ->
        match (x, y) with
        | MinMax (c1, Plus, Min, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Plus, Min, d1, _) ->
            Linear (Z.(c1 + d1 + c2), x2)
        | MinMax (c1, Minus, Max, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Minus, Max, d1, _) ->
            Linear (Z.(c1 - d1 + c2), x2)
        | _, _ ->
            PInf )


  let plus = function Symb.BoundEnd.LowerBound -> plus_l | Symb.BoundEnd.UpperBound -> plus_u

  let mult_const : Symb.BoundEnd.t -> NonZeroInt.t -> t -> t =
   fun bound_end n x ->
    if NonZeroInt.is_one n then x
    else
      match x with
      | MInf ->
          if NonZeroInt.is_positive n then MInf else PInf
      | PInf ->
          if NonZeroInt.is_positive n then PInf else MInf
      | Linear (c, x') ->
          Linear (Z.(c * (n :> Z.t)), SymLinear.mult_const n x')
      | MinMax _ -> (
          let int_bound =
            let bound_end' =
              if NonZeroInt.is_positive n then bound_end else Symb.BoundEnd.neg bound_end
            in
            big_int_of_minmax bound_end' x
          in
          match int_bound with
          | Some i ->
              of_big_int Z.(i * (n :> Z.t))
          | None ->
              of_bound_end bound_end )


  let mult_const_l = mult_const Symb.BoundEnd.LowerBound

  let mult_const_u = mult_const Symb.BoundEnd.UpperBound

  let overapprox_minmax_div_const x (n : NonZeroInt.t) =
    let c = if NonZeroInt.is_positive n then big_int_ub_of_minmax x else big_int_lb_of_minmax x in
    Option.map c ~f:(fun c -> Z.(c / (n :> Z.t)))


  let underapprox_minmax_div_const x (n : NonZeroInt.t) =
    let c = if NonZeroInt.is_positive n then big_int_lb_of_minmax x else big_int_ub_of_minmax x in
    Option.map c ~f:(fun c -> Z.(c / (n :> Z.t)))


  let div_const : Symb.BoundEnd.t -> t -> NonZeroInt.t -> t option =
   fun bound_end x n ->
    if NonZeroInt.is_one n then Some x
    else
      match x with
      | MInf ->
          Some (if NonZeroInt.is_positive n then MInf else PInf)
      | PInf ->
          Some (if NonZeroInt.is_positive n then PInf else MInf)
      | Linear (c, x') when SymLinear.is_zero x' ->
          Some (Linear (Z.(c / (n :> Z.t)), SymLinear.zero))
      | Linear (c, x') when NonZeroInt.is_multiple c n -> (
        match SymLinear.exact_div_const_exn x' n with
        | x'' ->
            Some (Linear (Z.(c / (n :> Z.t)), x''))
        | exception NonZeroInt.DivisionNotExact ->
            None )
      | MinMax _ ->
          let c =
            match bound_end with
            | Symb.BoundEnd.LowerBound ->
                underapprox_minmax_div_const x n
            | Symb.BoundEnd.UpperBound ->
                overapprox_minmax_div_const x n
          in
          Option.map c ~f:of_big_int
      | _ ->
          None


  let div_const_l = div_const Symb.BoundEnd.LowerBound

  let div_const_u = div_const Symb.BoundEnd.UpperBound

  let get_symbols : t -> Symb.SymbolSet.t = function
    | MInf | PInf ->
        Symb.SymbolSet.empty
    | Linear (_, se) ->
        SymLinear.get_symbols se
    | MinMax (_, _, _, _, s) ->
        Symb.SymbolSet.singleton s


  let are_similar b1 b2 = Symb.SymbolSet.equal (get_symbols b1) (get_symbols b2)

  let is_not_infty : t -> bool = function MInf | PInf -> false | _ -> true

  (** Substitutes ALL symbols in [x] with respect to [eval_sym]. Under/over-Approximate as good as possible according to [subst_pos]. *)
  let subst : subst_pos:Symb.BoundEnd.t -> t -> eval_sym -> t bottom_lifted =
    let lift1 : (t -> t) -> t bottom_lifted -> t bottom_lifted =
     fun f x -> match x with Bottom -> Bottom | NonBottom x -> NonBottom (f x)
    in
    let lift2 : (t -> t -> t) -> t bottom_lifted -> t bottom_lifted -> t bottom_lifted =
     fun f x y ->
      match (x, y) with
      | Bottom, _ | _, Bottom ->
          Bottom
      | NonBottom x, NonBottom y ->
          NonBottom (f x y)
    in
    fun ~subst_pos x eval_sym ->
      let get s bound_position =
        match eval_sym s bound_position with
        | NonBottom x when Symb.Symbol.is_unsigned s ->
            NonBottom (approx_max subst_pos x zero)
        | x ->
            x
      in
      let get_mult_const s coeff =
        let bound_position =
          if NonZeroInt.is_positive coeff then subst_pos else Symb.BoundEnd.neg subst_pos
        in
        if NonZeroInt.is_one coeff then get s bound_position
        else if NonZeroInt.is_minus_one coeff then get s bound_position |> lift1 neg
        else
          match eval_sym s bound_position with
          | Bottom -> (
            (* For unsigned symbols, we can over/under-approximate with zero depending on [bound_position]. *)
            match (Symb.Symbol.is_unsigned s, bound_position) with
            | true, Symb.BoundEnd.LowerBound ->
                NonBottom zero
            | _ ->
                Bottom )
          | NonBottom x ->
              let x = mult_const subst_pos coeff x in
              if Symb.Symbol.is_unsigned s then NonBottom (approx_max subst_pos x zero)
              else NonBottom x
      in
      match x with
      | MInf | PInf ->
          NonBottom x
      | Linear (c, se) ->
          if SymLinear.is_empty se then NonBottom x
          else
            SymLinear.fold se
              ~init:(NonBottom (of_big_int c))
              ~f:(fun acc s coeff -> lift2 (plus subst_pos) acc (get_mult_const s coeff))
      | MinMax (c, sign, min_max, d, s) -> (
          let bound_position =
            match sign with Plus -> subst_pos | Minus -> Symb.BoundEnd.neg subst_pos
          in
          match get s bound_position with
          | Bottom ->
              Option.value_map (big_int_of_minmax subst_pos x) ~default:Bottom ~f:(fun i ->
                  NonBottom (of_big_int i) )
          | NonBottom x' ->
              let res =
                match (sign, min_max, x') with
                | Plus, Min, MInf | Minus, Max, PInf ->
                    MInf
                | Plus, Max, PInf | Minus, Min, MInf ->
                    PInf
                | sign, Min, PInf | sign, Max, MInf ->
                    of_big_int (Sign.eval_big_int sign c d)
                | _, _, Linear (c2, se) -> (
                    if SymLinear.is_zero se then
                      of_big_int (Sign.eval_big_int sign c (MinMax.eval_big_int min_max d c2))
                    else if SymLinear.is_one_symbol se then
                      mk_MinMax
                        ( Sign.eval_big_int sign c c2
                        , sign
                        , min_max
                        , Z.(d - c2)
                        , SymLinear.get_one_symbol se )
                    else if SymLinear.is_mone_symbol se then
                      mk_MinMax
                        ( Sign.eval_big_int sign c c2
                        , Sign.neg sign
                        , MinMax.neg min_max
                        , Z.(c2 - d)
                        , SymLinear.get_mone_symbol se )
                    else
                      match big_int_of_minmax subst_pos x with
                      | Some i ->
                          of_big_int i
                      | None ->
                          of_bound_end subst_pos )
                | _, _, MinMax (c2, sign2, min_max2, d2, s2) -> (
                  match (min_max, sign2, min_max2) with
                  | Min, Plus, Min | Max, Plus, Max ->
                      let c' = Sign.eval_big_int sign c c2 in
                      let d' = MinMax.eval_big_int min_max Z.(d - c2) d2 in
                      mk_MinMax (c', sign, min_max, d', s2)
                  | Min, Minus, Max | Max, Minus, Min ->
                      let c' = Sign.eval_big_int sign c c2 in
                      let d' = MinMax.eval_big_int min_max2 Z.(c2 - d) d2 in
                      mk_MinMax (c', Sign.neg sign, min_max2, d', s2)
                  | _ ->
                      let bound_end =
                        match sign with Plus -> subst_pos | Minus -> Symb.BoundEnd.neg subst_pos
                      in
                      of_big_int
                        (Sign.eval_big_int sign c
                           (MinMax.eval_big_int min_max d
                              (big_int_of_minmax bound_end x' |> Option.value ~default:d))) )
              in
              NonBottom res )


  let subst_lb x eval_sym = subst ~subst_pos:Symb.BoundEnd.LowerBound x eval_sym

  let subst_ub x eval_sym = subst ~subst_pos:Symb.BoundEnd.UpperBound x eval_sym

  let simplify_bound_ends_from_paths x =
    match x with
    | MInf | PInf | MinMax _ ->
        x
    | Linear (c, se) ->
        let se' = SymLinear.simplify_bound_ends_from_paths se in
        if phys_equal se se' then x else Linear (c, se')


  let is_same_symbol b1 b2 =
    match (b1, b2) with
    | Linear (n1, se1), Linear (n2, se2) when Z.(equal n1 zero) && Z.(equal n2 zero) ->
        SymLinear.is_same_symbol se1 se2
    | _ ->
        None


  let exists_str ~f = function
    | MInf | PInf ->
        false
    | Linear (_, s) ->
        SymLinear.exists_str ~f s
    | MinMax (_, _, _, _, s) ->
        Symb.Symbol.exists_str ~f s
end

type ('c, 's) valclass = Constant of 'c | Symbolic of 's | ValTop

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
        ValTop
    | Bound.MInf ->
        assert false
    | b -> (
      match Bound.is_const b with
      | None ->
          Symbolic b
      | Some c ->
          Constant (NonNegativeInt.of_big_int_exn c) )


  let subst b map =
    match Bound.subst_ub b map with
    | Bottom ->
        Constant NonNegativeInt.zero
    | NonBottom b ->
        of_bound b |> classify
end
