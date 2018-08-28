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

exception Symbol_not_found of Symb.Symbol.t

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
    let le_one_pair s v1_opt v2_opt =
      let v1 = NonZeroInt.opt_to_int v1_opt in
      let v2 = NonZeroInt.opt_to_int v2_opt in
      Int.equal v1 v2 || (Symb.Symbol.is_unsigned s && v1 <= v2)
    in
    M.for_all2 ~f:le_one_pair x y


  let make :
         unsigned:bool
      -> Typ.Procname.t
      -> Symb.SymbolTable.t
      -> Symb.SymbolPath.t
      -> Counter.t
      -> t * t =
   fun ~unsigned pname symbol_table path new_sym_num ->
    let lb, ub = Symb.SymbolTable.lookup ~unsigned pname path symbol_table new_sym_num in
    (singleton_one lb, singleton_one ub)


  let eq : t -> t -> bool =
   fun x y ->
    let eq_pair _ (coeff1 : NonZeroInt.t option) (coeff2 : NonZeroInt.t option) =
      [%compare.equal: int option] (coeff1 :> int option) (coeff2 :> int option)
    in
    M.for_all2 ~f:eq_pair x y


  let pp1 : F.formatter -> Symb.Symbol.t * NonZeroInt.t -> unit =
   fun fmt (s, c) ->
    let c = (c :> int) in
    if Int.equal c 1 then Symb.Symbol.pp fmt s
    else if Int.equal c (-1) then F.fprintf fmt "-%a" Symb.Symbol.pp s
    else F.fprintf fmt "%dx%a" c Symb.Symbol.pp s


  let pp : F.formatter -> t -> unit =
   fun fmt x ->
    if M.is_empty x then F.pp_print_string fmt "empty"
    else Pp.seq ~sep:" + " pp1 fmt (M.bindings x)


  let zero : t = M.empty

  let is_zero : t -> bool = M.is_empty

  let neg : t -> t = fun x -> M.map NonZeroInt.( ~- ) x

  let plus : t -> t -> t =
   fun x y ->
    let plus_coeff _ c1 c2 = NonZeroInt.plus c1 c2 in
    M.union plus_coeff x y


  let mult_const : NonZeroInt.t -> t -> t = fun n x -> M.map (NonZeroInt.( * ) n) x

  let exact_div_const_exn : t -> NonZeroInt.t -> t =
   fun x n -> M.map (fun c -> NonZeroInt.exact_div_exn c n) x


  (* Returns a symbol when the map contains only one symbol s with a
     given coefficient. *)
  let one_symbol_of_coeff : NonZeroInt.t -> t -> Symb.Symbol.t option =
   fun coeff x ->
    match M.is_singleton x with
    | Some (k, v) when Int.equal (v :> int) (coeff :> int) ->
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


  let get_symbols : t -> Symb.Symbol.t list =
   fun x -> M.fold (fun symbol _coeff acc -> symbol :: acc) x []


  (* we can give integer bounds (obviously 0) only when all symbols are unsigned *)

  let int_lb x = if is_ge_zero x then Some 0 else None

  let int_ub x = if is_le_zero x then Some 0 else None

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
end

module Bound = struct
  type sign = Plus | Minus [@@deriving compare]

  module Sign = struct
    type t = sign [@@deriving compare]

    let equal = [%compare.equal: t]

    let neg = function Plus -> Minus | Minus -> Plus

    let eval_int x i1 i2 = match x with Plus -> i1 + i2 | Minus -> i1 - i2

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

    let equal = [%compare.equal: t]

    let neg = function Min -> Max | Max -> Min

    let eval_int x i1 i2 = match x with Min -> min i1 i2 | Max -> max i1 i2

    let pp : F.formatter -> t -> unit =
     fun fmt -> function Min -> F.pp_print_string fmt "min" | Max -> F.pp_print_string fmt "max"
  end

  (* MinMax constructs a bound that is in the "int [+|-] [min|max](int, Symb.Symbol)" format.
     e.g. `MinMax (1, Minus, Max, 2, s)` means "1 - max (2, s)". *)
  type t =
    | MInf
    | Linear of int * SymLinear.t
    | MinMax of int * Sign.t * MinMax.t * int * Symb.Symbol.t
    | PInf
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let pp : F.formatter -> t -> unit =
   fun fmt -> function
    | MInf ->
        F.pp_print_string fmt "-oo"
    | PInf ->
        F.pp_print_string fmt "+oo"
    | Linear (c, x) ->
        if SymLinear.is_zero x then F.pp_print_int fmt c
        else if Int.equal c 0 then SymLinear.pp fmt x
        else F.fprintf fmt "%a + %d" SymLinear.pp x c
    | MinMax (c, sign, m, d, x) ->
        if Int.equal c 0 then (Sign.pp ~need_plus:false) fmt sign
        else F.fprintf fmt "%d%a" c (Sign.pp ~need_plus:true) sign ;
        F.fprintf fmt "%a(%d, %a)" MinMax.pp m d Symb.Symbol.pp x


  let of_bound_end = function Symb.BoundEnd.LowerBound -> MInf | Symb.BoundEnd.UpperBound -> PInf

  let of_int : int -> t = fun n -> Linear (n, SymLinear.empty)

  let minus_one = of_int (-1)

  let _255 = of_int 255

  let of_sym : SymLinear.t -> t = fun s -> Linear (0, s)

  let is_symbolic : t -> bool = function
    | MInf | PInf ->
        false
    | Linear (_, se) ->
        not (SymLinear.is_empty se)
    | MinMax _ ->
        true


  let lift_symlinear : (SymLinear.t -> 'a option) -> t -> 'a option =
   fun f -> function Linear (0, se) -> f se | _ -> None


  let get_one_symbol_opt : t -> Symb.Symbol.t option = lift_symlinear SymLinear.get_one_symbol_opt

  let get_mone_symbol_opt : t -> Symb.Symbol.t option =
    lift_symlinear SymLinear.get_mone_symbol_opt


  let get_one_symbol : t -> Symb.Symbol.t =
   fun x -> match get_one_symbol_opt x with Some s -> s | None -> raise Not_One_Symbol


  let get_mone_symbol : t -> Symb.Symbol.t =
   fun x -> match get_mone_symbol_opt x with Some s -> s | None -> raise Not_One_Symbol


  let is_one_symbol : t -> bool = fun x -> get_one_symbol_opt x <> None

  let is_mone_symbol : t -> bool = fun x -> get_mone_symbol_opt x <> None

  let mk_MinMax (c, sign, m, d, s) =
    if Symb.Symbol.is_unsigned s && d <= 0 then
      match m with
      | Min ->
          of_int (Sign.eval_int sign c d)
      | Max -> (
        match sign with
        | Plus ->
            Linear (c, SymLinear.singleton_one s)
        | Minus ->
            Linear (c, SymLinear.singleton_minus_one s) )
    else MinMax (c, sign, m, d, s)


  let int_ub_of_minmax = function
    | MinMax (c, Plus, Min, d, _) ->
        Some (c + d)
    | MinMax (c, Minus, Max, d, s) when Symb.Symbol.is_unsigned s ->
        Some (min c (c - d))
    | MinMax (c, Minus, Max, d, _) ->
        Some (c - d)
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let int_lb_of_minmax = function
    | MinMax (c, Plus, Max, d, s) when Symb.Symbol.is_unsigned s ->
        Some (max c (c + d))
    | MinMax (c, Plus, Max, d, _) ->
        Some (c + d)
    | MinMax (c, Minus, Min, d, _) ->
        Some (c - d)
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let int_of_minmax = function
    | Symb.BoundEnd.LowerBound ->
        int_lb_of_minmax
    | Symb.BoundEnd.UpperBound ->
        int_ub_of_minmax


  let int_lb = function
    | MInf ->
        None
    | PInf ->
        assert false
    | MinMax _ as b ->
        int_lb_of_minmax b
    | Linear (c, se) ->
        SymLinear.int_lb se |> Option.map ~f:(( + ) c)


  let int_ub = function
    | MInf ->
        assert false
    | PInf ->
        None
    | MinMax _ as b ->
        int_ub_of_minmax b
    | Linear (c, se) ->
        SymLinear.int_ub se |> Option.map ~f:(( + ) c)


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
    match (int_ub_of_minmax x, int_lb_of_minmax y) with Some n, Some m -> n <= m | _, _ -> false


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
    | MinMax (c1, sign1, m1, d1, x1), MinMax (c2, sign2, m2, d2, x2)
      when Sign.equal sign1 sign2 && MinMax.equal m1 m2 ->
        c1 <= c2 && Int.equal d1 d2 && Symb.Symbol.equal x1 x2
    | MinMax _, MinMax _ when le_minmax_by_int x y ->
        true
    | MinMax (c1, Plus, Min, _, x1), MinMax (c2, Plus, Max, _, x2)
    | MinMax (c1, Minus, Max, _, x1), MinMax (c2, Minus, Min, _, x2) ->
        c1 <= c2 && Symb.Symbol.equal x1 x2
    | MinMax _, Linear (c, se) ->
        (SymLinear.is_ge_zero se && le_opt1 Int.( <= ) (int_ub_of_minmax x) c)
        || le_opt1 le (linear_ub_of_minmax x) y
    | Linear (c, se), MinMax _ ->
        (SymLinear.is_le_zero se && le_opt2 Int.( <= ) c (int_lb_of_minmax y))
        || le_opt2 le x (linear_lb_of_minmax y)
    | _, _ ->
        false


  let lt : t -> t -> bool =
   fun x y ->
    match (x, y) with
    | MInf, Linear _ | MInf, MinMax _ | MInf, PInf | Linear _, PInf | MinMax _, PInf ->
        true
    | Linear (c, x), _ ->
        le (Linear (c + 1, x)) y
    | MinMax (c, sign, min_max, d, x), _ ->
        le (mk_MinMax (c + 1, sign, min_max, d, x)) y
    | _, _ ->
        false


  let gt : t -> t -> bool = fun x y -> lt y x

  let eq : t -> t -> bool = fun x y -> le x y && le y x

  let xcompare = PartialOrder.of_le ~le

  let remove_max_int : t -> t =
   fun x ->
    match x with
    | MinMax (c, Plus, Max, _, s) ->
        Linear (c, SymLinear.singleton_one s)
    | MinMax (c, Minus, Min, _, s) ->
        Linear (c, SymLinear.singleton_minus_one s)
    | _ ->
        x


  let rec lb : default:t -> t -> t -> t =
   fun ~default x y ->
    if le x y then x
    else if le y x then y
    else
      match (x, y) with
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_one_symbol x2 ->
          mk_MinMax (c2, Plus, Min, c1 - c2, SymLinear.get_one_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_one_symbol x1 && SymLinear.is_zero x2 ->
          mk_MinMax (c1, Plus, Min, c2 - c1, SymLinear.get_one_symbol x1)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2
        ->
          mk_MinMax (c2, Minus, Max, c2 - c1, SymLinear.get_mone_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2
        ->
          mk_MinMax (c1, Minus, Max, c1 - c2, SymLinear.get_mone_symbol x1)
      | MinMax (c1, Plus, Min, d1, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, Plus, Min, d1, s)
        when SymLinear.is_zero se ->
          mk_MinMax (c1, Plus, Min, min d1 (c2 - c1), s)
      | MinMax (c1, Plus, Max, _, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, Plus, Max, _, s)
        when SymLinear.is_zero se ->
          mk_MinMax (c1, Plus, Min, c2 - c1, s)
      | MinMax (c1, Minus, Min, _, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, Minus, Min, _, s)
        when SymLinear.is_zero se ->
          mk_MinMax (c1, Minus, Max, c1 - c2, s)
      | MinMax (c1, Minus, Max, d1, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, Minus, Max, d1, s)
        when SymLinear.is_zero se ->
          mk_MinMax (c1, Minus, Max, max d1 (c1 - c2), s)
      | MinMax (_, Plus, Min, _, _), MinMax (_, Plus, Max, _, _)
      | MinMax (_, Plus, Min, _, _), MinMax (_, Minus, Min, _, _)
      | MinMax (_, Minus, Max, _, _), MinMax (_, Plus, Max, _, _)
      | MinMax (_, Minus, Max, _, _), MinMax (_, Minus, Min, _, _) ->
          lb ~default x (remove_max_int y)
      | MinMax (_, Plus, Max, _, _), MinMax (_, Plus, Min, _, _)
      | MinMax (_, Minus, Min, _, _), MinMax (_, Plus, Min, _, _)
      | MinMax (_, Plus, Max, _, _), MinMax (_, Minus, Max, _, _)
      | MinMax (_, Minus, Min, _, _), MinMax (_, Minus, Max, _, _) ->
          lb ~default (remove_max_int x) y
      | MinMax (c1, Plus, Max, d1, _), MinMax (c2, Plus, Max, d2, _) ->
          Linear (min (c1 + d1) (c2 + d2), SymLinear.zero)
      | _, _ ->
          default


  (** underapproximation of min b1 b2 *)
  let min_l b1 b2 = lb ~default:MInf b1 b2

  (** overapproximation of min b1 b2 *)
  let min_u b1 b2 =
    lb
      ~default:
        (* When the result is not representable, our best effort is to return the first argument. Any other deterministic heuristics would work too. *)
        b1 b1 b2


  let ub : default:t -> t -> t -> t =
   fun ~default x y ->
    if le x y then y
    else if le y x then x
    else
      match (x, y) with
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_one_symbol x2 ->
          mk_MinMax (c2, Plus, Max, c1 - c2, SymLinear.get_one_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_one_symbol x1 && SymLinear.is_zero x2 ->
          mk_MinMax (c1, Plus, Max, c2 - c1, SymLinear.get_one_symbol x1)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2
        ->
          mk_MinMax (c2, Minus, Min, c2 - c1, SymLinear.get_mone_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2
        ->
          mk_MinMax (c1, Minus, Min, c1 - c2, SymLinear.get_mone_symbol x1)
      | _, _ ->
          default


  let max_u : t -> t -> t = ub ~default:PInf

  let widen_l : t -> t -> t =
   fun x y ->
    match (x, y) with
    | PInf, _ | _, PInf ->
        L.(die InternalError) "Lower bound cannot be +oo."
    | MinMax (n1, Plus, Max, _, s1), Linear (n2, s2)
      when Int.equal n1 n2 && SymLinear.is_one_symbol_of s1 s2 ->
        y
    | MinMax (n1, Minus, Min, _, s1), Linear (n2, s2)
      when Int.equal n1 n2 && SymLinear.is_mone_symbol_of s1 s2 ->
        y
    | _ ->
        if le x y then x else MInf


  let widen_u : t -> t -> t =
   fun x y ->
    match (x, y) with
    | MInf, _ | _, MInf ->
        L.(die InternalError) "Upper bound cannot be -oo."
    | MinMax (n1, Plus, Min, _, s1), Linear (n2, s2)
      when Int.equal n1 n2 && SymLinear.is_one_symbol_of s1 s2 ->
        y
    | MinMax (n1, Minus, Max, _, s1), Linear (n2, s2)
      when Int.equal n1 n2 && SymLinear.is_mone_symbol_of s1 s2 ->
        y
    | _ ->
        if le y x then x else PInf


  let zero : t = Linear (0, SymLinear.zero)

  let one : t = Linear (1, SymLinear.zero)

  let mone : t = Linear (-1, SymLinear.zero)

  let is_some_const : int -> t -> bool =
   fun c x -> match x with Linear (c', y) -> Int.equal c c' && SymLinear.is_zero y | _ -> false


  let is_zero : t -> bool = is_some_const 0

  let is_const : t -> int option =
   fun x -> match x with Linear (c, y) when SymLinear.is_zero y -> Some c | _ -> None


  let plus_common : f:(t -> t -> t) -> t -> t -> t =
   fun ~f x y ->
    match (x, y) with
    | _, _ when is_zero x ->
        y
    | _, _ when is_zero y ->
        x
    | Linear (c1, x1), Linear (c2, x2) ->
        Linear (c1 + c2, SymLinear.plus x1 x2)
    | MinMax (c1, sign, min_max, d1, x1), Linear (c2, x2)
    | Linear (c2, x2), MinMax (c1, sign, min_max, d1, x1)
      when SymLinear.is_zero x2 ->
        mk_MinMax (c1 + c2, sign, min_max, d1, x1)
    | _ ->
        f x y


  let plus_l : t -> t -> t =
    plus_common ~f:(fun x y ->
        match (x, y) with
        | MinMax (c1, Plus, Max, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Plus, Max, d1, _) ->
            Linear (c1 + d1 + c2, x2)
        | MinMax (c1, Minus, Min, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Minus, Min, d1, _) ->
            Linear (c1 - d1 + c2, x2)
        | _, _ ->
            MInf )


  let plus_u : t -> t -> t =
    plus_common ~f:(fun x y ->
        match (x, y) with
        | MinMax (c1, Plus, Min, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Plus, Min, d1, _) ->
            Linear (c1 + d1 + c2, x2)
        | MinMax (c1, Minus, Max, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Minus, Max, d1, _) ->
            Linear (c1 - d1 + c2, x2)
        | _, _ ->
            PInf )


  let plus = function Symb.BoundEnd.LowerBound -> plus_l | Symb.BoundEnd.UpperBound -> plus_u

  let mult_const : Symb.BoundEnd.t -> NonZeroInt.t -> t -> t =
   fun bound_end n x ->
    match x with
    | MInf ->
        if NonZeroInt.is_positive n then MInf else PInf
    | PInf ->
        if NonZeroInt.is_positive n then PInf else MInf
    | Linear (c, x') ->
        Linear (c * (n :> int), SymLinear.mult_const n x')
    | MinMax _ -> (
        let int_bound =
          let bound_end' =
            if NonZeroInt.is_positive n then bound_end else Symb.BoundEnd.neg bound_end
          in
          int_of_minmax bound_end' x
        in
        match int_bound with Some i -> of_int (i * (n :> int)) | None -> of_bound_end bound_end )


  let mult_const_l = mult_const Symb.BoundEnd.LowerBound

  let mult_const_u = mult_const Symb.BoundEnd.UpperBound

  let neg : t -> t = function
    | MInf ->
        PInf
    | PInf ->
        MInf
    | Linear (c, x) ->
        Linear (-c, SymLinear.neg x)
    | MinMax (c, sign, min_max, d, x) ->
        mk_MinMax (-c, Sign.neg sign, min_max, d, x)


  let div_const : t -> NonZeroInt.t -> t option =
   fun x n ->
    match x with
    | MInf ->
        Some (if NonZeroInt.is_positive n then MInf else PInf)
    | PInf ->
        Some (if NonZeroInt.is_positive n then PInf else MInf)
    | Linear (c, x') when NonZeroInt.is_multiple c n -> (
      match SymLinear.exact_div_const_exn x' n with
      | x'' ->
          Some (Linear (c / (n :> int), x''))
      | exception NonZeroInt.DivisionNotExact ->
          None )
    | _ ->
        None


  let get_symbols : t -> Symb.Symbol.t list = function
    | MInf | PInf ->
        []
    | Linear (_, se) ->
        SymLinear.get_symbols se
    | MinMax (_, _, _, _, s) ->
        [s]


  let are_similar b1 b2 =
    match (b1, b2) with
    | MInf, MInf ->
        true
    | PInf, PInf ->
        true
    | (Linear _ | MinMax _), (Linear _ | MinMax _) ->
        true
    | _ ->
        false


  let is_not_infty : t -> bool = function MInf | PInf -> false | _ -> true

  let lift1 : (t -> t) -> t bottom_lifted -> t bottom_lifted =
   fun f x -> match x with Bottom -> Bottom | NonBottom x -> NonBottom (f x)


  let lift2 : (t -> t -> t) -> t bottom_lifted -> t bottom_lifted -> t bottom_lifted =
   fun f x y ->
    match (x, y) with
    | Bottom, _ | _, Bottom ->
        Bottom
    | NonBottom x, NonBottom y ->
        NonBottom (f x y)


  (** Substitutes ALL symbols in [x] with respect to [map]. Throws [Symbol_not_found] if a symbol in [x] can't be found in [map]. Under/over-Approximate as good as possible according to [subst_pos]. *)
  let subst_exn :
      subst_pos:Symb.BoundEnd.t -> t -> t bottom_lifted Symb.SymbolMap.t -> t bottom_lifted =
   fun ~subst_pos x map ->
    let get_exn s =
      match Symb.SymbolMap.find s map with
      | NonBottom x when Symb.Symbol.is_unsigned s ->
          NonBottom (ub ~default:x zero x)
      | x ->
          x
    in
    let get_mult_const s coeff =
      try
        if NonZeroInt.is_one coeff then get_exn s
        else if NonZeroInt.is_minus_one coeff then get_exn s |> lift1 neg
        else
          match Symb.SymbolMap.find s map with
          | Bottom ->
              Bottom
          | NonBottom x ->
              let x = mult_const subst_pos coeff x in
              if Symb.Symbol.is_unsigned s then NonBottom (ub ~default:x zero x) else NonBottom x
      with Caml.Not_found -> (
        (* For unsigned symbols, we can over/under-approximate with zero depending on [subst_pos] and the sign of the coefficient. *)
        match (Symb.Symbol.is_unsigned s, subst_pos, NonZeroInt.is_positive coeff) with
        | true, Symb.BoundEnd.LowerBound, true | true, Symb.BoundEnd.UpperBound, false ->
            NonBottom zero
        | _ ->
            raise (Symbol_not_found s) )
    in
    match x with
    | MInf | PInf ->
        NonBottom x
    | Linear (c, se) ->
        SymLinear.fold se
          ~init:(NonBottom (of_int c))
          ~f:(fun acc s coeff -> lift2 (plus subst_pos) acc (get_mult_const s coeff))
    | MinMax (c, sign, min_max, d, s) -> (
      match get_exn s with
      | Bottom ->
          Bottom
      | exception Caml.Not_found -> (
        match int_of_minmax subst_pos x with
        | Some i ->
            NonBottom (of_int i)
        | None ->
            raise (Symbol_not_found s) )
      | NonBottom x' ->
          let res =
            match (sign, min_max, x') with
            | Plus, Min, MInf | Minus, Max, PInf ->
                MInf
            | Plus, Max, PInf | Minus, Min, MInf ->
                PInf
            | sign, Min, PInf | sign, Max, MInf ->
                of_int (Sign.eval_int sign c d)
            | _, _, Linear (c2, se) -> (
                if SymLinear.is_zero se then
                  of_int (Sign.eval_int sign c (MinMax.eval_int min_max d c2))
                else if SymLinear.is_one_symbol se then
                  mk_MinMax
                    (Sign.eval_int sign c c2, sign, min_max, d - c2, SymLinear.get_one_symbol se)
                else if SymLinear.is_mone_symbol se then
                  mk_MinMax
                    ( Sign.eval_int sign c c2
                    , Sign.neg sign
                    , MinMax.neg min_max
                    , c2 - d
                    , SymLinear.get_mone_symbol se )
                else
                  match int_of_minmax subst_pos x with
                  | Some i ->
                      of_int i
                  | None ->
                      of_bound_end subst_pos )
            | _, _, MinMax (c2, sign2, min_max2, d2, s2) -> (
              match (min_max, sign2, min_max2) with
              | Min, Plus, Min | Max, Plus, Max ->
                  let c' = Sign.eval_int sign c c2 in
                  let d' = MinMax.eval_int min_max (d - c2) d2 in
                  mk_MinMax (c', sign, min_max, d', s2)
              | Min, Minus, Max | Max, Minus, Min ->
                  let c' = Sign.eval_int sign c c2 in
                  let d' = MinMax.eval_int min_max2 (c2 - d) d2 in
                  mk_MinMax (c', Sign.neg sign, min_max2, d', s2)
              | _ ->
                  let bound_end =
                    match sign with Plus -> subst_pos | Minus -> Symb.BoundEnd.neg subst_pos
                  in
                  of_int
                    (Sign.eval_int sign c
                       (MinMax.eval_int min_max d
                          (int_of_minmax bound_end x' |> Option.value ~default:d))) )
          in
          NonBottom res )


  let subst_lb_exn x map = subst_exn ~subst_pos:Symb.BoundEnd.LowerBound x map

  let subst_ub_exn x map = subst_exn ~subst_pos:Symb.BoundEnd.UpperBound x map

  let simplify_bound_ends_from_paths x =
    match x with
    | MInf | PInf | MinMax _ ->
        x
    | Linear (c, se) ->
        let se' = SymLinear.simplify_bound_ends_from_paths se in
        if phys_equal se se' then x else Linear (c, se')


  let is_same_symbol b1 b2 =
    match (b1, b2) with
    | Linear (0, se1), Linear (0, se2) ->
        SymLinear.is_same_symbol se1 se2
    | _ ->
        None
end

type ('c, 's) valclass = Constant of 'c | Symbolic of 's | ValTop

(** A NonNegativeBound is a Bound that is either non-negative or symbolic but will be evaluated to a non-negative value once instantiated *)
module NonNegativeBound = struct
  type t = Bound.t [@@deriving compare]

  let zero = Bound.zero

  let of_bound b = if Bound.le b Bound.zero then Bound.zero else b

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
          Constant (NonNegativeInt.of_int_exn c) )
end
