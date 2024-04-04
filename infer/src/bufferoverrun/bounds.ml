(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

type sign = Plus | Minus [@@deriving compare, equal]

module Sign = struct
  type t = sign [@@deriving compare, equal]

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

module SymLinear = struct
  module M = Symb.SymbolMap

  (** Map from symbols to integer coefficients. [{ x -> 2, y -> 5 }] represents the value
      [2 * x + 5 * y] *)
  type t = NonZeroInt.t M.t [@@deriving compare, equal]

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
      Z.(equal v1 v2) || (Symb.Symbol.is_unsigned s && Z.leq v1 v2)
    in
    M.for_all2 ~f:le_one_pair x y


  let pp1 : markup:bool -> is_beginning:bool -> F.formatter -> Symb.Symbol.t -> NonZeroInt.t -> unit
      =
   fun ~markup ~is_beginning f s c ->
    let c = (c :> Z.t) in
    let c =
      if is_beginning then c
      else if Z.gt c Z.zero then (
        F.pp_print_string f " + " ;
        c )
      else (
        F.pp_print_string f " - " ;
        Z.neg c )
    in
    if Z.(equal c one) then (Symb.Symbol.pp_mark ~markup) f s
    else if Z.(equal c minus_one) then F.fprintf f "-%a" (Symb.Symbol.pp_mark ~markup) s
    else F.fprintf f "%a%s%a" Z.pp_print c SpecialChars.dot_operator (Symb.Symbol.pp_mark ~markup) s


  let pp : markup:bool -> is_beginning:bool -> F.formatter -> t -> unit =
   fun ~markup ~is_beginning f x ->
    if M.is_empty x then (if is_beginning then F.pp_print_string f "0")
    else
      ( M.fold
          (fun s c is_beginning ->
            pp1 ~markup ~is_beginning f s c ;
            false )
          x is_beginning
        : bool )
      |> ignore


  let zero : t = M.empty

  let is_zero : t -> bool = M.is_empty

  let neg : t -> t = fun x -> M.map NonZeroInt.( ~- ) x

  let remove_positive_length_symbol : t -> t =
    M.filter (fun symb coeff ->
        let path = Symb.Symbol.path symb in
        not (NonZeroInt.is_positive coeff && Symb.SymbolPath.is_length path) )


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


  let is_one_symbol_of_common get_symbol_opt ?(weak = false) s x =
    Option.exists (get_symbol_opt x) ~f:(fun s' ->
        (if weak then Symb.Symbol.paths_equal else Symb.Symbol.equal) s s' )


  let is_one_symbol_of : ?weak:bool -> Symb.Symbol.t -> t -> bool =
    is_one_symbol_of_common get_one_symbol_opt


  let is_mone_symbol_of : ?weak:bool -> Symb.Symbol.t -> t -> bool =
    is_one_symbol_of_common get_mone_symbol_opt


  let is_signed_one_symbol_of : ?weak:bool -> Sign.t -> Symb.Symbol.t -> t -> bool =
   fun ?weak sign s x ->
    match sign with Plus -> is_one_symbol_of ?weak s x | Minus -> is_mone_symbol_of ?weak s x


  let get_symbols : t -> Symb.SymbolSet.t =
   fun x -> M.fold (fun symbol _coeff acc -> Symb.SymbolSet.add symbol acc) x Symb.SymbolSet.empty


  (* we can give integer bounds (obviously 0) only when all symbols are unsigned *)

  let big_int_lb x = if is_ge_zero x then Some Z.zero else None

  let big_int_ub x = if is_le_zero x then Some Z.zero else None

  (** When two following symbols are from the same path, simplify what would lead to a zero sum.
      E.g. 2 * x.lb - x.ub = x.lb *)
  let simplify_bound_ends_from_paths : t -> t =
   fun x ->
    let f (prev_opt, to_add) symb coeff =
      match prev_opt with
      | Some (prev_coeff, prev_symb)
        when Symb.Symbol.paths_equal prev_symb symb
             && Bool.(NonZeroInt.is_positive coeff <> NonZeroInt.is_positive prev_coeff) ->
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


  let get_same_one_symbol x1 x2 =
    match (get_one_symbol_opt x1, get_one_symbol_opt x2) with
    | Some s1, Some s2 when Symb.Symbol.paths_equal s1 s2 ->
        Some (Symb.Symbol.path s1)
    | _ ->
        None


  let exists_str ~f x = M.exists (fun k _ -> Symb.Symbol.exists_str ~f k) x
end

module Bound = struct
  type min_max = Min | Max [@@deriving compare, equal]

  module MinMax = struct
    type t = min_max [@@deriving compare, equal]

    let neg = function Min -> Max | Max -> Min

    let eval_big_int x i1 i2 = match x with Min -> Z.min i1 i2 | Max -> Z.max i1 i2

    let pp : F.formatter -> t -> unit =
     fun fmt -> function Min -> F.pp_print_string fmt "min" | Max -> F.pp_print_string fmt "max"
  end

  type t =
    | MInf  (** -oo *)
    | Linear of Z.t * SymLinear.t  (** [Linear (c, se)] represents [c+se] where [se] is Σ(c⋅x). *)
    | MinMax of Z.t * Sign.t * MinMax.t * Z.t * Symb.Symbol.t
        (** [MinMax] represents a bound of "int [+|-] [min|max](int, symbol)" format. For example,
            [MinMax (1, Minus, Max, 2, s)] represents [1-max(2,s)]. *)
    | MinMaxB of MinMax.t * t * t  (** [MinMaxB] represents a min/max of two bounds. *)
    | MultB of Z.t * t * t
        (** [MultB] represents a multiplication of two bounds. For example, [MultB (1, x, y)]
            represents [1 + x × y]. *)
    | PInf  (** +oo *)
  [@@deriving compare, equal]

  type eval_sym = t Symb.Symbol.eval

  let mask_min_max_constant b =
    match b with
    | Linear (_c, x) ->
        Linear (Z.zero, x)
    | MinMax (_c, Plus, _m, _d, x) ->
        Linear (Z.zero, SymLinear.singleton_one x)
    | MinMax (c, Minus, _m, _d, x) ->
        Linear (c, SymLinear.singleton_minus_one x)
    | _ ->
        b


  let rec pp_mark : markup:bool -> F.formatter -> t -> unit =
    let pp_c f c =
      if not Z.(equal c zero) then
        if Z.gt c Z.zero then F.fprintf f " + %a" Z.pp_print c
        else F.fprintf f " - %a" Z.pp_print (Z.neg c)
    in
    fun ~markup f -> function
      | MInf ->
          F.pp_print_string f "-oo"
      | PInf ->
          F.pp_print_string f "+oo"
      | Linear (c, x) ->
          if SymLinear.is_zero x then Z.pp_print f c
          else (
            SymLinear.pp ~markup ~is_beginning:true f x ;
            pp_c f c )
      | MinMax (c, sign, m, d, x) ->
          if Z.(equal c zero) then (Sign.pp ~need_plus:false) f sign
          else F.fprintf f "%a%a" Z.pp_print c (Sign.pp ~need_plus:true) sign ;
          F.fprintf f "%a(%a, %a)" MinMax.pp m Z.pp_print d (Symb.Symbol.pp_mark ~markup) x
      | MinMaxB (m, x, y) ->
          F.fprintf f "%a(%a, %a)" MinMax.pp m (pp_mark ~markup) x (pp_mark ~markup) y
      | MultB (c, x, y) ->
          F.fprintf f "%a%s%a%a" (pp_mark ~markup) x SpecialChars.multiplication_sign
            (pp_mark ~markup) y pp_c c


  let pp = pp_mark ~markup:false

  let of_bound_end = function Symb.BoundEnd.LowerBound -> MInf | Symb.BoundEnd.UpperBound -> PInf

  let of_big_int : Z.t -> t = fun n -> Linear (n, SymLinear.empty)

  let of_int : int -> t = fun n -> of_big_int (Z.of_int n)

  let minf = MInf

  let mone = of_big_int Z.minus_one

  let z255 = of_int 255

  let zero = of_big_int Z.zero

  let one = of_big_int Z.one

  let pinf = PInf

  let is_some_const : Z.t -> t -> bool =
   fun c x -> match x with Linear (c', y) -> Z.equal c c' && SymLinear.is_zero y | _ -> false


  let is_zero : t -> bool = is_some_const Z.zero

  let is_infty : t -> bool = function MInf | PInf -> true | _ -> false

  let is_not_infty : t -> bool = function MInf | PInf -> false | _ -> true

  let is_minf = function MInf -> true | _ -> false

  let is_pinf = function PInf -> true | _ -> false

  let of_sym : SymLinear.t -> t = fun s -> Linear (Z.zero, s)

  let of_path path_of_partial make_symbol ~unsigned ?non_int partial =
    let s = make_symbol ~unsigned ?non_int (path_of_partial partial) in
    of_sym (SymLinear.singleton_one s)


  let of_normal_path = of_path Symb.SymbolPath.normal

  let of_offset_path ~is_void =
    of_path (Symb.SymbolPath.offset ~is_void) ~unsigned:false ~non_int:false


  let of_length_path ~is_void =
    of_path (Symb.SymbolPath.length ~is_void) ~unsigned:true ~non_int:false


  let of_modeled_path = of_path Symb.SymbolPath.modeled ~unsigned:true ~non_int:false

  let is_path_of ~f = function
    | Linear (n, se) when Z.(equal n zero) ->
        Option.exists (SymLinear.get_one_symbol_opt se) ~f:(fun s -> f (Symb.Symbol.path s))
    | _ ->
        false


  let is_offset_path_of path =
    is_path_of ~f:(function
      | Symb.SymbolPath.Offset {p} ->
          Symb.SymbolPath.equal_partial p path
      | _ ->
          false )


  let is_length_path_of path =
    is_path_of ~f:(function
      | Symb.SymbolPath.Length {p} ->
          Symb.SymbolPath.equal_partial p path
      | _ ->
          false )


  let rec is_symbolic : t -> bool = function
    | MInf | PInf ->
        false
    | Linear (_, se) ->
        not (SymLinear.is_empty se)
    | MinMax _ ->
        true
    | MinMaxB (_, x, y) | MultB (_, x, y) ->
        is_symbolic x || is_symbolic y


  let is_incr_of path = function
    | Linear (i, se) ->
        Z.(equal i one)
        && Option.value_map (SymLinear.get_one_symbol_opt se) ~default:false ~f:(fun sym ->
               Symb.SymbolPath.equal (Symb.SymbolPath.normal path) (Symb.Symbol.path sym) )
    | _ ->
        false


  let mk_MinMax (c, sign, m, d, s) =
    if Symb.Symbol.is_unsigned s && Z.(leq d zero) then
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


  let mk_MultB (n, x, y) =
    (* NOTE: We have some simplication opportunities here. *)
    MultB (n, x, y)


  let big_int_ub_of_minmax = function
    | MinMax (c, Plus, Min, d, _) ->
        Some Z.(c + d)
    | MinMax (c, Minus, Max, d, _) ->
        Some Z.(c - d)
    | MinMax (c, Minus, Min, _, s) when Symb.Symbol.is_unsigned s ->
        Some c
    | MinMax _ ->
        None
    | MinMaxB _ | MultB _ | MInf | PInf | Linear _ ->
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
    | MinMaxB _ | MultB _ | MInf | PInf | Linear _ ->
        assert false


  let big_int_of_minmax = function
    | Symb.BoundEnd.LowerBound ->
        big_int_lb_of_minmax
    | Symb.BoundEnd.UpperBound ->
        big_int_ub_of_minmax


  let rec big_int_lb = function
    | MInf | MultB _ ->
        None
    | PInf ->
        assert false
    | MinMax _ as b ->
        big_int_lb_of_minmax b
    | Linear (c, se) ->
        SymLinear.big_int_lb se |> Option.map ~f:(Z.( + ) c)
    | MinMaxB (m, x, y) ->
        Option.map2 (big_int_lb x) (big_int_lb y) ~f:(MinMax.eval_big_int m)


  let rec big_int_ub = function
    | MInf ->
        assert false
    | PInf | MultB _ ->
        None
    | MinMax _ as b ->
        big_int_ub_of_minmax b
    | Linear (c, se) ->
        SymLinear.big_int_ub se |> Option.map ~f:(Z.( + ) c)
    | MinMaxB (m, x, y) ->
        Option.map2 (big_int_ub x) (big_int_ub y) ~f:(MinMax.eval_big_int m)


  let linear_ub_of_minmax = function
    | MinMax (c, Plus, Min, _, x) ->
        Some (Linear (c, SymLinear.singleton_one x))
    | MinMax (c, Minus, Max, _, x) ->
        Some (Linear (c, SymLinear.singleton_minus_one x))
    | MinMax _ ->
        None
    | MinMaxB _ | MultB _ | MInf | PInf | Linear _ ->
        assert false


  let linear_lb_of_minmax = function
    | MinMax (c, Plus, Max, _, x) ->
        Some (Linear (c, SymLinear.singleton_one x))
    | MinMax (c, Minus, Min, _, x) ->
        Some (Linear (c, SymLinear.singleton_minus_one x))
    | MinMax _ ->
        None
    | MinMaxB _ | MultB _ | MInf | PInf | Linear _ ->
        assert false


  let le_minmax_by_int x y =
    match (big_int_ub_of_minmax x, big_int_lb_of_minmax y) with
    | Some n, Some m ->
        Z.leq n m
    | _, _ ->
        false


  let le_opt1 le opt_n m = Option.exists opt_n ~f:(fun n -> le n m)

  let le_opt2 le n opt_m = Option.exists opt_m ~f:(fun m -> le n m)

  let rec le : t -> t -> bool =
   fun x y ->
    match (x, y) with
    | MInf, _ | _, PInf ->
        true
    | _, MInf | PInf, _ ->
        false
    | MultB (xc, x1, x2), MultB (yc, y1, y2) ->
        (* NOTE: We define the order for only straightforward cases. *)
        Z.leq xc yc && equal x1 y1 && equal x2 y2
    | MultB _, _ | _, MultB _ ->
        false
    | Linear (c0, x0), Linear (c1, x1) ->
        Z.leq c0 c1 && SymLinear.le x0 x1
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
    | MinMaxB (Max, x1, x2), y ->
        le x1 y && le x2 y
    | MinMaxB (Min, x1, x2), y ->
        le x1 y || le x2 y
    | x, MinMaxB (Max, y1, y2) ->
        le x y1 || le x y2
    | x, MinMaxB (Min, y1, y2) ->
        le x y1 && le x y2


  let rec lt : t -> t -> bool =
   fun x y ->
    match (x, y) with
    | MInf, Linear _ | MInf, MinMax _ | MInf, PInf | Linear _, PInf | MinMax _, PInf ->
        true
    | MultB (xc, x1, x2), MultB (yc, y1, y2) ->
        (* NOTE: We define the order for only straightforward cases. *)
        Z.lt xc yc && equal x1 y1 && equal x2 y2
    | MultB _, _ | _, MultB _ ->
        false
    | Linear (c, x), _ ->
        le (Linear (Z.succ c, x)) y
    | MinMax (c, sign, min_max, d, x), _ ->
        le (mk_MinMax (Z.succ c, sign, min_max, d, x)) y
    | MinMaxB (Max, x1, x2), y ->
        lt x1 y && lt x2 y
    | MinMaxB (Min, x1, x2), y ->
        lt x1 y || lt x2 y
    | x, MinMaxB (Max, y1, y2) ->
        lt x y1 || lt x y2
    | x, MinMaxB (Min, y1, y2) ->
        lt x y1 && lt x y2
    | _, _ ->
        false


  let gt : t -> t -> bool = fun x y -> lt y x

  let eq : t -> t -> bool = fun x y -> le x y && le y x

  let mk_MinMaxB (m, x, y) =
    if le x y then match m with Min -> x | Max -> y
    else if le y x then match m with Min -> y | Max -> x
    else
      match (x, y) with
      | (Linear _ | MinMax _), (Linear _ | MinMax _) ->
          MinMaxB (m, x, y)
      | _, _ -> (
        match m with Min -> MInf | Max -> PInf )


  let of_minmax_bound_min x y = mk_MinMaxB (Min, x, y)

  let of_minmax_bound_max x y = mk_MinMaxB (Max, x, y)

  let xcompare = PartialOrder.of_le ~le

  let is_const : t -> bool = function Linear (_, se) -> SymLinear.is_zero se | _ -> false

  let rec neg : t -> t = function
    | MInf ->
        PInf
    | PInf ->
        MInf
    | Linear (c, x) as b ->
        if Z.(equal c zero) && SymLinear.is_zero x then b else Linear (Z.neg c, SymLinear.neg x)
    | MinMax (c, sign, min_max, d, x) ->
        mk_MinMax (Z.neg c, Sign.neg sign, min_max, d, x)
    | MinMaxB (m, x, y) ->
        mk_MinMaxB (MinMax.neg m, neg x, neg y)
    | MultB (c, x, y) ->
        mk_MultB (Z.neg c, neg x, y)


  let rec remove_positive_length_symbol b =
    match b with
    | MInf | PInf ->
        b
    | Linear (c, x) ->
        Linear (c, SymLinear.remove_positive_length_symbol x)
    | MinMax (c, sign, min_max, d, x) ->
        if Symb.Symbol.is_length x then
          Linear (Sign.eval_big_int sign c (MinMax.eval_big_int min_max d Z.zero), SymLinear.empty)
        else b
    | MinMaxB (m, x, y) ->
        mk_MinMaxB (m, remove_positive_length_symbol x, remove_positive_length_symbol y)
    | MultB (c, x, y) ->
        mk_MultB (c, remove_positive_length_symbol x, remove_positive_length_symbol y)


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
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2 ->
          mk_MinMax (c2, Minus, Max, Z.(c2 - c1), SymLinear.get_mone_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2 ->
          mk_MinMax (c1, Minus, Max, Z.(c1 - c2), SymLinear.get_mone_symbol x1)
      | MinMax (c1, (Plus as sign), (Min as minmax), _, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, (Plus as sign), (Min as minmax), _, s)
      | MinMax (c1, (Minus as sign), (Max as minmax), _, s), Linear (c2, se)
      | Linear (c2, se), MinMax (c1, (Minus as sign), (Max as minmax), _, s)
        when SymLinear.is_zero se ->
          let d = Sign.eval_neg_if_minus sign Z.(c2 - c1) in
          mk_MinMax (c1, sign, minmax, d, s)
      | MinMax (c1, Plus, Min, d1, s1), Linear (c2, s2)
      | Linear (c2, s2), MinMax (c1, Plus, Min, d1, s1)
        when SymLinear.is_one_symbol_of s1 s2 ->
          let c = Z.min c1 c2 in
          let d = Z.(c1 + d1) in
          mk_MinMax (c, Plus, Min, Z.(d - c), s1)
      | MinMax (c1, Minus, Max, d1, s1), Linear (c2, s2)
      | Linear (c2, s2), MinMax (c1, Minus, Max, d1, s1)
        when SymLinear.is_mone_symbol_of s1 s2 ->
          let c = Z.min c1 c2 in
          let d = Z.(c1 - d1) in
          mk_MinMax (c, Minus, Max, Z.(c - d), s1)
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
    let overapprox_min b1 b2 =
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
          | Linear (c1, x1), MinMax (c2, (Minus as sign), Max, d2, _)
          | Linear (c1, x1), MinMax (c2, (Plus as sign), Min, d2, _)
            when SymLinear.is_one_symbol x1 ->
              let d = Sign.eval_big_int sign c2 d2 in
              mk_MinMax (c1, Plus, Min, Z.(d - c1), SymLinear.get_one_symbol x1)
          | Linear (c1, x1), MinMax (c2, (Minus as sign), Max, d2, _)
          | Linear (c1, x1), MinMax (c2, (Plus as sign), Min, d2, _)
            when SymLinear.is_mone_symbol x1 ->
              let d = Sign.eval_big_int sign c2 d2 in
              mk_MinMax (c1, Minus, Max, Z.(c1 - d), SymLinear.get_mone_symbol x1)
          | _ ->
              (* When the result is not representable, our best effort is to return the first original argument. Any other deterministic heuristics would work too. *)
              original_b1 )
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


  module Thresholds : sig
    type bound = t

    type t

    val make_inc : Z.t list -> t

    val make_dec : Z.t list -> t

    val widen :
      cond:(threshold:bound -> bound -> bool) -> default:bound -> bound -> bound -> t -> bound
  end = struct
    type bound = t

    type t = bound list

    let default_thresholds = [Z.zero]

    let make ~compare thresholds =
      List.dedup_and_sort ~compare (default_thresholds @ thresholds) |> List.map ~f:of_big_int


    (* It makes a list of thresholds that will be applied with the increasing order. *)
    let make_inc = make ~compare:Z.compare

    (* It makes a list of thresholds that will be applied with the decreasing order. *)
    let make_dec = make ~compare:(fun x y -> -Z.compare x y)

    let rec widen ~cond ~default x y = function
      | [] ->
          default
      | threshold :: thresholds ->
          if cond ~threshold x && cond ~threshold y then threshold
          else widen ~default ~cond x y thresholds
  end

  let widen_l_thresholds : thresholds:Z.t list -> t -> t -> t =
   fun ~thresholds x y ->
    match (x, y) with
    | PInf, _ | _, PInf ->
        L.(die InternalError) "Lower bound cannot be +oo."
    | MinMax (n1, Plus, Max, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_one_symbol_of s1 s2 ->
        y
    | MinMax (n1, Minus, Min, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_mone_symbol_of s1 s2 ->
        y
    | Linear (n1, s1), MinMax (n2, (Plus as sign1), Min, n3, _)
    | Linear (n1, s1), MinMax (n2, (Minus as sign1), Max, n3, _)
      when Z.equal n1 (Sign.eval_big_int sign1 n2 n3) && SymLinear.is_empty s1 ->
        y
    | Linear (n1, s1), MinMax (n2, (Plus as sign1), Min, _, s2)
    | Linear (n1, s1), MinMax (n2, (Minus as sign1), Max, _, s2)
      when Z.equal n1 n2 && SymLinear.is_signed_one_symbol_of sign1 s2 s1 ->
        y
    | _ ->
        if le x y then x
        else
          let cond ~threshold x = le threshold x in
          Thresholds.widen ~cond ~default:MInf x y (Thresholds.make_dec thresholds)


  let widen_l : t -> t -> t = fun x y -> widen_l_thresholds ~thresholds:[] x y

  let widen_u_thresholds : thresholds:Z.t list -> t -> t -> t =
   fun ~thresholds x y ->
    match (x, y) with
    | MInf, _ | _, MInf ->
        L.(die InternalError) "Upper bound cannot be -oo."
    | MinMax (n1, Plus, Min, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_one_symbol_of s1 s2 ->
        y
    | MinMax (n1, Minus, Max, _, s1), Linear (n2, s2)
      when Z.equal n1 n2 && SymLinear.is_mone_symbol_of s1 s2 ->
        y
    | Linear (n1, s1), MinMax (n2, (Plus as sign1), Max, n3, _)
    | Linear (n1, s1), MinMax (n2, (Minus as sign1), Min, n3, _)
      when Z.equal n1 (Sign.eval_big_int sign1 n2 n3) && SymLinear.is_empty s1 ->
        y
    | Linear (n1, s1), MinMax (n2, (Plus as sign1), Max, _, s2)
    | Linear (n1, s1), MinMax (n2, (Minus as sign1), Min, _, s2)
      when Z.equal n1 n2 && SymLinear.is_signed_one_symbol_of sign1 s2 s1 ->
        y
    | _ ->
        if le y x then x
        else
          let cond ~threshold x = le x threshold in
          Thresholds.widen ~cond ~default:PInf x y (Thresholds.make_inc thresholds)


  let widen_u : t -> t -> t = fun x y -> widen_u_thresholds ~thresholds:[] x y

  let get_const : t -> Z.t option =
   fun x -> match x with Linear (c, y) when SymLinear.is_zero y -> Some c | _ -> None


  let rec plus_exact : weak:bool -> otherwise:(t -> t -> t) -> t -> t -> t =
   fun ~weak ~otherwise x y ->
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
      | MinMax (c1, sign, min_max, d, x1), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, sign, min_max, d, x1)
        when SymLinear.is_signed_one_symbol_of ~weak (Sign.neg sign) x1 x2 ->
          let c = Sign.eval_big_int sign Z.(c1 + c2) d in
          mk_MinMax (c, Sign.neg sign, MinMax.neg min_max, d, x1)
      | MinMaxB (m, x, y), z ->
          mk_MinMaxB (m, plus_exact ~weak ~otherwise x z, plus_exact ~weak ~otherwise y z)
      | (MultB (c, x1, x2), Linear (d, se) | Linear (d, se), MultB (c, x1, x2))
        when SymLinear.is_zero se ->
          mk_MultB (Z.add c d, x1, x2)
      | _ ->
          otherwise x y


  let plus_l : weak:bool -> t -> t -> t =
    plus_exact ~otherwise:(fun x y ->
        match (x, y) with
        | MinMax (c1, Plus, Max, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Plus, Max, d1, _) ->
            Linear (Z.(c1 + d1 + c2), x2)
        | MinMax (c1, Minus, Min, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Minus, Min, d1, _) ->
            Linear (Z.(c1 - d1 + c2), x2)
        | _, _ ->
            MInf )


  let plus_u : weak:bool -> t -> t -> t =
    plus_exact ~otherwise:(fun x y ->
        match (x, y) with
        | MinMax (c1, Plus, Min, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Plus, Min, d1, _) ->
            Linear (Z.(c1 + d1 + c2), x2)
        | MinMax (c1, Minus, Max, d1, _), Linear (c2, x2)
        | Linear (c2, x2), MinMax (c1, Minus, Max, d1, _) ->
            Linear (Z.(c1 - d1 + c2), x2)
        | _, _ ->
            PInf )


  let plus = function
    | Symb.BoundEnd.LowerBound ->
        plus_l ~weak:false
    | Symb.BoundEnd.UpperBound ->
        plus_u ~weak:false


  let rec mult_const : Symb.BoundEnd.t -> NonZeroInt.t -> t -> t =
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
      | MinMaxB (m, x, y) ->
          mk_MinMaxB (m, mult_const bound_end n x, mult_const bound_end n y)
      | MultB _ ->
          of_bound_end bound_end


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

  let rec get_symbols : t -> Symb.SymbolSet.t = function
    | MInf | PInf ->
        Symb.SymbolSet.empty
    | Linear (_, se) ->
        SymLinear.get_symbols se
    | MinMax (_, _, _, _, s) ->
        Symb.SymbolSet.singleton s
    | MinMaxB (_, x, y) | MultB (_, x, y) ->
        Symb.SymbolSet.union (get_symbols x) (get_symbols y)


  let has_void_ptr_symb x =
    Symb.SymbolSet.exists
      (fun s -> Symb.SymbolPath.is_void_ptr_path (Symb.Symbol.path s))
      (get_symbols x)


  let are_similar b1 b2 = Symb.SymbolSet.equal (get_symbols b1) (get_symbols b2)

  (** Substitutes ALL symbols in [x] with respect to [eval_sym]. Under/over-Approximate as good as
      possible according to [subst_pos]. *)
  let rec subst : subst_pos:Symb.BoundEnd.t -> t -> eval_sym -> t bottom_lifted =
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
        if Language.curr_language_is Java && Symb.Symbol.is_global s then
          NonBottom (of_sym (SymLinear.singleton_one s))
        else
          match eval_sym s bound_position with
          | NonBottom x when Symb.Symbol.is_unsigned s ->
              NonBottom (approx_max subst_pos x zero)
          | x ->
              x
      in
      let overapproximate_bound bound_position is_unsigned =
        (* Used when a symbol contained in a bound is imported as bot. This happens if something
           which is not initialized in the caller is weakly updated or constrained in the callee.*)
        match (is_unsigned, bound_position) with
        | true, Symb.BoundEnd.LowerBound ->
            (* For unsigned symbols, we can over-approximate lower bound with zero. *)
            NonBottom zero
        | false, Symb.BoundEnd.LowerBound ->
            NonBottom MInf
        | _, Symb.BoundEnd.UpperBound ->
            NonBottom PInf
      in
      let get_mult_const s coeff =
        let bound_position =
          if NonZeroInt.is_positive coeff then subst_pos else Symb.BoundEnd.neg subst_pos
        in
        if NonZeroInt.is_one coeff then get s bound_position
        else if NonZeroInt.is_minus_one coeff then get s bound_position |> lift1 neg
        else
          match eval_sym s bound_position with
          | Bottom ->
              overapproximate_bound bound_position (Symb.Symbol.is_unsigned s)
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
              (* Pass false as unsigned to overapproximate_bound because the bound doesn't
                 consist only of the symbol s so it cannot be overapproximated with 0 even
                 if s is signed and subst_pos is LowerBound. *)
              Option.value_map (big_int_of_minmax subst_pos x)
                ~default:(overapproximate_bound subst_pos false) ~f:(fun i ->
                  NonBottom (of_big_int i) )
          | NonBottom x' ->
              let res =
                match (sign, min_max, x') with
                | Plus, Min, (MInf | MinMaxB _ | MultB _) | Minus, Max, (PInf | MinMaxB _ | MultB _)
                  ->
                    MInf
                | Plus, Max, (PInf | MinMaxB _ | MultB _) | Minus, Min, (MInf | MinMaxB _ | MultB _)
                  ->
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
                              (big_int_of_minmax bound_end x' |> Option.value ~default:d) ) ) )
              in
              NonBottom res )
      | MinMaxB (m, x, y) ->
          subst2_merge ~subst_pos x y eval_sym ~f:(fun x y -> mk_MinMaxB (m, x, y))
      | MultB (c, x, y) when le zero x && le zero y ->
          subst2_merge ~subst_pos x y eval_sym ~f:(fun x y -> mk_MultB (c, x, y))
      | MultB (c, x, y) when le x zero && le y zero ->
          let subst_pos = Symb.BoundEnd.neg subst_pos in
          subst2_merge ~subst_pos x y eval_sym ~f:(fun x y -> mk_MultB (c, x, y))
      | MultB _ ->
          NonBottom (of_bound_end subst_pos)


  and subst2_merge ~subst_pos x y eval_sym ~f =
    match (subst ~subst_pos x eval_sym, subst ~subst_pos y eval_sym) with
    | Bottom, _ | _, Bottom ->
        Bottom
    | NonBottom x, NonBottom y ->
        NonBottom (f x y)


  let subst_lb x eval_sym = subst ~subst_pos:Symb.BoundEnd.LowerBound x eval_sym

  let subst_ub x eval_sym = subst ~subst_pos:Symb.BoundEnd.UpperBound x eval_sym

  (* When a positive bound is expected, min(1,x) can be simplified to 1. *)
  let simplify_min_one b =
    match b with
    | MinMax (c, Plus, Min, d, _x) when Z.(equal c zero) && Z.(equal d one) ->
        Linear (d, SymLinear.zero)
    | _ ->
        b


  let rec simplify_bound_ends_from_paths x =
    match x with
    | MInf | PInf | MinMax _ ->
        x
    | Linear (c, se) ->
        let se' = SymLinear.simplify_bound_ends_from_paths se in
        if phys_equal se se' then x else Linear (c, se')
    | MinMaxB (m, a, b) ->
        let a' = simplify_bound_ends_from_paths a in
        let b' = simplify_bound_ends_from_paths b in
        if phys_equal a a' && phys_equal b b' then x else mk_MinMaxB (m, a', b')
    | MultB (c, a, b) ->
        let a' = simplify_bound_ends_from_paths a in
        let b' = simplify_bound_ends_from_paths b in
        if phys_equal a a' && phys_equal b b' then x else mk_MultB (c, a', b')


  let simplify_minimum_length x =
    match x with
    | MultB _ | Linear _ | MInf | PInf | MinMaxB _ ->
        x
    | MinMax (c1, sign, Min, c2, symb) ->
        let path = Symb.Symbol.path symb in
        if Symb.SymbolPath.is_length path then
          let z = Sign.eval_big_int sign c1 (Z.min c2 Z.zero) in
          Linear (z, SymLinear.empty)
        else x
    | MinMax _ ->
        x


  let get_same_one_symbol b1 b2 =
    match (b1, b2) with
    | Linear (n1, se1), Linear (n2, se2) when Z.(equal n1 zero) && Z.(equal n2 zero) ->
        SymLinear.get_same_one_symbol se1 se2
    | _ ->
        None


  let is_same_one_symbol b1 b2 = Option.is_some (get_same_one_symbol b1 b2)

  let rec exists_str ~f = function
    | MInf | PInf ->
        false
    | Linear (_, s) ->
        SymLinear.exists_str ~f s
    | MinMax (_, _, _, _, s) ->
        Symb.Symbol.exists_str ~f s
    | MinMaxB (_, x, y) | MultB (_, x, y) ->
        exists_str ~f x || exists_str ~f y
end

type ('c, 's, 't) valclass = Constant of 'c | Symbolic of 's | ValTop of 't

module BoundTrace = struct
  type t =
    | Loop of Location.t
    | Call of {callee_pname: Procname.t; callee_trace: t; location: Location.t}
    | ModeledFunction of {pname: string; location: Location.t}
  [@@deriving compare]

  let rec length = function
    | Loop _ | ModeledFunction _ ->
        1
    | Call {callee_trace} ->
        1 + length callee_trace


  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let join x y = if length x <= length y then x else y

  let rec pp f = function
    | Loop loc ->
        F.fprintf f "Loop (%a)" Location.pp loc
    | ModeledFunction {pname; location} ->
        F.fprintf f "ModeledFunction `%s` (%a)" pname Location.pp location
    | Call {callee_pname; callee_trace; location} ->
        F.fprintf f "%a -> Call `%a` (%a)" pp callee_trace Procname.pp callee_pname Location.pp
          location


  let call ~callee_pname ~location callee_trace = Call {callee_pname; callee_trace; location}

  let rec make_err_trace_of_non_func_ptr ~depth trace =
    match trace with
    | Loop loop_head_loc ->
        [Errlog.make_trace_element depth loop_head_loc "Loop" []]
    | Call {callee_pname; location; callee_trace} ->
        let desc = F.asprintf "Call to %a" Procname.pp callee_pname in
        Errlog.make_trace_element depth location desc []
        :: make_err_trace_of_non_func_ptr ~depth:(depth + 1) callee_trace
    | ModeledFunction {pname; location} ->
        let desc = F.asprintf "Modeled call to %s" pname in
        [Errlog.make_trace_element depth location desc []]


  let make_err_trace ~depth trace = make_err_trace_of_non_func_ptr ~depth trace

  let of_loop location = Loop location
end

(** A NonNegativeBound is a Bound that is either non-negative or symbolic but will be evaluated to a
    non-negative value once instantiated *)
module NonNegativeBound = struct
  type t = Bound.t * BoundTrace.t [@@deriving compare]

  let leq ~lhs:(bound_lhs, _) ~rhs:(bound_rhs, _) = Bound.le bound_lhs bound_rhs

  let join (bound_x, trace_x) (bound_y, trace_y) =
    (Bound.overapprox_max bound_x bound_y, BoundTrace.join trace_x trace_y)


  let widen ~prev:(bound_prev, trace_prev) ~next:(bound_next, trace_next) ~num_iters:_ =
    (Bound.widen_u bound_prev bound_next, BoundTrace.join trace_prev trace_next)


  let make_err_trace (b, t) =
    let b = F.asprintf "{%a}" Bound.pp b in
    (b, BoundTrace.make_err_trace ~depth:0 t)


  let pp ~hum fmt (bound, t) =
    Bound.pp fmt bound ;
    if not hum then F.fprintf fmt ": %a" BoundTrace.pp t


  let mask_min_max_constant (b, bt) = (Bound.mask_min_max_constant b, bt)

  let zero loop_head_loc = (Bound.zero, BoundTrace.Loop loop_head_loc)

  let check_le_zero b = if Bound.le b Bound.zero then Bound.zero else b

  let of_bound ~trace b = (check_le_zero b, trace)

  let of_loop_bound loop_head_loc = of_bound ~trace:(BoundTrace.Loop loop_head_loc)

  let of_modeled_function pname location b =
    if Bound.lt b Bound.zero then (* we shouldn't have negative modeled bounds *)
      assert false
    else (b, BoundTrace.ModeledFunction {pname; location})


  let of_big_int ~trace c = (Bound.of_big_int c, trace)

  let int_lb (b, _) =
    Bound.big_int_lb b
    |> Option.bind ~f:NonNegativeInt.of_big_int
    |> Option.value ~default:NonNegativeInt.zero


  let int_ub (b, _) = Bound.big_int_ub b |> Option.map ~f:NonNegativeInt.of_big_int_exn

  let classify (b, trace) =
    match b with
    | Bound.PInf ->
        ValTop trace
    | Bound.MInf ->
        assert false
    | b -> (
      match Bound.get_const b with
      | None ->
          Symbolic (b, trace)
      | Some c ->
          Constant (NonNegativeInt.of_big_int_exn c) )


  let subst callee_pname location (b, callee_trace) map =
    match Bound.subst_ub b map with
    | Bottom ->
        Constant NonNegativeInt.zero
    | NonBottom b ->
        of_bound b ~trace:(BoundTrace.call ~callee_pname ~location callee_trace) |> classify


  let split_mult (b, trace) =
    match b with Bound.MultB (_, b1, b2) -> Some ((b1, trace), (b2, trace)) | _ -> None
end
