(*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging

exception Not_one_symbol

module Symbol = struct
  type t = {pname: Typ.Procname.t; id: int; unsigned: bool} [@@deriving compare]

  let eq = [%compare.equal : t]

  let make : unsigned:bool -> Typ.Procname.t -> int -> t =
    fun ~unsigned pname id -> {pname; id; unsigned}

  let pp : F.formatter -> t -> unit =
    fun fmt {pname; id; unsigned} ->
      let symbol_name = if unsigned then "u" else "s" in
      if Config.bo_debug <= 1 then F.fprintf fmt "%s$%d" symbol_name id
      else F.fprintf fmt "%s-%s$%d" (Typ.Procname.to_string pname) symbol_name id

  let is_unsigned : t -> bool = fun x -> x.unsigned
end

module SubstMap = Caml.Map.Make (Symbol)

module SymLinear = struct
  module M = struct
    include Caml.Map.Make (Symbol)

    let for_all2 : (key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool =
      fun cond x y ->
        let merge_function k x y = if cond k x y then None else raise Exit in
        match merge merge_function x y with _ -> true | exception Exit -> false
  end

  type t = int M.t [@@deriving compare]

  let empty : t = M.empty

  let is_empty : t -> bool = M.is_empty

  let add : Symbol.t -> int -> t -> t = M.add

  let cardinal : t -> int = M.cardinal

  let min_binding : t -> Symbol.t * int = M.min_binding

  let fold : (Symbol.t -> int -> 'b -> 'b) -> t -> 'b -> 'b = M.fold

  let mem : Symbol.t -> t -> bool = M.mem

  let initial : t = empty

  let singleton : Symbol.t -> int -> t = M.singleton

  let find : Symbol.t -> t -> int =
    fun s x ->
      try M.find s x
      with Not_found -> 0

  let is_le_zero : t -> bool =
    fun x -> M.for_all (fun s v -> Int.equal v 0 || Symbol.is_unsigned s && v <= 0) x

  let is_ge_zero : t -> bool =
    fun x -> M.for_all (fun s v -> Int.equal v 0 || Symbol.is_unsigned s && v >= 0) x

  let le : t -> t -> bool =
    fun x y ->
      let le_one_pair s v1_opt v2_opt =
        let v1, v2 = (Option.value v1_opt ~default:0, Option.value v2_opt ~default:0) in
        Int.equal v1 v2 || Symbol.is_unsigned s && v1 <= v2
      in
      M.for_all2 le_one_pair x y

  let make : unsigned:bool -> Typ.Procname.t -> int -> t =
    fun ~unsigned pname i -> M.add (Symbol.make ~unsigned pname i) 1 empty

  let eq : t -> t -> bool = fun x y -> le x y && le y x

  let pp1 : F.formatter -> Symbol.t * int -> unit =
    fun fmt (s, c) ->
      if Int.equal c 0 then ()
      else if Int.equal c 1 then F.fprintf fmt "%a" Symbol.pp s
      else if c < 0 then F.fprintf fmt "(%d)x%a" c Symbol.pp s
      else F.fprintf fmt "%dx%a" c Symbol.pp s

  let pp : F.formatter -> t -> unit =
    fun fmt x ->
      if M.is_empty x then F.fprintf fmt "empty"
      else
        let s1, c1 = M.min_binding x in
        pp1 fmt (s1, c1) ;
        M.iter (fun s c -> F.fprintf fmt " + %a" pp1 (s, c)) (M.remove s1 x)

  let zero : t = M.empty

  let is_zero : t -> bool = fun x -> M.for_all (fun _ v -> Int.equal v 0) x

  let is_mod_zero : t -> int -> bool =
    fun x n ->
      assert (n <> 0) ;
      M.for_all (fun _ v -> Int.equal (v mod n) 0) x

  let neg : t -> t = fun x -> M.map ( ~- )x

  (* Returns (Some n) only when n is not 0. *)
  let is_non_zero : int -> int option = fun n -> if Int.equal n 0 then None else Some n

  let plus : t -> t -> t =
    fun x y ->
      let plus' _ n_opt m_opt =
        match (n_opt, m_opt) with
        | None, None
         -> None
        | Some v, None | None, Some v
         -> is_non_zero v
        | Some n, Some m
         -> is_non_zero (n + m)
      in
      M.merge plus' x y

  let minus : t -> t -> t =
    fun x y ->
      let minus' _ n_opt m_opt =
        match (n_opt, m_opt) with
        | None, None
         -> None
        | Some v, None
         -> is_non_zero v
        | None, Some v
         -> is_non_zero (-v)
        | Some n, Some m
         -> is_non_zero (n - m)
      in
      M.merge minus' x y

  let mult_const : t -> int -> t = fun x n -> M.map (( * ) n) x

  let div_const : t -> int -> t = fun x n -> M.map (( / ) n) x

  (* Returns a symbol when the map contains only one symbol s with a
     given coefficient. *)
  let one_symbol_of_coeff : int -> t -> Symbol.t option =
    fun coeff x ->
      let x = M.filter (fun _ v -> v <> 0) x in
      if Int.equal (M.cardinal x) 1 then
        let k, v = M.min_binding x in
        if Int.equal v coeff then Some k else None
      else None

  let get_one_symbol_opt : t -> Symbol.t option = one_symbol_of_coeff 1

  let get_mone_symbol_opt : t -> Symbol.t option = one_symbol_of_coeff (-1)

  let get_one_symbol : t -> Symbol.t =
    fun x -> match get_one_symbol_opt x with Some s -> s | None -> raise Not_one_symbol

  let get_mone_symbol : t -> Symbol.t =
    fun x -> match get_mone_symbol_opt x with Some s -> s | None -> raise Not_one_symbol

  let is_one_symbol : t -> bool =
    fun x -> match get_one_symbol_opt x with Some _ -> true | None -> false

  let is_mone_symbol : t -> bool =
    fun x -> match get_mone_symbol_opt x with Some _ -> true | None -> false

  let get_symbols : t -> Symbol.t list = fun x -> List.map ~f:fst (M.bindings x)
end

module Bound = struct
  type sign_t = Plus | Minus [@@deriving compare]

  let sign_equal = [%compare.equal : sign_t]

  let neg_sign = function Plus -> Minus | Minus -> Plus

  type min_max_t = Min | Max [@@deriving compare]

  let min_max_equal = [%compare.equal : min_max_t]

  let neg_min_max = function Min -> Max | Max -> Min

  (* MinMax constructs a bound that is in the "int [+|-] [min|max](int, symbol)" format.
     e.g. `MinMax (1, Minus, Max, 2, s)` means "1 - max (2, s)". *)
  type t =
    | MInf
    | Linear of int * SymLinear.t
    | MinMax of int * sign_t * min_max_t * int * Symbol.t
    | PInf
    [@@deriving compare]

  let equal = [%compare.equal : t]

  let pp_sign ~need_plus : F.formatter -> sign_t -> unit =
    fun fmt -> function Plus -> if need_plus then F.fprintf fmt "+" | Minus -> F.fprintf fmt "-"

  let pp_min_max : F.formatter -> min_max_t -> unit =
    fun fmt -> function Min -> F.fprintf fmt "min" | Max -> F.fprintf fmt "max"

  let pp : F.formatter -> t -> unit =
    fun fmt ->
      function
        | MInf
         -> F.fprintf fmt "-oo"
        | PInf
         -> F.fprintf fmt "+oo"
        | Linear (c, x)
         -> if SymLinear.is_zero x then F.fprintf fmt "%d" c
            else if Int.equal c 0 then F.fprintf fmt "%a" SymLinear.pp x
            else F.fprintf fmt "%a + %d" SymLinear.pp x c
        | MinMax (c, sign, m, d, x)
         -> if Int.equal c 0 then F.fprintf fmt "%a" (pp_sign ~need_plus:false) sign
            else F.fprintf fmt "%d%a" c (pp_sign ~need_plus:true) sign ;
            F.fprintf fmt "%a(%d, %a)" pp_min_max m d Symbol.pp x

  let of_int : int -> t = fun n -> Linear (n, SymLinear.empty)

  let minus_one = of_int (-1)

  let _255 = of_int 255

  let of_sym : SymLinear.t -> t = fun s -> Linear (0, s)

  let is_symbolic : t -> bool = function
    | MInf | PInf
     -> false
    | Linear (_, se)
     -> not (SymLinear.is_empty se)
    | MinMax _
     -> true

  let opt_lift : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool =
    fun f a_opt b_opt ->
      match (a_opt, b_opt) with None, _ | _, None -> false | Some a, Some b -> f a b

  let eq_symbol : Symbol.t -> t -> bool =
    fun s ->
      function
        | Linear (0, se)
         -> opt_lift Symbol.eq (SymLinear.get_one_symbol_opt se) (Some s)
        | _
         -> false

  let lift_get_one_symbol : (SymLinear.t -> Symbol.t option) -> t -> Symbol.t option =
    fun f -> function Linear (0, se) -> f se | _ -> None

  let get_one_symbol_opt : t -> Symbol.t option = lift_get_one_symbol SymLinear.get_one_symbol_opt

  let get_mone_symbol_opt : t -> Symbol.t option =
    lift_get_one_symbol SymLinear.get_mone_symbol_opt

  let get_one_symbol : t -> Symbol.t =
    fun x -> match get_one_symbol_opt x with Some s -> s | None -> raise Not_one_symbol

  let get_mone_symbol : t -> Symbol.t =
    fun x -> match get_mone_symbol_opt x with Some s -> s | None -> raise Not_one_symbol

  let is_one_symbol : t -> bool = fun x -> get_one_symbol_opt x <> None

  let is_mone_symbol : t -> bool = fun x -> get_mone_symbol_opt x <> None

  let mk_MinMax (c, sign, m, d, s) =
    if Symbol.is_unsigned s then
      match m with
      | Min when d <= 0 -> (
        match sign with Plus -> of_int (c + d) | Minus -> of_int (c - d) )
      | Max when d <= 0 -> (
        match sign with
        | Plus
         -> Linear (c, SymLinear.singleton s 1)
        | Minus
         -> Linear (c, SymLinear.singleton s (-1)) )
      | _
       -> MinMax (c, sign, m, d, s)
    else MinMax (c, sign, m, d, s)

  let use_symbol : Symbol.t -> t -> bool =
    fun s ->
      function
        | PInf | MInf
         -> false
        | Linear (_, se)
         -> SymLinear.find s se <> 0
        | MinMax (_, _, _, _, s')
         -> Symbol.eq s s'

  let subst1 : default:t -> t bottom_lifted -> Symbol.t -> t bottom_lifted -> t bottom_lifted =
    fun ~default x0 s y0 ->
      match (x0, y0) with
      | Bottom, _
       -> x0
      | NonBottom x, _ when eq_symbol s x
       -> y0
      | NonBottom x, _ when not (use_symbol s x)
       -> x0
      | NonBottom _, Bottom
       -> NonBottom default
      | NonBottom x, NonBottom y
       -> let res =
            match (x, y) with
            | Linear (c1, se1), Linear (c2, se2)
             -> let coeff = SymLinear.find s se1 in
                let c' = c1 + coeff * c2 in
                let se1 = SymLinear.add s 0 se1 in
                let se' = SymLinear.plus se1 (SymLinear.mult_const se2 coeff) in
                Linear (c', se')
            | MinMax (_, Plus, Min, _, _), MInf
             -> MInf
            | MinMax (_, Minus, Min, _, _), MInf
             -> PInf
            | MinMax (_, Plus, Max, _, _), PInf
             -> PInf
            | MinMax (_, Minus, Max, _, _), PInf
             -> MInf
            | MinMax (c, Plus, Min, d, _), PInf
             -> Linear (c + d, SymLinear.zero)
            | MinMax (c, Minus, Min, d, _), PInf
             -> Linear (c - d, SymLinear.zero)
            | MinMax (c, Plus, Max, d, _), MInf
             -> Linear (c + d, SymLinear.zero)
            | MinMax (c, Minus, Max, d, _), MInf
             -> Linear (c - d, SymLinear.zero)
            | MinMax (c1, Plus, Min, d1, _), Linear (c2, se) when SymLinear.is_zero se
             -> Linear (c1 + min d1 c2, SymLinear.zero)
            | MinMax (c1, Minus, Min, d1, _), Linear (c2, se) when SymLinear.is_zero se
             -> Linear (c1 - min d1 c2, SymLinear.zero)
            | MinMax (c1, Plus, Max, d1, _), Linear (c2, se) when SymLinear.is_zero se
             -> Linear (c1 + max d1 c2, SymLinear.zero)
            | MinMax (c1, Minus, Max, d1, _), Linear (c2, se) when SymLinear.is_zero se
             -> Linear (c1 - max d1 c2, SymLinear.zero)
            | MinMax (c, sign, m, d, _), _ when is_one_symbol y
             -> mk_MinMax (c, sign, m, d, get_one_symbol y)
            | MinMax (c, sign, m, d, _), _ when is_mone_symbol y
             -> mk_MinMax (c, neg_sign sign, neg_min_max m, -d, get_mone_symbol y)
            | MinMax (c1, Plus, Min, d1, _), MinMax (c2, Plus, Min, d2, s')
             -> mk_MinMax (c1 + c2, Plus, Min, min (d1 - c2) d2, s')
            | MinMax (c1, Plus, Max, d1, _), MinMax (c2, Plus, Max, d2, s')
             -> mk_MinMax (c1 + c2, Plus, Max, max (d1 - c2) d2, s')
            | MinMax (c1, Minus, Min, d1, _), MinMax (c2, Plus, Min, d2, s')
             -> mk_MinMax (c1 - c2, Minus, Min, min (d1 - c2) d2, s')
            | MinMax (c1, Minus, Max, d1, _), MinMax (c2, Plus, Max, d2, s')
             -> mk_MinMax (c1 - c2, Minus, Max, max (d1 - c2) d2, s')
            | MinMax (c1, Plus, Min, d1, _), MinMax (c2, Minus, Max, d2, s')
             -> mk_MinMax (c1 + c2, Minus, Max, max (-d1 + c2) d2, s')
            | MinMax (c1, Plus, Max, d1, _), MinMax (c2, Minus, Min, d2, s')
             -> mk_MinMax (c1 + c2, Minus, Min, min (-d1 + c2) d2, s')
            | MinMax (c1, Minus, Min, d1, _), MinMax (c2, Minus, Max, d2, s')
             -> mk_MinMax (c1 - c2, Minus, Max, max (-d1 + c2) d2, s')
            | MinMax (c1, Minus, Max, d1, _), MinMax (c2, Minus, Min, d2, s')
             -> mk_MinMax (c1 - c2, Minus, Min, min (-d1 + c2) d2, s')
            | _
             -> default
          in
          NonBottom res

  let int_ub_of_minmax = function
    | MinMax (c, Plus, Min, d, _)
     -> Some (c + d)
    | MinMax (c, Minus, Max, d, s) when Symbol.is_unsigned s
     -> Some (min c (c - d))
    | MinMax (c, Minus, Max, d, _)
     -> Some (c - d)
    | MinMax _
     -> None
    | MInf | PInf | Linear _
     -> assert false

  let int_lb_of_minmax = function
    | MinMax (c, Plus, Max, d, s) when Symbol.is_unsigned s
     -> Some (max c (c + d))
    | MinMax (c, Plus, Max, d, _)
     -> Some (c + d)
    | MinMax (c, Minus, Min, d, _)
     -> Some (c - d)
    | MinMax _
     -> None
    | MInf | PInf | Linear _
     -> assert false

  let linear_ub_of_minmax = function
    | MinMax (c, Plus, Min, _, x)
     -> Some (Linear (c, SymLinear.singleton x 1))
    | MinMax (c, Minus, Max, _, x)
     -> Some (Linear (c, SymLinear.singleton x (-1)))
    | MinMax _
     -> None
    | MInf | PInf | Linear _
     -> assert false

  let linear_lb_of_minmax = function
    | MinMax (c, Plus, Max, _, x)
     -> Some (Linear (c, SymLinear.singleton x 1))
    | MinMax (c, Minus, Min, _, x)
     -> Some (Linear (c, SymLinear.singleton x (-1)))
    | MinMax _
     -> None
    | MInf | PInf | Linear _
     -> assert false

  let le_minmax_by_int x y =
    match (int_ub_of_minmax x, int_lb_of_minmax y) with Some n, Some m -> n <= m | _, _ -> false

  let le_opt1 le opt_n m = Option.value_map opt_n ~default:false ~f:(fun n -> le n m)

  let le_opt2 le n opt_m = Option.value_map opt_m ~default:false ~f:(fun m -> le n m)

  let rec le : t -> t -> bool =
    fun x y ->
      match (x, y) with
      | MInf, _ | _, PInf
       -> true
      | _, MInf | PInf, _
       -> false
      | Linear (c0, x0), Linear (c1, x1)
       -> c0 <= c1 && SymLinear.le x0 x1
      | MinMax (c1, sign1, m1, d1, x1), MinMax (c2, sign2, m2, d2, x2)
        when sign_equal sign1 sign2 && min_max_equal m1 m2
       -> c1 <= c2 && Int.equal d1 d2 && Symbol.eq x1 x2
      | MinMax _, MinMax _ when le_minmax_by_int x y
       -> true
      | MinMax (c1, Plus, Min, _, x1), MinMax (c2, Plus, Max, _, x2)
      | MinMax (c1, Minus, Max, _, x1), MinMax (c2, Minus, Min, _, x2)
       -> c1 <= c2 && Symbol.eq x1 x2
      | MinMax _, Linear (c, se)
       -> SymLinear.is_ge_zero se && le_opt1 ( <= ) (int_ub_of_minmax x) c
          || le_opt1 le (linear_ub_of_minmax x) y
      | Linear (c, se), MinMax _
       -> SymLinear.is_le_zero se && le_opt2 ( <= ) c (int_lb_of_minmax y)
          || le_opt2 le x (linear_lb_of_minmax y)
      | _, _
       -> false

  let lt : t -> t -> bool =
    fun x y ->
      match (x, y) with
      | MInf, Linear _ | MInf, MinMax _ | MInf, PInf | Linear _, PInf | MinMax _, PInf
       -> true
      | Linear (c, x), _
       -> le (Linear (c + 1, x)) y
      | MinMax (c, sign, min_max, d, x), _
       -> le (mk_MinMax (c + 1, sign, min_max, d, x)) y
      | _, _
       -> false

  let gt : t -> t -> bool = fun x y -> lt y x

  let eq : t -> t -> bool = fun x y -> le x y && le y x

  let xcompare ~lhs ~rhs =
    let ller = le lhs rhs in
    let rlel = le rhs lhs in
    match (ller, rlel) with
    | true, true
     -> `Equal
    | true, false
     -> `LeftSmallerThanRight
    | false, true
     -> `RightSmallerThanLeft
    | false, false
     -> `NotComparable

  let remove_max_int : t -> t =
    fun x ->
      match x with
      | MinMax (c, Plus, Max, _, s)
       -> Linear (c, SymLinear.singleton s 1)
      | MinMax (c, Minus, Min, _, s)
       -> Linear (c, SymLinear.singleton s (-1))
      | _
       -> x

  let rec lb : ?default:t -> t -> t -> t =
    fun ?(default= MInf) x y ->
      if le x y then x
      else if le y x then y
      else
        match (x, y) with
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_one_symbol x2
         -> mk_MinMax (c2, Plus, Min, c1 - c2, SymLinear.get_one_symbol x2)
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_one_symbol x1 && SymLinear.is_zero x2
         -> mk_MinMax (c1, Plus, Min, c2 - c1, SymLinear.get_one_symbol x1)
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2
         -> mk_MinMax (c2, Minus, Max, c2 - c1, SymLinear.get_mone_symbol x2)
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2
         -> mk_MinMax (c1, Minus, Max, c1 - c2, SymLinear.get_mone_symbol x1)
        | MinMax (c1, Plus, Min, d1, s), Linear (c2, se)
        | Linear (c2, se), MinMax (c1, Plus, Min, d1, s)
          when SymLinear.is_zero se
         -> mk_MinMax (c1, Plus, Min, min d1 (c2 - c1), s)
        | MinMax (c1, Plus, Max, _, s), Linear (c2, se)
        | Linear (c2, se), MinMax (c1, Plus, Max, _, s)
          when SymLinear.is_zero se
         -> mk_MinMax (c1, Plus, Min, c2 - c1, s)
        | MinMax (c1, Minus, Min, _, s), Linear (c2, se)
        | Linear (c2, se), MinMax (c1, Minus, Min, _, s)
          when SymLinear.is_zero se
         -> mk_MinMax (c1, Minus, Max, c1 - c2, s)
        | MinMax (c1, Minus, Max, d1, s), Linear (c2, se)
        | Linear (c2, se), MinMax (c1, Minus, Max, d1, s)
          when SymLinear.is_zero se
         -> mk_MinMax (c1, Minus, Max, max d1 (c1 - c2), s)
        | MinMax (_, Plus, Min, _, _), MinMax (_, Plus, Max, _, _)
        | MinMax (_, Plus, Min, _, _), MinMax (_, Minus, Min, _, _)
        | MinMax (_, Minus, Max, _, _), MinMax (_, Plus, Max, _, _)
        | MinMax (_, Minus, Max, _, _), MinMax (_, Minus, Min, _, _)
         -> lb ~default x (remove_max_int y)
        | MinMax (_, Plus, Max, _, _), MinMax (_, Plus, Min, _, _)
        | MinMax (_, Minus, Min, _, _), MinMax (_, Plus, Min, _, _)
        | MinMax (_, Plus, Max, _, _), MinMax (_, Minus, Max, _, _)
        | MinMax (_, Minus, Min, _, _), MinMax (_, Minus, Max, _, _)
         -> lb ~default (remove_max_int x) y
        | MinMax (c1, Plus, Max, d1, _), MinMax (c2, Plus, Max, d2, _)
         -> Linear (min (c1 + d1) (c2 + d2), SymLinear.zero)
        | _, _
         -> default

  let ub : ?default:t -> t -> t -> t =
    fun ?(default= PInf) x y ->
      if le x y then y
      else if le y x then x
      else
        match (x, y) with
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_one_symbol x2
         -> mk_MinMax (c2, Plus, Max, c1 - c2, SymLinear.get_one_symbol x2)
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_one_symbol x1 && SymLinear.is_zero x2
         -> mk_MinMax (c1, Plus, Max, c2 - c1, SymLinear.get_one_symbol x1)
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2
         -> mk_MinMax (c2, Minus, Min, c2 - c1, SymLinear.get_mone_symbol x2)
        | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2
         -> mk_MinMax (c1, Minus, Min, c1 - c2, SymLinear.get_mone_symbol x1)
        | _, _
         -> default

  let widen_l : t -> t -> t =
    fun x y ->
      if equal x PInf || equal y PInf then L.(die InternalError) "Lower bound cannot be +oo."
      else if le x y then x
      else MInf

  let widen_u : t -> t -> t =
    fun x y ->
      if equal x MInf || equal y MInf then L.(die InternalError) "Upper bound cannot be -oo."
      else if le y x then x
      else PInf

  let initial : t = of_int 0

  let zero : t = Linear (0, SymLinear.zero)

  let one : t = Linear (1, SymLinear.zero)

  let mone : t = Linear (-1, SymLinear.zero)

  let is_some_const : int -> t -> bool =
    fun c x -> match x with Linear (c', y) -> Int.equal c c' && SymLinear.is_zero y | _ -> false

  let is_zero : t -> bool = is_some_const 0

  let is_one : t -> bool = is_some_const 1

  let is_const : t -> int option =
    fun x -> match x with Linear (c, y) when SymLinear.is_zero y -> Some c | _ -> None

  (* substitution symbols in ``x'' with respect to ``map'' *)
  let subst : default:t -> t -> t bottom_lifted SubstMap.t -> t bottom_lifted =
    fun ~default x map ->
      let subst_helper s y x =
        let y' =
          match y with
          | Bottom
           -> Bottom
          | NonBottom r
           -> NonBottom (if Symbol.is_unsigned s then ub ~default:r zero r else r)
        in
        subst1 ~default x s y'
      in
      SubstMap.fold subst_helper map (NonBottom x)

  let plus_l : t -> t -> t =
    fun x y ->
      match (x, y) with
      | _, _ when is_zero x
       -> y
      | _, _ when is_zero y
       -> x
      | Linear (c1, x1), Linear (c2, x2)
       -> Linear (c1 + c2, SymLinear.plus x1 x2)
      | MinMax (c1, sign, min_max, d1, x1), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, sign, min_max, d1, x1)
        when SymLinear.is_zero x2
       -> mk_MinMax (c1 + c2, sign, min_max, d1, x1)
      | MinMax (c1, Plus, Max, d1, _), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, Plus, Max, d1, _)
       -> Linear (c1 + d1 + c2, x2)
      | MinMax (c1, Minus, Min, d1, _), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, Minus, Min, d1, _)
       -> Linear (c1 - d1 + c2, x2)
      | _, _
       -> MInf

  let plus_u : t -> t -> t =
    fun x y ->
      match (x, y) with
      | _, _ when is_zero x
       -> y
      | _, _ when is_zero y
       -> x
      | Linear (c1, x1), Linear (c2, x2)
       -> Linear (c1 + c2, SymLinear.plus x1 x2)
      | MinMax (c1, sign, min_max, d1, x1), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, sign, min_max, d1, x1)
        when SymLinear.is_zero x2
       -> mk_MinMax (c1 + c2, sign, min_max, d1, x1)
      | MinMax (c1, Plus, Min, d1, _), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, Plus, Min, d1, _)
       -> Linear (c1 + d1 + c2, x2)
      | MinMax (c1, Minus, Max, d1, _), Linear (c2, x2)
      | Linear (c2, x2), MinMax (c1, Minus, Max, d1, _)
       -> Linear (c1 - d1 + c2, x2)
      | _, _
       -> PInf

  let mult_const : t -> int -> t option =
    fun x n ->
      assert (n <> 0) ;
      match x with
      | MInf
       -> Some (if n > 0 then MInf else PInf)
      | PInf
       -> Some (if n > 0 then PInf else MInf)
      | Linear (c, x')
       -> Some (Linear (c * n, SymLinear.mult_const x' n))
      | _
       -> None

  let div_const : t -> int -> t option =
    fun x n ->
      if Int.equal n 0 then Some zero
      else
        match x with
        | MInf
         -> Some (if n > 0 then MInf else PInf)
        | PInf
         -> Some (if n > 0 then PInf else MInf)
        | Linear (c, x')
         -> if Int.equal (c mod n) 0 && SymLinear.is_mod_zero x' n then
              Some (Linear (c / n, SymLinear.div_const x' n))
            else None
        | _
         -> None

  let neg : t -> t option = function
    | MInf
     -> Some PInf
    | PInf
     -> Some MInf
    | Linear (c, x)
     -> Some (Linear (-c, SymLinear.neg x))
    | MinMax (c, sign, min_max, d, x)
     -> Some (mk_MinMax (-c, neg_sign sign, min_max, d, x))

  let get_symbols : t -> Symbol.t list = function
    | MInf | PInf
     -> []
    | Linear (_, se)
     -> SymLinear.get_symbols se
    | MinMax (_, _, _, _, s)
     -> [s]

  let are_similar b1 b2 =
    match (b1, b2) with
    | MInf, MInf
     -> true
    | PInf, PInf
     -> true
    | (Linear _ | MinMax _), (Linear _ | MinMax _)
     -> true
    | _
     -> false

  let is_not_infty : t -> bool = function MInf | PInf -> false | _ -> true
end

module ItvPure = struct
  (** (l, u) represents the closed interval [l; u] (of course infinite bounds are open) *)
  type astate = Bound.t * Bound.t [@@deriving compare]

  type t = astate

  let equal = [%compare.equal : astate]

  let initial : t = (Bound.initial, Bound.initial)

  let lb : t -> Bound.t = fst

  let ub : t -> Bound.t = snd

  let is_finite : t -> bool =
    fun (l, u) ->
      match (Bound.is_const l, Bound.is_const u) with Some _, Some _ -> true | _, _ -> false

  let have_similar_bounds (l1, u1) (l2, u2) = Bound.are_similar l1 l2 && Bound.are_similar u1 u2

  let make : Bound.t -> Bound.t -> t = fun l u -> (l, u)

  let subst : t -> Bound.t bottom_lifted SubstMap.t -> t bottom_lifted =
    fun x map ->
      match
        (Bound.subst ~default:Bound.MInf (lb x) map, Bound.subst ~default:Bound.PInf (ub x) map)
      with
      | NonBottom l, NonBottom u
       -> NonBottom (l, u)
      | _
       -> Bottom

  let ( <= ) : lhs:t -> rhs:t -> bool =
    fun ~lhs:(l1, u1) ~rhs:(l2, u2) -> Bound.le l2 l1 && Bound.le u1 u2

  let xcompare ~lhs:(l1, u1) ~rhs:(l2, u2) =
    let lcmp = Bound.xcompare ~lhs:l1 ~rhs:l2 in
    let ucmp = Bound.xcompare ~lhs:u1 ~rhs:u2 in
    match (lcmp, ucmp) with
    | `Equal, `Equal
     -> `Equal
    | `NotComparable, _ | _, `NotComparable -> (
      match Bound.xcompare ~lhs:u1 ~rhs:l2 with
      | `LeftSmallerThanRight
       -> `LeftSmallerThanRight
      | u1l2 ->
        match (Bound.xcompare ~lhs:u2 ~rhs:l1, u1l2) with
        | `LeftSmallerThanRight, _
         -> `RightSmallerThanLeft
        | `Equal, `Equal
         -> `Equal (* weird, though *)
        | _, `Equal
         -> `LeftSmallerThanRight
        | _
         -> `NotComparable )
    | (`LeftSmallerThanRight | `Equal), (`LeftSmallerThanRight | `Equal)
     -> `LeftSmallerThanRight
    | (`RightSmallerThanLeft | `Equal), (`RightSmallerThanLeft | `Equal)
     -> `RightSmallerThanLeft
    | `LeftSmallerThanRight, `RightSmallerThanLeft
     -> `LeftSubsumesRight
    | `RightSmallerThanLeft, `LeftSmallerThanRight
     -> `RightSubsumesLeft

  let join : t -> t -> t = fun (l1, u1) (l2, u2) -> (Bound.lb l1 l2, Bound.ub u1 u2)

  let widen : prev:t -> next:t -> num_iters:int -> t =
    fun ~prev:(l1, u1) ~next:(l2, u2) ~num_iters:_ -> (Bound.widen_l l1 l2, Bound.widen_u u1 u2)

  let pp : F.formatter -> t -> unit =
    fun fmt (l, u) -> F.fprintf fmt "[%a, %a]" Bound.pp l Bound.pp u

  let of_bound bound = (bound, bound)

  let of_int n = of_bound (Bound.of_int n)

  let of_int_lit : IntLit.t -> t option =
    fun s -> match IntLit.to_int s with size -> Some (of_int size) | exception _ -> None

  let make_sym : unsigned:bool -> Typ.Procname.t -> (unit -> int) -> t =
    fun ~unsigned pname new_sym_num ->
      let lower = Bound.of_sym (SymLinear.make ~unsigned pname (new_sym_num ())) in
      let upper = Bound.of_sym (SymLinear.make ~unsigned pname (new_sym_num ())) in
      (lower, upper)

  let m1_255 = (Bound.minus_one, Bound._255)

  let nat = (Bound.zero, Bound.PInf)

  let one = of_bound Bound.one

  let pos = (Bound.one, Bound.PInf)

  let top = (Bound.MInf, Bound.PInf)

  let zero = of_bound Bound.zero

  let true_sem = one

  let false_sem = zero

  let unknown_bool = join false_sem true_sem

  let is_top : t -> bool = function Bound.MInf, Bound.PInf -> true | _ -> false

  let is_nat : t -> bool = function l, Bound.PInf -> Bound.is_zero l | _ -> false

  let is_const : t -> int option =
    fun (l, u) ->
      match (Bound.is_const l, Bound.is_const u) with
      | Some n, Some m when Int.equal n m
       -> Some n
      | _, _
       -> None

  let is_one : t -> bool = fun (l, u) -> Bound.is_one l && Bound.is_one u

  let is_zero : t -> bool = fun (l, u) -> Bound.is_zero l && Bound.is_zero u

  let is_true : t -> bool =
    fun (l, u) -> Bound.le (Bound.of_int 1) l || Bound.le u (Bound.of_int (-1))

  let is_false : t -> bool = is_zero

  let is_symbolic : t -> bool = fun (lb, ub) -> Bound.is_symbolic lb || Bound.is_symbolic ub

  let is_ge_zero : t -> bool = fun (lb, _) -> Bound.le Bound.zero lb

  let is_le_zero : t -> bool = fun (_, ub) -> Bound.le ub Bound.zero

  let neg : t -> t =
    fun (l, u) ->
      let l' = Option.value ~default:Bound.MInf (Bound.neg u) in
      let u' = Option.value ~default:Bound.PInf (Bound.neg l) in
      (l', u')

  let lnot : t -> t =
    fun x -> if is_true x then false_sem else if is_false x then true_sem else unknown_bool

  let plus : t -> t -> t = fun (l1, u1) (l2, u2) -> (Bound.plus_l l1 l2, Bound.plus_u u1 u2)

  let minus : t -> t -> t = fun i1 i2 -> plus i1 (neg i2)

  let mult_const : t -> int -> t =
    fun (l, u) n ->
      if Int.equal n 0 then zero
      else if n > 0 then
        let l' = Option.value ~default:Bound.MInf (Bound.mult_const l n) in
        let u' = Option.value ~default:Bound.PInf (Bound.mult_const u n) in
        (l', u')
      else
        let l' = Option.value ~default:Bound.MInf (Bound.mult_const u n) in
        let u' = Option.value ~default:Bound.PInf (Bound.mult_const l n) in
        (l', u')

  (* Returns a correct value only when all coefficients are divided by
     n without remainder. *)
  let div_const : t -> int -> t =
    fun (l, u) n ->
      assert (n <> 0) ;
      if n > 0 then
        let l' = Option.value ~default:Bound.MInf (Bound.div_const l n) in
        let u' = Option.value ~default:Bound.PInf (Bound.div_const u n) in
        (l', u')
      else
        let l' = Option.value ~default:Bound.MInf (Bound.div_const u n) in
        let u' = Option.value ~default:Bound.PInf (Bound.div_const l n) in
        (l', u')

  let mult : t -> t -> t =
    fun x y ->
      match (is_const x, is_const y) with
      | _, Some n
       -> mult_const x n
      | Some n, _
       -> mult_const y n
      | None, None
       -> top

  let div : t -> t -> t =
    fun x y -> match is_const y with Some n when n <> 0 -> div_const x n | _ -> top

  (* x % [0,0] does nothing. *)
  let mod_sem : t -> t -> t =
    fun x y ->
      match (is_const x, is_const y) with
      | _, Some 0
       -> x
      | Some n, Some m
       -> of_int (n mod m)
      | _, Some m
       -> let abs_m = abs m in
          if is_ge_zero x then (Bound.zero, Bound.of_int (abs_m - 1))
          else if is_le_zero x then (Bound.of_int (-abs_m + 1), Bound.zero)
          else (Bound.of_int (-abs_m + 1), Bound.of_int (abs_m - 1))
      | _, None
       -> top

  (* x << [-1,-1] does nothing. *)
  let shiftlt : t -> t -> t =
    fun x y ->
      match is_const y with Some n -> if n >= 0 then mult_const x (1 lsl n) else x | None -> top

  (* x >> [-1,-1] does nothing. *)
  let shiftrt : t -> t -> t =
    fun x y ->
      match is_const y with
      | Some n
       -> if n >= 0 && n < 63 then div_const x (1 lsl n) else x
      | None
       -> top

  let lt_sem : t -> t -> t =
    fun (l1, u1) (l2, u2) ->
      if Bound.lt u1 l2 then true_sem else if Bound.le u2 l1 then false_sem else unknown_bool

  let gt_sem : t -> t -> t = fun x y -> lt_sem y x

  let le_sem : t -> t -> t =
    fun (l1, u1) (l2, u2) ->
      if Bound.le u1 l2 then true_sem else if Bound.lt u2 l1 then false_sem else unknown_bool

  let ge_sem : t -> t -> t = fun x y -> le_sem y x

  let eq_sem : t -> t -> t =
    fun (l1, u1) (l2, u2) ->
      if Bound.eq l1 u1 && Bound.eq u1 l2 && Bound.eq l2 u2 then true_sem
      else if Bound.lt u1 l2 || Bound.lt u2 l1 then false_sem
      else unknown_bool

  let ne_sem : t -> t -> t =
    fun (l1, u1) (l2, u2) ->
      if Bound.eq l1 u1 && Bound.eq u1 l2 && Bound.eq l2 u2 then false_sem
      else if Bound.lt u1 l2 || Bound.lt u2 l1 then true_sem
      else unknown_bool

  let land_sem : t -> t -> t =
    fun x y ->
      if is_true x && is_true y then true_sem
      else if is_false x || is_false y then false_sem
      else unknown_bool

  let lor_sem : t -> t -> t =
    fun x y ->
      if is_true x || is_true y then true_sem
      else if is_false x && is_false y then false_sem
      else unknown_bool

  let min_sem : t -> t -> t = fun (l1, u1) (l2, u2) -> (Bound.lb l1 l2, Bound.lb ~default:u1 u1 u2)

  let is_invalid : t -> bool = function
    | Bound.PInf, _ | _, Bound.MInf
     -> true
    | l, u
     -> Bound.lt u l

  let prune_le : t -> t -> t =
    fun x y ->
      match (x, y) with
      | (l1, Bound.PInf), (_, u2)
       -> (l1, u2)
      | (l1, Bound.Linear (c1, s1)), (_, Bound.Linear (c2, s2)) when SymLinear.eq s1 s2
       -> (l1, Bound.Linear (min c1 c2, s1))
      | (l1, Bound.Linear (c, se)), (_, u) when SymLinear.is_zero se && Bound.is_one_symbol u
       -> (l1, Bound.mk_MinMax (0, Bound.Plus, Bound.Min, c, Bound.get_one_symbol u))
      | (l1, u), (_, Bound.Linear (c, se)) when SymLinear.is_zero se && Bound.is_one_symbol u
       -> (l1, Bound.mk_MinMax (0, Bound.Plus, Bound.Min, c, Bound.get_one_symbol u))
      | (l1, Bound.Linear (c, se)), (_, u) when SymLinear.is_zero se && Bound.is_mone_symbol u
       -> (l1, Bound.mk_MinMax (0, Bound.Minus, Bound.Max, -c, Bound.get_mone_symbol u))
      | (l1, u), (_, Bound.Linear (c, se)) when SymLinear.is_zero se && Bound.is_mone_symbol u
       -> (l1, Bound.mk_MinMax (0, Bound.Minus, Bound.Max, -c, Bound.get_mone_symbol u))
      | (l1, Bound.Linear (c1, se)), (_, Bound.MinMax (c2, Bound.Plus, Bound.Min, d2, se'))
      | (l1, Bound.MinMax (c2, Bound.Plus, Bound.Min, d2, se')), (_, Bound.Linear (c1, se))
        when SymLinear.is_zero se
       -> (l1, Bound.mk_MinMax (c2, Bound.Plus, Bound.Min, min (c1 - c2) d2, se'))
      | ( (l1, Bound.MinMax (c1, Bound.Plus, Bound.Min, d1, se1))
        , (_, Bound.MinMax (c2, Bound.Plus, Bound.Min, d2, se2)) )
        when Int.equal c1 c2 && Symbol.eq se1 se2
       -> (l1, Bound.mk_MinMax (c1, Bound.Plus, Bound.Min, min d1 d2, se1))
      | _
       -> x

  let prune_ge : t -> t -> t =
    fun x y ->
      match (x, y) with
      | (Bound.MInf, u1), (l2, _)
       -> (l2, u1)
      | (Bound.Linear (c1, s1), u1), (Bound.Linear (c2, s2), _) when SymLinear.eq s1 s2
       -> (Bound.Linear (max c1 c2, s1), u1)
      | (Bound.Linear (c, se), u1), (l, _) when SymLinear.is_zero se && Bound.is_one_symbol l
       -> (Bound.mk_MinMax (0, Bound.Plus, Bound.Max, c, Bound.get_one_symbol l), u1)
      | (l, u1), (Bound.Linear (c, se), _) when SymLinear.is_zero se && Bound.is_one_symbol l
       -> (Bound.mk_MinMax (0, Bound.Plus, Bound.Max, c, Bound.get_one_symbol l), u1)
      | (Bound.Linear (c, se), u1), (l, _) when SymLinear.is_zero se && Bound.is_mone_symbol l
       -> (Bound.mk_MinMax (0, Bound.Minus, Bound.Min, c, Bound.get_mone_symbol l), u1)
      | (l, u1), (Bound.Linear (c, se), _) when SymLinear.is_zero se && Bound.is_mone_symbol l
       -> (Bound.mk_MinMax (0, Bound.Minus, Bound.Min, c, Bound.get_mone_symbol l), u1)
      | (Bound.Linear (c1, se), u1), (Bound.MinMax (c2, Bound.Plus, Bound.Max, d2, se'), _)
      | (Bound.MinMax (c2, Bound.Plus, Bound.Max, d2, se'), u1), (Bound.Linear (c1, se), _)
        when SymLinear.is_zero se
       -> (Bound.mk_MinMax (c2, Bound.Plus, Bound.Max, max (c1 - c2) d2, se'), u1)
      | ( (Bound.MinMax (c1, Bound.Plus, Bound.Max, d1, se1), u1)
        , (Bound.MinMax (c2, Bound.Plus, Bound.Max, d2, se2), _) )
        when Int.equal c1 c2 && Symbol.eq se1 se2
       -> (Bound.mk_MinMax (c1, Bound.Plus, Bound.Max, max d1 d2, se1), u1)
      | _
       -> x

  let prune_lt : t -> t -> t = fun x y -> prune_le x (minus y one)

  let prune_gt : t -> t -> t = fun x y -> prune_ge x (plus y one)

  let diff : t -> Bound.t -> t =
    fun (l, u) b ->
      if Bound.eq l b then (Bound.plus_l l Bound.one, u)
      else if Bound.eq u b then (l, Bound.plus_u u Bound.mone)
      else (l, u)

  let prune_zero : t -> t = fun x -> diff x Bound.zero

  let prune_comp : Binop.t -> t -> t -> t option =
    fun c x y ->
      if is_invalid y then Some x
      else
        let x =
          match c with
          | Binop.Le
           -> prune_le x y
          | Binop.Ge
           -> prune_ge x y
          | Binop.Lt
           -> prune_lt x y
          | Binop.Gt
           -> prune_gt x y
          | _
           -> assert false
        in
        if is_invalid x then None else Some x

  let prune_eq : t -> t -> t option =
    fun x y ->
      match prune_comp Binop.Le x y with None -> None | Some x' -> prune_comp Binop.Ge x' y

  let prune_ne : t -> t -> t option =
    fun x (l, u) ->
      if is_invalid (l, u) then Some x
      else
        let x = if Bound.eq l u then diff x l else x in
        if is_invalid x then None else Some x

  let get_symbols : t -> Symbol.t list =
    fun (l, u) -> List.append (Bound.get_symbols l) (Bound.get_symbols u)

  let make_positive : t -> t =
    fun (l, u as x) -> if Bound.lt l Bound.zero then (Bound.zero, u) else x

  let normalize : t -> t option = fun (l, u) -> if is_invalid (l, u) then None else Some (l, u)
end

include AbstractDomain.BottomLifted (ItvPure)

type t = astate

let compare : t -> t -> int =
  fun x y ->
    match (x, y) with
    | Bottom, Bottom
     -> 0
    | Bottom, _
     -> -1
    | _, Bottom
     -> 1
    | NonBottom x, NonBottom y
     -> ItvPure.compare_astate x y

let equal = [%compare.equal : t]

let compare_astate = compare

let bot : t = Bottom

let top : t = NonBottom ItvPure.top

let lb : t -> Bound.t = function
  | NonBottom x
   -> ItvPure.lb x
  | Bottom
   -> L.(die InternalError) "lower bound of bottom"

let ub : t -> Bound.t = function
  | NonBottom x
   -> ItvPure.ub x
  | Bottom
   -> L.(die InternalError) "upper bound of bottom"

let of_int : int -> astate = fun n -> NonBottom (ItvPure.of_int n)

let of_int_lit n =
  try of_int (IntLit.to_int n)
  with _ -> top

let is_bot : t -> bool = fun x -> equal x Bottom

let is_finite : t -> bool = function NonBottom x -> ItvPure.is_finite x | Bottom -> false

let false_sem = NonBottom ItvPure.false_sem

let m1_255 = NonBottom ItvPure.m1_255

let nat = NonBottom ItvPure.nat

let one = NonBottom ItvPure.one

let pos = NonBottom ItvPure.pos

let true_sem = NonBottom ItvPure.true_sem

let unknown_bool = NonBottom ItvPure.unknown_bool

let zero = NonBottom ItvPure.zero

let make : Bound.t -> Bound.t -> t =
  fun l u -> if Bound.lt u l then Bottom else NonBottom (ItvPure.make l u)

let is_symbolic : t -> bool = function NonBottom x -> ItvPure.is_symbolic x | Bottom -> false

let le : lhs:t -> rhs:t -> bool = ( <= )

let eq : t -> t -> bool = fun x y -> ( <= ) ~lhs:x ~rhs:y && ( <= ) ~lhs:y ~rhs:x

let to_string : t -> string = fun x -> pp F.str_formatter x ; F.flush_str_formatter ()

let lift1 : (ItvPure.t -> ItvPure.t) -> t -> t =
  fun f -> function Bottom -> Bottom | NonBottom x -> NonBottom (f x)

let lift1_opt : (ItvPure.t -> ItvPure.t option) -> t -> t =
  fun f ->
    function
      | Bottom -> Bottom | NonBottom x -> match f x with None -> Bottom | Some v -> NonBottom v

let lift2 : (ItvPure.t -> ItvPure.t -> ItvPure.t) -> t -> t -> t =
  fun f x y ->
    match (x, y) with
    | Bottom, _ | _, Bottom
     -> Bottom
    | NonBottom x, NonBottom y
     -> NonBottom (f x y)

let lift2_opt : (ItvPure.t -> ItvPure.t -> ItvPure.t option) -> t -> t -> t =
  fun f x y ->
    match (x, y) with
    | Bottom, _ | _, Bottom
     -> Bottom
    | NonBottom x, NonBottom y ->
      match f x y with Some v -> NonBottom v | None -> Bottom

let plus : t -> t -> t = lift2 ItvPure.plus

let minus : t -> t -> t = lift2 ItvPure.minus

let make_sym : ?unsigned:bool -> Typ.Procname.t -> (unit -> int) -> t =
  fun ?(unsigned= false) pname new_sym_num ->
    NonBottom (ItvPure.make_sym ~unsigned pname new_sym_num)

let neg : t -> t = lift1 ItvPure.neg

let lnot : t -> t = lift1 ItvPure.lnot

let mult : t -> t -> t = lift2 ItvPure.mult

let div : t -> t -> t = lift2 ItvPure.div

let mod_sem : t -> t -> t = lift2 ItvPure.mod_sem

let shiftlt : t -> t -> t = lift2 ItvPure.shiftlt

let shiftrt : t -> t -> t = lift2 ItvPure.shiftrt

let lt_sem : t -> t -> t = lift2 ItvPure.lt_sem

let gt_sem : t -> t -> t = lift2 ItvPure.gt_sem

let le_sem : t -> t -> t = lift2 ItvPure.le_sem

let ge_sem : t -> t -> t = lift2 ItvPure.ge_sem

let eq_sem : t -> t -> t = lift2 ItvPure.eq_sem

let ne_sem : t -> t -> t = lift2 ItvPure.ne_sem

let land_sem : t -> t -> t = lift2 ItvPure.land_sem

let lor_sem : t -> t -> t = lift2 ItvPure.lor_sem

let min_sem : t -> t -> t = lift2 ItvPure.min_sem

let prune_zero : t -> t = lift1 ItvPure.prune_zero

let prune_comp : Binop.t -> t -> t -> t = fun comp -> lift2_opt (ItvPure.prune_comp comp)

let prune_eq : t -> t -> t = lift2_opt ItvPure.prune_eq

let prune_ne : t -> t -> t = lift2_opt ItvPure.prune_ne

let subst : t -> Bound.t bottom_lifted SubstMap.t -> t =
  fun x map -> match x with NonBottom x' -> ItvPure.subst x' map | _ -> x

let get_symbols : t -> Symbol.t list = function
  | Bottom
   -> []
  | NonBottom x
   -> ItvPure.get_symbols x

let normalize : t -> t = lift1_opt ItvPure.normalize
