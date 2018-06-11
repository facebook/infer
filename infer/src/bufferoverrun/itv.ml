(*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging

module Counter = struct
  type t = unit -> int

  let make : int -> t =
   fun init ->
    let num_ref = ref init in
    let get_num () =
      let v = !num_ref in
      num_ref := v + 1 ;
      v
    in
    get_num


  let next : t -> int = fun counter -> counter ()
end

module Boolean = struct
  type t = Bottom | False | True | Top [@@deriving compare]

  let top = Top

  let equal = [%compare.equal : t]

  let is_false = function False -> true | _ -> false

  let is_true = function True -> true | _ -> false
end

exception Not_one_symbol

module Symbol = struct
  type t = {pname: Typ.Procname.t; id: int; unsigned: bool} [@@deriving compare]

  let equal = [%compare.equal : t]

  let make : unsigned:bool -> Typ.Procname.t -> int -> t =
   fun ~unsigned pname id -> {pname; id; unsigned}


  let pp : F.formatter -> t -> unit =
   fun fmt {pname; id; unsigned} ->
    let symbol_name = if unsigned then "u" else "s" in
    if Config.bo_debug <= 1 then F.fprintf fmt "%s$%d" symbol_name id
    else F.fprintf fmt "%s-%s$%d" (Typ.Procname.to_string pname) symbol_name id


  let is_unsigned : t -> bool = fun x -> x.unsigned
end

module SymbolMap = struct
  include PrettyPrintable.MakePPMap (Symbol)

  let for_all2 : f:(key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool =
   fun ~f x y ->
    match merge (fun k x y -> if f k x y then None else raise Exit) x y with
    | _ ->
        true
    | exception Exit ->
        false


  let is_singleton : 'a t -> (key * 'a) option =
   fun m ->
    if is_empty m then None
    else
      let (kmin, _) as binding = min_binding m in
      let kmax, _ = max_binding m in
      if Symbol.equal kmin kmax then Some binding else None
end

module NonZeroInt : sig
  type t = private int [@@deriving compare]

  exception DivisionNotExact

  val one : t

  val minus_one : t

  val of_int : int -> t option

  val opt_to_int : t option -> int

  val is_one : t -> bool

  val is_minus_one : t -> bool

  val is_multiple : int -> t -> bool

  val is_negative : t -> bool

  val is_positive : t -> bool

  val ( ~- ) : t -> t

  val ( * ) : t -> t -> t

  val plus : t -> t -> t option

  val exact_div_exn : t -> t -> t
end = struct
  type t = int [@@deriving compare]

  exception DivisionNotExact

  let one = 1

  let minus_one = -1

  let of_int = function 0 -> None | i -> Some i

  let opt_to_int = function None -> 0 | Some i -> i

  let is_one = Int.equal 1

  let is_minus_one = Int.equal (-1)

  let is_multiple mul div = Int.equal (mul mod div) 0

  let is_negative x = x < 0

  let is_positive x = x > 0

  let ( ~- ) = Int.( ~- )

  let ( * ) = Int.( * )

  let plus x y = of_int (x + y)

  let exact_div_exn num den =
    if not (is_multiple num den) then raise DivisionNotExact ;
    num / den
end

module Ints : sig
  module NonNegativeInt : sig
    type t = private int [@@deriving compare]

    val zero : t

    val one : t

    val of_int_exn : int -> t

    val is_zero : t -> bool

    val is_one : t -> bool

    val ( <= ) : lhs:t -> rhs:t -> bool

    val ( + ) : t -> t -> t

    val ( * ) : t -> t -> t

    val max : t -> t -> t

    val pp : F.formatter -> t -> unit
  end

  module PositiveInt : sig
    type t = private NonNegativeInt.t [@@deriving compare]

    val one : t

    val of_int : int -> t option

    val succ : t -> t

    val pp : F.formatter -> t -> unit
  end
end = struct
  module NonNegativeInt = struct
    type t = int [@@deriving compare]

    let zero = 0

    let one = 1

    let is_zero = function 0 -> true | _ -> false

    let is_one = function 1 -> true | _ -> false

    let of_int_exn i =
      assert (i >= 0) ;
      i


    let ( <= ) ~lhs ~rhs = Int.(lhs <= rhs)

    let ( + ) = Int.( + )

    let ( * ) = Int.( * )

    let max = Int.max

    let pp = F.pp_print_int
  end

  module PositiveInt = struct
    type t = NonNegativeInt.t [@@deriving compare]

    let one = 1

    let of_int i = if i <= 0 then None else Some i

    let succ = Int.succ

    let pp = F.pp_print_int
  end
end

open Ints

module SymLinear = struct
  module M = SymbolMap

  (**
     Map from symbols to integer coefficients.
     { x -> 2, y -> 5 } represents the value 2 * x + 5 * y
  *)
  type t = NonZeroInt.t M.t [@@deriving compare]

  let empty : t = M.empty

  let is_empty : t -> bool = fun x -> M.is_empty x

  let remove : Symbol.t -> t -> t = fun s x -> M.remove s x

  let singleton_one : Symbol.t -> t = fun s -> M.singleton s NonZeroInt.one

  let singleton_minus_one : Symbol.t -> t = fun s -> M.singleton s NonZeroInt.minus_one

  let find : Symbol.t -> t -> NonZeroInt.t = fun s x -> M.find s x

  let use_symbol : Symbol.t -> t -> bool = fun s x -> M.mem s x

  let is_le_zero : t -> bool =
   fun x -> M.for_all (fun s v -> Symbol.is_unsigned s && NonZeroInt.is_negative v) x


  let is_ge_zero : t -> bool =
   fun x -> M.for_all (fun s v -> Symbol.is_unsigned s && NonZeroInt.is_positive v) x


  let le : t -> t -> bool =
   fun x y ->
    let le_one_pair s v1_opt v2_opt =
      let v1 = NonZeroInt.opt_to_int v1_opt in
      let v2 = NonZeroInt.opt_to_int v2_opt in
      Int.equal v1 v2 || (Symbol.is_unsigned s && v1 <= v2)
    in
    M.for_all2 ~f:le_one_pair x y


  let make : unsigned:bool -> Typ.Procname.t -> int -> t =
   fun ~unsigned pname i -> singleton_one (Symbol.make ~unsigned pname i)


  let eq : t -> t -> bool =
   fun x y ->
    let eq_pair _ (coeff1: NonZeroInt.t option) (coeff2: NonZeroInt.t option) =
      [%compare.equal : int option] (coeff1 :> int option) (coeff2 :> int option)
    in
    M.for_all2 ~f:eq_pair x y


  let pp1 : F.formatter -> Symbol.t * NonZeroInt.t -> unit =
   fun fmt (s, c) ->
    let c = (c :> int) in
    if Int.equal c 1 then Symbol.pp fmt s
    else if Int.equal c (-1) then F.fprintf fmt "-%a" Symbol.pp s
    else F.fprintf fmt "%dx%a" c Symbol.pp s


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


  (** [se1] * [c] + [se2] *)
  let mult_const_plus : t -> NonZeroInt.t -> t -> t =
   fun se1 c se2 ->
    let f _ (coeff1: NonZeroInt.t option) (coeff2: NonZeroInt.t option) =
      match (coeff1, coeff2) with
      | None, None ->
          None
      | None, (Some _ as some_v) ->
          some_v
      | Some v, None ->
          Some NonZeroInt.(v * c)
      | Some v1, Some v2 ->
          NonZeroInt.(v1 * c |> plus v2)
    in
    M.merge f se1 se2


  let mult_const : NonZeroInt.t -> t -> t = fun n x -> M.map (NonZeroInt.( * ) n) x

  let exact_div_const_exn : t -> NonZeroInt.t -> t =
   fun x n -> M.map (fun c -> NonZeroInt.exact_div_exn c n) x


  (* Returns a symbol when the map contains only one symbol s with a
     given coefficient. *)
  let one_symbol_of_coeff : NonZeroInt.t -> t -> Symbol.t option =
   fun coeff x ->
    match M.is_singleton x with
    | Some (k, v) when Int.equal (v :> int) (coeff :> int) ->
        Some k
    | _ ->
        None


  let get_one_symbol_opt : t -> Symbol.t option = one_symbol_of_coeff NonZeroInt.one

  let get_mone_symbol_opt : t -> Symbol.t option = one_symbol_of_coeff NonZeroInt.minus_one

  let get_one_symbol : t -> Symbol.t =
   fun x -> match get_one_symbol_opt x with Some s -> s | None -> raise Not_one_symbol


  let get_mone_symbol : t -> Symbol.t =
   fun x -> match get_mone_symbol_opt x with Some s -> s | None -> raise Not_one_symbol


  let is_one_symbol : t -> bool =
   fun x -> match get_one_symbol_opt x with Some _ -> true | None -> false


  let is_mone_symbol : t -> bool =
   fun x -> match get_mone_symbol_opt x with Some _ -> true | None -> false


  let is_one_symbol_of : Symbol.t -> t -> bool =
   fun s x ->
    Option.value_map (get_one_symbol_opt x) ~default:false ~f:(fun s' -> Symbol.equal s s')


  let is_mone_symbol_of : Symbol.t -> t -> bool =
   fun s x ->
    Option.value_map (get_mone_symbol_opt x) ~default:false ~f:(fun s' -> Symbol.equal s s')


  let get_symbols : t -> Symbol.t list =
   fun x -> M.fold (fun symbol _coeff acc -> symbol :: acc) x []
end

module Bound = struct
  type sign = Plus | Minus [@@deriving compare]

  module Sign = struct
    type t = sign [@@deriving compare]

    let equal = [%compare.equal : t]

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

    let equal = [%compare.equal : t]

    let neg = function Min -> Max | Max -> Min

    let eval_int x i1 i2 = match x with Min -> min i1 i2 | Max -> max i1 i2

    let pp : F.formatter -> t -> unit =
     fun fmt -> function Min -> F.pp_print_string fmt "min" | Max -> F.pp_print_string fmt "max"
  end

  (* MinMax constructs a bound that is in the "int [+|-] [min|max](int, symbol)" format.
     e.g. `MinMax (1, Minus, Max, 2, s)` means "1 - max (2, s)". *)
  type t =
    | MInf
    | Linear of int * SymLinear.t
    | MinMax of int * Sign.t * MinMax.t * int * Symbol.t
    | PInf
  [@@deriving compare]

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
        F.fprintf fmt "%a(%d, %a)" MinMax.pp m d Symbol.pp x


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


  let eq_symbol : Symbol.t -> t -> bool =
   fun s -> function
    | Linear (0, se) -> (
      match SymLinear.get_one_symbol_opt se with None -> false | Some s' -> Symbol.equal s s' )
    | _ ->
        false


  let lift_symlinear : (SymLinear.t -> 'a option) -> t -> 'a option =
   fun f -> function Linear (0, se) -> f se | _ -> None


  let get_one_symbol_opt : t -> Symbol.t option = lift_symlinear SymLinear.get_one_symbol_opt

  let get_mone_symbol_opt : t -> Symbol.t option = lift_symlinear SymLinear.get_mone_symbol_opt

  let get_one_symbol : t -> Symbol.t =
   fun x -> match get_one_symbol_opt x with Some s -> s | None -> raise Not_one_symbol


  let get_mone_symbol : t -> Symbol.t =
   fun x -> match get_mone_symbol_opt x with Some s -> s | None -> raise Not_one_symbol


  let is_one_symbol : t -> bool = fun x -> get_one_symbol_opt x <> None

  let is_mone_symbol : t -> bool = fun x -> get_mone_symbol_opt x <> None

  let mk_MinMax (c, sign, m, d, s) =
    if Symbol.is_unsigned s && d <= 0 then
      match m with
      | Min ->
          of_int (Sign.eval_int sign c d)
      | Max ->
        match sign with
        | Plus ->
            Linear (c, SymLinear.singleton_one s)
        | Minus ->
            Linear (c, SymLinear.singleton_minus_one s)
    else MinMax (c, sign, m, d, s)


  let use_symbol : Symbol.t -> t -> bool =
   fun s -> function
    | PInf | MInf ->
        false
    | Linear (_, se) ->
        SymLinear.use_symbol s se
    | MinMax (_, _, _, _, s') ->
        Symbol.equal s s'


  type subst_pos_t = SubstLowerBound | SubstUpperBound

  (* [subst1] substitutes [s] in [x0] to [y0].

     - If the precise result is expressible by the domain, the
     function returns it.

     - If the precise result is not expressible, but a compromized
     value, with regard to [subst_pos], is expressible by the domain,
     the function returns the compromized value.

     - Otherwise, it returns the default values, -oo for lower bound and
     +oo for upper bound.  *)
  let subst1
      : subst_pos:subst_pos_t -> t bottom_lifted -> Symbol.t -> t bottom_lifted -> t bottom_lifted =
    let get_default = function SubstLowerBound -> MInf | SubstUpperBound -> PInf in
    let subst1_linears c1 se1 s c2 se2 =
      let coeff = SymLinear.find s se1 in
      let c' = c1 + ((coeff :> int) * c2) in
      let se1 = SymLinear.remove s se1 in
      let se' = SymLinear.mult_const_plus se2 coeff se1 in
      Linear (c', se')
    in
    let subst1_non_bottom ~subst_pos x s y =
      match (x, y) with
      | Linear (c1, se1), Linear (c2, se2) ->
          subst1_linears c1 se1 s c2 se2
      | Linear (c1, se1), MinMax (c2, sign, min_max, d2, s2) when SymLinear.is_one_symbol se1 ->
          assert (Symbol.equal (SymLinear.get_one_symbol se1) s) ;
          MinMax (c1 + c2, sign, min_max, d2, s2)
      | Linear (c1, se1), MinMax (c2, sign, min_max, d2, s2) when SymLinear.is_mone_symbol se1 ->
          assert (Symbol.equal (SymLinear.get_mone_symbol se1) s) ;
          MinMax (c1 - c2, Sign.neg sign, min_max, d2, s2)
      | Linear (c1, se1), MinMax (c2, bop, min_max, d2, _) ->
          let coeff = SymLinear.find s se1 in
          let compromisable =
            match (subst_pos, min_max) with
            | SubstLowerBound, Max | SubstUpperBound, Min ->
                NonZeroInt.is_positive coeff
            | SubstUpperBound, Max | SubstLowerBound, Min ->
                NonZeroInt.is_negative coeff
          in
          if compromisable then subst1_linears c1 se1 s (Sign.eval_int bop c2 d2) SymLinear.empty
          else get_default subst_pos
      | MinMax (_, Plus, Min, _, _), MInf ->
          MInf
      | MinMax (_, Minus, Min, _, _), MInf ->
          PInf
      | MinMax (_, Plus, Max, _, _), PInf ->
          PInf
      | MinMax (_, Minus, Max, _, _), PInf ->
          MInf
      | MinMax (c, sign, Min, d, _), PInf | MinMax (c, sign, Max, d, _), MInf ->
          of_int (Sign.eval_int sign c d)
      | MinMax (c1, sign, min_max, d1, _), Linear (c2, se) when SymLinear.is_zero se ->
          of_int (Sign.eval_int sign c1 (MinMax.eval_int min_max d1 c2))
      | MinMax (c, sign, m, d, _), _ when is_one_symbol y ->
          mk_MinMax (c, sign, m, d, get_one_symbol y)
      | MinMax (c, sign, m, d, _), _ when is_mone_symbol y ->
          mk_MinMax (c, Sign.neg sign, MinMax.neg m, -d, get_mone_symbol y)
      | MinMax (c1, sign1, min_max, d1, _), MinMax (c2, Plus, min_max', d2, s')
        when MinMax.equal min_max min_max' ->
          let c = Sign.eval_int sign1 c1 c2 in
          let d = MinMax.eval_int min_max (d1 - c2) d2 in
          mk_MinMax (c, sign1, min_max, d, s')
      | MinMax (c1, sign1, min_max, d1, _), MinMax (c2, Minus, min_max', d2, s')
        when MinMax.equal (MinMax.neg min_max) min_max' ->
          let c = Sign.eval_int sign1 c1 c2 in
          let d = MinMax.eval_int min_max' (c2 - d1) d2 in
          mk_MinMax (c, Sign.neg sign1, min_max', d, s')
      | _ ->
          get_default subst_pos
    in
    fun ~subst_pos x0 s y0 ->
      match (x0, y0) with
      | Bottom, _ ->
          x0
      | NonBottom x, _ when eq_symbol s x ->
          y0
      | NonBottom x, _ when not (use_symbol s x) ->
          x0
      | NonBottom _, Bottom ->
          NonBottom (get_default subst_pos)
      | NonBottom x, NonBottom y ->
          NonBottom (subst1_non_bottom ~subst_pos x s y)


  let int_ub_of_minmax = function
    | MinMax (c, Plus, Min, d, _) ->
        Some (c + d)
    | MinMax (c, Minus, Max, d, s) when Symbol.is_unsigned s ->
        Some (min c (c - d))
    | MinMax (c, Minus, Max, d, _) ->
        Some (c - d)
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


  let int_lb_of_minmax = function
    | MinMax (c, Plus, Max, d, s) when Symbol.is_unsigned s ->
        Some (max c (c + d))
    | MinMax (c, Plus, Max, d, _) ->
        Some (c + d)
    | MinMax (c, Minus, Min, d, _) ->
        Some (c - d)
    | MinMax _ ->
        None
    | MInf | PInf | Linear _ ->
        assert false


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
        c1 <= c2 && Int.equal d1 d2 && Symbol.equal x1 x2
    | MinMax _, MinMax _ when le_minmax_by_int x y ->
        true
    | MinMax (c1, Plus, Min, _, x1), MinMax (c2, Plus, Max, _, x2)
    | MinMax (c1, Minus, Max, _, x1), MinMax (c2, Minus, Min, _, x2) ->
        c1 <= c2 && Symbol.equal x1 x2
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
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2 ->
          mk_MinMax (c2, Minus, Max, c2 - c1, SymLinear.get_mone_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2 ->
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
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_zero x1 && SymLinear.is_mone_symbol x2 ->
          mk_MinMax (c2, Minus, Min, c2 - c1, SymLinear.get_mone_symbol x2)
      | Linear (c1, x1), Linear (c2, x2) when SymLinear.is_mone_symbol x1 && SymLinear.is_zero x2 ->
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


  (* substitution symbols in ``x'' with respect to ``map'' *)
  let subst : subst_pos:subst_pos_t -> t -> t bottom_lifted SymbolMap.t -> t bottom_lifted =
   fun ~subst_pos x map ->
    let subst_helper s y x =
      let y' =
        match y with
        | Bottom ->
            Bottom
        | NonBottom r ->
            NonBottom (if Symbol.is_unsigned s then ub ~default:r zero r else r)
      in
      subst1 ~subst_pos x s y'
    in
    SymbolMap.fold subst_helper map (NonBottom x)


  let subst_lb x map = subst ~subst_pos:SubstLowerBound x map

  let subst_ub x map = subst ~subst_pos:SubstUpperBound x map

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


  let mult_const : default:t -> NonZeroInt.t -> t -> t =
   fun ~default n x ->
    match x with
    | MInf ->
        if NonZeroInt.is_positive n then MInf else PInf
    | PInf ->
        if NonZeroInt.is_positive n then PInf else MInf
    | Linear (c, x') ->
        Linear (c * (n :> int), SymLinear.mult_const n x')
    | _ ->
        default


  let mult_const_l = mult_const ~default:MInf

  let mult_const_u = mult_const ~default:PInf

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


  let get_symbols : t -> Symbol.t list = function
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
end

type ('c, 's) valclass = Constant of 'c | Symbolic of 's | ValTop

(** A NonNegativeBound is a Bound that is either non-negative or symbolic but will be evaluated to a non-negative value once instantiated *)
module NonNegativeBound = struct
  type t = Bound.t [@@deriving compare]

  let pp = Bound.pp

  let zero = Bound.zero

  let of_bound b = if Bound.le b Bound.zero then Bound.zero else b

  let classify = function
    | Bound.PInf ->
        ValTop
    | Bound.MInf ->
        assert false
    | b ->
      match Bound.is_const b with
      | None ->
          Symbolic b
      | Some c ->
          Constant (NonNegativeInt.of_int_exn c)


  let subst b map =
    match Bound.subst_ub b map with
    | Bottom ->
        Constant NonNegativeInt.zero
    | NonBottom b ->
        of_bound b |> classify
end

module type NonNegativeSymbol = sig
  type t [@@deriving compare]

  val subst : t -> Bound.t bottom_lifted SymbolMap.t -> (NonNegativeInt.t, t) valclass

  val pp : F.formatter -> t -> unit
end

module MakePolynomial (S : NonNegativeSymbol) = struct
  module M = struct
    include Caml.Map.Make (S)

    let increasing_union ~f m1 m2 = union (fun _ v1 v2 -> Some (f v1 v2)) m1 m2

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

  let of_valclass : (NonNegativeInt.t, S.t) valclass -> t top_lifted = function
    | ValTop ->
        Top
    | Constant i ->
        NonTop (of_non_negative_int i)
    | Symbolic s ->
        NonTop {const= NonNegativeInt.zero; terms= M.singleton s one}


  let is_zero : t -> bool = fun {const; terms} -> NonNegativeInt.is_zero const && M.is_empty terms

  let is_one : t -> bool = fun {const; terms} -> NonNegativeInt.is_one const && M.is_empty terms

  let is_symbolic : t -> bool = fun {terms} -> not (M.is_empty terms)

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
    match PositiveInt.of_int (c :> int) with None -> zero | Some c -> mult_const_positive p c


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


  (* assumes symbols are not comparable *)
  let rec ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs ~rhs ->
    NonNegativeInt.( <= ) ~lhs:lhs.const ~rhs:rhs.const && M.le ~le_elt:( <= ) lhs.terms rhs.terms


  (* Possible optimization for later: x join x^2 = x^2 instead of x + x^2 *)
  let rec join : t -> t -> t =
   fun p1 p2 ->
    { const= NonNegativeInt.max p1.const p2.const
    ; terms= M.increasing_union ~f:join p1.terms p2.terms }


  (* assumes symbols are not comparable *)
  (* TODO: improve this for comparable symbols *)
  let min : t -> t -> t =
   fun p1 p2 ->
    if ( <= ) ~lhs:p1 ~rhs:p2 then p1
    else (* either can't decide which one is smaller or p2 is smaller  *)
      p2


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:_ ~next:_ ~num_iters:_ -> assert false


  let subst =
    let exception ReturnTop in
    (* avoids top-lifting everything *)
    let rec subst {const; terms} map =
      M.fold
        (fun s p acc ->
          match S.subst s map with
          | Constant c -> (
            match PositiveInt.of_int (c :> int) with
            | None ->
                acc
            | Some c ->
                let p = subst p map in
                mult_const_positive p c |> plus acc )
          | ValTop ->
              let p = subst p map in
              if is_zero p then acc else raise ReturnTop
          | Symbolic s ->
              let p = subst p map in
              mult_symb p s |> plus acc )
        terms (of_non_negative_int const)
    in
    fun p map -> match subst p map with p -> NonTop p | exception ReturnTop -> Top


  let pp : F.formatter -> t -> unit =
    let add_symb s (((last_s, last_occ) as last), others) =
      if Int.equal 0 (S.compare s last_s) then ((last_s, PositiveInt.succ last_occ), others)
      else ((s, PositiveInt.one), last :: others)
    in
    let pp_coeff fmt (c: NonNegativeInt.t) =
      if (c :> int) > 1 then F.fprintf fmt "%a * " NonNegativeInt.pp c
    in
    let pp_exp fmt (e: PositiveInt.t) =
      if (e :> int) > 1 then F.fprintf fmt "^%a" PositiveInt.pp e
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
    let rec pp_sub symbs fmt {const; terms} =
      if not (NonNegativeInt.is_zero const) then
        F.fprintf fmt " + %a%a" pp_coeff const pp_symbs symbs ;
      M.iter (fun s p -> pp_sub (add_symb s symbs) fmt p) terms
    in
    fun fmt {const; terms} ->
      NonNegativeInt.pp fmt const ;
      M.iter (fun s p -> pp_sub ((s, PositiveInt.one), []) fmt p) terms
end

module NonNegativePolynomial = struct
  module NonNegativeNonTopPolynomial = MakePolynomial (NonNegativeBound)
  include AbstractDomain.TopLifted (NonNegativeNonTopPolynomial)

  let zero = NonTop NonNegativeNonTopPolynomial.zero

  let one = NonTop NonNegativeNonTopPolynomial.one

  let of_int_exn i = NonTop (NonNegativeNonTopPolynomial.of_int_exn i)

  let of_non_negative_bound b =
    b |> NonNegativeBound.classify |> NonNegativeNonTopPolynomial.of_valclass


  let is_symbolic = function Top -> false | NonTop p -> NonNegativeNonTopPolynomial.is_symbolic p

  let is_top = function Top -> true | _ -> false

  let top_lifted_increasing ~f p1 p2 =
    match (p1, p2) with Top, _ | _, Top -> Top | NonTop p1, NonTop p2 -> NonTop (f p1 p2)


  let plus = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.plus

  let mult = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.mult

  let min p1 p2 =
    match (p1, p2) with
    | Top, x | x, Top ->
        x
    | NonTop p1, NonTop p2 ->
        NonTop (NonNegativeNonTopPolynomial.min p1 p2)


  let widen ~prev ~next ~num_iters:_ = if ( <= ) ~lhs:next ~rhs:prev then prev else Top

  let subst p map = match p with Top -> Top | NonTop p -> NonNegativeNonTopPolynomial.subst p map
end

module ItvRange = struct
  type t = NonNegativeBound.t

  let zero : t = NonNegativeBound.zero

  let of_bounds : lb:Bound.t -> ub:Bound.t -> t =
   fun ~lb ~ub ->
    Bound.plus_u ub Bound.one |> Bound.plus_u (Bound.neg lb) |> NonNegativeBound.of_bound


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

  let subst : t -> Bound.t bottom_lifted SymbolMap.t -> t bottom_lifted =
   fun (l, u) map ->
    match (Bound.subst_lb l map, Bound.subst_ub u map) with
    | NonBottom l, NonBottom u ->
        NonBottom (l, u)
    | _ ->
        Bottom


  let ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs:(l1, u1) ~rhs:(l2, u2) -> Bound.le l2 l1 && Bound.le u1 u2


  let xcompare ~lhs:(l1, u1) ~rhs:(l2, u2) =
    let lcmp = Bound.xcompare ~lhs:l1 ~rhs:l2 in
    let ucmp = Bound.xcompare ~lhs:u1 ~rhs:u2 in
    match (lcmp, ucmp) with
    | `Equal, `Equal ->
        `Equal
    | `NotComparable, _ | _, `NotComparable -> (
      match Bound.xcompare ~lhs:u1 ~rhs:l2 with
      | `LeftSmallerThanRight ->
          `LeftSmallerThanRight
      | u1l2 ->
        match (Bound.xcompare ~lhs:u2 ~rhs:l1, u1l2) with
        | `LeftSmallerThanRight, _ ->
            `RightSmallerThanLeft
        | `Equal, `Equal ->
            `Equal (* weird, though *)
        | _, `Equal ->
            `LeftSmallerThanRight
        | _ ->
            `NotComparable )
    | (`LeftSmallerThanRight | `Equal), (`LeftSmallerThanRight | `Equal) ->
        `LeftSmallerThanRight
    | (`RightSmallerThanLeft | `Equal), (`RightSmallerThanLeft | `Equal) ->
        `RightSmallerThanLeft
    | `LeftSmallerThanRight, `RightSmallerThanLeft ->
        `LeftSubsumesRight
    | `RightSmallerThanLeft, `LeftSmallerThanRight ->
        `RightSubsumesLeft


  let join : t -> t -> t = fun (l1, u1) (l2, u2) -> (Bound.min_l l1 l2, Bound.max_u u1 u2)

  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:(l1, u1) ~next:(l2, u2) ~num_iters:_ -> (Bound.widen_l l1 l2, Bound.widen_u u1 u2)


  let pp : F.formatter -> t -> unit =
   fun fmt (l, u) -> F.fprintf fmt "[%a, %a]" Bound.pp l Bound.pp u


  let of_bound bound = (bound, bound)

  let of_int n = of_bound (Bound.of_int n)

  let make_sym : unsigned:bool -> Typ.Procname.t -> Counter.t -> t =
   fun ~unsigned pname new_sym_num ->
    let lower = Bound.of_sym (SymLinear.make ~unsigned pname (Counter.next new_sym_num)) in
    let upper = Bound.of_sym (SymLinear.make ~unsigned pname (Counter.next new_sym_num)) in
    (lower, upper)


  let mone = of_bound Bound.mone

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
    | Some n, Some m when Int.equal n m ->
        Some n
    | _, _ ->
        None


  let is_zero : t -> bool = fun (l, u) -> Bound.is_zero l && Bound.is_zero u

  let is_true : t -> bool = fun (l, u) -> Bound.le Bound.one l || Bound.le u Bound.mone

  let is_false : t -> bool = is_zero

  let is_symbolic : t -> bool = fun (lb, ub) -> Bound.is_symbolic lb || Bound.is_symbolic ub

  let is_ge_zero : t -> bool = fun (lb, _) -> Bound.le Bound.zero lb

  let is_le_zero : t -> bool = fun (_, ub) -> Bound.le ub Bound.zero

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

  let mult_const : int -> t -> t =
   fun n ((l, u) as itv) ->
    match NonZeroInt.of_int n with
    | None ->
        zero
    | Some n ->
        if NonZeroInt.is_one n then itv
        else if NonZeroInt.is_minus_one n then neg itv
        else if NonZeroInt.is_positive n then (Bound.mult_const_l n l, Bound.mult_const_u n u)
        else (Bound.mult_const_l n u, Bound.mult_const_u n l)


  (* Returns a precise value only when all coefficients are divided by
     n without remainder. *)
  let div_const : t -> int -> t =
   fun ((l, u) as itv) n ->
    match NonZeroInt.of_int n with
    | None ->
        top
    | Some n ->
        if NonZeroInt.is_one n then itv
        else if NonZeroInt.is_minus_one n then neg itv
        else if NonZeroInt.is_positive n then
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
    | Some 0 ->
        x (* x % [0,0] does nothing. *)
    | Some m ->
      match is_const x with
      | Some n ->
          of_int (n mod m)
      | None ->
          let abs_m = abs m in
          if is_ge_zero x then (Bound.zero, Bound.of_int (abs_m - 1))
          else if is_le_zero x then (Bound.of_int (-abs_m + 1), Bound.zero)
          else (Bound.of_int (-abs_m + 1), Bound.of_int (abs_m - 1))


  (* x << [-1,-1] does nothing. *)
  let shiftlt : t -> t -> t =
   fun x y -> match is_const y with Some n -> mult_const (1 lsl n) x | None -> top


  (* x >> [-1,-1] does nothing. *)
  let shiftrt : t -> t -> t =
   fun x y ->
    match is_const y with
    | Some n when Int.( <= ) n 0 ->
        x
    | Some n when n >= 64 ->
        zero
    | Some n ->
        div_const x (1 lsl n)
    | None ->
        top


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


  let min_sem : t -> t -> t = fun (l1, u1) (l2, u2) -> (Bound.min_l l1 l2, Bound.min_u u1 u2)

  let is_invalid : t -> bool = function
    | Bound.PInf, _ | _, Bound.MInf ->
        true
    | l, u ->
        Bound.lt u l


  let normalize : t -> t bottom_lifted = fun x -> if is_invalid x then Bottom else NonBottom x

  let prune_le : t -> t -> t =
   fun x y ->
    match (x, y) with
    | (l1, Bound.PInf), (_, u2) ->
        (l1, u2)
    | (l1, Bound.Linear (c1, s1)), (_, Bound.Linear (c2, s2)) when SymLinear.eq s1 s2 ->
        (l1, Bound.Linear (min c1 c2, s1))
    | (l1, Bound.Linear (c, se)), (_, u) when SymLinear.is_zero se && Bound.is_one_symbol u ->
        (l1, Bound.mk_MinMax (0, Bound.Plus, Bound.Min, c, Bound.get_one_symbol u))
    | (l1, u), (_, Bound.Linear (c, se)) when SymLinear.is_zero se && Bound.is_one_symbol u ->
        (l1, Bound.mk_MinMax (0, Bound.Plus, Bound.Min, c, Bound.get_one_symbol u))
    | (l1, Bound.Linear (c, se)), (_, u) when SymLinear.is_zero se && Bound.is_mone_symbol u ->
        (l1, Bound.mk_MinMax (0, Bound.Minus, Bound.Max, -c, Bound.get_mone_symbol u))
    | (l1, u), (_, Bound.Linear (c, se)) when SymLinear.is_zero se && Bound.is_mone_symbol u ->
        (l1, Bound.mk_MinMax (0, Bound.Minus, Bound.Max, -c, Bound.get_mone_symbol u))
    | (l1, Bound.Linear (c1, se)), (_, Bound.MinMax (c2, Bound.Plus, Bound.Min, d2, se'))
    | (l1, Bound.MinMax (c2, Bound.Plus, Bound.Min, d2, se')), (_, Bound.Linear (c1, se))
      when SymLinear.is_zero se ->
        (l1, Bound.mk_MinMax (c2, Bound.Plus, Bound.Min, min (c1 - c2) d2, se'))
    | ( (l1, Bound.MinMax (c1, Bound.Plus, Bound.Min, d1, se1))
      , (_, Bound.MinMax (c2, Bound.Plus, Bound.Min, d2, se2)) )
      when Int.equal c1 c2 && Symbol.equal se1 se2 ->
        (l1, Bound.mk_MinMax (c1, Bound.Plus, Bound.Min, min d1 d2, se1))
    | _ ->
        x


  let prune_ge : t -> t -> t =
   fun x y ->
    match (x, y) with
    | (Bound.MInf, u1), (l2, _) ->
        (l2, u1)
    | (Bound.Linear (c1, s1), u1), (Bound.Linear (c2, s2), _) when SymLinear.eq s1 s2 ->
        (Bound.Linear (max c1 c2, s1), u1)
    | (Bound.Linear (c, se), u1), (l, _) when SymLinear.is_zero se && Bound.is_one_symbol l ->
        (Bound.mk_MinMax (0, Bound.Plus, Bound.Max, c, Bound.get_one_symbol l), u1)
    | (l, u1), (Bound.Linear (c, se), _) when SymLinear.is_zero se && Bound.is_one_symbol l ->
        (Bound.mk_MinMax (0, Bound.Plus, Bound.Max, c, Bound.get_one_symbol l), u1)
    | (Bound.Linear (c, se), u1), (l, _) when SymLinear.is_zero se && Bound.is_mone_symbol l ->
        (Bound.mk_MinMax (0, Bound.Minus, Bound.Min, c, Bound.get_mone_symbol l), u1)
    | (l, u1), (Bound.Linear (c, se), _) when SymLinear.is_zero se && Bound.is_mone_symbol l ->
        (Bound.mk_MinMax (0, Bound.Minus, Bound.Min, c, Bound.get_mone_symbol l), u1)
    | (Bound.Linear (c1, se), u1), (Bound.MinMax (c2, Bound.Plus, Bound.Max, d2, se'), _)
    | (Bound.MinMax (c2, Bound.Plus, Bound.Max, d2, se'), u1), (Bound.Linear (c1, se), _)
      when SymLinear.is_zero se ->
        (Bound.mk_MinMax (c2, Bound.Plus, Bound.Max, max (c1 - c2) d2, se'), u1)
    | ( (Bound.MinMax (c1, Bound.Plus, Bound.Max, d1, se1), u1)
      , (Bound.MinMax (c2, Bound.Plus, Bound.Max, d2, se2), _) )
      when Int.equal c1 c2 && Symbol.equal se1 se2 ->
        (Bound.mk_MinMax (c1, Bound.Plus, Bound.Max, max d1 d2, se1), u1)
    | _ ->
        x


  let prune_lt : t -> t -> t = fun x y -> prune_le x (minus y one)

  let prune_gt : t -> t -> t = fun x y -> prune_ge x (plus y one)

  let prune_diff : t -> Bound.t -> t bottom_lifted =
   fun ((l, u) as itv) b ->
    if Bound.eq l b then normalize (Bound.plus_l l Bound.one, u)
    else if Bound.eq u b then normalize (l, Bound.plus_u u Bound.mone)
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


  let get_symbols : t -> Symbol.t list =
   fun (l, u) -> List.append (Bound.get_symbols l) (Bound.get_symbols u)


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

let of_int_lit n = try of_int (IntLit.to_int n) with _ -> top

let of_int64 : Int64.t -> astate =
 fun n -> Int64.to_int n |> Option.value_map ~f:of_int ~default:top


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

let make_sym : ?unsigned:bool -> Typ.Procname.t -> Counter.t -> t =
 fun ?(unsigned= false) pname new_sym_num ->
  NonBottom (ItvPure.make_sym ~unsigned pname new_sym_num)


let neg : t -> t = lift1 ItvPure.neg

let lnot : t -> Boolean.t = bind1b ItvPure.lnot

let mult : t -> t -> t = lift2 ItvPure.mult

let div : t -> t -> t = lift2 ItvPure.div

let mod_sem : t -> t -> t = lift2 ItvPure.mod_sem

let shiftlt : t -> t -> t = lift2 ItvPure.shiftlt

let shiftrt : t -> t -> t = lift2 ItvPure.shiftrt

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

let subst : t -> Bound.t bottom_lifted SymbolMap.t -> t =
 fun x map -> match x with NonBottom x' -> ItvPure.subst x' map | _ -> x


let get_symbols : t -> Symbol.t list = function
  | Bottom ->
      []
  | NonBottom x ->
      ItvPure.get_symbols x


let normalize : t -> t = bind1 ItvPure.normalize
