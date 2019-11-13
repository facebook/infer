(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

  val classify : t -> (Ints.NonNegativeInt.t, t, Bounds.BoundTrace.t) Bounds.valclass

  val int_lb : t -> NonNegativeInt.t

  val int_ub : t -> NonNegativeInt.t option

  val mask_min_max_constant : t -> t

  val subst :
       Typ.Procname.t
    -> Location.t
    -> t
    -> Bound.eval_sym
    -> (NonNegativeInt.t, t, Bounds.BoundTrace.t) Bounds.valclass

  val pp : hum:bool -> F.formatter -> t -> unit
end

module type NonNegativeSymbolWithDegreeKind = sig
  type t0

  include NonNegativeSymbol

  val make : DegreeKind.t -> t0 -> t

  val degree_kind : t -> DegreeKind.t

  val symbol : t -> t0
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
    | ValTop trace ->
        Bounds.ValTop trace


  let mask_min_max_constant {degree_kind; symbol} =
    {degree_kind; symbol= S.mask_min_max_constant symbol}


  let make degree_kind symbol = {degree_kind; symbol}

  let int_lb {degree_kind; symbol} = S.int_lb symbol |> DegreeKind.compute degree_kind

  let int_ub {degree_kind; symbol} =
    S.int_ub symbol |> Option.map ~f:(DegreeKind.compute degree_kind)


  let subst callee_pname location {degree_kind; symbol} eval =
    match S.subst callee_pname location symbol eval with
    | Constant c ->
        Bounds.Constant (DegreeKind.compute degree_kind c)
    | Symbolic symbol ->
        Bounds.Symbolic {degree_kind; symbol}
    | ValTop trace ->
        Logging.d_printfln_escaped "subst(%a) became top." (S.pp ~hum:false) symbol ;
        Bounds.ValTop trace


  let pp ~hum f {degree_kind; symbol} = DegreeKind.pp_hole (S.pp ~hum) f degree_kind symbol

  let degree_kind {degree_kind} = degree_kind

  let symbol {symbol} = symbol
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

  let of_valclass : (NonNegativeInt.t, S.t, 't) Bounds.valclass -> (t, 't) below_above = function
    | ValTop trace ->
        Above trace
    | Constant i ->
        Below (of_non_negative_int i)
    | Symbolic s ->
        Below {const= NonNegativeInt.zero; terms= M.singleton s one}


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
  let rec leq : lhs:t -> rhs:t -> bool =
   fun ~lhs ~rhs ->
    phys_equal lhs rhs
    || (NonNegativeInt.leq ~lhs:lhs.const ~rhs:rhs.const && M.le ~le_elt:leq lhs.terms rhs.terms)
    || Option.exists (int_ub lhs) ~f:(fun lhs_ub -> NonNegativeInt.leq ~lhs:lhs_ub ~rhs:(int_lb rhs))


  let rec xcompare ~lhs ~rhs =
    let cmp_const =
      PartialOrder.of_compare ~compare:NonNegativeInt.compare ~lhs:lhs.const ~rhs:rhs.const
    in
    let cmp_terms = M.xcompare ~xcompare_elt:xcompare ~lhs:lhs.terms ~rhs:rhs.terms in
    PartialOrder.join cmp_const cmp_terms


  let rec mask_min_max_constant {const; terms} =
    { const
    ; terms=
        M.fold
          (fun s p acc ->
            let p' = mask_min_max_constant p in
            M.update (S.mask_min_max_constant s)
              (function None -> Some p' | Some p -> if leq ~lhs:p ~rhs:p' then Some p' else Some p)
              acc )
          terms M.empty }


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


  let subst callee_pname location =
    let exception ReturnTop of (S.t * Bounds.BoundTrace.t) in
    (* avoids top-lifting everything *)
    let rec subst {const; terms} eval_sym =
      M.fold
        (fun s p acc ->
          match S.subst callee_pname location s eval_sym with
          | Constant c -> (
            match PositiveInt.of_big_int (c :> Z.t) with
            | None ->
                acc
            | Some c ->
                let p = subst p eval_sym in
                mult_const_positive p c |> plus acc )
          | ValTop trace ->
              let p = subst p eval_sym in
              if is_zero p then acc else raise (ReturnTop (s, trace))
          | Symbolic s ->
              let p = subst p eval_sym in
              mult_symb p s |> plus acc )
        terms (of_non_negative_int const)
    in
    fun p eval_sym ->
      match subst p eval_sym with p -> Below p | exception ReturnTop s_trace -> Above s_trace


  (** Emit a pair (d,t) where d is the degree of the polynomial and t is the first term with such degree *)
  let rec degree_with_term {terms} =
    M.fold
      (fun t p acc ->
        let d, p' = degree_with_term p in
        max acc (Degree.succ (S.degree_kind t) d, mult_symb p' t) )
      terms (Degree.zero, one)


  let degree p = fst (degree_with_term p)

  let multiplication_sep = F.sprintf " %s " SpecialChars.multiplication_sign

  let pp : hum:bool -> F.formatter -> t -> unit =
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
    let pp_symb ~hum fmt symb = pp_magic_parentheses (S.pp ~hum) fmt symb in
    let pp_symb_exp ~hum fmt (symb, exp) = F.fprintf fmt "%a%a" (pp_symb ~hum) symb pp_exp exp in
    let pp_symbs ~hum fmt (last, others) =
      List.rev_append others [last] |> Pp.seq ~sep:multiplication_sep (pp_symb_exp ~hum) fmt
    in
    let rec pp_sub ~hum ~print_plus symbs fmt {const; terms} =
      let print_plus =
        if not (NonNegativeInt.is_zero const) then (
          if print_plus then F.pp_print_string fmt " + " ;
          F.fprintf fmt "%a%a" pp_coeff const (pp_symbs ~hum) symbs ;
          true )
        else print_plus
      in
      ( M.fold
          (fun s p print_plus ->
            pp_sub ~hum ~print_plus (add_symb s symbs) fmt p ;
            true )
          terms print_plus
        : bool )
      |> ignore
    in
    fun ~hum fmt {const; terms} ->
      let const_not_zero = not (NonNegativeInt.is_zero const) in
      if const_not_zero || M.is_empty terms then NonNegativeInt.pp fmt const ;
      ( M.fold
          (fun s p print_plus ->
            pp_sub ~hum ~print_plus ((s, PositiveInt.one), []) fmt p ;
            true )
          terms const_not_zero
        : bool )
      |> ignore


  let get_symbols p : S.t0 list =
    let rec get_symbols_sub {terms} acc =
      M.fold (fun s p acc -> get_symbols_sub p (S.symbol s :: acc)) terms acc
    in
    get_symbols_sub p []
end

module NonNegativeBoundWithDegreeKind = MakeSymbolWithDegreeKind (Bounds.NonNegativeBound)
module NonNegativeNonTopPolynomial = MakePolynomial (NonNegativeBoundWithDegreeKind)

module TopTrace = struct
  module S = NonNegativeBoundWithDegreeKind

  type t =
    | UnboundedLoop of {bound_trace: Bounds.BoundTrace.t}
    | UnboundedSymbol of {location: Location.t; symbol: S.t; bound_trace: Bounds.BoundTrace.t}
    | Call of {location: Location.t; callee_pname: Typ.Procname.t; callee_trace: t}
  [@@deriving compare]

  let rec length = function
    | UnboundedLoop {bound_trace} | UnboundedSymbol {bound_trace} ->
        1 + Bounds.BoundTrace.length bound_trace
    | Call {callee_trace} ->
        1 + length callee_trace


  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let unbounded_loop bound_trace = UnboundedLoop {bound_trace}

  let unbounded_symbol ~location ~symbol bound_trace =
    UnboundedSymbol {location; symbol; bound_trace}


  let call ~location ~callee_pname callee_trace = Call {location; callee_pname; callee_trace}

  let rec pp f = function
    | UnboundedLoop {bound_trace} ->
        F.fprintf f "%a -> UnboundedLoop" Bounds.BoundTrace.pp bound_trace
    | UnboundedSymbol {location; symbol; bound_trace} ->
        F.fprintf f "%a -> UnboundedSymbol (%a): %a" Bounds.BoundTrace.pp bound_trace Location.pp
          location (S.pp ~hum:false) symbol
    | Call {callee_pname; callee_trace; location} ->
        F.fprintf f "%a -> Call `%a` (%a)" pp callee_trace Typ.Procname.pp callee_pname Location.pp
          location


  let rec make_err_trace ~depth trace =
    match trace with
    | UnboundedLoop {bound_trace} ->
        let bound_err_trace = Bounds.BoundTrace.make_err_trace ~depth bound_trace in
        [("Unbounded loop", bound_err_trace)] |> Errlog.concat_traces
    | UnboundedSymbol {location; symbol; bound_trace} ->
        let desc = F.asprintf "Unbounded value %a" (S.pp ~hum:true) symbol in
        Errlog.make_trace_element depth location desc []
        :: Bounds.BoundTrace.make_err_trace ~depth bound_trace
    | Call {location; callee_pname; callee_trace} ->
        let desc = F.asprintf "Call to %a" Typ.Procname.pp callee_pname in
        Errlog.make_trace_element depth location desc []
        :: make_err_trace ~depth:(depth + 1) callee_trace
end

module TopTraces = struct
  include AbstractDomain.MinReprSet (TopTrace)

  let make_err_trace traces =
    match min_elt traces with None -> [] | Some trace -> TopTrace.make_err_trace ~depth:0 trace
end

module NonNegativePolynomial = struct
  (* Use Below for non-Top values and Above for Top values with their trace *)
  type t = (NonNegativeNonTopPolynomial.t, TopTraces.t) below_above

  type degree_with_term =
    (Degree.t * NonNegativeNonTopPolynomial.t, TopTraces.t) AbstractDomain.Types.below_above

  let leq =
    AbstractDomain.StackedUtils.leq ~leq_below:NonNegativeNonTopPolynomial.leq
      ~leq_above:TopTraces.leq


  let pp ~hum =
    let pp_above f traces =
      AbstractDomain.TopLiftedUtils.pp_top f ;
      if not hum then F.fprintf f ": %a" TopTraces.pp traces
    in
    AbstractDomain.StackedUtils.pp ~pp_below:(NonNegativeNonTopPolynomial.pp ~hum) ~pp_above


  let pp_hum = pp ~hum:true

  let pp = pp ~hum:false

  let top = Above TopTraces.bottom

  let zero = Below NonNegativeNonTopPolynomial.zero

  let one = Below NonNegativeNonTopPolynomial.one

  let of_int_exn i = Below (NonNegativeNonTopPolynomial.of_int_exn i)

  let make_trace_set ~map_above =
    AbstractDomain.StackedUtils.map ~f_below:Fn.id ~f_above:(fun above ->
        TopTraces.singleton (map_above above) )


  let of_non_negative_bound ?(degree_kind = DegreeKind.Linear) b =
    b
    |> NonNegativeBoundWithDegreeKind.make degree_kind
    |> NonNegativeBoundWithDegreeKind.classify |> NonNegativeNonTopPolynomial.of_valclass
    |> make_trace_set ~map_above:TopTrace.unbounded_loop


  let is_symbolic = function
    | Above _ ->
        false
    | Below p ->
        NonNegativeNonTopPolynomial.is_symbolic p


  let is_top = function Above _ -> true | _ -> false

  let is_zero = function Below p when NonNegativeNonTopPolynomial.is_zero p -> true | _ -> false

  let is_one = function Below p when NonNegativeNonTopPolynomial.is_one p -> true | _ -> false

  let top_lifted_increasing ~f p1 p2 =
    AbstractDomain.StackedUtils.combine ~dir:`Increasing p1 p2 ~f_below:f ~f_above:TopTraces.join


  let plus = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.plus

  let mult = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.mult

  let min_default_left p1 p2 =
    AbstractDomain.StackedUtils.combine ~dir:`Decreasing p1 p2
      ~f_below:NonNegativeNonTopPolynomial.min_default_left ~f_above:TopTraces.join


  let subst callee_pname location p eval_sym =
    match p with
    | Above callee_traces ->
        Above
          (TopTraces.map
             (fun callee_trace -> TopTrace.call ~callee_pname ~location callee_trace)
             callee_traces)
    | Below p ->
        NonNegativeNonTopPolynomial.subst callee_pname location p eval_sym
        |> make_trace_set ~map_above:(fun (symbol, bound_trace) ->
               TopTrace.unbounded_symbol ~location ~symbol bound_trace )


  let degree p =
    match p with Above _ -> None | Below p -> Some (NonNegativeNonTopPolynomial.degree p)


  let compare_by_degree =
    let cmp_above _ _ = 0 (* All Top should be considered equal *) in
    AbstractDomain.StackedUtils.compare ~cmp_above ~cmp_below:(fun p1 p2 ->
        Degree.compare
          (NonNegativeNonTopPolynomial.degree p1)
          (NonNegativeNonTopPolynomial.degree p2) )


  let get_degree_with_term =
    AbstractDomain.StackedUtils.map ~f_below:NonNegativeNonTopPolynomial.degree_with_term
      ~f_above:Fn.id


  let get_symbols =
    AbstractDomain.StackedUtils.map ~f_below:NonNegativeNonTopPolynomial.get_symbols ~f_above:Fn.id


  let pp_degree ~only_bigO fmt = function
    | Above _ ->
        Format.pp_print_string fmt "Top"
    | Below (degree, degree_term) ->
        if only_bigO then
          Format.fprintf fmt "O(%a)"
            (NonNegativeNonTopPolynomial.pp ~hum:true)
            (NonNegativeNonTopPolynomial.mask_min_max_constant degree_term)
        else Degree.pp fmt degree


  let degree_str p =
    match degree p with
    | Some degree ->
        Format.asprintf ", degree = %a" Degree.pp degree
    | None ->
        ""


  let polynomial_traces p =
    match get_symbols p with
    | Below symbols ->
        List.map symbols ~f:Bounds.NonNegativeBound.make_err_trace |> Errlog.concat_traces
    | Above trace ->
        TopTraces.make_err_trace trace


  let encode astate = Marshal.to_string astate [] |> Base64.encode_exn

  let decode enc_str = Marshal.from_string (Base64.decode_exn enc_str) 0
end
