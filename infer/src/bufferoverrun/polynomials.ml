(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
open Ints
open Bounds

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


  let pp f d =
    NonNegativeInt.pp f d.linear ;
    if not (NonNegativeInt.is_zero d.log) then
      F.fprintf f " + %a%slog" NonNegativeInt.pp d.log SpecialChars.dot_operator


  let is_constant {linear; log} = NonNegativeInt.is_zero linear && NonNegativeInt.is_zero log
end

module NonNegativeBoundWithDegreeKind = struct
  type t = {degree_kind: DegreeKind.t; symbol: NonNegativeBound.t} [@@deriving compare]

  let classify ({degree_kind; symbol} as self) =
    match NonNegativeBound.classify symbol with
    | Constant c ->
        Constant (DegreeKind.compute degree_kind c)
    | Symbolic _ ->
        Symbolic self
    | ValTop trace ->
        ValTop trace


  let mask_min_max_constant {degree_kind; symbol} =
    {degree_kind; symbol= NonNegativeBound.mask_min_max_constant symbol}


  let make degree_kind symbol = {degree_kind; symbol}

  let int_lb {degree_kind; symbol} =
    NonNegativeBound.int_lb symbol |> DegreeKind.compute degree_kind


  let int_ub {degree_kind; symbol} =
    NonNegativeBound.int_ub symbol |> Option.map ~f:(DegreeKind.compute degree_kind)


  let subst callee_pname location {degree_kind; symbol} eval =
    match NonNegativeBound.subst callee_pname location symbol eval with
    | Constant c ->
        Constant (DegreeKind.compute degree_kind c)
    | Symbolic symbol ->
        Symbolic {degree_kind; symbol}
    | ValTop trace ->
        Logging.d_printfln_escaped "subst(%a) became top." (NonNegativeBound.pp ~hum:false) symbol ;
        ValTop trace


  let pp ~hum f {degree_kind; symbol} =
    DegreeKind.pp_hole (NonNegativeBound.pp ~hum) f degree_kind symbol


  let degree_kind {degree_kind} = degree_kind

  let symbol {symbol} = symbol

  let split_mult {degree_kind; symbol} =
    Option.map (NonNegativeBound.split_mult symbol) ~f:(fun (s1, s2) ->
        (make degree_kind s1, make degree_kind s2) )


  let make_err_trace_symbol symbol = NonNegativeBound.make_err_trace symbol
end

let pp_magic_parentheses pp fmt x =
  let s = F.asprintf "%a" pp x in
  if String.contains s ' ' then F.fprintf fmt "(%s)" s else F.pp_print_string fmt s


module NonNegativeNonTopPolynomial = struct
  module Key = struct
    type t =
      | NonNegativeBoundWithDegreeKind of NonNegativeBoundWithDegreeKind.t
      | FuncPtr of Symb.SymbolPath.partial
    [@@deriving compare]

    let lift_valclass = function
      | Symbolic s ->
          Symbolic (NonNegativeBoundWithDegreeKind s)
      | (Constant _ | ValTop _) as x ->
          x


    let pp_hum ~hum f = function
      | NonNegativeBoundWithDegreeKind bound ->
          pp_magic_parentheses (NonNegativeBoundWithDegreeKind.pp ~hum) f bound
      | FuncPtr partial ->
          F.fprintf f "|%a|" Symb.SymbolPath.pp_partial partial


    let pp = pp_hum ~hum:true

    let is_func_ptr = function FuncPtr _ -> true | NonNegativeBoundWithDegreeKind _ -> false
  end

  module M = struct
    include PrettyPrintable.MakePPMap (Key)

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

  (** If x < y < z then [2 + 3 * x + 4 * x ^ 2 + x * y + 7 * y ^ 2 * z] is represented by

      {[
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
      ]}

      The representation is a tree, each edge from a node to a child (terms) represents a
      multiplication by a symbol. If a node has a non-zero const, it represents the multiplication
      (of the path) by this constant. In the example above, we have the following paths:

      - [2]
      - [x * 3]
      - [x * x * 4]
      - [x * y * 1]
      - [y * y * z * 7]

      Invariants:

      - except for the root, [terms <> {} || const <> 0]
      - symbols children of a term are 'smaller' than its self symbol
      - contents of terms are not zero
      - symbols in terms are only symbolic values *)
  type t = {const: NonNegativeInt.t; terms: t M.t} [@@deriving compare]

  let of_non_negative_int (const : NonNegativeInt.t) = {const; terms= M.empty}

  let zero = of_non_negative_int NonNegativeInt.zero

  let one = of_non_negative_int NonNegativeInt.one

  let of_int_exn i = NonNegativeInt.of_int_exn i |> of_non_negative_int

  let is_zero {const; terms} = NonNegativeInt.is_zero const && M.is_empty terms

  let is_one {const; terms} = NonNegativeInt.is_one const && M.is_empty terms

  let is_constant {terms} = M.is_empty terms

  let is_symbolic : t -> bool = fun p -> not (is_constant p)

  let rec plus p1 p2 =
    { const= NonNegativeInt.(p1.const + p2.const)
    ; terms= M.increasing_union ~f:plus p1.terms p2.terms }


  let rec mult_const_positive {const; terms} (c : PositiveInt.t) =
    { const= NonNegativeInt.(const * (c :> NonNegativeInt.t))
    ; terms= M.map (fun p -> mult_const_positive p c) terms }


  let mult_const p (c : NonNegativeInt.t) =
    match PositiveInt.of_big_int (c :> Z.t) with None -> zero | Some c -> mult_const_positive p c


  (* (c + r * R + s * S + t * T) x s
     = 0 + r * (R x s) + s * (c + s * S + t * T) *)
  let rec mult_symb_aux {const; terms} (s : Key.t) =
    let less_than_s, equal_s_opt, greater_than_s = M.split s terms in
    let less_than_s = M.map (fun p -> mult_symb_aux p s) less_than_s in
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


  let rec mult p1 p2 =
    if is_zero p1 || is_zero p2 then zero
    else if is_one p1 then p2
    else if is_one p2 then p1
    else
      mult_const p1 p2.const
      |> M.fold (fun s p acc -> plus (mult_symb_aux (mult p p1) s) acc) p2.terms


  (** It takes only the trace of the body part, because the trace for the iteration number will be
      taken later from symbolic values. *)
  let mult_loop ~iter ~body = mult iter body

  let singleton key = {const= NonNegativeInt.zero; terms= M.singleton key one}

  let of_func_ptr path = singleton (FuncPtr path)

  let rec of_valclass : (NonNegativeInt.t, Key.t, 't) valclass -> ('t, t, 't) below_above = function
    | ValTop trace ->
        Above trace
    | Constant i ->
        Val (of_non_negative_int i)
    | Symbolic (NonNegativeBoundWithDegreeKind s as key) -> (
      match NonNegativeBoundWithDegreeKind.split_mult s with
      | None ->
          Val (singleton key)
      | Some (s1, s2) -> (
        match
          ( of_valclass (Key.lift_valclass (NonNegativeBoundWithDegreeKind.classify s1))
          , of_valclass (Key.lift_valclass (NonNegativeBoundWithDegreeKind.classify s2)) )
        with
        | Val s1, Val s2 ->
            Val (mult s1 s2)
        | Below _, _ | _, Below _ ->
            assert false
        | (Above _ as t), _ | _, (Above _ as t) ->
            t ) )
    | Symbolic (FuncPtr _ as key) ->
        Val (singleton key)


  let rec int_lb {const; terms} =
    M.fold
      (fun symbol polynomial acc ->
        match symbol with
        | NonNegativeBoundWithDegreeKind symbol ->
            let s_lb = NonNegativeBoundWithDegreeKind.int_lb symbol in
            let p_lb = int_lb polynomial in
            NonNegativeInt.((s_lb * p_lb) + acc)
        | FuncPtr _ ->
            acc )
      terms const


  let rec int_ub {const; terms} =
    M.fold
      (fun symbol polynomial acc ->
        match symbol with
        | NonNegativeBoundWithDegreeKind symbol ->
            Option.bind acc ~f:(fun acc ->
                Option.bind (NonNegativeBoundWithDegreeKind.int_ub symbol) ~f:(fun s_ub ->
                    Option.map (int_ub polynomial) ~f:(fun p_ub ->
                        NonNegativeInt.((s_ub * p_ub) + acc) ) ) )
        | FuncPtr _ ->
            acc )
      terms (Some const)


  (* assumes symbols are not comparable *)
  let rec leq ~lhs ~rhs =
    phys_equal lhs rhs
    || (NonNegativeInt.leq ~lhs:lhs.const ~rhs:rhs.const && M.le ~le_elt:leq lhs.terms rhs.terms)
    || Option.exists (int_ub lhs) ~f:(fun lhs_ub ->
           NonNegativeInt.leq ~lhs:lhs_ub ~rhs:(int_lb rhs) )


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
            match s with
            | NonNegativeBoundWithDegreeKind s ->
                M.update
                  (NonNegativeBoundWithDegreeKind
                     (NonNegativeBoundWithDegreeKind.mask_min_max_constant s) )
                  (function
                    | None -> Some p' | Some p -> if leq ~lhs:p ~rhs:p' then Some p' else Some p )
                  acc
            | FuncPtr _ as key ->
                M.add key p' acc )
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


  let subst callee_pname location p eval_sym eval_func_ptrs get_closure_callee_cost
      ~default_closure_cost =
    let exception ReturnTop of (NonNegativeBoundWithDegreeKind.t * BoundTrace.t) in
    (* avoids top-lifting everything *)
    let rec subst {const; terms} =
      M.fold
        (fun s p acc ->
          match s with
          | NonNegativeBoundWithDegreeKind s -> (
            match NonNegativeBoundWithDegreeKind.subst callee_pname location s eval_sym with
            | Constant c -> (
              match PositiveInt.of_big_int (c :> Z.t) with
              | None ->
                  acc
              | Some c ->
                  let p = subst p in
                  mult_const_positive p c |> plus acc )
            | ValTop trace ->
                let p = subst p in
                if is_zero p then acc else raise (ReturnTop (s, trace))
            | Symbolic s ->
                let p = subst p in
                mult_symb_aux p (NonNegativeBoundWithDegreeKind s) |> plus acc )
          | FuncPtr s ->
              let funcptr_p =
                let p = subst p in
                match FuncPtr.Set.is_singleton_or_more (eval_func_ptrs s) with
                | Singleton (Closure {name}) ->
                    let closure_p =
                      match get_closure_callee_cost name with
                      | Some closure_p ->
                          closure_p
                      | None ->
                          of_non_negative_int default_closure_cost
                    in
                    mult closure_p p
                | Singleton (Path path) ->
                    mult_symb_aux p (FuncPtr path)
                | Empty | More ->
                    mult (of_non_negative_int default_closure_cost) p
              in
              plus acc funcptr_p )
        terms (of_non_negative_int const)
    in
    match subst p with poly -> Val poly | exception ReturnTop s_trace -> Above s_trace


  let rec is_zero_degree {const; terms} = NonNegativeInt.is_zero const && is_zero_degree_terms terms

  and is_zero_degree_terms terms =
    M.for_all (fun key v -> Key.is_func_ptr key || is_zero_degree v) terms


  (** Emit a pair (d,t) where d is the degree of the polynomial and t is the first term with such
      degree. When calculating the degree, it ignores symbols of function pointer, so they are
      addressed as if zero cost. *)
  let rec degree_with_term {const; terms} =
    let degree_terms =
      M.fold
        (fun t p cur_max ->
          match (t, degree_with_term p) with
          (* It ignores function pointers when calculating degree of polynomial, since their
             semantics is different to the other symbolic values.  For example, when a function
             has a complexity of |fptr| where fptr is a function pointer, it does not make sense
             to say the function has a linear complexity. *)
          | FuncPtr _, _ ->
              cur_max
          | _, (_, p') when is_zero p' ->
              cur_max
          | NonNegativeBoundWithDegreeKind b, (d, p') ->
              let d' = Degree.succ (NonNegativeBoundWithDegreeKind.degree_kind b) d in
              if Degree.compare d' (fst cur_max) > 0 then (d', mult_symb_aux p' t) else cur_max )
        terms (Degree.zero, zero)
    in
    if is_zero (snd degree_terms) then
      if NonNegativeInt.is_zero const then (Degree.zero, zero) else (Degree.zero, one)
    else degree_terms


  let degree p = fst (degree_with_term p)

  let multiplication_sep = F.sprintf " %s " SpecialChars.multiplication_sign

  let pp : hum:bool -> F.formatter -> t -> unit =
    let add_symb s (((last_s, last_occ) as last), others) =
      if Int.equal 0 (Key.compare s last_s) then ((last_s, PositiveInt.succ last_occ), others)
      else ((s, PositiveInt.one), last :: others)
    in
    let pp_coeff fmt (c : NonNegativeInt.t) =
      if Z.(gt (c :> Z.t) one) then
        F.fprintf fmt "%a %s " NonNegativeInt.pp c SpecialChars.dot_operator
    in
    let pp_exp fmt (e : PositiveInt.t) =
      if Z.(gt (e :> Z.t) one) then PositiveInt.pp_exponent fmt e
    in
    let pp_symb_exp ~hum fmt (symb, exp) = F.fprintf fmt "%a%a" (Key.pp_hum ~hum) symb pp_exp exp in
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
            if Config.cost_suppress_func_ptr && (Key.is_func_ptr s || is_zero_degree p) then
              print_plus
            else (
              pp_sub ~hum ~print_plus (add_symb s symbs) fmt p ;
              true ) )
          terms print_plus
        : bool )
      |> ignore
    in
    fun ~hum fmt {const; terms} ->
      let const_not_zero = not (NonNegativeInt.is_zero const) in
      if
        const_not_zero || M.is_empty terms
        || (Config.cost_suppress_func_ptr && is_zero_degree_terms terms)
      then NonNegativeInt.pp fmt const ;
      ( M.fold
          (fun s p print_plus ->
            if Config.cost_suppress_func_ptr && (Key.is_func_ptr s || is_zero_degree p) then
              print_plus
            else (
              pp_sub ~hum ~print_plus ((s, PositiveInt.one), []) fmt p ;
              true ) )
          terms const_not_zero
        : bool )
      |> ignore


  let get_symbols p : NonNegativeBound.t list =
    let rec get_symbols_sub {terms} acc =
      M.fold
        (fun s p acc ->
          let acc =
            match s with
            | NonNegativeBoundWithDegreeKind s ->
                NonNegativeBoundWithDegreeKind.symbol s :: acc
            | FuncPtr _ ->
                acc
          in
          get_symbols_sub p acc )
        terms acc
    in
    get_symbols_sub p []


  let polynomial_traces p =
    get_symbols p |> List.map ~f:NonNegativeBoundWithDegreeKind.make_err_trace_symbol
end

module TopTrace = struct
  type t =
    | UnboundedLoop of {bound_trace: BoundTrace.t}
    | UnboundedSymbol of
        {location: Location.t; symbol: NonNegativeBoundWithDegreeKind.t; bound_trace: BoundTrace.t}
    | Call of {location: Location.t; callee_pname: Procname.t; callee_trace: t}
  [@@deriving compare]

  let rec length = function
    | UnboundedLoop {bound_trace} | UnboundedSymbol {bound_trace} ->
        1 + BoundTrace.length bound_trace
    | Call {callee_trace} ->
        1 + length callee_trace


  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let unbounded_loop bound_trace = UnboundedLoop {bound_trace}

  let unbounded_symbol ~location ~symbol bound_trace =
    UnboundedSymbol {location; symbol; bound_trace}


  let call ~location ~callee_pname callee_trace = Call {location; callee_pname; callee_trace}

  let rec pp f = function
    | UnboundedLoop {bound_trace} ->
        F.fprintf f "%a -> UnboundedLoop" BoundTrace.pp bound_trace
    | UnboundedSymbol {location; symbol; bound_trace} ->
        F.fprintf f "%a -> UnboundedSymbol (%a): %a" BoundTrace.pp bound_trace Location.pp location
          (NonNegativeBoundWithDegreeKind.pp ~hum:false)
          symbol
    | Call {callee_pname; callee_trace; location} ->
        F.fprintf f "%a -> Call `%a` (%a)" pp callee_trace Procname.pp callee_pname Location.pp
          location


  let rec make_err_trace ~depth trace =
    match trace with
    | UnboundedLoop {bound_trace} ->
        let bound_err_trace = BoundTrace.make_err_trace ~depth bound_trace in
        [("Unbounded loop", bound_err_trace)] |> Errlog.concat_traces
    | UnboundedSymbol {location; symbol; bound_trace} ->
        let desc =
          F.asprintf "Unbounded value %a" (NonNegativeBoundWithDegreeKind.pp ~hum:true) symbol
        in
        Errlog.make_trace_element depth location desc []
        :: BoundTrace.make_err_trace ~depth bound_trace
    | Call {location; callee_pname; callee_trace} ->
        let desc = F.asprintf "Call to %a" Procname.pp callee_pname in
        Errlog.make_trace_element depth location desc []
        :: make_err_trace ~depth:(depth + 1) callee_trace
end

module TopTraces = struct
  include AbstractDomain.MinReprSet (TopTrace)

  let make_err_trace traces =
    match min_elt traces with None -> [] | Some trace -> TopTrace.make_err_trace ~depth:0 trace
end

module UnreachableTrace = struct
  type t =
    | UnreachableNode of Location.t
    | Call of {location: Location.t; callee_pname: Procname.t; callee_trace: t}
  [@@deriving compare]

  let rec length = function
    | UnreachableNode _ ->
        1
    | Call {callee_trace} ->
        1 + length callee_trace


  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let unreachable_node node_loc = UnreachableNode node_loc

  let call ~location ~callee_pname callee_trace = Call {location; callee_pname; callee_trace}

  let rec pp f = function
    | UnreachableNode node_loc ->
        F.fprintf f "UnreachableNode (%a)" Location.pp node_loc
    | Call {callee_pname; callee_trace; location} ->
        F.fprintf f "%a -> Call `%a` (%a)" pp callee_trace Procname.pp callee_pname Location.pp
          location


  let rec make_err_trace ~depth trace =
    match trace with
    | UnreachableNode node_loc ->
        [Errlog.make_trace_element depth node_loc "Unreachable node" []]
    | Call {location; callee_pname; callee_trace} ->
        let desc = F.asprintf "Call to %a" Procname.pp callee_pname in
        Errlog.make_trace_element depth location desc []
        :: make_err_trace ~depth:(depth + 1) callee_trace
end

module UnreachableTraces = struct
  include AbstractDomain.MinReprSet (UnreachableTrace)

  let make_err_trace traces =
    match min_elt traces with
    | None ->
        []
    | Some trace ->
        UnreachableTrace.make_err_trace ~depth:0 trace
end

module NonNegativePolynomial = struct
  (* Use Above for Top values, Below for Unreachable values with their trace, and Val for non-negative polynomials *)
  type t = (UnreachableTraces.t, NonNegativeNonTopPolynomial.t, TopTraces.t) below_above

  type degree_with_term =
    ( UnreachableTraces.t
    , Degree.t * NonNegativeNonTopPolynomial.t
    , TopTraces.t )
    AbstractDomain.Types.below_above

  let leq =
    AbstractDomain.StackedUtils.leq ~leq_below:UnreachableTraces.leq
      ~leq:NonNegativeNonTopPolynomial.leq ~leq_above:TopTraces.leq


  let pp ~hum =
    let pp_below f traces =
      AbstractDomain.BottomLiftedUtils.pp_bottom f ;
      if not hum then F.fprintf f ": %a" UnreachableTraces.pp traces
    in
    let pp_above f traces =
      AbstractDomain.TopLiftedUtils.pp_top f ;
      if not hum then F.fprintf f ": %a" TopTraces.pp traces
    in
    AbstractDomain.StackedUtils.pp ~pp:(NonNegativeNonTopPolynomial.pp ~hum) ~pp_above ~pp_below


  let pp_hum = pp ~hum:true

  let pp = pp ~hum:false

  let top = Above TopTraces.bottom

  let zero = Val NonNegativeNonTopPolynomial.zero

  let one = Val NonNegativeNonTopPolynomial.one

  let of_unreachable node_loc =
    Below (UnreachableTraces.singleton (UnreachableTrace.unreachable_node node_loc))


  let of_int_exn i = Val (NonNegativeNonTopPolynomial.of_int_exn i)

  let make_trace_set ~map_above =
    AbstractDomain.StackedUtils.map
      ~f_below:(fun _ -> assert false)
      ~f:Fn.id
      ~f_above:(fun above -> TopTraces.singleton (map_above above))


  let of_non_negative_bound ?(degree_kind = DegreeKind.Linear) b =
    b
    |> NonNegativeBoundWithDegreeKind.make degree_kind
    |> NonNegativeBoundWithDegreeKind.classify |> NonNegativeNonTopPolynomial.Key.lift_valclass
    |> NonNegativeNonTopPolynomial.of_valclass
    (* Invariant: we always get a non-below bound from [of_valclass] *)
    |> make_trace_set ~map_above:TopTrace.unbounded_loop


  let of_func_ptr path = Val (NonNegativeNonTopPolynomial.of_func_ptr path)

  let is_symbolic = function
    | Below _ | Above _ ->
        false
    | Val p ->
        NonNegativeNonTopPolynomial.is_symbolic p


  let is_top = function Above _ -> true | _ -> false

  let is_unreachable = function Below _ -> true | _ -> false

  let is_zero = function Val p when NonNegativeNonTopPolynomial.is_zero p -> true | _ -> false

  let is_one = function Val p when NonNegativeNonTopPolynomial.is_one p -> true | _ -> false

  let top_lifted_increasing ~f p1 p2 =
    AbstractDomain.StackedUtils.combine ~f_below:UnreachableTraces.join ~dir:`Increasing p1 p2 ~f
      ~f_above:TopTraces.join


  let unreachable_lifted_increasing ~f p1 p2 =
    match (p1, p2) with
    | (Below _ as below), _ | _, (Below _ as below) ->
        below
    | _ ->
        top_lifted_increasing ~f p1 p2


  let plus = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.plus

  let mult_unreachable = unreachable_lifted_increasing ~f:NonNegativeNonTopPolynomial.mult

  let mult = top_lifted_increasing ~f:NonNegativeNonTopPolynomial.mult

  let mult_loop ~iter ~body =
    top_lifted_increasing iter body ~f:(fun iter body ->
        NonNegativeNonTopPolynomial.mult_loop ~iter ~body )


  let min_default_left p1 p2 =
    AbstractDomain.StackedUtils.combine ~dir:`Decreasing p1 p2
      ~f:NonNegativeNonTopPolynomial.min_default_left ~f_above:TopTraces.join
      ~f_below:UnreachableTraces.join


  let subst callee_pname location p eval_sym eval_func_ptrs get_closure_callee_cost
      ~default_closure_cost =
    match p with
    | Above callee_traces ->
        Above
          (TopTraces.map
             (fun callee_trace -> TopTrace.call ~callee_pname ~location callee_trace)
             callee_traces )
    | Below callee_traces ->
        Below
          (UnreachableTraces.map
             (fun callee_trace -> UnreachableTrace.call ~callee_pname ~location callee_trace)
             callee_traces )
    | Val p ->
        let get_closure_callee_cost pname =
          match get_closure_callee_cost pname with
          | Some (Val p) ->
              Some p
          | None | Some (Below _ | Above _) ->
              (* It doesn't propagate Top/Bottoms if the closure has these costs. *)
              None
        in
        NonNegativeNonTopPolynomial.subst callee_pname location p eval_sym eval_func_ptrs
          get_closure_callee_cost ~default_closure_cost
        |> make_trace_set ~map_above:(fun (symbol, bound_trace) ->
               TopTrace.unbounded_symbol ~location ~symbol bound_trace )


  let degree p =
    match p with Above _ | Below _ -> None | Val p -> Some (NonNegativeNonTopPolynomial.degree p)


  let compare_by_degree =
    let cmp _ _ = 0 (* All pairs of Top/Unreachable should be considered equal *) in
    AbstractDomain.StackedUtils.compare ~cmp_above:cmp
      ~cmp:(fun p1 p2 ->
        Degree.compare
          (NonNegativeNonTopPolynomial.degree p1)
          (NonNegativeNonTopPolynomial.degree p2) )
      ~cmp_below:cmp


  let get_degree_with_term =
    AbstractDomain.StackedUtils.map ~f:NonNegativeNonTopPolynomial.degree_with_term ~f_above:Fn.id
      ~f_below:Fn.id


  let pp_degree ~only_bigO fmt = function
    | Above _ ->
        Format.pp_print_string fmt "Top"
    | Below _ ->
        Format.pp_print_string fmt "Unreachable"
    | Val (degree, degree_term) ->
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


  let polynomial_traces = function
    | Below trace ->
        UnreachableTraces.make_err_trace trace
    | Val p ->
        NonNegativeNonTopPolynomial.polynomial_traces p |> Errlog.concat_traces
    | Above trace ->
        TopTraces.make_err_trace trace


  let encode astate = Marshal.to_string astate [] |> Base64.encode_exn

  let decode enc_str = Marshal.from_string (Base64.decode_exn enc_str) 0
end
