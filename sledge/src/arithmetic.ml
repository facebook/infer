(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Arithmetic terms *)

include Arithmetic_intf

module Representation (Trm : INDETERMINATE) = struct
  module Prod = struct
    include Multiset.Make
              (Int)
              (struct
                type t = Trm.trm [@@deriving compare, equal, sexp]
              end)

    let t_of_sexp = t_of_sexp Trm.trm_of_sexp
  end

  module Mono = struct
    type t = Prod.t [@@deriving compare, equal, sexp]

    let num_den m = Prod.partition m ~f:(fun _ i -> i >= 0)

    let ppx strength ppf power_product =
      let pp_factor ppf (indet, exponent) =
        if exponent = 1 then (Trm.ppx strength) ppf indet
        else Format.fprintf ppf "%a^%i" (Trm.ppx strength) indet exponent
      in
      let pp_num ppf num =
        if Prod.is_empty num then Trace.pp_styled `Magenta "1" ppf
        else Prod.pp "@ @<2>× " pp_factor ppf num
      in
      let pp_den ppf den =
        if not (Prod.is_empty den) then
          Format.fprintf ppf "@ / %a"
            (Prod.pp "@ / " pp_factor)
            (Prod.map_counts ~f:Int.neg den)
      in
      let num, den = num_den power_product in
      Format.fprintf ppf "@[<2>(%a%a)@]" pp_num num pp_den den

    (** [one] is the empty product Πᵢ₌₁⁰ xᵢ^pᵢ *)
    let one = Prod.empty

    let equal_one = Prod.is_empty

    (** [of_ x₁ p₁] is the singleton product Πᵢ₌₁¹ x₁^p₁ *)
    let of_ x p = Prod.of_ x p

    (** [pow (Πᵢ₌₁ⁿ xᵢ^pᵢ) p] is Πᵢ₌₁ⁿ xᵢ^(pᵢ×p) *)
    let pow mono = function
      | 0 -> Prod.empty
      | 1 -> mono
      | power -> Prod.map_counts ~f:(Int.mul power) mono

    let mul x y = Prod.union x y

    (** [get_trm m] is [Some x] iff [equal m (of_ x 1)] *)
    let get_trm mono =
      match Prod.only_elt mono with Some (trm, 1) -> Some trm | _ -> None
  end

  module Sum = struct
    include Multiset.Make (Q) (Mono)

    let t_of_sexp = t_of_sexp Mono.t_of_sexp
  end

  module Poly = struct
    type t = Sum.t [@@deriving compare, equal, sexp]
    type trm = Trm.trm
  end

  include Poly

  module Make (Embed : EMBEDDING with type trm := Trm.trm and type t := t) =
  struct
    include Poly

    let ppx strength ppf poly =
      if Sum.is_empty poly then Trace.pp_styled `Magenta "0" ppf
      else
        let pp_coeff_mono ppf (m, c) =
          if Mono.equal_one m then Trace.pp_styled `Magenta "%a" ppf Q.pp c
          else
            Format.fprintf ppf "%a @<2>× %a" Q.pp c (Mono.ppx strength) m
        in
        Format.fprintf ppf "@[<2>(%a)@]" (Sum.pp "@ + " pp_coeff_mono) poly

    let mono_invariant mono =
      let@ () = Invariant.invariant [%here] mono [%sexp_of: Mono.t] in
      Prod.iter mono ~f:(fun base power ->
          (* powers are non-zero *)
          assert (not (Int.equal Int.zero power)) ;
          match Embed.get_arith base with
          | None -> ()
          | Some poly -> (
            match Sum.classify poly with
            | `Many -> ()
            | `Zero | `One _ ->
                (* polynomial factors are not constant or singleton, which
                   should have been flattened into the parent monomial *)
                assert false ) ) ;
      match Mono.get_trm mono with
      | None -> ()
      | Some trm -> (
        match Embed.get_arith trm with
        | None -> ()
        | Some _ ->
            (* singleton monomials are not polynomials, which should have
               been flattened into the parent polynomial *)
            assert false )

    let invariant poly =
      let@ () = Invariant.invariant [%here] poly [%sexp_of: t] in
      Sum.iter poly ~f:(fun mono coeff ->
          (* coefficients are non-zero *)
          assert (not (Q.equal Q.zero coeff)) ;
          mono_invariant mono )

    (* constants *)

    let const q = Sum.of_ Mono.one q |> check invariant
    let zero = const Q.zero |> check (fun p -> assert (Sum.is_empty p))

    (* core constructors *)

    let neg poly = Sum.map_counts ~f:Q.neg poly |> check invariant
    let add p q = Sum.union p q |> check invariant
    let sub p q = add p (neg q)

    let mulc coeff poly =
      ( if Q.equal Q.one coeff then poly
      else if Q.equal Q.zero coeff then zero
      else Sum.map_counts ~f:(Q.mul coeff) poly )
      |> check invariant

    (* projections and embeddings *)

    type view = Trm of trm | Const of Q.t | Compound

    let classify poly =
      match Sum.classify poly with
      | `Zero -> Const Q.zero
      | `One (mono, coeff) -> (
        match Prod.classify mono with
        | `Zero -> Const coeff
        | `One (trm, 1) when Q.equal Q.one coeff -> Trm trm
        | _ -> Compound )
      | `Many -> Compound

    let get_const poly =
      match Sum.classify poly with
      | `Zero -> Some Q.zero
      | `One (mono, coeff) when Mono.equal_one mono -> Some coeff
      | _ -> None

    let get_mono poly =
      match Sum.only_elt poly with
      | Some (mono, coeff) when Q.equal Q.one coeff -> Some mono
      | _ -> None

    (** Terms of a polynomial: product of a coefficient and a monomial *)
    module CM = struct
      type t = Q.t * Prod.t

      let one = (Q.one, Mono.one)
      let equal_one (c, m) = Mono.equal_one m && Q.equal Q.one c
      let mul (c1, m1) (c2, m2) = (Q.mul c1 c2, Mono.mul m1 m2)

      (** Monomials [Mono.t] have [trm] indeterminates, which include, via
          [get_arith], polynomials [t] over monomials themselves. To avoid
          redundant representations, singleton polynomials are flattened. *)
      let of_trm : ?power:int -> trm -> t =
       fun ?(power = 1) base ->
        match Embed.get_arith base with
        | Some poly -> (
          match Sum.classify poly with
          (* 0 ^ p₁ ==> 0 × 1 *)
          | `Zero -> (Q.zero, Mono.one)
          (* (Σᵢ₌₁¹ cᵢ × Xᵢ) ^ p₁ ==> cᵢ^p₁ × Πⱼ₌₁¹ Xⱼ^pⱼ *)
          | `One (mono, coeff) -> (Q.pow coeff power, Mono.pow mono power)
          (* (Σᵢ₌₁ⁿ cᵢ × Xᵢ) ^ p₁ ==> 1 × Πⱼ₌₁¹ (Σᵢ₌₁ⁿ cᵢ × Xᵢ)^pⱼ *)
          | `Many -> (Q.one, Mono.of_ base power) )
        (* X₁ ^ p₁ ==> 1 × Πⱼ₌₁¹ Xⱼ^pⱼ *)
        | None -> (Q.one, Mono.of_ base power)

      (** Polynomials [t] have [trm] indeterminates, which, via [get_arith],
          include polynomials themselves. To avoid redundant
          representations, singleton monomials are flattened. Also, constant
          multiplication is not interpreted in [Prod], so constant
          polynomials are multiplied by their coefficients directly. *)
      let to_poly : t -> Poly.t =
       fun (coeff, mono) ->
        ( match Mono.get_trm mono with
        | Some trm -> (
          match Embed.get_arith trm with
          (* c × (Σᵢ₌₁ⁿ cᵢ × Xᵢ) ==> Σᵢ₌₁ⁿ c×cᵢ × Xᵢ *)
          | Some poly -> mulc coeff poly
          (* c₁ × X₁ ==> Σᵢ₌₁¹ cᵢ × Xᵢ *)
          | None -> Sum.of_ mono coeff )
        (* c₁ × (Πⱼ₌₁ᵐ X₁ⱼ^p₁ⱼ) ==> Σᵢ₌₁¹ cᵢ × (Πⱼ₌₁ᵐ Xᵢⱼ^pᵢⱼ) *)
        | None -> Sum.of_ mono coeff )
        |> check invariant
    end

    (** Embed a term into a polynomial, by projecting a polynomial out of
        the term if possible *)
    let trm trm =
      ( match Embed.get_arith trm with
      | Some poly -> poly
      | None -> Sum.of_ (Mono.of_ trm 1) Q.one )
      |> check (fun poly ->
             assert (equal poly (CM.to_poly (CM.of_trm trm))) )

    (** Project out the term embedded into a polynomial, if possible *)
    let get_trm poly =
      match get_mono poly with
      | Some mono -> Mono.get_trm mono
      | None -> None

    (* constructors over indeterminates *)

    let mul e1 e2 = CM.to_poly (CM.mul (CM.of_trm e1) (CM.of_trm e2))

    let div n d =
      CM.to_poly (CM.mul (CM.of_trm n) (CM.of_trm d ~power:(-1)))

    let pow base power = CM.to_poly (CM.of_trm base ~power)

    (* transform *)

    let split_const poly =
      match Sum.find_and_remove Mono.one poly with
      | Some (c, p_c) -> (p_c, c)
      | None -> (poly, Q.zero)

    let map poly ~f =
      let p, p' = (poly, Sum.empty) in
      let p, p' =
        Sum.fold poly ~init:(p, p') ~f:(fun mono coeff (p, p') ->
            let m, cm' = (mono, CM.one) in
            let m, cm' =
              Prod.fold mono ~init:(m, cm') ~f:(fun trm power (m, cm') ->
                  let trm' = f trm in
                  if trm == trm' then (m, cm')
                  else
                    (Prod.remove trm m, CM.mul cm' (CM.of_trm trm' ~power)) )
            in
            if CM.equal_one cm' then (p, p')
            else
              ( Sum.remove mono p
              , Sum.union p' (CM.to_poly (CM.mul (coeff, m) cm')) ) )
      in
      if Sum.is_empty p' then poly else Sum.union p p' |> check invariant

    (* traverse *)

    let iter poly =
      Iter.from_iter (fun f ->
          Sum.iter poly ~f:(fun mono _ ->
              Prod.iter mono ~f:(fun trm _ -> f trm) ) )

    type product = Prod.t

    let fold_factors = Prod.fold
    let fold_monomials = Sum.fold
  end
end
