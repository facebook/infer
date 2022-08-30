(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format

module NonZeroInt = struct
  type t = Z.t [@@deriving compare, equal]

  exception DivisionNotExact

  let one = Z.one

  let minus_one = Z.minus_one

  let of_big_int x = if Z.(equal x zero) then None else Some x

  let opt_to_big_int = function None -> Z.zero | Some i -> i

  let is_one = Z.equal one

  let is_minus_one = Z.equal minus_one

  let is_multiple m d = Z.(equal (m mod d) zero)

  let is_negative x = Z.(lt x zero)

  let is_positive x = Z.(gt x zero)

  let ( ~- ) = Z.( ~- )

  let ( * ) = Z.( * )

  let plus x y = of_big_int Z.(x + y)

  let exact_div_exn num den =
    let q, r = Z.div_rem num den in
    if Z.(equal r zero) then q else raise DivisionNotExact


  let max = Z.max

  let min = Z.min
end

module NonNegativeInt = struct
  type t = Z.t [@@deriving compare, equal]

  let zero = Z.zero

  let one = Z.one

  let is_zero = Z.equal zero

  let is_one = Z.equal one

  let of_big_int i = if Z.(lt i zero) then None else Some i

  let of_int_exn i =
    assert (i >= 0) ;
    Z.of_int i


  let of_big_int_exn i =
    assert (Z.(geq i zero)) ;
    i


  let to_int_exn = Z.to_int

  let leq ~lhs ~rhs = Z.leq lhs rhs

  let succ = Z.succ

  let log2_ceil_exn i = Z.log2up i |> Z.of_int

  let ( + ) = Z.( + )

  let ( * ) = Z.( * )

  let pp = Z.pp_print
end

module PositiveInt = struct
  type t = NonNegativeInt.t [@@deriving compare, equal]

  let one = Z.one

  let of_big_int i = if Z.(leq i zero) then None else Some i

  let succ = Z.succ

  let ten = Z.of_int 10

  let exponent_prefix, exponent_chars = SpecialChars.superscript_digits

  let pp_exponent f i =
    let rec aux f i =
      if not Z.(leq i zero) then (
        let d, r = Z.ediv_rem i ten in
        aux f d ;
        F.pp_print_string f exponent_chars.(Z.to_int r) )
    in
    F.pp_print_string f exponent_prefix ;
    aux f i
end
