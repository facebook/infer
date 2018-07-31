(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format

module NonZeroInt = struct
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


  let max = Int.max

  let min = Int.min
end

module NonNegativeInt = struct
  type t = int [@@deriving compare]

  let zero = 0

  let one = 1

  let is_zero = function 0 -> true | _ -> false

  let is_one = function 1 -> true | _ -> false

  let of_int i = if i < 0 then None else Some i

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
