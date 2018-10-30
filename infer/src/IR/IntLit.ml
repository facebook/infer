(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

(** signed and unsigned integer literals *)
type t = bool * Z.t * bool

exception OversizedShift

(* the first bool indicates whether this is an unsigned value,
   and the second whether it is a pointer *)

let area u i =
  match (Z.(i < zero), u) with
  | true, false ->
      (* only representable as signed *) 1
  | false, _ ->
      (* in the intersection between signed and unsigned *) 2
  | true, true ->
      (* only representable as unsigned *) 3


let to_signed (unsigned, i, ptr) =
  if Int.equal (area unsigned i) 3 then None
  else (* not representable as signed *) Some (false, i, ptr)


let compare (unsigned1, i1, _) (unsigned2, i2, _) =
  let n = Bool.compare unsigned1 unsigned2 in
  if n <> 0 then n else Z.compare i1 i2


let compare_value (unsigned1, i1, _) (unsigned2, i2, _) =
  [%compare: int * Z.t] (area unsigned1 i1, i1) (area unsigned2 i2, i2)


let eq i1 i2 = Int.equal (compare_value i1 i2) 0

let neq i1 i2 = compare_value i1 i2 <> 0

let leq i1 i2 = compare_value i1 i2 <= 0

let lt i1 i2 = compare_value i1 i2 < 0

let geq i1 i2 = compare_value i1 i2 >= 0

let gt i1 i2 = compare_value i1 i2 > 0

let of_z z_of_int i = (false, z_of_int i, false)

let of_int64 = of_z Z.of_int64

let of_int32 = of_z Z.of_int32

let of_int = of_z Z.of_int

let of_string = of_z Z.of_string

let z_to_int_opt i = try Some (Z.to_int i) with Z.Overflow -> None

let to_int (_, i, _) = z_to_int_opt i

let to_int_exn (_, i, _) = Z.to_int i

let to_big_int (_, i, _) = i

let to_float (_, i, _) = Z.to_float i

let null = (false, Z.zero, true)

let zero = of_int 0

let one = of_int 1

let two = of_int 2

let minus_one = of_int (-1)

let isone (_, i, _) = Z.(equal i one)

let iszero (_, i, _) = Z.(equal i zero)

let isnull (_, i, ptr) = Z.(equal i zero) && ptr

let isminusone (unsigned, i, _) = (not unsigned) && Z.(equal i minus_one)

let isnegative (unsigned, i, _) = (not unsigned) && Z.(lt i zero)

let neg (unsigned, i, ptr) = (unsigned, Z.neg i, ptr)

let lift binop (unsigned1, i1, ptr1) (unsigned2, i2, ptr2) =
  (unsigned1 || unsigned2, binop i1 i2, ptr1 || ptr2)


let lift1 unop (unsigned, i, ptr) = (unsigned, unop i, ptr)

let add i1 i2 = lift Z.( + ) i1 i2

let mul i1 i2 = lift Z.( * ) i1 i2

let div i1 i2 = lift Z.( / ) i1 i2

let rem i1 i2 = lift Z.rem i1 i2

let logand i1 i2 = lift Z.logand i1 i2

let logor i1 i2 = lift Z.logor i1 i2

let logxor i1 i2 = lift Z.logxor i1 i2

let lognot i = lift1 Z.lognot i

let sub i1 i2 = add i1 (neg i2)

let shift_left (unsigned1, i1, ptr1) (_, i2, _) =
  match z_to_int_opt i2 with
  | None ->
      L.(die InternalError) "Shifting failed with operand %a" Z.pp_print i2
  | Some i2 ->
      if i2 < 0 || i2 >= 64 then raise OversizedShift ;
      let res = Z.shift_left i1 i2 in
      (unsigned1, res, ptr1)


let shift_right (unsigned1, i1, ptr1) (_, i2, _) =
  match z_to_int_opt i2 with
  | None ->
      L.(die InternalError) "Shifting failed with operand %a" Z.pp_print i2
  | Some i2 ->
      if i2 < 0 || i2 >= 64 then raise OversizedShift ;
      let res = Z.shift_right i1 i2 in
      (unsigned1, res, ptr1)


let pp f (_, n, ptr) =
  if ptr && Z.(equal n zero) then F.pp_print_string f "null" else Z.pp_print f n


let to_string i = F.asprintf "%a" pp i
