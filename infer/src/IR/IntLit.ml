(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format

(** signed and unsigned integer literals *)
type t = bool * Int64.t * bool

exception OversizedShift

(* the first bool indicates whether this is an unsigned value,
   and the second whether it is a pointer *)

let area u i =
  match (i < 0L, u) with
  | true, false
   -> (* only representable as signed *) 1
  | false, _
   -> (* in the intersection between signed and unsigned *) 2
  | true, true
   -> (* only representable as unsigned *) 3

let to_signed (unsigned, i, ptr) =
  if Int.equal (area unsigned i) 3 then None
  else (* not representable as signed *) Some (false, i, ptr)

let compare (unsigned1, i1, _) (unsigned2, i2, _) =
  let n = Bool.compare unsigned1 unsigned2 in
  if n <> 0 then n else Int64.compare i1 i2

let compare_value (unsigned1, i1, _) (unsigned2, i2, _) =
  [%compare : int * Int64.t] (area unsigned1 i1, i1) (area unsigned2 i2, i2)

let eq i1 i2 = Int.equal (compare_value i1 i2) 0

let neq i1 i2 = compare_value i1 i2 <> 0

let leq i1 i2 = compare_value i1 i2 <= 0

let lt i1 i2 = compare_value i1 i2 < 0

let geq i1 i2 = compare_value i1 i2 >= 0

let gt i1 i2 = compare_value i1 i2 > 0

let of_int64 i = (false, i, false)

let of_int32 i = of_int64 (Int64.of_int32 i)

let of_int64_unsigned i unsigned = (unsigned, i, false)

let of_int i = of_int64 (Int64.of_int i)

let to_int (_, i, _) = Int64.to_int_exn i

let null = (false, 0L, true)

let zero = of_int 0

let one = of_int 1

let two = of_int 2

let minus_one = of_int (-1)

let isone (_, i, _) = Int64.equal i 1L

let iszero (_, i, _) = Int64.equal i 0L

let isnull (_, i, ptr) = Int64.equal i 0L && ptr

let isminusone (unsigned, i, _) = not unsigned && Int64.equal i (-1L)

let isnegative (unsigned, i, _) = not unsigned && i < 0L

let neg (unsigned, i, ptr) = (unsigned, Int64.neg i, ptr)

let lift binop (unsigned1, i1, ptr1) (unsigned2, i2, ptr2) =
  (unsigned1 || unsigned2, binop i1 i2, ptr1 || ptr2)

let lift1 unop (unsigned, i, ptr) = (unsigned, unop i, ptr)

let add i1 i2 = lift Int64.( + ) i1 i2

let mul i1 i2 = lift Int64.( * ) i1 i2

let div i1 i2 = lift Int64.( / ) i1 i2

let rem i1 i2 = lift Int64.rem i1 i2

let logand i1 i2 = lift Int64.bit_and i1 i2

let logor i1 i2 = lift Int64.bit_or i1 i2

let logxor i1 i2 = lift Int64.bit_xor i1 i2

let lognot i = lift1 Int64.bit_not i

let sub i1 i2 = add i1 (neg i2)

let shift_left (unsigned1, i1, ptr1) (_, i2, _) =
  match Int64.to_int i2 with
  | None
   -> failwithf "Shifting failed with operand %a" Int64.pp i2
  | Some i2
   -> if i2 < 0 || i2 >= 64 then raise OversizedShift ;
      let res = Int64.shift_left i1 i2 in
      (unsigned1, res, ptr1)

let shift_right (unsigned1, i1, ptr1) (_, i2, _) =
  match Int64.to_int i2 with
  | None
   -> failwithf "Shifting failed with operand %a" Int64.pp i2
  | Some i2
   -> if i2 < 0 || i2 >= 64 then raise OversizedShift ;
      let res = Int64.shift_right i1 i2 in
      (unsigned1, res, ptr1)

let pp f (unsigned, n, ptr) =
  if ptr && Int64.equal n 0L then F.fprintf f "null"
  else if unsigned then F.fprintf f "%Lu" n
  else F.fprintf f "%Ld" n

let to_string i = F.asprintf "%a" pp i
