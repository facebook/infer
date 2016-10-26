/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;

let module F = Format;


/** signed and unsigned integer literals */
/* the first bool indicates whether this is an unsigned value,
   and the second whether it is a pointer */
type t = (bool, Int64.t, bool);

let area u i =>
  switch (i < 0L, u) {
  | (true, false) => 1 /* only representable as signed */
  | (false, _) => 2 /* in the intersection between signed and unsigned */
  | (true, true) => 3 /* only representable as unsigned */
  };

let to_signed (unsigned, i, ptr) =>
  if (area unsigned i == 3) {
    None
  } else {
    Some
      /* not representable as signed */
      (false, i, ptr)
  };

let compare (unsigned1, i1, _) (unsigned2, i2, _) => {
  let n = bool_compare unsigned1 unsigned2;
  if (n != 0) {
    n
  } else {
    Int64.compare i1 i2
  }
};

let compare_value (unsigned1, i1, _) (unsigned2, i2, _) => {
  let area1 = area unsigned1 i1;
  let area2 = area unsigned2 i2;
  let n = int_compare area1 area2;
  if (n != 0) {
    n
  } else {
    Int64.compare i1 i2
  }
};

let eq i1 i2 => compare_value i1 i2 == 0;

let neq i1 i2 => compare_value i1 i2 != 0;

let leq i1 i2 => compare_value i1 i2 <= 0;

let lt i1 i2 => compare_value i1 i2 < 0;

let geq i1 i2 => compare_value i1 i2 >= 0;

let gt i1 i2 => compare_value i1 i2 > 0;

let of_int64 i => (false, i, false);

let of_int32 i => of_int64 (Int64.of_int32 i);

let of_int64_unsigned i unsigned => (unsigned, i, false);

let of_int i => of_int64 (Int64.of_int i);

let to_int (_, i, _) => Int64.to_int i;

let null = (false, 0L, true);

let zero = of_int 0;

let one = of_int 1;

let two = of_int 2;

let minus_one = of_int (-1);

let isone (_, i, _) => i == 1L;

let iszero (_, i, _) => i == 0L;

let isnull (_, i, ptr) => i == 0L && ptr;

let isminusone (unsigned, i, _) => not unsigned && i == (-1L);

let isnegative (unsigned, i, _) => not unsigned && i < 0L;

let neg (unsigned, i, ptr) => (unsigned, Int64.neg i, ptr);

let lift binop (unsigned1, i1, ptr1) (unsigned2, i2, ptr2) => (
  unsigned1 || unsigned2,
  binop i1 i2,
  ptr1 || ptr2
);

let lift1 unop (unsigned, i, ptr) => (unsigned, unop i, ptr);

let add i1 i2 => lift Int64.add i1 i2;

let mul i1 i2 => lift Int64.mul i1 i2;

let div i1 i2 => lift Int64.div i1 i2;

let rem i1 i2 => lift Int64.rem i1 i2;

let logand i1 i2 => lift Int64.logand i1 i2;

let logor i1 i2 => lift Int64.logor i1 i2;

let logxor i1 i2 => lift Int64.logxor i1 i2;

let lognot i => lift1 Int64.lognot i;

let sub i1 i2 => add i1 (neg i2);

let pp f (unsigned, n, ptr) =>
  if (ptr && n == 0L) {
    F.fprintf f "null"
  } else if unsigned {
    F.fprintf f "%Lu" n
  } else {
    F.fprintf f "%Ld" n
  };

let to_string i => pp_to_string pp i;
