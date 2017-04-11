/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

module F = Format;


/** signed and unsigned integer literals */
type t;

let add: t => t => t;


/** compare integers ignoring the distinction between pointers and non-pointers */
let compare: t => t => int;


/** compare the value of the integers, notice this is different from const compare,
    which distinguished between signed and unsigned +1 */
let compare_value: t => t => int;

let div: t => t => t;

let eq: t => t => bool;

let of_int: int => t;

let of_int32: int32 => t;

let of_int64: int64 => t;

let of_int64_unsigned: int64 => bool => t;

let geq: t => t => bool;

let gt: t => t => bool;

let isminusone: t => bool;

let isnegative: t => bool;

let isnull: t => bool;

let isone: t => bool;

let iszero: t => bool;

let leq: t => t => bool;

let logand: t => t => t;

let lognot: t => t;

let logor: t => t => t;

let logxor: t => t => t;

let lt: t => t => bool;

let minus_one: t;

let mul: t => t => t;

let neg: t => t;

let neq: t => t => bool;

let null: t; /** null behaves like zero except for the function isnull */

let one: t;

let pp: F.formatter => t => unit;

let rem: t => t => t;

let sub: t => t => t;

let to_int: t => int;

let to_signed: t => option t; /** convert to signed if the value is representable */

let to_string: t => string;

let two: t;

let zero: t;
