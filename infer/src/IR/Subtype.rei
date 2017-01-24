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


/** The Smallfoot Intermediate Language: Subtypes */
let module L = Logging;

let module F = Format;

type t [@@deriving compare];

let pp: F.formatter => t => unit;

let exact: t; /** denotes the current type only */

let subtypes: t; /** denotes the current type and any subtypes */

let subtypes_cast: t;

let subtypes_instof: t;

let join: t => t => t;


/** [case_analysis tenv (c1, st1) (c2, st2)] performs case analysis on [c1 <: c2] according
    to [st1] and [st2].
    [case_analysis] returns a pair:
    - whether [st1] and [st2] admit [c1 <: c2], and in case returns the updated subtype [st1]
    - whether [st1] and [st2] admit [not(c1 <: c2)], and in case returns the updated subtype [st1] */
let case_analysis: Tenv.t => (Typename.t, t) => (Typename.t, t) => (option t, option t);


/** [is_known_subtype tenv c1 c2] returns true if there is enough information in [tenv] to prove
    that [c1] is a subtype of [c2].
    Note that [not (is_known_subtype tenv c1 c2) == true] does not imply
    that [is_known_not_subtype tenv c1 c2 == true] */
let is_known_subtype: Tenv.t => Typename.t => Typename.t => bool;


/** [is_known_not_subtype tenv c1 c2] returns true if there is enough information in [tenv] to prove
    that [c1] is not a subtype of [c2].
    Note that [not (is_known_not_subtype tenv c1 c2) == true] does not imply
    that [is_known_subtype tenv c1 c2 == true] */
let is_known_not_subtype: Tenv.t => Typename.t => Typename.t => bool;

let subtypes_to_string: t => string;

let is_cast: t => bool;

let is_instof: t => bool;


/** equality ignoring flags in the subtype */
let equal_modulo_flag: t => t => bool;
