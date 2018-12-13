(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(**
  Helpers function to enforce physical equality.

  Let suppose [construct/deconstruct] is a 1-level-allocation OCaml construction/deconstruction,
  such as variant type, tuple or record construction.
  Instead of writing
    let a = deconstruct a0 in
    let b = deconstruct b0 in
    let res = f a b in
    if phys_equal res a then a0
    else if phys_equal res b then b0
    else construct res

  Simply write
    PhysEqual.optim2 ~res:(construct (f a b)) a0 b0
*)

val optim1 : res:'a -> 'a -> 'a

val optim2 : res:'a -> 'a -> 'a -> 'a
