(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module SPath = Symb.SymbolPath

type t = Path of SPath.partial | Closure of Exp.closure [@@deriving compare]

let pp f = function
  | Path path ->
      SPath.pp_partial f path
  | Closure closure ->
      Exp.pp_closure f closure


module Set = struct
  include AbstractDomain.FiniteSet (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)

  type eval_func_ptrs = SPath.partial -> t

  let of_path path = singleton (Path path)

  let of_closure closure = singleton (Closure closure)

  let subst x eval_func_ptr =
    fold
      (fun func_ptr acc ->
        match func_ptr with
        | Path path ->
            join acc (eval_func_ptr path)
        | Closure _ ->
            add func_ptr acc )
      x empty
end
