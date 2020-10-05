(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Path of Symb.SymbolPath.partial | Closure of Exp.closure

module Set : sig
  include AbstractDomain.FiniteSetS with type elt = t

  type eval_func_ptrs = Symb.SymbolPath.partial -> t

  val of_path : Symb.SymbolPath.partial -> t

  val of_closure : Exp.closure -> t

  val subst : t -> eval_func_ptrs -> t
end
