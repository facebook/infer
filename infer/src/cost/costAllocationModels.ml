(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost

module ProcName = struct
  let dispatch : (Tenv.t, BasicCost.t, unit) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ +BuiltinDecl.(match_builtin __new) <>--> BasicCost.one
      ; +BuiltinDecl.(match_builtin __new_array) <>--> BasicCost.one
      ; +BuiltinDecl.(match_builtin __objc_alloc_no_fail) <>--> BasicCost.one
      ; +BuiltinDecl.(match_builtin malloc) <>--> BasicCost.one
      ; +BuiltinDecl.(match_builtin malloc_no_fail) <>--> BasicCost.one ]
end
