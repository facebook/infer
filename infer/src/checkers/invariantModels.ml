(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type model = Invariant | Variant

let is_invariant = function Invariant -> true | Variant -> false

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ -"__cast" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "iterator" <>--> Variant
      ; +PatternMatch.implements_iterator &:: "hasNext" <>--> Variant
      ; +PatternMatch.implements_iterator &:: "next" <>--> Variant
      ; +PatternMatch.implements_collection &:: "size" <>--> Invariant ]
end
