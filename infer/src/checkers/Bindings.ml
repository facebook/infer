(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Dead = AbstractDomain.InvertedSet (Var)
module Reverse = AbstractDomain.FiniteMultiMap (Var) (Var)

type t =
  { resolve: IdAccessPathMapDomain.t
  ; reverse: Reverse.t
        (** there is a [x -> y] mapping in reverse for each variable [y] appearing in [resolve x] *)
  ; dead: Dead.t }

let empty = {resolve= IdAccessPathMapDomain.empty; reverse= Reverse.empty; dead= Dead.empty}

let add id ap {resolve; reverse; dead} =
  let resolve = IdAccessPathMapDomain.add id ap resolve in
  let reverse =
    HilExp.AccessExpression.fold_vars ap ~init:reverse ~f:(fun acc var_in_ap ->
        Reverse.add var_in_ap id acc )
  in
  let dead = Dead.remove id dead in
  {resolve; reverse; dead}


let exit_scope id bindings =
  let {resolve; reverse; dead} = bindings in
  match (Reverse.mem id reverse, IdAccessPathMapDomain.find_opt id resolve) with
  | true, None ->
      let dead = Dead.add id dead in
      ({resolve; reverse; dead}, [])
  | true, Some _ ->
      L.(die InternalError) "Variable appearing on both sides of bindings"
  | false, None ->
      (bindings, [id])
  | false, Some ap ->
      let resolve = IdAccessPathMapDomain.remove id resolve in
      let reverse, vars, dead =
        HilExp.AccessExpression.fold_vars ap ~init:(reverse, [], dead)
          ~f:(fun (reverse, vars, dead) var_in_ap ->
            let reverse = Reverse.remove var_in_ap id reverse in
            if (not (Reverse.mem var_in_ap reverse)) && Dead.mem var_in_ap dead then
              (reverse, var_in_ap :: vars, Dead.remove var_in_ap dead)
            else (reverse, vars, dead) )
      in
      ({resolve; reverse; dead}, vars)


let resolve {resolve} id = IdAccessPathMapDomain.find_opt id resolve

let fold {resolve} ~init ~f = IdAccessPathMapDomain.fold f resolve init

let pp f {resolve; reverse; dead} =
  F.fprintf f "{@[<v1> resolve=@[<hv>%a@];@;reverse=@[<hv>%a@];@;dead=@[<hv>%a@];@]}"
    IdAccessPathMapDomain.pp resolve Reverse.pp reverse Dead.pp dead


let ( <= ) ~lhs ~rhs = IdAccessPathMapDomain.( <= ) ~lhs:lhs.resolve ~rhs:rhs.resolve

let join bindings1 bindings2 =
  if phys_equal bindings1 bindings2 then bindings1
  else
    let {resolve= resolve1; reverse= reverse1; dead= dead1} = bindings1 in
    let {resolve= resolve2; reverse= reverse2; dead= dead2} = bindings2 in
    { resolve= IdAccessPathMapDomain.join resolve1 resolve2
    ; reverse= Reverse.join reverse1 reverse2
    ; dead= Dead.join dead1 dead2 }


let widen ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let {resolve= resolve1; reverse= reverse1; dead= dead1} = prev in
    let {resolve= resolve2; reverse= reverse2; dead= dead2} = next in
    { resolve= IdAccessPathMapDomain.widen ~prev:resolve1 ~next:resolve2 ~num_iters
    ; reverse= Reverse.widen ~prev:reverse1 ~next:reverse2 ~num_iters
    ; dead= Dead.widen ~prev:dead1 ~next:dead2 ~num_iters }
