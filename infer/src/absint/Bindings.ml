(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module Reverse = struct
  module Dead = AbstractDomain.BooleanAnd
  module VarSet = AbstractDomain.FiniteSet (Var)
  module DeadAndVarSet = AbstractDomain.Pair (Dead) (VarSet)
  module M = AbstractDomain.Map (Var) (DeadAndVarSet)

  (* invariant: sets are not empty *)
  type t = M.t

  let empty = M.empty

  let add k v rm =
    M.update k
      (function
        | None -> Some (false, VarSet.singleton v) | Some (_, s) -> Some (false, VarSet.add v s) )
      rm


  let mem = M.mem

  let deadify k rm =
    M.update k
      (function (None | Some (true, _)) as vo -> vo | Some (false, s) -> Some (true, s))
      rm


  let remove k v rm =
    match M.find_opt k rm with
    | None ->
        (rm, false)
    | Some (dead, s) ->
        let s = VarSet.remove v s in
        if VarSet.is_empty s then (M.remove k rm, dead) else (M.add k (dead, s) rm, false)


  let pp = M.pp

  let join = M.join

  let widen = M.widen
end

type t =
  { resolve: IdAccessPathMapDomain.t
  ; reverse: Reverse.t
        (** there is a [x -> y] mapping in reverse for each variable [y] appearing in [resolve x] *)
  }

let empty = {resolve= IdAccessPathMapDomain.empty; reverse= Reverse.empty}

let add id ap {resolve; reverse} =
  if Reverse.mem id reverse then (
    L.internal_error "Variable %a appearing on both sides of bindings@\n" Var.pp id ;
    if Config.write_html then
      L.d_printfln "Variable %a appearing on both sides of bindings@\n" Var.pp id ) ;
  let resolve = IdAccessPathMapDomain.add id ap resolve in
  let reverse =
    HilExp.AccessExpression.fold_vars ap ~init:reverse ~f:(fun acc var_in_ap ->
        Reverse.add var_in_ap id acc )
  in
  {resolve; reverse}


let exit_scope id bindings =
  let {resolve; reverse} = bindings in
  match IdAccessPathMapDomain.find_opt id resolve with
  | None ->
      if Reverse.mem id reverse then
        let reverse = Reverse.deadify id reverse in
        ({resolve; reverse}, [])
      else (bindings, [id])
  | Some ap ->
      let resolve = IdAccessPathMapDomain.remove id resolve in
      let reverse, vars =
        HilExp.AccessExpression.fold_vars ap ~init:(reverse, [])
          ~f:(fun (reverse, vars) var_in_ap ->
            let reverse, dead = Reverse.remove var_in_ap id reverse in
            let vars = if dead then var_in_ap :: vars else vars in
            (reverse, vars) )
      in
      ({resolve; reverse}, vars)


let resolve {resolve} id = IdAccessPathMapDomain.find_opt id resolve

let fold {resolve} ~init ~f = IdAccessPathMapDomain.fold f resolve init

let pp f {resolve; reverse} =
  F.fprintf f "{@[<v1> resolve=@[<hv>%a@];@;reverse=@[<hv>%a@]@]}" IdAccessPathMapDomain.pp resolve
    Reverse.pp reverse


let leq ~lhs ~rhs = IdAccessPathMapDomain.leq ~lhs:lhs.resolve ~rhs:rhs.resolve

let join bindings1 bindings2 =
  if phys_equal bindings1 bindings2 then bindings1
  else
    let {resolve= resolve1; reverse= reverse1} = bindings1 in
    let {resolve= resolve2; reverse= reverse2} = bindings2 in
    {resolve= IdAccessPathMapDomain.join resolve1 resolve2; reverse= Reverse.join reverse1 reverse2}


let widen ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let {resolve= resolve1; reverse= reverse1} = prev in
    let {resolve= resolve2; reverse= reverse2} = next in
    { resolve= IdAccessPathMapDomain.widen ~prev:resolve1 ~next:resolve2 ~num_iters
    ; reverse= Reverse.widen ~prev:reverse1 ~next:reverse2 ~num_iters }
