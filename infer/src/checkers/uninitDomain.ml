(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** Forward analysis to compute uninitialized variables at each program point *)
module Domain = AbstractDomain.InvertedSet (AccessPath)

(* pre = set of parameters initialized inside the procedure;
   post = set of uninit local variables of the procedure *)
type summary = {pre: Domain.t; post: Domain.t}

module VarPair = struct
  type t = Var.t * Var.t [@@deriving compare]

  let pp fmt pair = F.fprintf fmt " (%a, %a)" Var.pp (fst pair) Var.pp (snd pair)
end

let pp_summary fmt {pre; post} =
  F.fprintf fmt "@\n Pre: %a @\nPost: %a @\n" Domain.pp pre Domain.pp post


module Record
    (Domain1 : AbstractDomain.S)
    (Domain2 : AbstractDomain.S)
    (Domain3 : AbstractDomain.S) =
struct
  type astate =
    { uninit_vars: Domain1.astate
    ; aliased_vars: Domain2.astate
    ; prepost: Domain3.astate * Domain3.astate }

  let ( <= ) ~lhs:({uninit_vars= lhs_uv; aliased_vars= lhs_av; prepost= lhs_pp} as lhs)
      ~rhs:({uninit_vars= rhs_uv; aliased_vars= rhs_av; prepost= rhs_pp} as rhs) =
    if phys_equal lhs rhs then true
    else
      Domain1.( <= ) ~lhs:lhs_uv ~rhs:rhs_uv && Domain2.( <= ) ~lhs:lhs_av ~rhs:rhs_av
      && Domain3.( <= ) ~lhs:(fst lhs_pp) ~rhs:(fst rhs_pp)
      && Domain3.( <= ) ~lhs:(snd lhs_pp) ~rhs:(snd rhs_pp)


  let join ({uninit_vars= uv1; aliased_vars= av1; prepost= pp1} as astate1)
      ({uninit_vars= uv2; aliased_vars= av2; prepost= pp2} as astate2) =
    if phys_equal astate1 astate2 then astate1
    else
      { uninit_vars= Domain1.join uv1 uv2
      ; aliased_vars= Domain2.join av1 av2
      ; prepost= (Domain3.join (fst pp1) (fst pp2), Domain3.join (snd pp1) (snd pp2)) }


  let widen ~prev:({uninit_vars= prev_uv; aliased_vars= prev_av; prepost= prev_pp} as prev)
      ~next:({uninit_vars= next_uv; aliased_vars= next_av; prepost= next_pp} as next) ~num_iters =
    if phys_equal prev next then prev
    else
      { uninit_vars= Domain1.widen ~prev:prev_uv ~next:next_uv ~num_iters
      ; aliased_vars= Domain2.widen ~prev:prev_av ~next:next_av ~num_iters
      ; prepost=
          ( Domain3.widen ~prev:(fst prev_pp) ~next:(fst next_pp) ~num_iters
          , Domain3.widen ~prev:(snd prev_pp) ~next:(snd next_pp) ~num_iters ) }


  let pp fmt {uninit_vars= uv; aliased_vars= av; prepost= pp} =
    F.fprintf fmt "@\n uninit_vars: %a @\n aliased_vars: %a @\n prepost: (%a, %a)" Domain1.pp uv
      Domain2.pp av Domain3.pp (fst pp) Domain3.pp (snd pp)
end
