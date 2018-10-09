(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Forward analysis to compute uninitialized variables at each program point *)
module Domain = AbstractDomain.InvertedSet (AccessExpression)

type 'a prepost = {pre: 'a; post: 'a}

module VarPair = struct
  type t = Var.t * Var.t [@@deriving compare]

  let pp fmt pair = F.fprintf fmt " (%a, %a)" Var.pp (fst pair) Var.pp (snd pair)
end

module Record
    (Domain1 : AbstractDomain.S)
    (Domain2 : AbstractDomain.S)
    (Domain3 : AbstractDomain.S) =
struct
  type astate =
    {uninit_vars: Domain1.astate; aliased_vars: Domain2.astate; prepost: Domain3.astate prepost}

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      let {uninit_vars= lhs_uv; aliased_vars= lhs_av; prepost= {pre= lhs_pre; post= lhs_post}} =
        lhs
      in
      let {uninit_vars= rhs_uv; aliased_vars= rhs_av; prepost= {pre= rhs_pre; post= rhs_post}} =
        rhs
      in
      Domain1.( <= ) ~lhs:lhs_uv ~rhs:rhs_uv
      && Domain2.( <= ) ~lhs:lhs_av ~rhs:rhs_av
      && Domain3.( <= ) ~lhs:lhs_pre ~rhs:rhs_pre
      && Domain3.( <= ) ~lhs:lhs_post ~rhs:rhs_post


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      let {uninit_vars= uv1; aliased_vars= av1; prepost= {pre= pre1; post= post1}} = astate1 in
      let {uninit_vars= uv2; aliased_vars= av2; prepost= {pre= pre2; post= post2}} = astate2 in
      { uninit_vars= Domain1.join uv1 uv2
      ; aliased_vars= Domain2.join av1 av2
      ; prepost= {pre= Domain3.join pre1 pre2; post= Domain3.join post1 post2} }


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      let {uninit_vars= prev_uv; aliased_vars= prev_av; prepost= {pre= prev_pre; post= prev_post}}
          =
        prev
      in
      let {uninit_vars= next_uv; aliased_vars= next_av; prepost= {pre= next_pre; post= next_post}}
          =
        next
      in
      { uninit_vars= Domain1.widen ~prev:prev_uv ~next:next_uv ~num_iters
      ; aliased_vars= Domain2.widen ~prev:prev_av ~next:next_av ~num_iters
      ; prepost=
          { pre= Domain3.widen ~prev:prev_pre ~next:next_pre ~num_iters
          ; post= Domain3.widen ~prev:prev_post ~next:next_post ~num_iters } }


  let pp fmt {uninit_vars= uv; aliased_vars= av; prepost= {pre; post}} =
    F.fprintf fmt "@\n uninit_vars: %a @\n aliased_vars: %a @\n prepost: (%a, %a)" Domain1.pp uv
      Domain2.pp av Domain3.pp pre Domain3.pp post
end

module Summary = struct
  (* pre = set of parameters initialized inside the procedure;
    post = set of uninit local variables of the procedure *)
  type t = Domain.t prepost

  let pp fmt {pre; post} = F.fprintf fmt "@\n Pre: %a @\nPost: %a @\n" Domain.pp pre Domain.pp post
end
