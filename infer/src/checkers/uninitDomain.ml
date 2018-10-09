(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Forward analysis to compute uninitialized variables at each program point *)
module Domain = struct
  include AbstractDomain.InvertedSet (AccessExpression)

  let remove_init_fields base formal_var uninit_vars init_fields =
    let subst_formal_actual_fields actual_base_var initialized_fields =
      map
        (fun access_expr ->
          let v, t = AccessExpression.get_base access_expr in
          let v' = if Var.equal v formal_var then actual_base_var else v in
          let t' =
            match t.desc with
            | Typ.Tptr ({Typ.desc= Tstruct _ as desc}, _) ->
                (* a pointer to struct needs to be changed into struct
                 as the actual is just type struct and it would make it
                 equality fail. Not sure why the actual are type struct when
                 passed by reference *)
                {t with Typ.desc}
            | _ ->
                t
          in
          AccessExpression.replace_base ~remove_deref_after_base:true (v', t') access_expr )
        initialized_fields
    in
    match base with
    | actual_base_var, {Typ.desc= Tptr ({Typ.desc= Tstruct _}, _) | Tstruct _} ->
        diff uninit_vars (subst_formal_actual_fields actual_base_var init_fields)
    | _ ->
        uninit_vars


  let remove_all_fields tenv base uninit_vars =
    match base with
    | _, {Typ.desc= Tptr ({Typ.desc= Tstruct name_struct}, _)} | _, {Typ.desc= Tstruct name_struct}
      -> (
      match Tenv.lookup tenv name_struct with
      | Some {fields} ->
          List.fold fields ~init:uninit_vars ~f:(fun acc (fn, _, _) ->
              remove (AccessExpression.FieldOffset (Base base, fn)) acc )
      | _ ->
          uninit_vars )
    | _ ->
        uninit_vars


  let remove_dereference_access base uninit_vars =
    match base with
    | _, {Typ.desc= Tptr _} ->
        remove (AccessExpression.Dereference (Base base)) uninit_vars
    | _ ->
        uninit_vars


  let remove_all_array_elements base uninit_vars =
    match base with
    | _, {Typ.desc= Tptr (elt, _)} ->
        remove (AccessExpression.ArrayOffset (Base base, elt, [])) uninit_vars
    | _ ->
        uninit_vars
end

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
