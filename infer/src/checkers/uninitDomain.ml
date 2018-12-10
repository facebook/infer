(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Domain = AbstractDomain.InvertedSet (HilExp.AccessExpression)

module MaybeUninitVars = struct
  include AbstractDomain.FiniteSet (HilExp.AccessExpression)

  let subst_formal_actual_fields formal_var actual_base_var init_formals =
    map
      (fun access_expr ->
        let v, t = HilExp.AccessExpression.get_base access_expr in
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
        HilExp.AccessExpression.replace_base ~remove_deref_after_base:true (v', t') access_expr )
      init_formals


  let remove_init_fields actual_base formal_var maybe_uninit_vars init_formals =
    match actual_base with
    | actual_base_var, {Typ.desc= Tptr ({Typ.desc= Tstruct _}, _) | Tstruct _} ->
        let actuals_to_remove =
          subst_formal_actual_fields formal_var actual_base_var init_formals
        in
        diff maybe_uninit_vars actuals_to_remove
    | _ ->
        maybe_uninit_vars


  let remove_all_fields tenv base maybe_uninit_vars =
    match base with
    | _, {Typ.desc= Tptr ({Typ.desc= Tstruct name_struct}, _)} | _, {Typ.desc= Tstruct name_struct}
      -> (
      match Tenv.lookup tenv name_struct with
      | Some {fields} ->
          List.fold fields ~init:maybe_uninit_vars ~f:(fun acc (fn, _, _) ->
              remove
                (HilExp.AccessExpression.field_offset (HilExp.AccessExpression.base base) fn)
                acc )
      | _ ->
          maybe_uninit_vars )
    | _ ->
        maybe_uninit_vars


  let remove_dereference_access base maybe_uninit_vars =
    match base with
    | _, {Typ.desc= Tptr _} ->
        remove
          (HilExp.AccessExpression.dereference (HilExp.AccessExpression.base base))
          maybe_uninit_vars
    | _ ->
        maybe_uninit_vars


  let remove_all_array_elements base maybe_uninit_vars =
    match base with
    | _, {Typ.desc= Tptr (elt, _)} ->
        remove
          (HilExp.AccessExpression.array_offset (HilExp.AccessExpression.base base) elt None)
          maybe_uninit_vars
    | _ ->
        maybe_uninit_vars


  let remove_everything_under tenv access_expr maybe_uninit_vars =
    let base = HilExp.AccessExpression.get_base access_expr in
    maybe_uninit_vars |> remove access_expr |> remove_all_fields tenv base
    |> remove_all_array_elements base |> remove_dereference_access base
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
  type t = {maybe_uninit_vars: Domain1.t; aliased_vars: Domain2.t; prepost: Domain3.t prepost}

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      let {maybe_uninit_vars= lhs_uv; aliased_vars= lhs_av; prepost= {pre= lhs_pre; post= lhs_post}}
          =
        lhs
      in
      let {maybe_uninit_vars= rhs_uv; aliased_vars= rhs_av; prepost= {pre= rhs_pre; post= rhs_post}}
          =
        rhs
      in
      Domain1.( <= ) ~lhs:lhs_uv ~rhs:rhs_uv
      && Domain2.( <= ) ~lhs:lhs_av ~rhs:rhs_av
      && Domain3.( <= ) ~lhs:lhs_pre ~rhs:rhs_pre
      && Domain3.( <= ) ~lhs:lhs_post ~rhs:rhs_post


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      let {maybe_uninit_vars= uv1; aliased_vars= av1; prepost= {pre= pre1; post= post1}} =
        astate1
      in
      let {maybe_uninit_vars= uv2; aliased_vars= av2; prepost= {pre= pre2; post= post2}} =
        astate2
      in
      { maybe_uninit_vars= Domain1.join uv1 uv2
      ; aliased_vars= Domain2.join av1 av2
      ; prepost= {pre= Domain3.join pre1 pre2; post= Domain3.join post1 post2} }


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      let { maybe_uninit_vars= prev_uv
          ; aliased_vars= prev_av
          ; prepost= {pre= prev_pre; post= prev_post} } =
        prev
      in
      let { maybe_uninit_vars= next_uv
          ; aliased_vars= next_av
          ; prepost= {pre= next_pre; post= next_post} } =
        next
      in
      { maybe_uninit_vars= Domain1.widen ~prev:prev_uv ~next:next_uv ~num_iters
      ; aliased_vars= Domain2.widen ~prev:prev_av ~next:next_av ~num_iters
      ; prepost=
          { pre= Domain3.widen ~prev:prev_pre ~next:next_pre ~num_iters
          ; post= Domain3.widen ~prev:prev_post ~next:next_post ~num_iters } }


  let pp fmt {maybe_uninit_vars= uv; aliased_vars= av; prepost= {pre; post}} =
    F.fprintf fmt "@\n maybe_uninit_vars: %a @\n aliased_vars: %a @\n prepost: (%a, %a)" Domain1.pp
      uv Domain2.pp av Domain3.pp pre Domain3.pp post
end

module Summary = struct
  (* pre = set of parameters initialized inside the procedure;
    post = set of uninit local variables of the procedure *)
  type t = Domain.t prepost

  let pp fmt {pre; post} =
    F.fprintf fmt "@\n Pre: %a @\nPost: %a @\n" Domain.pp pre Domain.pp post
end
