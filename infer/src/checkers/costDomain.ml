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
module L = Logging
open AbstractDomain.Types

module Alias = struct
  type astate = Prop.pi

  let pp = Prop.pp_pi Pp.text

  let join pi1 pi2 = pi1 @ pi2

  let widen ~prev ~next:_ ~num_iters:_ = prev

  (* DINO: ??? what should be this one?*)

  let ( <= ) ~lhs:_ ~rhs:_ = true

  (*DINO: ???? what should be this one?*)
end

module IntCost = struct
  type astate = int [@@deriving compare]

  let pp fmt i = F.fprintf fmt "%i" i

  let join = Int.max

  let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

  let ( <= ) ~lhs ~rhs = Int.( <= ) lhs rhs
end

module Cost = struct
  include AbstractDomain.TopLifted (IntCost)

  let widen ~prev ~next ~num_iters:_ =
    if phys_equal prev next then prev
    else
      match (prev, next) with
      | NonTop prev, NonTop next when IntCost.( <= ) ~lhs:next ~rhs:prev ->
          NonTop prev
      | _, _ ->
          Top


  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | Top, Top | NonTop _, Top ->
        true
    | Top, NonTop _ ->
        false
    | NonTop c1, NonTop c2 ->
        Int.( <= ) c1 c2


  let plus c1 c2 =
    match (c1, c2) with
    | Top, _ ->
        Top
    | _, Top ->
        Top
    | NonTop c1', NonTop c2' ->
        NonTop (c1' + c2')


  let _minus c1 c2 =
    match (c1, c2) with
    | Top, _ ->
        Top
    | _, Top ->
        assert false (* ????? *)
    | NonTop c1', NonTop c2' ->
        NonTop (c1' - c2')


  let _mult c1 c2 =
    match (c1, c2) with Top, _ | _, Top -> Top | NonTop c1', NonTop c2' -> NonTop (c1' * c2')


  let min c1 c2 =
    match (c1, c2) with
    | Top, _ ->
        c2
    | _, Top ->
        c1
    | NonTop c1', NonTop c2' ->
        NonTop (Int.min c1' c2')


  let max c1 c2 =
    match (c1, c2) with
    | Top, _ | _, Top ->
        Top
    | NonTop c1', NonTop c2' ->
        NonTop (Int.max c1' c2')


  let _one = NonTop 1

  let zero = NonTop 0

  let nth n = NonTop n

  let dec n = match n with NonTop n' -> NonTop (n' - 1) | _ -> n

  let inc n = match n with NonTop n' -> NonTop (n' + 1) | _ -> n

  let pp_l fmt c =
    let c'' = match c with Top -> Top | NonTop c' -> NonTop (-c') in
    pp fmt c''


  let pp_u = pp
end

module CostSingleIteration = Cost

module IntPair = struct
  (* Represents node id,instr index within node *)
  include AbstractDomain.Pair (IntCost) (IntCost)

  type t = IntCost.astate * IntCost.astate [@@deriving compare]
end

(* Map (node,instr) -> basic cost  *)
module NodeInstructionToCostMap = AbstractDomain.Map (IntPair) (Itv.Bound)

module ItvPureCost = struct
  (** (l, u) represents the closed interval [-l; u] (of course infinite bounds are open) *)
  type astate = Cost.astate * Cost.astate

  type t = astate

  let ( <= ) : lhs:t -> rhs:t -> bool =
   fun ~lhs:(l1, u1) ~rhs:(l2, u2) -> Cost.( <= ) ~lhs:l1 ~rhs:l2 && Cost.( <= ) ~lhs:u1 ~rhs:u2


  let join : t -> t -> t = fun (l1, u1) (l2, u2) -> (Cost.join l1 l2, Cost.join u1 u2)

  let meet (l1, u1) (l2, u2) =
    let l = match (l1, l2) with Top, _ -> l2 | _, Top -> l1 | _, _ -> Cost.max l1 l2 in
    L.(debug Analysis Medium)
      "@\n l1= %a, l2=%a  l/max=%a @\n" Cost.pp_l l1 Cost.pp_l l2 Cost.pp_l l ;
    (* l1 l2 are negative so we use min instead of max *)
    let u = Cost.min u1 u2 in
    L.(debug Analysis Medium)
      "@\n u1= %a, u2=%a  u/min=%a @\n" Cost.pp_u u1 Cost.pp_u u2 Cost.pp_u u ;
    (l, u)


  let widen : prev:t -> next:t -> num_iters:int -> t =
   fun ~prev:(l1, u1) ~next:(l2, u2) ~num_iters ->
    (Cost.widen ~prev:l1 ~next:l2 ~num_iters, Cost.widen ~prev:u1 ~next:u2 ~num_iters)


  let pp : F.formatter -> t -> unit =
   fun fmt (l, u) -> F.fprintf fmt "[%a, %a]" Cost.pp_l l Cost.pp_u u


  let of_int n = (Cost.nth (-n), Cost.nth n)

  let of_int_lit i = of_int (IntLit.to_int i)

  (* range [-a,b] = (b+a+1) *)
  let range (l, u) = Cost.inc (Cost.plus u l)

  let plus : t -> t -> t = fun (l1, u1) (l2, u2) -> (Cost.plus l1 l2, Cost.plus u1 u2)

  (* a const interval is of type [-n,n] *)
  let is_const (l, u) =
    match (l, u) with NonTop l', NonTop u' -> Int.equal (-l') u' | _, _ -> false


  let is_empty itv = Cost.( <= ) ~lhs:(range itv) ~rhs:Cost.zero

  let negation (u, l) = (l, u)
end

module EnvDomain = AbstractDomain.Map (Exp) (ItvPureCost)
module EnvDomainBO = AbstractDomain.Map (Exp) (Itv)

module SemanticDomain = struct
  include AbstractDomain.Pair (EnvDomain) (Alias)

  let get_val env e =
    match e with
    | Exp.Var _ | Exp.Lvar _ -> (
      match EnvDomain.find_opt e env with
      | Some c ->
          c
      | None ->
          L.(debug Analysis Medium)
            " @\n[WARNING:] Expression %a not found on env. Returning zero@\n" Exp.pp e ;
          ItvPureCost.of_int 0 )
    | Exp.Const Cint i ->
        ItvPureCost.of_int (IntLit.to_int i)
    | _ ->
        L.(debug Analysis Medium) " @\n[WARNING:] Returning value zero for %a @\n" Exp.pp e ;
        ItvPureCost.of_int 0


  let eval_bin_op op val1 val2 =
    match op with Binop.PlusA -> ItvPureCost.plus val1 val2 | _ -> assert false


  let update_env env lhs rhs =
    let open Exp in
    match rhs with
    | Var _ | UnOp _ ->
        env
    | BinOp (op, e1, e2) ->
        let val1 = get_val env e1 in
        let val2 = get_val env e2 in
        let res = eval_bin_op op val1 val2 in
        L.(debug Analysis Medium)
          " @\n #### Operation: %a       val1= %a  val2=%a  res = %a@\n" Exp.pp rhs ItvPureCost.pp
          val1 ItvPureCost.pp val2 ItvPureCost.pp res ;
        L.(debug Analysis Medium)
          " @\nAdding binding %a --> %a to env o@\n" Exp.pp lhs ItvPureCost.pp res ;
        EnvDomain.add lhs res env
    | Const Cint i ->
        (*let lhs_var = Var.of_pvar lhs in*)
        let itv = ItvPureCost.of_int_lit i in
        EnvDomain.add lhs itv env
    | Lvar _ ->
        let val_rhs = get_val env rhs in
        L.(debug Analysis Medium)
          " @\nAdding binding %a --> %a to env o@\n" Exp.pp lhs ItvPureCost.pp val_rhs ;
        EnvDomain.add lhs val_rhs env
    | Exn _ | Closure _ | Const _ | Cast _ | Lfield _ | Lindex _ | Sizeof _ ->
        env


  let remove_from_alias_list alias e =
    let remove_exp a =
      match a with Sil.Aeq (e1, e2) when Exp.equal e1 e || Exp.equal e e2 -> false | _ -> true
    in
    List.filter ~f:remove_exp alias


  let update_alias tenv alias e1 e2 =
    match (e1, e2) with
    | Exp.Var _, Exp.Var _ | Exp.Var _, Exp.Lvar _ | Exp.Lvar _, Exp.Var _ | Exp.Lvar _, Exp.Lvar _ ->
        (*Sil.Aeq (e1,e2) :: alias*)
        let alias' = remove_from_alias_list alias e1 in
        let prop = Prop.normalize tenv (Prop.from_pi alias') in
        let prop' = Prop.conjoin_eq tenv e1 e2 prop in
        Prop.get_pure prop'
    | Exp.Lvar _, _ ->
        remove_from_alias_list alias e1
    | _ ->
        alias


  (* Try to improve value for e using alias variables to get the tightest interval *)
  let update_alias_env env alias e =
    let try_restrict_itv env_acc e' e_val =
      let v' =
        match EnvDomain.find_opt e' env_acc with
        | Some v_pre ->
            L.(debug Analysis Medium) "@\n>>>Starting meet \n" ;
            let meet = ItvPureCost.meet v_pre e_val in
            L.(debug Analysis Medium)
              "@\n>>>Result Meet v_pre= %a   e_val= %a --> %a\n" ItvPureCost.pp v_pre
              ItvPureCost.pp e_val ItvPureCost.pp meet ;
            if ItvPureCost.is_empty meet then e_val else meet
        | None ->
            e_val
      in
      EnvDomain.add e' v' env_acc
    in
    let do_atom env_acc e_val a =
      match a with
      | Sil.Aeq (e1, e2) ->
          if Exp.equal e1 e then try_restrict_itv env_acc e2 e_val
          else if Exp.equal e2 e then try_restrict_itv env_acc e1 e_val
          else env_acc
      | _ ->
          env
    in
    match EnvDomain.find_opt e env with
    | Some e_val ->
        List.fold ~f:(fun acc a -> do_atom acc e_val a) ~init:env alias
    | _ ->
        env


  let sem_binop op (e1_opt, v1_opt) (e2_opt, v2_opt) =
    let open Binop in
    match op with
    | Lt -> (
      match ((e1_opt, v1_opt), (e2_opt, v2_opt)) with
      | (Some e, _), (_, Some (v1, v2)) when ItvPureCost.is_const (v1, v2) ->
          let ub = Cost.dec v2 in
          (Some e, Some (Top, ub))
      | (Some e1, Some v1), (Some e2, Some v2) ->
          L.die InternalError
            " @\n Incomplete Lt binon. Dies with (e1,v1)= (%a,%a)   (e2,v2)=(%a, %a) @\n" Exp.pp e1
            ItvPureCost.pp v1 Exp.pp e2 ItvPureCost.pp v2
      | (Some e1, None), (Some e2, None) ->
          L.die InternalError
            " @\n Incomplete Lt binon. Dies with (e1,v1)= (%a,None)   (e2,v2)=(%a,None) @\n" Exp.pp
            e1 Exp.pp e2
      | _ ->
          assert false )
    | Gt | Le | Ge | Eq | _ ->
        L.die InternalError " @\n Incomplete Sem_binop function. Dies with op= %s @\n"
          (Binop.str Pp.text op)


  let sem_unop op (e1_opt, v1_opt) =
    match op with
    | Unop.LNot -> (
      match (e1_opt, v1_opt) with
      | Some e, Some v ->
          (Some e, Some (ItvPureCost.negation v))
      | _ ->
          assert false )
    | _ ->
        L.die InternalError " @\n Incomplete Sem_unop function. Dies with op= %s @\n" (Unop.str op)


  (* semantics of expression e according to environment env *)
  let rec sem env e : Exp.t option * ItvPureCost.t option =
    match e with
    | Exp.BinOp (op, e1, e2) ->
        let res1 = sem env e1 in
        let res2 = sem env e2 in
        sem_binop op res1 res2
    | Exp.Const Cint i ->
        (None, Some (ItvPureCost.of_int_lit i))
    | Exp.Lvar _ | Exp.Var _ ->
        (Some e, None)
    | Exp.UnOp (op, e1, _) ->
        let res1 = sem env e1 in
        sem_unop op res1
    | _ ->
        L.die InternalError " @\n Incomplete Sem function. Dies with e= %a @\n" Exp.pp e
end

type summary = {post: Itv.Bound.t}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" Itv.Bound.pp post
