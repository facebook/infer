(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type S = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.S

  type extras

  type instr

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> instr -> Domain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module type SIL = sig
  include S with type instr := Sil.instr
end

module type HIL = sig
  include S with type instr := HilInstr.t
end

module type MakeHIL = functor (C : ProcCfg.S) -> sig
  include HIL with module CFG = C
end

module type DisjunctiveConfig = sig
  type domain_t [@@deriving compare]

  val join_policy : [`JoinAfter of int | `UnderApproximateAfter of int | `NeverJoin]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

module MakeHILDisjunctive
    (TransferFunctions : HIL)
    (DConfig : DisjunctiveConfig with type domain_t = TransferFunctions.Domain.t) =
struct
  module CFG = TransferFunctions.CFG

  type extras = TransferFunctions.extras

  module Domain = struct
    module Set = AbstractDomain.FiniteSet (struct
      include TransferFunctions.Domain

      let compare = DConfig.compare_domain_t
    end)

    let real_join lhs rhs =
      let union = Set.join lhs rhs in
      match DConfig.join_policy with
      | `NeverJoin ->
          union
      | `UnderApproximateAfter n ->
          if Set.cardinal union <= n then union else lhs
      | `JoinAfter n ->
          if Set.cardinal union <= n then union
          else
            let joined =
              Set.fold
                (fun dom joined ->
                  match joined with
                  | None ->
                      Some dom
                  | Some joined ->
                      Some (TransferFunctions.Domain.join dom joined) )
                union None
            in
            Set.singleton (Option.value_exn joined)


    let real_widen ~prev ~next ~num_iters =
      let (`UnderApproximateAfterNumIterations max_iter) = DConfig.widen_policy in
      if num_iters > max_iter then prev else real_join prev next


    include Set

    let join = real_join

    let widen = real_widen
  end

  let exec_instr disj_dom extras node instr =
    Domain.map (fun dom -> TransferFunctions.exec_instr dom extras node instr) disj_dom


  let pp_session_name node f = TransferFunctions.pp_session_name node f

  let of_domain x = Domain.singleton x
end
