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
  val join_policy : [`JoinAfter of int | `UnderApproximateAfter of int | `NeverJoin]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

module type DisjReady = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.S

  module DisjunctiveDomain : Caml.Set.S with type elt = Domain.t

  type extras

  type instr

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> instr -> DisjunctiveDomain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module type HILDisjReady = sig
  include DisjReady with type instr := HilInstr.t
end

module MakeHILDisjunctive (TransferFunctions : HILDisjReady) (DConfig : DisjunctiveConfig) = struct
  module CFG = TransferFunctions.CFG

  type extras = TransferFunctions.extras

  module Domain = struct
    module Set = TransferFunctions.DisjunctiveDomain

    let join lhs rhs =
      if phys_equal lhs rhs then lhs
      else
        let union = Set.union lhs rhs in
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


    let widen ~prev ~next ~num_iters =
      if phys_equal prev next then prev
      else
        let (`UnderApproximateAfterNumIterations max_iter) = DConfig.widen_policy in
        if num_iters > max_iter then prev else join prev next


    let ( <= ) ~lhs ~rhs = if phys_equal lhs rhs then true else Set.subset lhs rhs

    let pp f set =
      PrettyPrintable.pp_collection ~pp_item:TransferFunctions.Domain.pp f (Set.elements set)


    include Set
  end

  let exec_instr disj_dom extras node instr =
    Domain.fold
      (fun dom result ->
        TransferFunctions.exec_instr dom extras node instr |> Domain.Set.union result )
      disj_dom Domain.Set.empty


  let pp_session_name node f = TransferFunctions.pp_session_name node f
end
