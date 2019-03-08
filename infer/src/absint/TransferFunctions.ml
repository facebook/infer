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
  val join_policy : [`UnderApproximateAfter of int | `NeverJoin]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

module type DisjReady = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.S

  type extras

  type instr

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> instr -> Domain.t list

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module type HILDisjReady = sig
  include DisjReady with type instr := HilInstr.t
end

module MakeHILDisjunctive (TransferFunctions : HILDisjReady) (DConfig : DisjunctiveConfig) = struct
  module CFG = TransferFunctions.CFG

  type extras = TransferFunctions.extras

  module Disjuncts = struct
    type disjunct = TransferFunctions.Domain.t

    let pp_disjunct fmt astate = TransferFunctions.Domain.pp fmt astate

    type t = disjunct list

    let mk_disjunct astate = astate

    let singleton astate = [mk_disjunct astate]

    let elements disjuncts = disjuncts

    let pp f disjuncts = PrettyPrintable.pp_collection ~pp_item:pp_disjunct f disjuncts
  end

  module Domain = struct
    type t = Disjuncts.t

    let join_normalize_into s_froms ~into:s_intos =
      (* ignore states in [s_froms] that are under-approximated in [s_intos] *)
      let s_from_not_in_intos =
        List.rev_filter s_froms ~f:(fun s_from ->
            List.exists s_intos ~f:(fun s_into ->
                TransferFunctions.Domain.( <= ) ~rhs:s_from ~lhs:s_into )
            |> not )
      in
      List.rev_append s_from_not_in_intos s_intos


    let join : t -> t -> t =
     fun lhs rhs ->
      if phys_equal lhs rhs then lhs
      else
        match DConfig.join_policy with
        | `NeverJoin ->
            rhs @ lhs
        | `UnderApproximateAfter n ->
            let lhs_length = List.length lhs in
            if lhs_length >= n then lhs
            else
              (* do not add states from [rhs] (assumed to be the *new* states in the case of a loop)
                 that are already implied by some state in the [lhs] *)
              join_normalize_into rhs ~into:lhs


    let rec ( <= ) ~lhs ~rhs =
      if phys_equal lhs rhs then (* also takes care of the case [lhs = rhs = []] *) true
      else
        (* quick check: [lhs <= rhs] if [rhs] is [something @ lhs] *)
        match rhs with [] -> false | _ :: rhs' -> ( <= ) ~lhs ~rhs:rhs'


    let widen ~prev ~next ~num_iters =
      let (`UnderApproximateAfterNumIterations max_iter) = DConfig.widen_policy in
      if phys_equal prev next then prev else if num_iters > max_iter then prev else join prev next


    let pp = Disjuncts.pp
  end

  let exec_instr pre_disjuncts extras node instr =
    List.fold pre_disjuncts ~init:[] ~f:(fun post_disjuncts pre_disjunct ->
        let disjuncts' = TransferFunctions.exec_instr pre_disjunct extras node instr in
        Domain.join post_disjuncts (List.map disjuncts' ~f:Disjuncts.mk_disjunct) )


  let pp_session_name node f = TransferFunctions.pp_session_name node f
end
