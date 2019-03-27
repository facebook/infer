(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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

  module Domain : AbstractDomain.NoJoin

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
    (* The disjunctive domain transformer should really be part of the Abstract Interpreter instead
       of the Transfer Functions as the scheduler needs to know which disjuncts have already been
       explored. But for now let's emulate that with a disgusting hack in the transfer functions
       that try to skip disjuncts we have already explored, using the [visited] flag. *)
    type disjunct = {visited: bool; astate: TransferFunctions.Domain.t}

    let pp_disjunct fmt ({visited; astate}[@warning "+9"]) =
      F.fprintf fmt "{@[<hv>visited= %b;@;astate= @[%a@]@]}" visited TransferFunctions.Domain.pp
        astate


    type t = disjunct list

    let mk_disjunct astate = {visited= false; astate}

    let singleton astate = [mk_disjunct astate]

    let elements disjuncts = List.map disjuncts ~f:(fun {astate} -> astate)

    let pp f disjuncts = PrettyPrintable.pp_collection ~pp_item:pp_disjunct f disjuncts
  end

  type disjunct_t = Disjuncts.disjunct = {visited: bool; astate: TransferFunctions.Domain.t}

  module Domain = struct
    type t = Disjuncts.t

    let join_normalize_into s_froms ~into:s_intos =
      (* ignore states in [s_froms] that are under-approximated in [s_intos] *)
      let s_from_not_in_intos =
        List.rev_filter s_froms ~f:(fun s_from ->
            List.exists s_intos ~f:(fun s_into ->
                TransferFunctions.Domain.( <= ) ~rhs:s_from.astate ~lhs:s_into.astate )
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


    let set_visited b disjuncts =
      if List.for_all disjuncts ~f:(fun {visited} -> Bool.equal visited b) then disjuncts
      else List.map disjuncts ~f:(fun disjunct -> {disjunct with visited= b})


    let rec ( <= ) ~lhs ~rhs =
      if phys_equal lhs rhs then (* also takes care of the case [lhs = rhs = []] *) true
      else
        (* quick check: [lhs <= rhs] if [rhs] is [something @ lhs] *)
        match rhs with [] -> false | _ :: rhs' -> ( <= ) ~lhs ~rhs:rhs'


    let widen ~prev ~next ~num_iters =
      let (`UnderApproximateAfterNumIterations max_iter) = DConfig.widen_policy in
      if phys_equal prev next || num_iters > max_iter then set_visited false prev
      else
        let prev = set_visited true prev in
        let post = join prev next in
        if ( <= ) ~lhs:post ~rhs:prev then set_visited false post else post


    let pp = Disjuncts.pp
  end

  let exec_instr pre_disjuncts extras node instr =
    List.fold pre_disjuncts ~init:[] ~f:(fun post_disjuncts pre_disjunct ->
        if pre_disjunct.visited then
          (* SUBTLE: ignore pres that we know we have gone through already. This means that the
             invariant map at that program point will be junk since they are going to miss some
             states, but the overall result will still be ok because the loop heads are ok. *)
          post_disjuncts
        else
          let disjuncts' = TransferFunctions.exec_instr pre_disjunct.astate extras node instr in
          Domain.join post_disjuncts (List.map disjuncts' ~f:Disjuncts.mk_disjunct) )


  let pp_session_name node f = TransferFunctions.pp_session_name node f
end
