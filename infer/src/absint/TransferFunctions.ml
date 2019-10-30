(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

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

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t list

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module MakeDisjunctive (TransferFunctions : DisjReady) (DConfig : DisjunctiveConfig) = struct
  module CFG = TransferFunctions.CFG

  type extras = TransferFunctions.extras

  module Disjuncts = struct
    (* The disjunctive domain transformer should really be part of the Abstract Interpreter instead
       of the Transfer Functions as the scheduler needs to know which disjuncts have already been
       explored. But for now let's emulate that with a disgusting hack in the transfer functions
       that try to skip disjuncts we have already explored, using the [visited] flag. *)
    type disjunct = {visited: bool; astate: TransferFunctions.Domain.t}

    let pp_disjunct fmt ({visited; astate}[@warning "+9"]) =
      F.fprintf fmt "@[<hv>visited=%b;@;@[%a@]@]" visited TransferFunctions.Domain.pp astate


    type t = disjunct list

    let mk_disjunct astate = {visited= false; astate}

    let singleton astate = [mk_disjunct astate]

    let elements disjuncts = List.map disjuncts ~f:(fun {astate} -> astate)

    let pp f disjuncts =
      let pp_disjuncts f disjuncts =
        List.iteri disjuncts ~f:(fun i disjunct ->
            F.fprintf f "#%d: @[%a@]@;" i pp_disjunct disjunct )
      in
      F.fprintf f "@[<v>%d disjuncts:@;%a@]" (List.length disjuncts) pp_disjuncts disjuncts
  end

  type disjunct_t = Disjuncts.disjunct = {visited: bool; astate: TransferFunctions.Domain.t}

  module Domain = struct
    type t = Disjuncts.t

    let rev_filter_not_over_approximated disjuncts ~not_in =
      List.rev_filter disjuncts ~f:(fun disjunct ->
          List.exists not_in ~f:(fun disj_not_in ->
              TransferFunctions.Domain.leq ~lhs:disjunct.astate ~rhs:disj_not_in.astate )
          |> not )


    (* Ignore states in [lhs] that are over-approximated in [rhs] and vice-versa. Favors keeping
       states in [lhs]. *)
    let join_up_to_imply lhs rhs =
      let rev_rhs_not_in_lhs = rev_filter_not_over_approximated rhs ~not_in:lhs in
      (* cheeky: this is only used in pulse, whose (<=) is actually a symmetric relation so there's
         no need to filter out elements of [lhs] *)
      List.rev_append rev_rhs_not_in_lhs lhs


    let join : t -> t -> t =
     fun lhs rhs ->
      if phys_equal lhs rhs then lhs
      else
        match DConfig.join_policy with
        | `NeverJoin ->
            List.rev_append rhs lhs
        | `UnderApproximateAfter n ->
            let lhs_length = List.length lhs in
            if lhs_length >= n then lhs else List.rev_append rhs lhs


    let set_visited b disjuncts =
      if List.for_all disjuncts ~f:(fun {visited} -> Bool.equal visited b) then disjuncts
      else List.map disjuncts ~f:(fun disjunct -> {disjunct with visited= b})


    (** check if elements of [disj] appear in [of_] in the same order, using pointer equality on
        abstract states to compare elements quickly *)
    let rec is_trivial_subset disj ~of_ =
      match (disj, of_) with
      | [], _ ->
          true
      | x :: disj', y :: of' when phys_equal x.astate y.astate ->
          is_trivial_subset disj' ~of_:of'
      | _, _ :: of' ->
          is_trivial_subset disj ~of_:of'
      | _, [] ->
          false


    let leq ~lhs ~rhs = phys_equal lhs rhs || is_trivial_subset lhs ~of_:rhs

    let widen ~prev ~next ~num_iters =
      let (`UnderApproximateAfterNumIterations max_iter) = DConfig.widen_policy in
      if phys_equal prev next || num_iters > max_iter then set_visited false prev
      else
        let visited_prev = set_visited true prev in
        let post = join_up_to_imply visited_prev next in
        if leq ~lhs:post ~rhs:prev then set_visited false prev else post


    let pp = Disjuncts.pp
  end

  let exec_instr pre_disjuncts extras node instr =
    List.foldi pre_disjuncts ~init:[] ~f:(fun i post_disjuncts pre_disjunct ->
        if pre_disjunct.visited then
          (* SUBTLE/WORST HACK EVER: ignore pres that we know we have gone through already. This
             means that the invariant map at that program point will be junk since they are going to
             miss some states, but the overall result will still be ok because the loop heads are
             ok.

             This should really be implemented in {!AbstractInterpreter}. *)
          post_disjuncts
        else (
          L.d_printfln "@[<v2>Executing from disjunct #%d@;" i ;
          let disjuncts' =
            TransferFunctions.exec_instr pre_disjunct.astate extras node instr
            |> List.map ~f:Disjuncts.mk_disjunct
          in
          ( if Config.write_html then
            let n = List.length disjuncts' in
            L.d_printfln "@]@\n@[Got %d disjunct%s back@]" n (if Int.equal n 1 then "" else "s") ) ;
          Domain.join post_disjuncts disjuncts' ) )


  let pp_session_name node f = TransferFunctions.pp_session_name node f
end
