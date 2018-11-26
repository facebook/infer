(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** functor that makes it easy to write a basic abstract interpretation checker by lifting
    a type, comparison function, reporting function, and exec function into an analyzer *)

module type Spec = sig
  (** what state do you want to propagate? *)
  type t

  val initial : t
  (** implement the state the analysis should start from here *)

  val exec_instr : t -> Sil.instr -> Procdesc.Node.nodekind -> Typ.Procname.t -> Tenv.t -> t
  (** implement how an instruction changes your state here.
      input is the previous state, current instruction, current node kind, current procedure and
      type environment.
  *)

  val report : t -> Location.t -> Typ.Procname.t -> unit
  (** log errors here.
      input is a state, location where the state occurs in the source, and the current procedure.
  *)

  val compare : t -> t -> int
end

module type S = sig
  val checker : Callbacks.proc_callback_t
  (** add YourChecker.checker to registerCallbacks.ml to run your checker *)
end

module Make (Spec : Spec) : S = struct
  (* powerset domain over Spec.t *)
  module Domain = struct
    include AbstractDomain.FiniteSet (struct
      type t = Spec.t

      let compare = Spec.compare

      let pp _ _ = ()
    end)

    let widen ~prev ~next ~num_iters =
      let iters_befor_timeout = 1000 in
      (* failsafe for accidental non-finite height domains *)
      if num_iters >= iters_befor_timeout then
        L.(die InternalError)
          "Stopping analysis after 1000 iterations without convergence. Make sure your domain is \
           finite height."
      else widen ~prev ~next ~num_iters
  end

  module TransferFunctions (CFG : ProcCfg.S) = struct
    module CFG = CFG
    module Domain = Domain

    type extras = ProcData.no_extras

    let exec_instr astate_set proc_data node instr =
      let node_kind = CFG.Node.kind node in
      let pname = Procdesc.get_proc_name proc_data.ProcData.pdesc in
      Domain.fold
        (fun astate acc ->
          Domain.add (Spec.exec_instr astate instr node_kind pname proc_data.ProcData.tenv) acc )
        astate_set Domain.empty


    let pp_session_name _node fmt = F.pp_print_string fmt "simple checker"
  end

  module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Exceptional))

  let checker {Callbacks.proc_desc; tenv; summary} : Summary.t =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let nodes = Procdesc.get_nodes proc_desc in
    let do_reporting node_id state =
      let astate_set = state.AbstractInterpreter.State.post in
      if not (Domain.is_empty astate_set) then
        (* should never fail since keys in the invariant map should always be real node id's *)
        let node =
          List.find_exn
            ~f:(fun node -> Procdesc.Node.equal_id node_id (Procdesc.Node.get_id node))
            nodes
        in
        Domain.iter
          (fun astate -> Spec.report astate (ProcCfg.Exceptional.Node.loc node) proc_name)
          astate_set
    in
    let inv_map =
      Analyzer.exec_pdesc (ProcData.make_default proc_desc tenv) ~initial:Domain.empty
    in
    Analyzer.InvariantMap.iter do_reporting inv_map ;
    summary
end
