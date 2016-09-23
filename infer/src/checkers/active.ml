(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** I want to use various powersets instead of just variables like Var.Set
    For example to track the analogues of attributes
    I will do this running forwards later, but backwards for now.
*)

open! Utils

module F = Format
module L = Logging

module PPstring = PrettyPrintable.MakePPSet(struct
    type nonrec t = string
    let compare = string_compare
    let pp_element fmt s = F.fprintf fmt "%s" s
  end)

module PPrawpath = PrettyPrintable.MakePPSet(struct
    type nonrec t = AccessPath.raw
    let compare = AccessPath.raw_compare
    let pp_element = AccessPath.pp_raw
  end)

module LocksDomain =  AbstractDomain.FiniteSet(PPstring)

module PathDomain =  AbstractDomain.FiniteSet(PPrawpath)

module ReadWriteDomain = AbstractDomain.Pair (PathDomain) (PathDomain)

module CombinedDomain = AbstractDomain.Pair (LocksDomain) (ReadWriteDomain)
(* a typical element is (){locked}, {vars and fields})  *)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = CombinedDomain
  type extras = ProcData.no_extras

  let is_lock_procedure pn = Procname.equal pn ModelBuiltins.__set_locked_attribute

  let is_unlock_procedure pn = Procname.equal pn ModelBuiltins.__delete_locked_attribute

  let add_path_to_state exp typ pathdomainstate =
    match AccessPath.of_exp exp typ ~f_resolve_id:(fun _ -> None) with
    | Some rawpath -> PathDomain.add rawpath pathdomainstate
    | None -> pathdomainstate


  let exec_instr ((lockstate,(readstate,writestate)) as astate) _ _ = function
    | Sil.Call (_, Exp.Const (Const.Cfun pn), _, _, _) ->
        if is_lock_procedure pn
        then
          ((LocksDomain.add "locked" lockstate), (readstate,writestate))
        else if is_unlock_procedure pn
        then
          ((LocksDomain.remove "locked" lockstate) , (readstate,writestate))
        else
          astate

    | Sil.Store ((Exp.Lfield ( _, _, typ) as lhsfield) , _, _, _) ->
        if LocksDomain.is_empty lockstate then (* abstracts no lock being held*)
          (lockstate, (readstate, add_path_to_state lhsfield typ writestate))
        else astate

    (* Note: it appears that the third arg of Store is never an Lfield in the targets of,
       our translations, which is why we have not covered that case. *)
    | Sil.Store (_, _, Exp.Lfield _, _) ->
        failwith "Unexpected store instruction with rhs field"

    | Sil.Load (_, (Exp.Lfield ( _, _, typ) as rhsfield) , _, _) ->
        if LocksDomain.is_empty lockstate then (* abstracts no lock being held*)
          (lockstate, (add_path_to_state rhsfield typ readstate, writestate))
        else astate

    |  _  ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

let checker { Callbacks.proc_desc; tenv; } =
  match Analyzer.compute_post (ProcData.make_default proc_desc tenv) with
  | Some post ->  (* I am printing to commandline and out to cater to javac and buck*)
      ((L.stdout  "\n Procedure: %s@ "
          (Procname.to_string (Cfg.Procdesc.get_proc_name proc_desc) )
       );
       L.stdout "\n POST: %a\n" CombinedDomain.pp post;
       (L.out  "\n Procedure: %s@ "
          (Procname.to_string (Cfg.Procdesc.get_proc_name proc_desc) )
       );
       L.out "\n POST: %a\n" CombinedDomain.pp post
      )
  | None -> ()
