(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Graph
module L = Logging
module CFG = ProcCfg.Normal

(* Use ocamlgraph's dominators functor to get the dominators *)
module GDoms = Dominator.Make (ProcCfg.MakeOcamlGraph (CFG))

let print_dominators cfg idom =
  L.(debug Analysis Medium) "@\n ----- **** Dominators TABLE **** -------@\n" ;
  Procdesc.get_nodes cfg
  |> List.iter ~f:(fun n ->
         L.(debug Analysis Medium) "@\n   Node: %a   Dominators:\n" Procdesc.Node.pp n ;
         List.iter
           ~f:(L.(debug Analysis Medium) "%a;" Procdesc.Node.pp)
           (GDoms.idom_to_dominators idom n) )


(* Computes the dominator tree, using ocamlgraph's Lengauer-Tarjan algorithm.
      [compute_idom cfg node] returns a function [idom : Procdesc.Node.t -> Procdesc.Node.t] s.t.
      [idom n] returns the immediate dominator of [n]. *)
let get_idoms pdesc =
  let idom = GDoms.compute_idom pdesc (ProcCfg.Normal.start_node pdesc) in
  print_dominators pdesc idom ; idom


(* make each node to be dominated by itself, i.e reflexive, unlike ocamlgraph *)
let dominates idom x y = GDoms.idom_to_dom idom x y || Procdesc.Node.equal x y
