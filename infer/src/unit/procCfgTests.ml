(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format

let tests =
  let cfg = Cfg.Node.create_cfg () in
  let test_pdesc =
    Cfg.Procdesc.create cfg (ProcAttributes.default Procname.empty_block !Config.curr_language) in
  let create_node cfg =
    Cfg.Node.create cfg Location.dummy (Cfg.Node.Stmt_node "") [] test_pdesc [] in

  let n1, n2, n3, n4 = create_node cfg, create_node cfg, create_node cfg, create_node cfg in
  (* let -> represent normal transitions and -*-> represent exceptional transitions *)
  (* creating graph n1 -> n2, n1 -*-> n3, n2 -> n4, n2 -*-> n3, n3 -> n4 , n3 -*> n4 *)
  Cfg.Node.set_succs_exn cfg n1 [n2] [n3];
  Cfg.Node.set_succs_exn cfg n2 [n4] [n3];
  Cfg.Node.set_succs_exn cfg n3 [n4] [n4];

  let normal_proc_cfg = ProcCfg.Normal.from_pdesc test_pdesc in
  let exceptional_proc_cfg = ProcCfg.Exceptional.from_pdesc test_pdesc in

  let open OUnit2 in
  let cmp = IList.equal Cfg.Node.compare in
  (* TODO: cleanup *)
  let pp_diff fmt (actual, expected) =
    let pp_node_list fmt l = F.pp_print_list Cfg.Node.pp fmt l in
    F.fprintf fmt "Expected output %a but got %a" pp_node_list expected pp_node_list actual in
  let create_test input expected _ =
    assert_equal ~cmp ~pp_diff input expected in
  let test_list = [
    (* test the succs of the normal cfg *)
    ("succs_n1", ProcCfg.Normal.succs normal_proc_cfg n1, [n2]);
    ("normal_succs_n1", ProcCfg.Normal.normal_succs normal_proc_cfg n1, [n2]);
    ("succs_n2", ProcCfg.Normal.succs normal_proc_cfg n2, [n4]);
    ("normal_succs_n2", ProcCfg.Normal.normal_succs normal_proc_cfg n2, [n4]);
    ("succs_n3", ProcCfg.Normal.succs normal_proc_cfg n3, [n4]);
    ("normal_succs_n3", ProcCfg.Normal.normal_succs normal_proc_cfg n3, [n4]);
    (* test the preds of the normal cfg *)
    ("preds_n2", ProcCfg.Normal.normal_preds normal_proc_cfg n2, [n1]);
    ("normal_preds_n2", ProcCfg.Normal.normal_preds normal_proc_cfg n2, [n1]);
    (* we shouldn't see any exn succs or preds even though we added them *)
    ("no_exn_succs_n1", ProcCfg.Normal.exceptional_succs normal_proc_cfg n1, []);
    ("no_exn_preds_n3", ProcCfg.Normal.exceptional_preds normal_proc_cfg n3, []);

    (* now, test the exceptional succs in the exceptional cfg. *)
    ("exn_succs_n1", ProcCfg.Exceptional.exceptional_succs exceptional_proc_cfg n1, [n3]);
    ("exn_succs_n2", ProcCfg.Exceptional.exceptional_succs exceptional_proc_cfg n2, [n3]);
    ("exn_succs_n3", ProcCfg.Exceptional.exceptional_succs exceptional_proc_cfg n3, [n4]);
    (* test exceptional pred links *)
    ("exn_preds_n3", ProcCfg.Exceptional.exceptional_preds exceptional_proc_cfg n3, [n2; n1]);
    (* succs should return both normal and exceptional successors *)
    ("exn_all_succs_n1", ProcCfg.Exceptional.succs exceptional_proc_cfg n1, [n3; n2]);
    (* but, should not return duplicates *)
    ("exn_all_succs_n3", ProcCfg.Exceptional.succs exceptional_proc_cfg n3, [n4]);
    (* similarly, preds should return both normal and exceptional predecessors *)
    ("exn_all_preds_n3", ProcCfg.Exceptional.preds exceptional_proc_cfg n3, [n2; n1]);
    ("exn_all_preds_n4", ProcCfg.Exceptional.preds exceptional_proc_cfg n4, [n3; n2]);
    (* finally, normal_succs/normal_preds shouldn't return exceptional edges *)
    ("exn_normal_succs_n1", ProcCfg.Exceptional.normal_succs exceptional_proc_cfg n1, [n2]);
    ("exn_normal_preds_n2", ProcCfg.Exceptional.normal_preds exceptional_proc_cfg n2, [n1]);
  ]
    |> IList.map (fun (name, test, expected) -> name>::create_test test expected) in
  "procCfgSuite">:::test_list
