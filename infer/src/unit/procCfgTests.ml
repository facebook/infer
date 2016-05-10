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

module InstrCfg = ProcCfg.OneInstrPerNode (ProcCfg.Normal)

let tests =
  let cfg = Cfg.Node.create_cfg () in
  let test_pdesc =
    Cfg.Procdesc.create cfg (ProcAttributes.default Procname.empty_block !Config.curr_language) in
  let dummy_instr1 = Sil.Remove_temps ([], Location.dummy) in
  let dummy_instr2 = Sil.Abstract Location.dummy in
  let dummy_instr3 = Sil.Stackop (Pop, Location.dummy) in
  let instrs1 = [dummy_instr1; dummy_instr2;] in
  let instrs2 = [dummy_instr3] in
  let instrs3 = [] in
  let instrs4 = [] in
  let create_node cfg instrs =
    Cfg.Node.create cfg Location.dummy (Cfg.Node.Stmt_node "") instrs test_pdesc [] in
  let n1 = create_node cfg instrs1 in
  let n2 = create_node cfg instrs2 in
  let n3 = create_node cfg instrs3 in
  let n4 = create_node cfg instrs4 in

  Cfg.Procdesc.set_start_node test_pdesc n1;
  (* let -> represent normal transitions and -*-> represent exceptional transitions *)
  (* creating graph n1 -> n2, n1 -*-> n3, n2 -> n4, n2 -*-> n3, n3 -> n4 , n3 -*> n4 *)
  Cfg.Node.set_succs_exn cfg n1 [n2] [n3];
  Cfg.Node.set_succs_exn cfg n2 [n4] [n3];
  Cfg.Node.set_succs_exn cfg n3 [n4] [n4];

  let normal_proc_cfg = ProcCfg.Normal.from_pdesc test_pdesc in
  let exceptional_proc_cfg = ProcCfg.Exceptional.from_pdesc test_pdesc in
  let instr_cfg = InstrCfg.from_pdesc test_pdesc in

  let open OUnit2 in
  let instr_cfg_test =
    let instr_cfg_test_ _ =
      (* CFG should look like: dummy_instr1 -> dummy_instr2 -> dummy_instr3 *)
      let assert_one_instr node =
        let instrs = InstrCfg.instrs node in
        assert_bool "Nodes should contain one instruction" ((IList.length instrs) = 1);
        IList.hd instrs in
      let assert_one_succ node =
        let succs = InstrCfg.succs instr_cfg node in
        assert_bool "Should only have one succ" ((IList.length succs) = 1);
        IList.hd succs in
      (* walk forward through the CFG and make sure everything looks ok *)
      let start_node = InstrCfg.start_node instr_cfg in
      let instr1 = assert_one_instr start_node in
      assert_bool "instr should be dummy_instr1" (instr1 = dummy_instr1);
      let succ_node1 = assert_one_succ start_node in
      let instr2 = assert_one_instr succ_node1 in
      assert_bool "instr should be dummy_instr2" (instr2 = dummy_instr2);
      let succ_node2 = assert_one_succ succ_node1 in
      let instr3 = assert_one_instr succ_node2 in
      assert_bool "instr should be dummy_instr3" (instr3 = dummy_instr3);
      (* now, do the same thing going backward *)
      let assert_one_pred node =
        let preds = InstrCfg.preds instr_cfg node in
        assert_bool "Should only have one pred" ((IList.length preds) = 1);
        IList.hd preds in
      let pred_node1 = assert_one_pred succ_node2 in
      let instr2 = assert_one_instr pred_node1 in
      assert_bool "instr should be dummy_instr2" (instr2 = dummy_instr2);
      let start_node = assert_one_pred pred_node1 in
      let instr1 = assert_one_instr start_node in
      assert_bool "instr should be dummy_instr1" (instr1 = dummy_instr1) in
    "instr_cfg_test">::instr_cfg_test_ in

  let cmp l1 l2 =
    let sort = IList.sort Cfg.Node.compare in
    IList.equal Cfg.Node.compare (sort l1) (sort l2) in
  let pp_diff fmt (actual, expected) =
    let pp_sep fmt _ = F.pp_print_char fmt ',' in
    let pp_node_list fmt l = F.pp_print_list ~pp_sep Cfg.Node.pp fmt l in
    F.fprintf fmt "Expected output %a but got %a" pp_node_list expected pp_node_list actual in
  let create_test input expected _ =
    assert_equal ~cmp ~pp_diff input expected in
  let normal_exceptional_tests = [
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
  let tests = instr_cfg_test :: normal_exceptional_tests in
  "procCfgSuite">:::tests
