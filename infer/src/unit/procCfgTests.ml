(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module BackwardCfg = ProcCfg.Backward (ProcCfg.Normal)
module InstrCfg = ProcCfg.OneInstrPerNode (ProcCfg.Normal)
module BackwardInstrCfg = ProcCfg.Backward (InstrCfg)

let tests =
  let cfg = Cfg.create_cfg () in
  let test_pdesc =
    Cfg.create_proc_desc cfg
      (ProcAttributes.default Typ.Procname.empty_block !Config.curr_language)
  in
  let dummy_instr1 = Sil.Remove_temps ([], Location.dummy) in
  let dummy_instr2 = Sil.Abstract Location.dummy in
  let dummy_instr3 = Sil.Remove_temps ([Ident.create_fresh Ident.knormal], Location.dummy) in
  let dummy_instr4 = Sil.Remove_temps ([], Location.dummy) in
  let instrs1 = [dummy_instr1; dummy_instr2] in
  let instrs2 = [dummy_instr3] in
  let instrs3 = [dummy_instr4] in
  let instrs4 = [] in
  let create_node instrs =
    Procdesc.create_node test_pdesc Location.dummy (Procdesc.Node.Stmt_node "") instrs
  in
  let n1 = create_node instrs1 in
  let n2 = create_node instrs2 in
  let n3 = create_node instrs3 in
  let n4 = create_node instrs4 in
  Procdesc.set_start_node test_pdesc n1 ;
  (* let -> represent normal transitions and -*-> represent exceptional transitions *)
  (* creating graph n1 -> n2, n1 -*-> n3, n2 -> n4, n2 -*-> n3, n3 -> n4 , n3 -*> n4 *)
  Procdesc.node_set_succs_exn test_pdesc n1 [n2] [n3] ;
  Procdesc.node_set_succs_exn test_pdesc n2 [n4] [n3] ;
  Procdesc.node_set_succs_exn test_pdesc n3 [n4] [n4] ;
  let normal_proc_cfg = ProcCfg.Normal.from_pdesc test_pdesc in
  let exceptional_proc_cfg = ProcCfg.Exceptional.from_pdesc test_pdesc in
  let backward_proc_cfg = BackwardCfg.from_pdesc test_pdesc in
  let backward_instr_proc_cfg = BackwardInstrCfg.from_pdesc test_pdesc in
  let open OUnit2 in
  let cmp l1 l2 =
    let sort = List.sort ~cmp:Procdesc.Node.compare in
    List.equal ~equal:Procdesc.Node.equal (sort l1) (sort l2)
  in
  let pp_diff fmt (actual, expected) =
    let pp_sep fmt _ = F.pp_print_char fmt ',' in
    let pp_node_list fmt l = F.pp_print_list ~pp_sep Procdesc.Node.pp fmt l in
    F.fprintf fmt "Expected output %a but got %a" pp_node_list expected pp_node_list actual
  in
  let create_test input expected _ = assert_equal ~cmp ~pp_diff input expected in
  let instr_test =
    let instr_test_ _ =
      ( match ProcCfg.Normal.instrs n1 with
      | [instr1; instr2]
       -> assert_bool "First instr should be dummy_instr1" (phys_equal instr1 dummy_instr1) ;
          assert_bool "Second instr should be dummy_instr2" (phys_equal instr2 dummy_instr2)
      | _
       -> assert_failure "Expected exactly two instructions" ) ;
      ( match BackwardCfg.instrs n1 with
      | [instr1; instr2]
       -> assert_bool "First instr should be dummy_instr2" (phys_equal instr1 dummy_instr2) ;
          assert_bool "Second instr should be dummy_instr1" (phys_equal instr2 dummy_instr1)
      | _
       -> assert_failure "Expected exactly two instructions" ) ;
      (let node_id, _ = InstrCfg.id n1 in
       match InstrCfg.instr_ids n1 with
       | [(instr1, Some (id1, ProcCfg.Instr_index 0)); (instr2, Some (id2, ProcCfg.Instr_index 1))]
        -> assert_bool "First instr should be dummy_instr1" (phys_equal instr1 dummy_instr1) ;
           assert_bool "Second instr should be dummy_instr2" (phys_equal instr2 dummy_instr2) ;
           assert_bool "id1 should be id of underlying node" (phys_equal id1 node_id) ;
           assert_bool "id2 should be id of underlying node" (phys_equal id2 node_id)
       | _
        -> assert_failure "Expected exactly two instructions with correct indices") ;
      let backward_node_id, _ = BackwardInstrCfg.id n1 in
      ( match BackwardInstrCfg.instr_ids n1 with
      | [(instr1, Some (id1, ProcCfg.Instr_index 1)); (instr2, Some (id2, ProcCfg.Instr_index 0))]
       -> assert_bool "First instr should be dummy_instr2" (phys_equal instr1 dummy_instr2) ;
          assert_bool "Second instr should be dummy_instr1" (phys_equal instr2 dummy_instr1) ;
          assert_bool "id1 should be id of underlying node" (phys_equal id1 backward_node_id) ;
          assert_bool "id2 should be id of underlying node" (phys_equal id2 backward_node_id)
      | _
       -> assert_failure "Expected exactly two instructions with correct indices" ) ;
      assert_bool "underlying_node should return node of underlying CFG type"
        (Procdesc.Node.equal_id
           (Procdesc.Node.get_id (BackwardInstrCfg.underlying_node n1))
           (BackwardCfg.id n1))
    in
    "instr_test" >:: instr_test_
  in
  let graph_tests =
    [ (* test the succs of the normal cfg. forward... *)
    ("succs_n1", ProcCfg.Normal.succs normal_proc_cfg n1, [n2])
    ; ("normal_succs_n1", ProcCfg.Normal.normal_succs normal_proc_cfg n1, [n2])
    ; ("succs_n2", ProcCfg.Normal.succs normal_proc_cfg n2, [n4])
    ; ("normal_succs_n2", ProcCfg.Normal.normal_succs normal_proc_cfg n2, [n4])
    ; ("succs_n3", ProcCfg.Normal.succs normal_proc_cfg n3, [n4])
    ; ("normal_succs_n3", ProcCfg.Normal.normal_succs normal_proc_cfg n3, [n4])
    ; (* ... and backward... *)
    ("succs_n1_bw", BackwardCfg.preds backward_proc_cfg n1, [n2])
    ; ("normal_succs_n1_bw", BackwardCfg.normal_preds backward_proc_cfg n1, [n2])
    ; ("succs_n2_bw", BackwardCfg.preds backward_proc_cfg n2, [n4])
    ; ("normal_succs_n2_bw", BackwardCfg.normal_preds backward_proc_cfg n2, [n4])
    ; ("succs_n3_bw", BackwardCfg.preds backward_proc_cfg n3, [n4])
    ; ("normal_succs_n3_bw", BackwardCfg.normal_preds backward_proc_cfg n3, [n4])
    ; (* ...and make sure it all works when using backward + instr cfg *)
    ("succs_n1_bw_instrcfg", BackwardInstrCfg.preds backward_instr_proc_cfg n1, [n2])
    ; ( "normal_succs_n1_bw_instrcfg"
      , BackwardInstrCfg.normal_preds backward_instr_proc_cfg n1
      , [n2] )
    ; ("succs_n2_bw_instrcfg", BackwardInstrCfg.preds backward_instr_proc_cfg n2, [n4])
    ; ( "normal_succs_n2_bw_instrcfg"
      , BackwardInstrCfg.normal_preds backward_instr_proc_cfg n2
      , [n4] )
    ; ("succs_n3_bw_instrcfg", BackwardInstrCfg.preds backward_instr_proc_cfg n3, [n4])
    ; ( "normal_succs_n3_bw_instrcfg"
      , BackwardInstrCfg.normal_preds backward_instr_proc_cfg n3
      , [n4] )
    ; (* test the preds of the normal cfg... *)
    ("preds_n2", ProcCfg.Normal.normal_preds normal_proc_cfg n2, [n1])
    ; ("normal_preds_n2", ProcCfg.Normal.normal_preds normal_proc_cfg n2, [n1])
    ; (* ...and the backward cfg... *)
    ("preds_n2_bw", BackwardCfg.normal_succs backward_proc_cfg n2, [n1])
    ; ("normal_preds_n2_bw", BackwardCfg.normal_succs backward_proc_cfg n2, [n1])
    ; (* ...and again make sure it works with backward + instr cfg *)
    ("preds_n2_bw_instr", BackwardInstrCfg.normal_succs backward_instr_proc_cfg n2, [n1])
    ; ("normal_preds_n2_bw_instr", BackwardInstrCfg.normal_succs backward_instr_proc_cfg n2, [n1])
    ; (* we shouldn't see any exn succs or preds even though we added them *)
    ("no_exn_succs_n1", ProcCfg.Normal.exceptional_succs normal_proc_cfg n1, [])
    ; ("no_exn_preds_n3", ProcCfg.Normal.exceptional_preds normal_proc_cfg n3, [])
    ; (* same in the backward cfg *)
    ("no_exn_succs_n1_bw", BackwardCfg.exceptional_preds backward_proc_cfg n1, [])
    ; ("no_exn_preds_n3_bw", BackwardCfg.exceptional_succs backward_proc_cfg n3, [])
    ; (* same in backward + instr cfg *)
    ("no_exn_succs_n1_bw_instr", BackwardInstrCfg.exceptional_preds backward_instr_proc_cfg n1, [])
    ; ( "no_exn_preds_n3_bw_instr"
      , BackwardInstrCfg.exceptional_succs backward_instr_proc_cfg n3
      , [] )
    ; (* now, test the exceptional succs in the exceptional cfg. *)
    ("exn_succs_n1", ProcCfg.Exceptional.exceptional_succs exceptional_proc_cfg n1, [n3])
    ; ("exn_succs_n2", ProcCfg.Exceptional.exceptional_succs exceptional_proc_cfg n2, [n3])
    ; ("exn_succs_n3", ProcCfg.Exceptional.exceptional_succs exceptional_proc_cfg n3, [n4])
    ; (* test exceptional pred links *)
    ("exn_preds_n3", ProcCfg.Exceptional.exceptional_preds exceptional_proc_cfg n3, [n2; n1])
    ; (* succs should return both normal and exceptional successors *)
    ("exn_all_succs_n1", ProcCfg.Exceptional.succs exceptional_proc_cfg n1, [n3; n2])
    ; (* but, should not return duplicates *)
    ("exn_all_succs_n3", ProcCfg.Exceptional.succs exceptional_proc_cfg n3, [n4])
    ; (* similarly, preds should return both normal and exceptional predecessors *)
    ("exn_all_preds_n3", ProcCfg.Exceptional.preds exceptional_proc_cfg n3, [n2; n1])
    ; ("exn_all_preds_n4", ProcCfg.Exceptional.preds exceptional_proc_cfg n4, [n3; n2])
    ; (* finally, normal_succs/normal_preds shouldn't return exceptional edges *)
    ("exn_normal_succs_n1", ProcCfg.Exceptional.normal_succs exceptional_proc_cfg n1, [n2])
    ; ("exn_normal_preds_n2", ProcCfg.Exceptional.normal_preds exceptional_proc_cfg n2, [n1]) ]
    |> List.map ~f:(fun (name, test, expected) -> name >:: create_test test expected)
  in
  let tests = instr_test :: graph_tests in
  "procCfgSuite" >::: tests
