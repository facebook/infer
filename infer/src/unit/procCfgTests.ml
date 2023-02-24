(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BackwardCfg = ProcCfg.Backward (ProcCfg.Normal)
module InstrCfg = ProcCfg.OneInstrPerNode (ProcCfg.Normal)
module BackwardInstrCfg = ProcCfg.Backward (InstrCfg)

let tests =
  let cfg = Cfg.create () in
  let test_pdesc =
    Cfg.create_proc_desc cfg
      (ProcAttributes.default (SourceFile.invalid __FILE__) Procname.empty_block)
  in
  let dummy_instr1 = Sil.skip_instr in
  let dummy_instr2 = Sil.Metadata (Abstract Location.dummy) in
  let dummy_instr3 =
    Sil.Metadata (ExitScope ([Var.of_id (Ident.create_fresh Ident.knormal)], Location.dummy))
  in
  let dummy_instr4 = Sil.skip_instr in
  let instrs1 = [dummy_instr1; dummy_instr2] in
  let instrs2 = [dummy_instr3] in
  let instrs3 = [dummy_instr4] in
  let instrs4 = [] in
  let create_node instrs = Procdesc.create_node test_pdesc Location.dummy (Stmt_node Skip) instrs in
  let n1 = create_node instrs1 in
  let n2 = create_node instrs2 in
  let n3 = create_node instrs3 in
  let n4 = create_node instrs4 in
  Procdesc.set_start_node test_pdesc n1 ;
  (* let -> represent normal transitions and -*-> represent exceptional transitions *)
  (* creating graph n1 -> n2, n1 -*-> n3, n2 -> n4, n2 -*-> n3, n3 -> n4 , n3 -*> n4 *)
  Procdesc.node_set_succs test_pdesc n1 ~normal:[n2] ~exn:[n3] ;
  Procdesc.node_set_succs test_pdesc n2 ~normal:[n4] ~exn:[n3] ;
  Procdesc.node_set_succs test_pdesc n3 ~normal:[n4] ~exn:[n4] ;
  let normal_proc_cfg = ProcCfg.Normal.from_pdesc test_pdesc in
  let exceptional_proc_cfg = ProcCfg.Exceptional.from_pdesc test_pdesc in
  let backward_proc_cfg = BackwardCfg.from_pdesc test_pdesc in
  let backward_instr_proc_cfg = BackwardInstrCfg.from_pdesc test_pdesc in
  let open OUnit2 in
  let cmp l1 l2 =
    let sort = List.sort ~compare:Procdesc.Node.compare in
    List.equal Procdesc.Node.equal (sort l1) (sort l2)
  in
  let pp_diff fmt (actual, expected) =
    let pp_sep fmt _ = F.pp_print_char fmt ',' in
    let pp_node_list fmt l = F.pp_print_list ~pp_sep Procdesc.Node.pp fmt l in
    F.fprintf fmt "Expected output %a but got %a" pp_node_list expected pp_node_list actual
  in
  let create_test ~fold input expected _ =
    let input = Container.to_list input ~fold in
    assert_equal ~cmp ~pp_diff input expected
  in
  let instr_test =
    let instr_test_ _ =
      let list_of_instrs instrs = Instrs.fold instrs ~init:[] ~f:(fun l i -> i :: l) |> List.rev in
      ( match ProcCfg.Normal.instrs n1 |> list_of_instrs with
      | [instr1; instr2] ->
          assert_bool "First instr should be dummy_instr1" (phys_equal instr1 dummy_instr1) ;
          assert_bool "Second instr should be dummy_instr2" (phys_equal instr2 dummy_instr2)
      | _ ->
          assert_failure "Expected exactly two instructions" ) ;
      ( match BackwardCfg.instrs n1 |> list_of_instrs with
      | [instr1; instr2] ->
          assert_bool "First instr should be dummy_instr2" (phys_equal instr1 dummy_instr2) ;
          assert_bool "Second instr should be dummy_instr1" (phys_equal instr2 dummy_instr1)
      | _ ->
          assert_failure "Expected exactly two instructions" ) ;
      let instr_n1 = InstrCfg.Node.of_underlying_node n1 in
      ( match InstrCfg.instrs instr_n1 |> list_of_instrs with
      | [instr] ->
          assert_bool "Only instr should be dummy_instr1" (phys_equal instr dummy_instr1)
      | _ ->
          assert_failure "Expected exactly one instruction" ) ;
      let n1' = InstrCfg.Node.underlying_node instr_n1 in
      assert_bool "underlying_node should return node of underlying CFG type" (phys_equal n1 n1') ;
      let backward_instr_n1 = BackwardInstrCfg.Node.of_underlying_node n1 in
      ( match BackwardInstrCfg.instrs backward_instr_n1 |> list_of_instrs with
      | [instr] ->
          assert_bool "Only instr should be dummy_instr1" (phys_equal instr dummy_instr1)
      | _ ->
          assert_failure "Expected exactly one instruction" ) ;
      let n1'' = BackwardInstrCfg.Node.underlying_node backward_instr_n1 in
      assert_bool "underlying_node should return node of underlying CFG type" (phys_equal n1 n1'') ;
      (* test the preds/succs using backward + instr cfg *)
      let check_backward_instr_ fold backward_instr_node expected_instrs =
        match Container.to_list ~fold:(fold backward_instr_proc_cfg) backward_instr_node with
        | [n] ->
            assert_equal (BackwardInstrCfg.instrs n) (expected_instrs |> Instrs.reverse_order)
        | _ ->
            assert_failure "Expected exactly one node"
      in
      check_backward_instr_ BackwardInstrCfg.fold_preds backward_instr_n1
        (Instrs.singleton dummy_instr2) ;
      let backward_instr_n2 = BackwardInstrCfg.Node.of_underlying_node n2 in
      check_backward_instr_ BackwardInstrCfg.fold_preds backward_instr_n2 Instrs.empty ;
      let backward_instr_n3 = BackwardInstrCfg.Node.of_underlying_node n3 in
      check_backward_instr_ BackwardInstrCfg.fold_preds backward_instr_n3 Instrs.empty ;
      check_backward_instr_ BackwardInstrCfg.fold_normal_succs backward_instr_n2
        (Instrs.singleton dummy_instr2)
    in
    "instr_test" >:: instr_test_
  in
  let graph_tests =
    [ (* test the succs of the normal cfg. forward... *)
      ("succs_n1", ProcCfg.Normal.fold_succs normal_proc_cfg, n1, [n2])
    ; ("normal_succs_n1", ProcCfg.Normal.fold_normal_succs normal_proc_cfg, n1, [n2])
    ; ("succs_n2", ProcCfg.Normal.fold_succs normal_proc_cfg, n2, [n4])
    ; ("normal_succs_n2", ProcCfg.Normal.fold_normal_succs normal_proc_cfg, n2, [n4])
    ; ("succs_n3", ProcCfg.Normal.fold_succs normal_proc_cfg, n3, [n4])
    ; ("normal_succs_n3", ProcCfg.Normal.fold_normal_succs normal_proc_cfg, n3, [n4])
    ; (* ... and backward... *)
      ("succs_n1_bw", BackwardCfg.fold_preds backward_proc_cfg, n1, [n2])
    ; ("normal_succs_n1_bw", BackwardCfg.fold_normal_preds backward_proc_cfg, n1, [n2])
    ; ("succs_n2_bw", BackwardCfg.fold_preds backward_proc_cfg, n2, [n4])
    ; ("normal_succs_n2_bw", BackwardCfg.fold_normal_preds backward_proc_cfg, n2, [n4])
    ; ("succs_n3_bw", BackwardCfg.fold_preds backward_proc_cfg, n3, [n4])
    ; ("normal_succs_n3_bw", BackwardCfg.fold_normal_preds backward_proc_cfg, n3, [n4])
    ; (* test the preds of the normal cfg... *)
      ("preds_n2", ProcCfg.Normal.fold_normal_preds normal_proc_cfg, n2, [n1])
    ; ("normal_preds_n2", ProcCfg.Normal.fold_normal_preds normal_proc_cfg, n2, [n1])
    ; (* ...and the backward cfg... *)
      ("preds_n2_bw", BackwardCfg.fold_normal_succs backward_proc_cfg, n2, [n1])
    ; ("normal_preds_n2_bw", BackwardCfg.fold_normal_succs backward_proc_cfg, n2, [n1])
    ; (* we shouldn't see any exn succs or preds even though we added them *)
      ("no_exn_succs_n1", ProcCfg.Normal.fold_exceptional_succs normal_proc_cfg, n1, [])
    ; ("no_exn_preds_n3", ProcCfg.Normal.fold_exceptional_preds normal_proc_cfg, n3, [])
    ; (* same in the backward cfg *)
      ("no_exn_succs_n1_bw", BackwardCfg.fold_exceptional_preds backward_proc_cfg, n1, [])
    ; ("no_exn_preds_n3_bw", BackwardCfg.fold_exceptional_succs backward_proc_cfg, n3, [])
    ; (* now, test the exceptional succs in the exceptional cfg. *)
      ("exn_succs_n1", ProcCfg.Exceptional.fold_exceptional_succs exceptional_proc_cfg, n1, [n3])
    ; ("exn_succs_n2", ProcCfg.Exceptional.fold_exceptional_succs exceptional_proc_cfg, n2, [n3])
    ; ("exn_succs_n3", ProcCfg.Exceptional.fold_exceptional_succs exceptional_proc_cfg, n3, [n4])
    ; (* test exceptional pred links *)
      ("exn_preds_n3", ProcCfg.Exceptional.fold_exceptional_preds exceptional_proc_cfg, n3, [n2; n1])
    ; (* succs should return both normal and exceptional successors *)
      ("exn_all_succs_n1", ProcCfg.Exceptional.fold_succs exceptional_proc_cfg, n1, [n3; n2])
    ; (* but, should not return duplicates *)
      ("exn_all_succs_n3", ProcCfg.Exceptional.fold_succs exceptional_proc_cfg, n3, [n4])
    ; (* similarly, preds should return both normal and exceptional predecessors *)
      ("exn_all_preds_n3", ProcCfg.Exceptional.fold_preds exceptional_proc_cfg, n3, [n2; n1])
    ; ("exn_all_preds_n4", ProcCfg.Exceptional.fold_preds exceptional_proc_cfg, n4, [n3; n2])
    ; (* finally, normal_succs/normal_preds shouldn't return exceptional edges *)
      ("exn_normal_succs_n1", ProcCfg.Exceptional.fold_normal_succs exceptional_proc_cfg, n1, [n2])
    ; ("exn_normal_preds_n2", ProcCfg.Exceptional.fold_normal_preds exceptional_proc_cfg, n2, [n1])
    ]
    |> List.map ~f:(fun (name, fold, input, expected) -> name >:: create_test ~fold input expected)
  in
  let tests = instr_test :: graph_tests in
  "procCfgSuite" >::: tests
