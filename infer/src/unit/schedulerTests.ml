(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** mocks for creating CFG's from adjacency lists *)
module MockNode = struct
  type t = int [@@deriving hash]

  type id = int [@@deriving equal, hash]

  let id n = n

  let loc _ = assert false

  let underlying_node _ = assert false

  let of_underlying_node _ = assert false

  let kind _ = Procdesc.Node.Stmt_node Skip

  let compare_id = Int.compare

  let pp_id fmt i = F.pp_print_int fmt i

  module OrderedId = struct
    type t = id [@@deriving compare]

    let pp = pp_id
  end

  module IdMap = PrettyPrintable.MakePPMap (OrderedId)
  module IdSet = PrettyPrintable.MakePPSet (OrderedId)

  let to_instr _ _ = assert false
end

module MockProcCfg = struct
  module Node = MockNode

  type t = (Node.t * Node.t list) list

  type instrs_dir = Instrs.not_reversed

  let instrs _ = Instrs.empty

  let equal_id = Int.equal

  let fold_succs t n ~init ~f =
    let node_id = Node.id n in
    List.find ~f:(fun (node, _) -> equal_id (Node.id node) node_id) t
    |> Option.value_map ~f:snd ~default:[]
    |> List.fold ~init ~f


  let fold_preds t n ~init ~f =
    try
      let node_id = Node.id n in
      List.filter
        ~f:(fun (_, succs) -> List.exists ~f:(fun node -> equal_id (Node.id node) node_id) succs)
        t
      |> List.map ~f:fst |> List.fold ~init ~f
    with Not_found_s _ | Caml.Not_found -> init


  let fold_nodes t ~init ~f = List.map ~f:fst t |> List.fold ~init ~f

  let fold_normal_succs = fold_succs

  let fold_normal_preds = fold_preds

  let fold_exceptional_succs _ _ ~init ~f:_ = init

  let fold_exceptional_preds _ _ ~init ~f:_ = init

  let from_adjacency_list t = t

  let start_node _ = 1

  let exit_node _ = assert false

  let exn_sink_node _ = assert false

  let proc_desc _ = assert false

  let from_pdesc _ = assert false

  let is_loop_head _ = assert false

  module WTO = WeakTopologicalOrder.Bourdoncle_SCC (struct
    module Node = Node

    type nonrec t = t

    let fold_succs = fold_succs

    let start_node = start_node
  end)

  let wto = WTO.make
end

module S = Scheduler.ReversePostorder (MockProcCfg)

let create_test test_graph expected_result _ =
  (* keep popping and scheduling until the queue is empty, record the results *)
  let rec pop_schedule_record q visited_acc =
    match S.pop q with
    | Some (n, _, q') ->
        pop_schedule_record (S.schedule_succs q' n) (n :: visited_acc)
    | None ->
        List.rev visited_acc
  in
  let pp_diff fmt (exp, actual) =
    let pp_sched fmt l = F.pp_print_list ~pp_sep:F.pp_print_space F.pp_print_int fmt l in
    F.fprintf fmt "Expected schedule %a but got schedule %a" pp_sched exp pp_sched actual
  in
  let cfg = MockProcCfg.from_adjacency_list test_graph in
  let q = S.schedule_succs (S.empty cfg) 1 in
  let result = pop_schedule_record q [1] in
  OUnit2.assert_equal ~pp_diff result expected_result


let inputs =
  [ ("straightline", [(1, [2]); (2, [3]); (3, [4])], [1; 2; 3; 4], "1 2 3 4")
  ; ("if_then_else", [(1, [2; 3]); (2, [4]); (3, [4]); (4, [5])], [1; 2; 3; 4; 5], "1 2 3 4 5")
  ; ("if_then", [(1, [2; 4]); (2, [3]); (3, [4]); (4, [5])], [1; 2; 3; 4; 5], "1 2 3 4 5")
  ; ( "diamond"
    , [(1, [2; 3]); (2, [4]); (3, [4]); (4, [5; 6]); (5, [7]); (6, [7]); (7, [8])]
    , [1; 2; 3; 4; 5; 6; 7; 8]
    , "1 2 3 4 5 6 7 8" )
  ; ( "switch"
    , [(1, [2; 3; 4; 5]); (2, [6]); (3, [6]); (4, [6]); (5, [6]); (6, [7])]
    , [1; 2; 3; 4; 5; 6; 7]
    , "1 2 3 4 5 6 7" )
  ; ( "nums_order_irrelevant"
    , [(11, [10]); (1, [7; 2]); (2, [3; 11]); (7, [11]); (3, [7])]
    , [1; 2; 3; 7; 11; 10]
    , "1 2 3 7 11 10" ) ]


let tests =
  let open OUnit2 in
  let test_list =
    inputs |> List.map ~f:(fun (name, test, expected, _wto) -> name >:: create_test test expected)
  in
  "scheduler_suite" >::: test_list
