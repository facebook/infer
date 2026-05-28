(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module T = Textual
module S = StructuredIR

let pp_sir sir = F.printf "@[<v>%a@]@." S.pp sir

let pp_cfg sir =
  let nodes, _start = S.to_cfg sir in
  List.iter nodes ~f:(fun (node : T.Node.t) ->
      F.printf "#%a:@." T.NodeName.pp node.label ;
      List.iter node.instrs ~f:(fun instr ->
          F.printf "    %a@." (T.Instr.pp ~show_location:false) instr ) ;
      F.printf "    %a@." T.Terminator.pp node.last )


let mk_let name =
  T.Instr.Let
    { id= Some (T.Ident.of_int name)
    ; exp= T.Exp.Const (T.Const.Str (F.asprintf "action_%d" name))
    ; loc= T.Location.Unknown }


let nn s = T.NodeName.of_string s

let bexp0 = T.BoolExp.Exp (T.Exp.Var (T.Ident.of_int 0))

let sir_straight =
  S.Seq
    ( S.Instrs {label= nn "entry"; instrs= [mk_let 0]}
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 42))} )


let sir_diamond =
  S.Seq
    ( S.If
        { label= nn "cond"
        ; bexp= bexp0
        ; then_= S.Instrs {label= nn "then_"; instrs= [mk_let 1]}
        ; else_= S.Instrs {label= nn "else_"; instrs= [mk_let 2]} }
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 0))} )


let sir_loop =
  S.Seq
    ( S.Loop
        { label= nn "loop"
        ; body=
            S.Seq
              ( S.Instrs {label= nn "body"; instrs= [mk_let 0]}
              , S.If
                  { label= nn "test"
                  ; bexp= bexp0
                  ; then_= S.Branch 0
                  ; else_= S.Seq (S.Instrs {label= nn "cont"; instrs= [mk_let 1]}, S.Branch 1) } )
        }
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 0))} )


(* nested loops + multiple ifs:
   outer_loop { a0; if c { a1; inner_loop { a2; if c then break else { a3; continue } }; a4; continue_outer } else { a5; break_outer } }; ret *)
let sir_nested =
  S.Seq
    ( S.Loop
        { label= nn "outer"
        ; body=
            S.Seq
              ( S.Instrs {label= nn "a0"; instrs= [mk_let 0]}
              , S.If
                  { label= nn "t0"
                  ; bexp= bexp0
                  ; then_=
                      S.Seq
                        ( S.Instrs {label= nn "a1"; instrs= [mk_let 1]}
                        , S.Seq
                            ( S.Loop
                                { label= nn "inner"
                                ; body=
                                    S.Seq
                                      ( S.Instrs {label= nn "a2"; instrs= [mk_let 2]}
                                      , S.If
                                          { label= nn "t1"
                                          ; bexp= bexp0
                                          ; then_= S.Branch 0
                                          ; else_=
                                              S.Seq
                                                ( S.Instrs {label= nn "a3"; instrs= [mk_let 3]}
                                                , S.Branch 1 ) } ) }
                            , S.Seq (S.Instrs {label= nn "a4"; instrs= [mk_let 4]}, S.Branch 1) ) )
                  ; else_= S.Instrs {label= nn "a5"; instrs= [mk_let 5]} } ) }
    , S.Return {label= nn "ret"; exp= T.Exp.Const (T.Const.Int (Z.of_int 0))} )


(* Merge node B into its unique predecessor A when A ends with an unconditional jump to B.
   Keeps the label of A, concatenates instructions, uses B's terminator. *)
let coalesce_cfg ~start nodes =
  let pred_count =
    List.fold nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
        let succs = S.successor_labels node.last in
        List.fold succs ~init:acc ~f:(fun acc succ ->
            let n = T.NodeName.Map.find_opt succ acc |> Option.value ~default:0 in
            T.NodeName.Map.add succ (n + 1) acc ) )
  in
  let node_map =
    List.fold nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
        T.NodeName.Map.add node.label node acc )
  in
  let can_absorb target =
    (not (T.NodeName.equal target start))
    && T.NodeName.Map.find_opt target pred_count |> Option.exists ~f:(fun n -> Int.equal n 1)
  in
  let absorbed = T.NodeName.HashSet.create 16 in
  let merge_into (a : T.Node.t) =
    match a.last with
    | Jump [{T.Terminator.label= target; ssa_args= []}] when can_absorb target ->
        let b = T.NodeName.Map.find target node_map in
        T.NodeName.HashSet.add target absorbed ;
        {a with instrs= a.instrs @ b.instrs; last= b.last}
    | _ ->
        a
  in
  let rec fix node =
    let node' = merge_into node in
    if phys_equal node node' then node else fix node'
  in
  List.filter_map nodes ~f:(fun (node : T.Node.t) ->
      if T.NodeName.HashSet.mem absorbed node.label then None else Some (fix node) )


let pp_nodes nodes =
  let buf = Buffer.create 256 in
  let fmt = F.formatter_of_buffer buf in
  let node_map =
    List.fold nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
        T.NodeName.Map.add node.label node acc )
  in
  T.NodeName.Map.iter
    (fun _label (node : T.Node.t) ->
      F.fprintf fmt "#%a:@." T.NodeName.pp node.label ;
      List.iter node.instrs ~f:(fun instr ->
          F.fprintf fmt "    %a@." (T.Instr.pp ~show_location:false) instr ) ;
      F.fprintf fmt "    %a@." T.Terminator.pp node.last )
    node_map ;
  F.pp_print_flush fmt () ;
  Buffer.contents buf


(* Remove empty nodes (no instructions, unconditional jump) using union-find.
   Step 1: build parent map — empty non-start nodes point to their jump target
   Step 2: find with path compression resolves chains
   Step 3: rewrite all terminators using find
   Step 4: remove dead nodes (no predecessor) *)
let bypass_empty_nodes ~start nodes =
  (* step 1: union-find parent map *)
  let parent = ref T.NodeName.Map.empty in
  let merge label target = parent := T.NodeName.Map.add label target !parent in
  let rec find label =
    match T.NodeName.Map.find_opt label !parent with
    | None ->
        label
    | Some target ->
        let root = find target in
        if not (T.NodeName.equal root target) then merge label root ;
        root
  in
  List.iter nodes ~f:(fun (node : T.Node.t) ->
      match (node.instrs, node.last) with
      | [], Jump [{T.Terminator.label= target; ssa_args= []}]
        when not (T.NodeName.equal node.label start) ->
          merge node.label target
      | _ ->
          () ) ;
  if T.NodeName.Map.is_empty !parent then nodes
  else
    (* step 3: rewrite terminators *)
    let rewrite_call {T.Terminator.label; ssa_args} = {T.Terminator.label= find label; ssa_args} in
    let rec rewrite_term (term : T.Terminator.t) =
      match term with
      | Jump targets ->
          T.Terminator.Jump (List.map targets ~f:rewrite_call)
      | If {bexp; then_; else_} ->
          T.Terminator.If {bexp; then_= rewrite_term then_; else_= rewrite_term else_}
      | (Ret _ | Throw _ | Unreachable) as t ->
          t
    in
    let nodes =
      List.map nodes ~f:(fun (node : T.Node.t) -> {node with last= rewrite_term node.last})
    in
    (* step 4: remove dead nodes *)
    let referenced = T.NodeName.HashSet.create 16 in
    T.NodeName.HashSet.add start referenced ;
    List.iter nodes ~f:(fun (node : T.Node.t) ->
        List.iter (S.successor_labels node.last) ~f:(fun succ ->
            T.NodeName.HashSet.add succ referenced ) ) ;
    List.filter nodes ~f:(fun (node : T.Node.t) -> T.NodeName.HashSet.mem referenced node.label)


let normalize_cfg ~start nodes = bypass_empty_nodes ~start (coalesce_cfg ~start nodes)

let check_roundtrip proc =
  let start = proc.T.ProcDesc.start in
  let sir = S.of_cfg proc.T.ProcDesc.nodes start in
  F.printf "@[<v>%a@]@." S.pp sir ;
  let roundtripped, rt_start = S.to_cfg sir in
  let roundtripped = normalize_cfg ~start:rt_start roundtripped in
  let rt_str = pp_nodes roundtripped in
  let orig_str = pp_nodes proc.T.ProcDesc.nodes in
  if String.equal orig_str rt_str then F.printf "roundtrip: OK@."
  else
    let orig_norm_str = pp_nodes (normalize_cfg ~start proc.T.ProcDesc.nodes) in
    if String.equal orig_norm_str rt_str then
      F.printf "roundtrip: OK (after normalising original)@."
    else F.printf "roundtrip: MISMATCH@.--- original@.%s--- roundtrip@.%s" orig_norm_str rt_str


let pp_rpo nodes start =
  let rpo = S.reverse_postorder nodes start in
  let sorted =
    T.NodeName.Map.bindings rpo |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  in
  List.iter sorted ~f:(fun (name, n) -> F.printf "%d: %a@." n T.NodeName.pp name)


let pp_domtree nodes start =
  let idom = S.compute_idom nodes start in
  let children = S.dominator_children idom in
  let rpo = S.reverse_postorder nodes start in
  let sort_by_rpo names =
    List.sort names ~compare:(fun a b ->
        Int.compare (T.NodeName.Map.find a rpo) (T.NodeName.Map.find b rpo) )
  in
  let rec pp_children ~prefix kids =
    let n = List.length kids in
    List.iteri kids ~f:(fun i kid ->
        let is_last = Int.equal i (n - 1) in
        let connector = if is_last then "└── " else "├── " in
        let child_prefix = prefix ^ if is_last then "    " else "│   " in
        F.printf "%s%s%a@." prefix connector T.NodeName.pp kid ;
        let grandkids =
          sort_by_rpo (T.NodeName.Map.find_opt kid children |> Option.value ~default:[])
        in
        pp_children ~prefix:child_prefix grandkids )
  in
  F.printf "%a@." T.NodeName.pp start ;
  let kids = sort_by_rpo (T.NodeName.Map.find_opt start children |> Option.value ~default:[]) in
  pp_children ~prefix:"" kids


let%test_module "structured IR" =
  ( module struct
    let%expect_test "straight-line pp" =
      pp_sir sir_straight ;
      [%expect {|
        n0 = "action_0"
        ret 42 |}]


    let%expect_test "straight-line to_cfg" =
      pp_cfg sir_straight ;
      [%expect
        {|
        #entry:
            n0 = "action_0"
            jmp ret
        #ret:
            ret 42
        |}]


    let%expect_test "diamond pp" =
      pp_sir sir_diamond ;
      [%expect
        {|
        if n0 then
          n1 = "action_1"

        else
          n2 = "action_2"

        ret 0
        |}]


    let%expect_test "diamond to_cfg" =
      pp_cfg sir_diamond ;
      [%expect
        {|
        #cond:
            if n0 then jmp then_ else jmp else_
        #then_:
            n1 = "action_1"
            jmp ret
        #else_:
            n2 = "action_2"
            jmp ret
        #ret:
            ret 0
        |}]


    let%expect_test "loop pp" =
      pp_sir sir_loop ;
      [%expect
        {|
        loop
          n0 = "action_0"
          if n0 then
            branch 0

          else
            n1 = "action_1"
            branch 1


        ret 0
        |}]


    let%expect_test "loop to_cfg" =
      pp_cfg sir_loop ;
      [%expect
        {|
        #loop:
            jmp body
        #body:
            n0 = "action_0"
            jmp test
        #test:
            if n0 then jmp ret else jmp cont
        #cont:
            n1 = "action_1"
            jmp loop
        #ret:
            ret 0
        |}]


    let%expect_test "RPO straight-line" =
      let nodes, start = S.to_cfg sir_straight in
      pp_rpo nodes start ;
      [%expect {|
        0: entry
        1: ret
        |}]


    let%expect_test "RPO diamond" =
      let nodes, start = S.to_cfg sir_diamond in
      pp_rpo nodes start ;
      [%expect {|
        0: cond
        1: else_
        2: then_
        3: ret
        |}]


    let%expect_test "RPO loop" =
      let nodes, start = S.to_cfg sir_loop in
      pp_rpo nodes start ;
      [%expect
        {|
        0: loop
        1: body
        2: test
        3: cont
        4: ret
        |}]


    let%expect_test "domtree straight-line" =
      let nodes, start = S.to_cfg sir_straight in
      pp_domtree nodes start ;
      [%expect {|
        entry
        └── ret
        |}]


    let%expect_test "domtree diamond" =
      let nodes, start = S.to_cfg sir_diamond in
      pp_domtree nodes start ;
      [%expect {|
        cond
        ├── else_
        ├── then_
        └── ret
        |}]


    let%expect_test "domtree loop" =
      let nodes, start = S.to_cfg sir_loop in
      pp_domtree nodes start ;
      [%expect
        {|
        loop
        └── body
            └── test
                ├── cont
                └── ret
        |}]


    let%expect_test "nested pp" =
      pp_sir sir_nested ;
      [%expect
        {|
        loop
          n0 = "action_0"
          if n0 then
            n1 = "action_1"
            loop
              n2 = "action_2"
              if n0 then
                branch 0

              else
                n3 = "action_3"
                branch 1


            n4 = "action_4"
            branch 1

          else
            n5 = "action_5"


        ret 0
        |}]


    let%expect_test "nested to_cfg" =
      pp_cfg sir_nested ;
      [%expect
        {|
        #outer:
            jmp a0
        #a0:
            n0 = "action_0"
            jmp t0
        #t0:
            if n0 then jmp a1 else jmp a5
        #a1:
            n1 = "action_1"
            jmp inner
        #inner:
            jmp a2
        #a2:
            n2 = "action_2"
            jmp t1
        #t1:
            if n0 then jmp a4 else jmp a3
        #a3:
            n3 = "action_3"
            jmp inner
        #a4:
            n4 = "action_4"
            jmp outer
        #a5:
            n5 = "action_5"
            jmp ret
        #ret:
            ret 0
        |}]


    let%expect_test "domtree nested" =
      let nodes, start = S.to_cfg sir_nested in
      pp_domtree nodes start ;
      [%expect
        {|
        outer
        └── a0
            └── t0
                ├── a5
                │   └── ret
                └── a1
                    └── inner
                        └── a2
                            └── t1
                                ├── a3
                                └── a4
        |}]


    let pp_of_cfg nodes start =
      let sir = S.of_cfg nodes start in
      F.printf "@[<v>%a@]@." S.pp sir


    let%expect_test "of_cfg straight-line" =
      let nodes, start = S.to_cfg sir_straight in
      pp_of_cfg nodes start ;
      [%expect {|
        n0 = "action_0"
        ret 42
        |}]


    let%expect_test "of_cfg diamond" =
      let nodes, start = S.to_cfg sir_diamond in
      pp_of_cfg nodes start ;
      [%expect
        {|
        block
          if n0 then
            n1 = "action_1"
            branch 1

          else
            n2 = "action_2"
            branch 1


        ret 0
        |}]


    let%expect_test "of_cfg loop" =
      let nodes, start = S.to_cfg sir_loop in
      pp_of_cfg nodes start ;
      [%expect
        {|
        loop
          n0 = "action_0"
          if n0 then
            ret 0

          else
            n1 = "action_1"
            branch 1
        |}]
  end )


let python_to_proc source ~proc_name =
  let procs = ref [] in
  PyIR.test source ~run:(fun pyir ->
      let textual = PyIR2Textual.mk_module pyir in
      List.iter textual.decls ~f:(fun decl ->
          match decl with T.Module.Proc p -> procs := p :: !procs | _ -> () ) ) ;
  List.find_exn !procs ~f:(fun (proc : T.ProcDesc.t) ->
      String.is_suffix
        (F.asprintf "%a" T.QualifiedProcName.pp proc.procdecl.qualified_name)
        ~suffix:proc_name )


let%test_module "roundtrip Python → Textual → of_cfg → to_cfg → coalesce" =
  ( module struct
    let%expect_test "straight-line" =
      let proc = python_to_proc {|
def foo():
    return 42
|} ~proc_name:"foo" in
      check_roundtrip proc ;
      [%expect
        {|
        n2 = globals
        n1 = locals
        n0 = $builtins.py_make_none()
        ret $builtins.py_make_int(42)

        roundtrip: OK
        |}]


    let%expect_test "if-then-else" =
      let proc =
        python_to_proc
          {|
def foo(c):
    if c:
        x = 1
    else:
        x = 2
    return x
|}
          ~proc_name:"foo"
      in
      check_roundtrip proc ;
      [%expect
        {|
        n2 = globals
        n1 = locals
        n0 = $builtins.py_make_none()
        n3 = $builtins.py_load_fast("c", n1)
        if $builtins.py_bool(n3) then
          _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(1))
          n5 = $builtins.py_load_fast("x", n1)
          _ = $builtins.py_nullify_locals(n1, "x")
          ret n5

        else
          _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(2))
          n4 = $builtins.py_load_fast("x", n1)
          _ = $builtins.py_nullify_locals(n1, "x")
          ret n4


        roundtrip: OK (after normalising original)
        |}]


    let%expect_test "while loop" =
      let proc =
        python_to_proc {|
def foo(c):
    while c:
        x = 1
    return 0
|} ~proc_name:"foo"
      in
      check_roundtrip proc ;
      [%expect
        {|
        block
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_fast("c", n1)
          if $builtins.py_bool(n3) then
            loop
              _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(1))
              n4 = $builtins.py_load_fast("c", n1)
              if $builtins.py_bool(n4) then
                branch 1

              else
                branch 3



          else
            branch 1


        _ = $builtins.py_nullify_locals(n1, "x")
        ret $builtins.py_make_int(0)

        roundtrip: OK (after normalising original)
        |}]


    let%expect_test "nested if" =
      let proc =
        python_to_proc
          {|
def foo(a, b):
    if a:
        if b:
            x = 1
        else:
            x = 2
    else:
        x = 3
    return x
|}
          ~proc_name:"foo"
      in
      check_roundtrip proc ;
      [%expect
        {|
        n2 = globals
        n1 = locals
        n0 = $builtins.py_make_none()
        n3 = $builtins.py_load_fast("a", n1)
        if $builtins.py_bool(n3) then
          n5 = $builtins.py_load_fast("b", n1)
          if $builtins.py_bool(n5) then
            _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(1))
            n7 = $builtins.py_load_fast("x", n1)
            _ = $builtins.py_nullify_locals(n1, "x")
            ret n7

          else
            _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(2))
            n6 = $builtins.py_load_fast("x", n1)
            _ = $builtins.py_nullify_locals(n1, "x")
            ret n6


        else
          _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(3))
          n4 = $builtins.py_load_fast("x", n1)
          _ = $builtins.py_nullify_locals(n1, "x")
          ret n4


        roundtrip: OK (after normalising original)
        |}]


    let%expect_test "sequential ifs" =
      let proc =
        python_to_proc
          {|
def foo(a, b):
    if a:
        x = 1
    else:
        x = 2
    if b:
        y = 3
    else:
        y = 4
    return y
|}
          ~proc_name:"foo"
      in
      check_roundtrip proc ;
      [%expect
        {|
        block
          n2 = globals
          n1 = locals
          n0 = $builtins.py_make_none()
          n3 = $builtins.py_load_fast("a", n1)
          if $builtins.py_bool(n3) then
            _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(1))
            branch 1

          else
            _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(2))
            branch 1


        n4 = $builtins.py_load_fast("b", n1)
        if $builtins.py_bool(n4) then
          _ = $builtins.py_store_fast("y", n1, $builtins.py_make_int(3))
          n6 = $builtins.py_load_fast("y", n1)
          _ = $builtins.py_nullify_locals(n1, "x", "y")
          ret n6

        else
          _ = $builtins.py_store_fast("y", n1, $builtins.py_make_int(4))
          n5 = $builtins.py_load_fast("y", n1)
          _ = $builtins.py_nullify_locals(n1, "x", "y")
          ret n5


        roundtrip: OK (after normalising original)
        |}]


    let%expect_test "loop with nested if" =
      let proc =
        python_to_proc
          {|
def foo(items):
    s = 0
    for x in items:
        if x > 0:
            s += x
    return s
|}
          ~proc_name:"foo"
      in
      check_roundtrip proc ;
      [%expect
        {|
        n2 = globals
        n1 = locals
        n0 = $builtins.py_make_none()
        _ = $builtins.py_store_fast("s", n1, $builtins.py_make_int(0))
        n3 = $builtins.py_load_fast("items", n1)
        n4 = $builtins.py_get_iter(n3)
        loop
          n5 = $builtins.py_next_iter(n4)
          n6 = $builtins.py_has_next_iter(n4)
          if $builtins.py_bool(n6) then
            _ = $builtins.py_store_fast("x", n1, n5)
            block
              n8 = $builtins.py_load_fast("x", n1)
              n9 = $builtins.py_compare_gt(n8, $builtins.py_make_int(0))
              if $builtins.py_bool(n9) then
                n10 = $builtins.py_load_fast("s", n1)
                n11 = $builtins.py_load_fast("x", n1)
                n12 = $builtins.py_inplace_add(n10, n11)
                _ = $builtins.py_store_fast("s", n1, n12)
                branch 1

              else
                branch 1


            branch 1

          else
            n7 = $builtins.py_load_fast("s", n1)
            _ = $builtins.py_nullify_locals(n1, "s", "x")
            ret n7



        roundtrip: OK (after normalising original)
        |}]


    let%expect_test "while with break and continue" =
      let proc =
        python_to_proc
          {|
def foo(n):
    i = 0
    while i < n:
        i += 1
        if i == 3:
            continue
        if i == 7:
            break
        print(i)
    print(i)
    return i
|}
          ~proc_name:"foo"
      in
      check_roundtrip proc ;
      [%expect
        {|
        n2 = globals
        n1 = locals
        n0 = $builtins.py_make_none()
        _ = $builtins.py_store_fast("i", n1, $builtins.py_make_int(0))
        loop
          block
            n3 = $builtins.py_load_fast("i", n1)
            n4 = $builtins.py_load_fast("n", n1)
            n5 = $builtins.py_compare_lt(n3, n4)
            if $builtins.py_bool(n5) then
              loop
                n6 = $builtins.py_load_fast("i", n1)
                n7 = $builtins.py_inplace_add(n6, $builtins.py_make_int(1))
                _ = $builtins.py_store_fast("i", n1, n7)
                n8 = $builtins.py_load_fast("i", n1)
                n9 = $builtins.py_compare_eq(n8, $builtins.py_make_int(3))
                if $builtins.py_bool(n9) then
                  branch 4

                else
                  n10 = $builtins.py_load_fast("i", n1)
                  n11 = $builtins.py_compare_eq(n10, $builtins.py_make_int(7))
                  if $builtins.py_bool(n11) then
                    branch 4

                  else
                    n12 = $builtins.py_load_global("print", n2)
                    n13 = $builtins.py_load_fast("i", n1)
                    n14 = $builtins.py_call(n12, n0, n13)
                    n15 = $builtins.py_load_fast("i", n1)
                    n16 = $builtins.py_load_fast("n", n1)
                    n17 = $builtins.py_compare_lt(n15, n16)
                    if $builtins.py_bool(n17) then
                      branch 3

                    else
                      branch 5





            else
              branch 1


          n18 = $builtins.py_load_global("print", n2)
          n19 = $builtins.py_load_fast("i", n1)
          n20 = $builtins.py_call(n18, n0, n19)
          n21 = $builtins.py_load_fast("i", n1)
          _ = $builtins.py_nullify_locals(n1, "i")
          ret n21


        roundtrip: OK (after normalising original)
        |}]
  end )
