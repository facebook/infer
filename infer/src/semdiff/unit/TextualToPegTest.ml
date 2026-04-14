(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(* ---------- Helpers ---------- *)

let convert_and_print text = TextualPegDiff.convert_and_print text

(* ---------- Tests ---------- *)

let%test_module "textual to peg" =
  ( module struct
    let%expect_test "straight-line: ret constant" =
      convert_and_print
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n0 = $builtins.py_make_int(42)
              ret n0
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        n0     = ($builtins.py_make_int 42)  [let]
        RET    = (@ret (@seq @state0 ($builtins.py_make_int 42)) ($builtins.py_make_int 42))  [ret]
        PEG: (@ret (@seq @state0 ($builtins.py_make_int 42)) ($builtins.py_make_int 42)) |}]


    let%expect_test "store_fast then load_fast" =
      convert_and_print
        {|
        .source_language = "python"
        define .args = "x" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n1 = locals
              n2 = $builtins.py_load_fast("x", n1)
              n3 = $builtins.py_make_int(1)
              n4 = $builtins.py_binary_add(n2, n3)
              _ = $builtins.py_store_fast("y", n1, n4)
              jmp b1

          #b1:
              n5 = $builtins.py_load_fast("y", n1)
              ret n5
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        x      = @param:x  [param]
        n1     = (@load (@lvar locals))  [let]
        n2     = @param:x  [load_fast: locals]
        n3     = ($builtins.py_make_int 1)  [let]
        n4     = ($builtins.py_binary_add @param:x ($builtins.py_make_int 1))  [let]
        y      = ($builtins.py_binary_add @param:x ($builtins.py_make_int 1))  [store_fast: locals]
        n5     = ($builtins.py_binary_add @param:x ($builtins.py_make_int 1))  [load_fast: locals]
        RET    = (@ret
                     (@seq
                         (@seq @state0 ($builtins.py_make_int 1))
                         ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
                     ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))  [ret]
        PEG: (@ret
                 (@seq
                     (@seq @state0 ($builtins.py_make_int 1))
                     ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
                 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
        |}]


    let%expect_test "if branch with phi" =
      convert_and_print
        {|
        .source_language = "python"
        define .args = "c" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n1 = locals
              n2 = $builtins.py_load_fast("c", n1)
              if n2 then jmp b1 else jmp b2

          #b1:
              n3 = $builtins.py_make_int(1)
              ret n3

          #b2:
              n4 = $builtins.py_make_int(2)
              ret n4
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        c      = @param:c  [param]
        n1     = (@load (@lvar locals))  [let]
        n2     = @param:c  [load_fast: locals]
        n3     = ($builtins.py_make_int 1)  [let]
        RET    = (@ret (@seq @state0 ($builtins.py_make_int 1)) ($builtins.py_make_int 1))  [ret]
        n4     = ($builtins.py_make_int 2)  [let]
        RET    = (@ret (@seq @state0 ($builtins.py_make_int 2)) ($builtins.py_make_int 2))  [ret]
        PHI    = (@phi
                     @param:c
                     (@ret (@seq @state0 ($builtins.py_make_int 1)) ($builtins.py_make_int 1))
                     (@ret (@seq @state0 ($builtins.py_make_int 2)) ($builtins.py_make_int 2)))  [if]
        PEG: (@phi
                 @param:c
                 (@ret (@seq @state0 ($builtins.py_make_int 1)) ($builtins.py_make_int 1))
                 (@ret (@seq @state0 ($builtins.py_make_int 2)) ($builtins.py_make_int 2)))
        |}]


    let%expect_test "equivalence: same semantics, different ident names" =
      let text1 =
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n0 = $builtins.py_make_int(42)
              ret n0
        }
        |}
      in
      let text2 =
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n99 = $builtins.py_make_int(42)
              ret n99
        }
        |}
      in
      let sourcefile = Textual.SourceFile.create "test.sil" in
      let get_proc text =
        match TextualParser.parse_string sourcefile text with
        | Ok module_ ->
            List.find_map_exn module_.decls ~f:(fun decl ->
                match decl with Textual.Module.Proc p -> Some p | _ -> None )
        | Error _ ->
            assert false
      in
      let proc1 = get_proc text1 in
      let proc2 = get_proc text2 in
      let result = TextualPegDiff.check_equivalence proc1 proc2 in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: true |}]


    let%expect_test "non-equivalence: different constants" =
      let text1 =
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n0 = $builtins.py_make_int(42)
              ret n0
        }
        |}
      in
      let text2 =
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n0 = $builtins.py_make_int(99)
              ret n0
        }
        |}
      in
      let sourcefile = Textual.SourceFile.create "test.sil" in
      let get_proc text =
        match TextualParser.parse_string sourcefile text with
        | Ok module_ ->
            List.find_map_exn module_.decls ~f:(fun decl ->
                match decl with Textual.Module.Proc p -> Some p | _ -> None )
        | Error _ ->
            assert false
      in
      let proc1 = get_proc text1 in
      let proc2 = get_proc text2 in
      let result = TextualPegDiff.check_equivalence proc1 proc2 in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]


    let%expect_test "phi simplification: identical branches" =
      let text1 =
        {|
        .source_language = "python"
        define .args = "c" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n1 = locals
              n2 = $builtins.py_load_fast("c", n1)
              if n2 then jmp b1 else jmp b2

          #b1:
              n3 = $builtins.py_make_int(1)
              ret n3

          #b2:
              n4 = $builtins.py_make_int(1)
              ret n4
        }
        |}
      in
      let text2 =
        {|
        .source_language = "python"
        define foo() : int {
          #b0:
              n0 = $builtins.py_make_int(1)
              ret n0
        }
        |}
      in
      let sourcefile = Textual.SourceFile.create "test.sil" in
      let get_proc text =
        match TextualParser.parse_string sourcefile text with
        | Ok module_ ->
            List.find_map_exn module_.decls ~f:(fun decl ->
                match decl with Textual.Module.Proc p -> Some p | _ -> None )
        | Error _ ->
            assert false
      in
      let proc1 = get_proc text1 in
      let proc2 = get_proc text2 in
      let result = TextualPegDiff.check_equivalence ~debug:true proc1 proc2 in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === Rule stats ===
          (@phi ?C ?X ?X) ==> ?X: fired 1 time(s)
        equivalent: true
        |}]
  end )


(* ---------- Python source tests ---------- *)

let python_to_procs source =
  let procs = ref [] in
  PyIR.test source ~run:(fun pyir ->
      let textual = PyIR2Textual.mk_module pyir in
      List.iter textual.decls ~f:(fun decl ->
          match decl with Textual.Module.Proc p -> procs := p :: !procs | _ -> () ) ) ;
  List.rev !procs


let find_proc procs name =
  List.find_exn procs ~f:(fun (p : Textual.ProcDesc.t) ->
      let proc_name = F.asprintf "%a" Textual.QualifiedProcName.pp p.procdecl.qualified_name in
      String.is_suffix proc_name ~suffix:name )


let pp_proc_textual fmt (proc : Textual.ProcDesc.t) =
  F.fprintf fmt "%a" (Textual.ProcDesc.pp ~show_location:false) proc


let pp_proc_peg fmt (proc : Textual.ProcDesc.t) =
  let cc = CongruenceClosureSolver.init ~debug:false in
  match TextualPeg.convert_proc cc proc with
  | Ok (root, _eqs) ->
      F.fprintf fmt "%a" (CongruenceClosureSolver.pp_nested_term cc) root
  | Error msg ->
      F.fprintf fmt "Error: %s" msg


let check_python_equivalence source1 source2 ~proc_name =
  let procs1 = python_to_procs source1 in
  let procs2 = python_to_procs source2 in
  let p1 = find_proc procs1 proc_name in
  let p2 = find_proc procs2 proc_name in
  F.printf "=== Textual 1 ===@.%a@.=== PEG 1 ===@.%a@.@." pp_proc_textual p1 pp_proc_peg p1 ;
  F.printf "=== Textual 2 ===@.%a@.=== PEG 2 ===@.%a@.@." pp_proc_textual p2 pp_proc_peg p2 ;
  TextualPegDiff.check_equivalence p1 p2


let%test_module "python source to peg" =
  ( module struct
    let%expect_test "same function is equivalent to itself" =
      let source = {|
def f(x):
    y = x + 1
    return y
|} in
      let result = check_python_equivalence source source ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === Textual 1 ===
        define .args = "x" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_binary_add(n3, $builtins.py_make_int(1))
              _ = $builtins.py_store_fast("y", n1, n4)
              jmp b1

          #b1:
              n5 = $builtins.py_load_fast("y", n1)
              _ = $builtins.py_nullify_locals(n1, "y")
              ret n5

        }


        === PEG 1 ===
        (@ret
            (@seq
                (@seq @state0 $builtins.py_make_none)
                ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
            ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))

        === Textual 2 ===
        define .args = "x" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_binary_add(n3, $builtins.py_make_int(1))
              _ = $builtins.py_store_fast("y", n1, n4)
              jmp b1

          #b1:
              n5 = $builtins.py_load_fast("y", n1)
              _ = $builtins.py_nullify_locals(n1, "y")
              ret n5

        }


        === PEG 2 ===
        (@ret
            (@seq
                (@seq @state0 $builtins.py_make_none)
                ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
            ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))

        equivalent: true
        |}]


    let%expect_test "different computation" =
      let source1 = {|
def f(x):
    return x + 1
|} in
      let source2 = {|
def f(x):
    return x + 2
|} in
      let result = check_python_equivalence source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === Textual 1 ===
        define .args = "x" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_binary_add(n3, $builtins.py_make_int(1))
              ret n4

        }


        === PEG 1 ===
        (@ret
            (@seq
                (@seq @state0 $builtins.py_make_none)
                ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
            ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))

        === Textual 2 ===
        define .args = "x" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_binary_add(n3, $builtins.py_make_int(2))
              ret n4

        }


        === PEG 2 ===
        (@ret
            (@seq
                (@seq @state0 $builtins.py_make_none)
                ($builtins.py_binary_add @param:x ($builtins.py_make_int 2)))
            ($builtins.py_binary_add @param:x ($builtins.py_make_int 2)))

        equivalent: false
        |}]


    let%expect_test "reorder independent assignments" =
      let source1 = {|
def f():
    x = 1
    y = 2
    return x + y
|} in
      let source2 = {|
def f():
    y = 2
    x = 1
    return x + y
|} in
      let result = check_python_equivalence source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === Textual 1 ===
        define dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(1))
              jmp b1

          #b1:
              _ = $builtins.py_store_fast("y", n1, $builtins.py_make_int(2))
              jmp b2

          #b2:
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_load_fast("y", n1)
              n5 = $builtins.py_binary_add(n3, n4)
              _ = $builtins.py_nullify_locals(n1, "x", "y")
              ret n5

        }


        === PEG 1 ===
        (@ret
            (@seq
                (@seq @state0 $builtins.py_make_none)
                ($builtins.py_binary_add ($builtins.py_make_int 1) ($builtins.py_make_int 2)))
            ($builtins.py_binary_add ($builtins.py_make_int 1) ($builtins.py_make_int 2)))

        === Textual 2 ===
        define dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              _ = $builtins.py_store_fast("y", n1, $builtins.py_make_int(2))
              jmp b1

          #b1:
              _ = $builtins.py_store_fast("x", n1, $builtins.py_make_int(1))
              jmp b2

          #b2:
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_load_fast("y", n1)
              n5 = $builtins.py_binary_add(n3, n4)
              _ = $builtins.py_nullify_locals(n1, "y", "x")
              ret n5

        }


        === PEG 2 ===
        (@ret
            (@seq
                (@seq @state0 $builtins.py_make_none)
                ($builtins.py_binary_add ($builtins.py_make_int 1) ($builtins.py_make_int 2)))
            ($builtins.py_binary_add ($builtins.py_make_int 1) ($builtins.py_make_int 2)))

        equivalent: true
        |}]
  end )
