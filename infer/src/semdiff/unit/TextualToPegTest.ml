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
        RET    = (@ret @state0 ($builtins.py_make_int 42))  [ret]
        PEG: (@ret @state0 ($builtins.py_make_int 42))
        |}]


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
        RET    = (@ret @state0 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))  [ret]
        PEG: (@ret @state0 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))
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
        RET    = (@ret @state0 ($builtins.py_make_int 1))  [ret]
        n4     = ($builtins.py_make_int 2)  [let]
        RET    = (@ret @state0 ($builtins.py_make_int 2))  [ret]
        PHI    = (@phi
                     @param:c
                     (@ret @state0 ($builtins.py_make_int 1))
                     (@ret @state0 ($builtins.py_make_int 2)))  [if]
        PEG: (@phi
                 @param:c
                 (@ret @state0 ($builtins.py_make_int 1))
                 (@ret @state0 ($builtins.py_make_int 2)))
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
  | Ok (root, _eqs, _) ->
      F.fprintf fmt "%a" (CongruenceClosureSolver.pp_nested_term cc) root
  | Error msg ->
      F.fprintf fmt "Error: %s" msg


let pp_proc_eqs fmt (proc : Textual.ProcDesc.t) =
  let cc = CongruenceClosureSolver.init ~debug:false in
  match TextualPeg.convert_proc cc proc with
  | Ok (_root, eqs, _) ->
      TextualPeg.Equations.pp cc fmt eqs
  | Error msg ->
      F.fprintf fmt "Error: %s" msg


let pp_proc_tree fmt (proc : Textual.ProcDesc.t) =
  let cc = CongruenceClosureSolver.init ~debug:false in
  match TextualPeg.convert_proc cc proc with
  | Ok (root, _eqs, _) ->
      TextualPeg.pp_tree cc fmt root
  | Error msg ->
      F.fprintf fmt "Error: %s" msg


let check_python_equivalence ?(debug = false) ?(show_textual = true) ?(show_peg = true) source1
    source2 ~proc_name =
  let procs1 = python_to_procs source1 in
  let procs2 = python_to_procs source2 in
  let p1 = find_proc procs1 proc_name in
  let p2 = find_proc procs2 proc_name in
  let pp_proc label p =
    if show_textual then F.printf "=== Textual %s ===@.%a@." label pp_proc_textual p ;
    if show_peg then F.printf "=== PEG %s ===@.%a@.@." label pp_proc_peg p
  in
  pp_proc "1" p1 ;
  pp_proc "2" p2 ;
  TextualPegDiff.check_equivalence ~debug p1 p2


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
        (@ret @state0 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))

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
        (@ret @state0 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))

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
        (@ret @state0 ($builtins.py_binary_add @param:x ($builtins.py_make_int 1)))

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
        (@ret @state0 ($builtins.py_binary_add @param:x ($builtins.py_make_int 2)))

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
        (@ret @state0 ($builtins.py_binary_add ($builtins.py_make_int 1) ($builtins.py_make_int 2)))

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
        (@ret @state0 ($builtins.py_binary_add ($builtins.py_make_int 1) ($builtins.py_make_int 2)))

        equivalent: true
        |}]


    let%expect_test "if/else: same branches" =
      let source1 = {|
def f(c):
    if c:
        x = 1
    else:
        x = 2
    return x
|} in
      let source2 = {|
def f(c):
    if c:
        x = 1
    else:
        x = 2
    return x
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@phi
            ($builtins.py_bool @param:c)
            (@ret @state0 ($builtins.py_make_int 1))
            (@ret @state0 ($builtins.py_make_int 2)))

        === PEG 2 ===
        (@phi
            ($builtins.py_bool @param:c)
            (@ret @state0 ($builtins.py_make_int 1))
            (@ret @state0 ($builtins.py_make_int 2)))

        equivalent: true
        |}]


    let%expect_test "ASCII tree: if/else" =
      let source = {|
def f(c):
    if c:
        x = 1
    else:
        x = 2
    return x
|} in
      let procs = python_to_procs source in
      let p = find_proc procs "f" in
      F.printf "%a" pp_proc_tree p ;
      [%expect
        {|
        @phi
        ├── $builtins.py_bool
        │   └── @param:c
        ├── @ret
        │   ├── @state0
        │   └── $builtins.py_make_int
        │       └── 1
        └── @ret
            ├── @state0
            └── $builtins.py_make_int
                └── 2
        |}]


    let%expect_test "py_make_none as constant" =
      let source1 = {|
def f():
    return None
|} in
      let source2 = {|
def f():
    x = None
    return x
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@ret @state0 @None)

        === PEG 2 ===
        (@ret @state0 @None)

        equivalent: true
        |}]


    let%expect_test "if/else: identical branches simplify to no branch" =
      let source1 = {|
def f(c):
    if c:
        x = 1
    else:
        x = 1
    return x
|} in
      let source2 = {|
def f(c):
    x = 1
    return x
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@phi
            ($builtins.py_bool @param:c)
            (@ret @state0 ($builtins.py_make_int 1))
            (@ret @state0 ($builtins.py_make_int 1)))

        === PEG 2 ===
        (@ret @state0 ($builtins.py_make_int 1))

        equivalent: true
        |}]


    let%expect_test "dead code: unused assignment before return" =
      let source1 = {|
def f():
    x = 1
    y = 2
    return x
|} in
      let source2 = {|
def f():
    x = 1
    return x
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@ret @state0 ($builtins.py_make_int 1))

        === PEG 2 ===
        (@ret @state0 ($builtins.py_make_int 1))

        equivalent: true
        |}]


    let%expect_test "renamed local variables" =
      let source1 = {|
def f(a):
    x = a + 1
    return x
|} in
      let source2 = {|
def f(a):
    y = a + 1
    return y
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@ret @state0 ($builtins.py_binary_add @param:a ($builtins.py_make_int 1)))

        === PEG 2 ===
        (@ret @state0 ($builtins.py_binary_add @param:a ($builtins.py_make_int 1)))

        equivalent: true
        |}]


    let%expect_test "function call: @seq chains effectful calls" =
      let source = {|
def f(x):
    y = g(x)
    z = h(y)
    return z
|} in
      let procs = python_to_procs source in
      let p = find_proc procs "f" in
      F.printf "=== PEG ===@.%a@." pp_proc_peg p ;
      F.printf "=== Tree ===@.%a" pp_proc_tree p ;
      [%expect
        {|
        === PEG ===
        (@ret
            (@heap
                ($builtins.py_call
                    (@heap
                        ($builtins.py_call
                            @state0
                            ($builtins.py_load_global @state0 (@str g) (@load (@lvar globals)))
                            @None
                            @param:x))
                    ($builtins.py_load_global
                        (@heap
                            ($builtins.py_call
                                @state0
                                ($builtins.py_load_global @state0 (@str g) (@load (@lvar globals)))
                                @None
                                @param:x))
                        (@str h)
                        (@load (@lvar globals)))
                    @None
                    (@eval
                        ($builtins.py_call
                            @state0
                            ($builtins.py_load_global @state0 (@str g) (@load (@lvar globals)))
                            @None
                            @param:x))))
            (@eval
                ($builtins.py_call
                    (@heap
                        ($builtins.py_call
                            @state0
                            ($builtins.py_load_global @state0 (@str g) (@load (@lvar globals)))
                            @None
                            @param:x))
                    ($builtins.py_load_global
                        (@heap
                            ($builtins.py_call
                                @state0
                                ($builtins.py_load_global @state0 (@str g) (@load (@lvar globals)))
                                @None
                                @param:x))
                        (@str h)
                        (@load (@lvar globals)))
                    @None
                    (@eval
                        ($builtins.py_call
                            @state0
                            ($builtins.py_load_global @state0 (@str g) (@load (@lvar globals)))
                            @None
                            @param:x)))))
        === Tree ===
        @ret
        ├── @heap
        │   └── $builtins.py_call
        │       ├── @heap
        │       │   └── $builtins.py_call
        │       │       ├── @state0
        │       │       ├── $builtins.py_load_global
        │       │       │   ├── @state0
        │       │       │   ├── @str
        │       │       │   │   └── g
        │       │       │   └── @load
        │       │       │       └── @lvar
        │       │       │           └── globals
        │       │       ├── @None
        │       │       └── @param:x
        │       ├── $builtins.py_load_global
        │       │   ├── @heap
        │       │   │   └── $builtins.py_call
        │       │   │       ├── @state0
        │       │   │       ├── $builtins.py_load_global
        │       │   │       │   ├── @state0
        │       │   │       │   ├── @str
        │       │   │       │   │   └── g
        │       │   │       │   └── @load
        │       │   │       │       └── @lvar
        │       │   │       │           └── globals
        │       │   │       ├── @None
        │       │   │       └── @param:x
        │       │   ├── @str
        │       │   │   └── h
        │       │   └── @load
        │       │       └── @lvar
        │       │           └── globals
        │       ├── @None
        │       └── @eval
        │           └── $builtins.py_call
        │               ├── @state0
        │               ├── $builtins.py_load_global
        │               │   ├── @state0
        │               │   ├── @str
        │               │   │   └── g
        │               │   └── @load
        │               │       └── @lvar
        │               │           └── globals
        │               ├── @None
        │               └── @param:x
        └── @eval
            └── $builtins.py_call
                ├── @heap
                │   └── $builtins.py_call
                │       ├── @state0
                │       ├── $builtins.py_load_global
                │       │   ├── @state0
                │       │   ├── @str
                │       │   │   └── g
                │       │   └── @load
                │       │       └── @lvar
                │       │           └── globals
                │       ├── @None
                │       └── @param:x
                ├── $builtins.py_load_global
                │   ├── @heap
                │   │   └── $builtins.py_call
                │   │       ├── @state0
                │   │       ├── $builtins.py_load_global
                │   │       │   ├── @state0
                │   │       │   ├── @str
                │   │       │   │   └── g
                │   │       │   └── @load
                │   │       │       └── @lvar
                │   │       │           └── globals
                │   │       ├── @None
                │   │       └── @param:x
                │   ├── @str
                │   │   └── h
                │   └── @load
                │       └── @lvar
                │           └── globals
                ├── @None
                └── @eval
                    └── $builtins.py_call
                        ├── @state0
                        ├── $builtins.py_load_global
                        │   ├── @state0
                        │   ├── @str
                        │   │   └── g
                        │   └── @load
                        │       └── @lvar
                        │           └── globals
                        ├── @None
                        └── @param:x
        |}]


    let%expect_test "simple for loop with py_store_fast/py_load_fast" =
      convert_and_print
        {|
        .source_language = "python"
        define .args = "l" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n0 = $builtins.py_make_none()
              n1 = locals
              n2 = globals
              n3 = $builtins.py_load_fast("l", n1)
              n4 = $builtins.py_get_iter(n3)
              jmp b1

          #b1:
              n5 = $builtins.py_next_iter(n4)
              n6 = $builtins.py_has_next_iter(n4)
              if $builtins.py_bool(n6) then jmp b2 else jmp b3

          #b2:
              _ = $builtins.py_store_fast("i", n1, n5)
              n7 = $builtins.py_load_fast("i", n1)
              n8 = $builtins.py_load_global("print", n2)
              n9 = $builtins.py_call(n8, n0, n7)
              jmp b1

          #b3:
              ret n0
        }
        |} ;
      [%expect
        {|
        === foo ===
        Equations:
        l      = @param:l  [param]
        n0     = @None  [let]
        n1     = (@load (@lvar locals))  [let]
        n2     = (@load (@lvar globals))  [let]
        n3     = @param:l  [load_fast: locals]
        n4     = ($builtins.py_get_iter @state0 @param:l)  [let]
        n5     = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [let]
        n6     = ($builtins.py_has_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [let]
        i      = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [store_fast: locals]
        n7     = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [load_fast: locals]
        n8     = ($builtins.py_load_global @theta:state:0 (@str print) (@load (@lvar globals)))  [let]
        n9     = (@eval
                     ($builtins.py_call
                         @theta:state:0
                         ($builtins.py_load_global @theta:state:0 (@str print) (@load (@lvar globals)))
                         @None
                         ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))))  [let]
        θ_state_0 = (@theta_0
                         @state0
                         (@heap
                             ($builtins.py_call
                                 @theta:state:0
                                 ($builtins.py_load_global
                                     @theta:state:0
                                     (@str print)
                                     (@load (@lvar globals)))
                                 @None
                                 ($builtins.py_next_iter
                                     @theta:state:0
                                     ($builtins.py_get_iter @state0 @param:l)))))  [theta_close]
        θ_i_0 = (@theta_0
                     @undef
                     ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l)))  [theta_close]
        RET    = (@ret @theta:state:0 @None)  [ret]
        PEG: (@ret @theta:state:0 @None)
        |}]


    let%expect_test "loop equivalence: renamed loop variable" =
      let loop_text var_name =
        F.asprintf
          {|
        .source_language = "python"
        define .args = "l" foo(globals: *PyGlobals, locals: *PyLocals) : *PyObject {
          #b0:
              n0 = $builtins.py_make_none()
              n1 = locals
              n2 = globals
              n3 = $builtins.py_load_fast("l", n1)
              n4 = $builtins.py_get_iter(n3)
              jmp b1

          #b1:
              n5 = $builtins.py_next_iter(n4)
              n6 = $builtins.py_has_next_iter(n4)
              if $builtins.py_bool(n6) then jmp b2 else jmp b3

          #b2:
              _ = $builtins.py_store_fast("%s", n1, n5)
              n7 = $builtins.py_load_fast("%s", n1)
              n8 = $builtins.py_load_global("print", n2)
              n9 = $builtins.py_call(n8, n0, n7)
              jmp b1

          #b3:
              ret n0
        }
        |}
          var_name var_name
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
      let proc1 = get_proc (loop_text "i") in
      let proc2 = get_proc (loop_text "x") in
      let result = TextualPegDiff.check_equivalence proc1 proc2 in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: true |}]


    let%expect_test "python: for loop PEG structure" =
      let source = {|
def f(l):
    for i in l:
        print(i)
|} in
      let procs = python_to_procs source in
      let p = find_proc procs "f" in
      F.printf "=== PEG ===@.%a@." pp_proc_peg p ;
      F.printf "=== Tree ===@.%a" pp_proc_tree p ;
      [%expect
        {|
        === PEG ===
        (@ret @theta:state:0 @None)
        === Tree ===
        @ret
        ├── @theta:state:0
        └── @None
        |}]


    let%expect_test "python: for loop renamed variable equivalence" =
      let source1 = {|
def f(l):
    for i in l:
        print(i)
|} in
      let source2 = {|
def f(l):
    for x in l:
        print(x)
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@ret @theta:state:0 @None)

        === PEG 2 ===
        (@ret @theta:state:0 @None)

        equivalent: true
        |}]


    let%expect_test "python: for loop different body not equivalent" =
      let source1 = {|
def f(l):
    for i in l:
        print(i)
|} in
      let source2 = {|
def f(l):
    for i in l:
        print(i + 1)
|} in
      let result = check_python_equivalence ~show_textual:false source1 source2 ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@ret @theta:state:0 @None)

        === PEG 2 ===
        (@ret @theta:state:0 @None)

        equivalent: false
        |}]


    let%expect_test "python: while loop" =
      let source = {|
def f(x):
    while x > 0:
        x = x - 1
    return x
|} in
      let procs = python_to_procs source in
      let p = find_proc procs "f" in
      F.printf "=== Textual ===@.%a@." pp_proc_textual p ;
      F.printf "=== Equations ===@.%a@." pp_proc_eqs p ;
      F.printf "=== PEG ===@.%a@." pp_proc_peg p ;
      [%expect
        {|
        === Textual ===
        define .args = "x" dummy.f(globals: *PyGlobals<dummy>, locals: *PyLocals) : *PyObject {
          #b0:
              n2 = globals
              n1 = locals
              n0 = $builtins.py_make_none()
              n3 = $builtins.py_load_fast("x", n1)
              n4 = $builtins.py_compare_gt(n3, $builtins.py_make_int(0))
              if $builtins.py_bool(n4) then jmp b1 else jmp b3

          #b1:
              n5 = $builtins.py_load_fast("x", n1)
              n6 = $builtins.py_binary_substract(n5, $builtins.py_make_int(1))
              _ = $builtins.py_store_fast("x", n1, n6)
              jmp b2

          #b2:
              n7 = $builtins.py_load_fast("x", n1)
              n8 = $builtins.py_compare_gt(n7, $builtins.py_make_int(0))
              if $builtins.py_bool(n8) then jmp b1 else jmp b3

          #b3:
              n9 = $builtins.py_load_fast("x", n1)
              ret n9

        }


        === Equations ===
        x      = @param:x  [param]
        n2     = (@load (@lvar globals))  [let]
        n1     = (@load (@lvar locals))  [let]
        n0     = @None  [let]
        n3     = @param:x  [load_fast: locals]
        n4     = ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0))  [let]
        n5     = @theta:x:0  [load_fast: locals]
        n6     = ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1))  [let]
        x      = ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1))  [store_fast: locals]
        n7     = ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1))  [load_fast: locals]
        n8     = ($builtins.py_compare_gt
                     ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1))
                     ($builtins.py_make_int 0))  [let]
        θ_state_0 = (@theta_0 @state0 @theta:state:0)  [theta_close]
        θ_x_0 = (@theta_0 @param:x ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1)))  [theta_close]
        n9     = ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1))  [load_fast: locals]
        RET    = (@ret @theta:state:0 ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1)))  [ret]
        n9     = @param:x  [load_fast: locals]
        RET    = (@ret @state0 @param:x)  [ret]
        PHI    = (@phi
                     ($builtins.py_bool ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0)))
                     (@ret
                         @theta:state:0
                         ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1)))
                     (@ret @state0 @param:x))  [if]
        PHI_state = (@phi
                        ($builtins.py_bool ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0)))
                        @theta:state:0
                        @state0)  [if]
        PHI_x  = (@phi
                     ($builtins.py_bool ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0)))
                     ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1))
                     @param:x)  [if]

        === PEG ===
        (@phi
            ($builtins.py_bool ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0)))
            (@ret @theta:state:0 ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1)))
            (@ret @state0 @param:x))
        |}]


    let%expect_test "python: while loop same function is equivalent to itself" =
      let source = {|
def f(x):
    while x > 0:
        x = x - 1
    return x
|} in
      let result = check_python_equivalence ~show_textual:false source source ~proc_name:"f" in
      F.printf "equivalent: %b@." result ;
      [%expect
        {|
        === PEG 1 ===
        (@phi
            ($builtins.py_bool ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0)))
            (@ret @theta:state:0 ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1)))
            (@ret @state0 @param:x))

        === PEG 2 ===
        (@phi
            ($builtins.py_bool ($builtins.py_compare_gt @param:x ($builtins.py_make_int 0)))
            (@ret @theta:state:0 ($builtins.py_binary_substract @theta:x:0 ($builtins.py_make_int 1)))
            (@ret @state0 @param:x))

        equivalent: true
        |}]


    let%expect_test "python: if inside for loop preserves state" =
      let source =
        {|
def f(l, c):
    for i in l:
        if c:
            print(1)
        else:
            print(2)
|}
      in
      let procs = python_to_procs source in
      let p = find_proc procs "f" in
      F.printf "=== Equations ===@.%a@." pp_proc_eqs p ;
      F.printf "=== PEG ===@.%a@." pp_proc_peg p ;
      [%expect
        {|
        === Equations ===
        l      = @param:l  [param]
        c      = @param:c  [param]
        n2     = (@load (@lvar globals))  [let]
        n1     = (@load (@lvar locals))  [let]
        n0     = @None  [let]
        n3     = @param:l  [load_fast: locals]
        n4     = ($builtins.py_get_iter @state0 @param:l)  [let]
        n5     = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [let]
        n6     = ($builtins.py_has_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [let]
        i      = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l))  [store_fast: locals]
        n7     = @param:c  [load_fast: locals]
        n10    = ($builtins.py_load_global @theta:state:0 (@str print) (@load (@lvar globals)))  [let]
        n11    = (@eval
                     ($builtins.py_call
                         @theta:state:0
                         ($builtins.py_load_global @theta:state:0 (@str print) (@load (@lvar globals)))
                         @None
                         ($builtins.py_make_int 1)))  [let]
        n8     = ($builtins.py_load_global @theta:state:0 (@str print) (@load (@lvar globals)))  [let]
        n9     = (@eval
                     ($builtins.py_call
                         @theta:state:0
                         ($builtins.py_load_global @theta:state:0 (@str print) (@load (@lvar globals)))
                         @None
                         ($builtins.py_make_int 2)))  [let]
        PHI    = (@phi ($builtins.py_bool @param:c) @back_edge @back_edge)  [if]
        PHI_state = (@phi
                        ($builtins.py_bool @param:c)
                        (@heap
                            ($builtins.py_call
                                @theta:state:0
                                ($builtins.py_load_global
                                    @theta:state:0
                                    (@str print)
                                    (@load (@lvar globals)))
                                @None
                                ($builtins.py_make_int 1)))
                        (@heap
                            ($builtins.py_call
                                @theta:state:0
                                ($builtins.py_load_global
                                    @theta:state:0
                                    (@str print)
                                    (@load (@lvar globals)))
                                @None
                                ($builtins.py_make_int 2))))  [if]
        θ_state_0 = (@theta_0
                         @state0
                         (@phi
                             ($builtins.py_bool @param:c)
                             (@heap
                                 ($builtins.py_call
                                     @theta:state:0
                                     ($builtins.py_load_global
                                         @theta:state:0
                                         (@str print)
                                         (@load (@lvar globals)))
                                     @None
                                     ($builtins.py_make_int 1)))
                             (@heap
                                 ($builtins.py_call
                                     @theta:state:0
                                     ($builtins.py_load_global
                                         @theta:state:0
                                         (@str print)
                                         (@load (@lvar globals)))
                                     @None
                                     ($builtins.py_make_int 2)))))  [theta_close]
        θ_i_0 = (@theta_0
                     @undef
                     ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:l)))  [theta_close]
        RET    = (@ret @theta:state:0 @None)  [ret]

        === PEG ===
        (@ret @theta:state:0 @None)
        |}]


    let%expect_test "python: for loop different function in body not equivalent" =
      let source1 = {|
def f(l):
    for i in l:
        print(i)
|} in
      let source2 = {|
def f(l):
    for i in l:
        len(i)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]


    let%expect_test "python: while loop different operation not equivalent" =
      let source1 = {|
def f(x):
    while x > 0:
        x = x - 1
    return x
|} in
      let source2 = {|
def f(x):
    while x > 0:
        x = x + 1
    return x
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]


    let%expect_test "python: while loop different condition not equivalent" =
      let source1 = {|
def f(x):
    while x > 0:
        x = x - 1
    return x
|} in
      let source2 = {|
def f(x):
    while x < 0:
        x = x - 1
    return x
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]


    let%expect_test "python: while loop different init not equivalent" =
      let source1 = {|
def f(x):
    while x > 0:
        x = x - 1
    return x
|} in
      let source2 = {|
def f(x):
    x = x + 1
    while x > 0:
        x = x - 1
    return x
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]


    let%expect_test "B007: unused loop variable renamed to _i" =
      let source1 = {|
def f(l):
    for i in l:
        print(1)
|} in
      let source2 = {|
def f(l):
    for _i in l:
        print(1)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: true |}]


    let%expect_test "B007: remove unnecessary enumerate" =
      let source1 = {|
def f(l):
    for i, x in enumerate(l):
        print(x)
|} in
      let source2 = {|
def f(l):
    for x in l:
        print(x)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: true |}]


    let%expect_test "B007: enumerate with both i and x used is not equivalent to plain loop" =
      let source1 = {|
def f(l):
    for i, x in enumerate(l):
        print(i, x)
|} in
      let source2 = {|
def f(l):
    for x in l:
        print(x)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]


    let%expect_test "B007: dict items() to keys() when value unused" =
      let source1 = {|
def f(d):
    for k, v in d.items():
        print(k)
|} in
      let source2 = {|
def f(d):
    for k in d.keys():
        print(k)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: true |}]


    let%expect_test "B007: dict items() to values() when key unused" =
      let source1 = {|
def f(d):
    for k, v in d.items():
        print(v)
|} in
      let source2 = {|
def f(d):
    for v in d.values():
        print(v)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: true |}]


    let%expect_test "B007: dict items() with both k and v used is not equivalent to keys()" =
      let source1 = {|
def f(d):
    for k, v in d.items():
        print(k, v)
|} in
      let source2 = {|
def f(d):
    for k in d.keys():
        print(k)
|} in
      let result =
        check_python_equivalence ~show_textual:false ~show_peg:false source1 source2 ~proc_name:"f"
      in
      F.printf "equivalent: %b@." result ;
      [%expect {| equivalent: false |}]
  end )
