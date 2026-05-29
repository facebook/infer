(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module T = Textual
module CC = CongruenceClosureSolver

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


let check_peg source ~proc_name =
  let proc = python_to_proc source ~proc_name in
  let cc = CC.init ~debug:false in
  match StructuredPeg.convert_proc cc proc with
  | Ok (root, equations, _loops) ->
      F.printf "--- equations ---@." ;
      StructuredPeg.Equations.pp cc F.std_formatter equations ;
      F.printf "--- root ---@." ;
      F.printf "%a@." (CC.pp_nested_term cc) root
  | Error msg ->
      F.printf "ERROR: %s@." msg


let%test_module "StructuredPeg" =
  ( module struct
    let%expect_test "straight-line" =
      check_peg {|
def foo():
    return 42
|} ~proc_name:"foo" ;
      [%expect
        {|
        --- equations ---
        n2     = (@load (@lvar globals))  [let]
        n1     = (@load (@lvar locals))  [let]
        n0     = @None  [let]
        --- root ---
        (@ret @state0 ($builtins.py_make_int 42))
        |}]


    let%expect_test "if-then-else" =
      check_peg
        {|
def foo(c):
    if c:
        x = 1
    else:
        x = 2
    return x
|}
        ~proc_name:"foo" ;
      [%expect
        {|
        --- equations ---
        c      = @param:c  [param]
        n2     = (@load (@lvar globals))  [let]
        n1     = (@load (@lvar locals))  [let]
        n0     = @None  [let]
        n3     = @param:c  [load_fast: locals]
        x      = ($builtins.py_make_int 1)  [store_fast: locals]
        n5     = ($builtins.py_make_int 1)  [load_fast: locals]
        x      = ($builtins.py_make_int 2)  [store_fast: locals]
        n4     = ($builtins.py_make_int 2)  [load_fast: locals]
        --- root ---
        (@phi
            ($builtins.py_bool @param:c)
            (@ret @state0 ($builtins.py_make_int 1))
            (@ret @state0 ($builtins.py_make_int 2)))
        |}]
  end )
