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
module S = StructuredIR

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


    let%expect_test "while loop" =
      check_peg
        {|
def foo(n):
    i = 0
    while i < n:
        i += 1
    return i
|}
        ~proc_name:"foo" ;
      [%expect
        {|
        --- equations ---
        n      = @param:n  [param]
        n2     = (@load (@lvar globals))  [let]
        n1     = (@load (@lvar locals))  [let]
        n0     = @None  [let]
        i      = ($builtins.py_make_int 0)  [store_fast: locals]
        n3     = ($builtins.py_make_int 0)  [load_fast: locals]
        n4     = @param:n  [load_fast: locals]
        n5     = ($builtins.py_compare_lt ($builtins.py_make_int 0) @param:n)  [let]
        n6     = @theta:i:0  [load_fast: locals]
        n7     = ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))  [let]
        i      = ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))  [store_fast: locals]
        n8     = ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))  [load_fast: locals]
        n9     = @param:n  [load_fast: locals]
        n10    = ($builtins.py_compare_lt
                     ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))
                     @param:n)  [let]
        θ_state_0 = (@theta_0 @state0 @theta:state:0)  [theta_close]
        θ_i_0 = (@theta_0
                    ($builtins.py_make_int 0)
                    ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1)))  [theta_close]
        n11    = (@phi
                     ($builtins.py_bool ($builtins.py_compare_lt ($builtins.py_make_int 0) @param:n))
                     (@exit_value
                         @theta:i:0
                         (@not
                             ($builtins.py_bool
                                 ($builtins.py_compare_lt
                                     ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))
                                     @param:n))))
                     ($builtins.py_make_int 0))  [load_fast: locals]
        --- root ---
        (@ret
            (@phi
                ($builtins.py_bool ($builtins.py_compare_lt ($builtins.py_make_int 0) @param:n))
                (@exit_value
                    @theta:state:0
                    (@not
                        ($builtins.py_bool
                            ($builtins.py_compare_lt
                                ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))
                                @param:n))))
                @state0)
            (@phi
                ($builtins.py_bool ($builtins.py_compare_lt ($builtins.py_make_int 0) @param:n))
                (@exit_value
                    @theta:i:0
                    (@not
                        ($builtins.py_bool
                            ($builtins.py_compare_lt
                                ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1))
                                @param:n))))
                ($builtins.py_make_int 0)))
        |}]


    let%expect_test "for loop with accumulator" =
      check_peg
        {|
def foo(items):
    s = 0
    for x in items:
        s += x
    return s
|}
        ~proc_name:"foo" ;
      [%expect
        {|
        --- equations ---
        items  = @param:items  [param]
        n2     = (@load (@lvar globals))  [let]
        n1     = (@load (@lvar locals))  [let]
        n0     = @None  [let]
        s      = ($builtins.py_make_int 0)  [store_fast: locals]
        n3     = @param:items  [load_fast: locals]
        n4     = ($builtins.py_get_iter @state0 @param:items)  [let]
        n5     = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items))  [let]
        n6     = ($builtins.py_has_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items))  [let]
        x      = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items))  [store_fast: locals]
        n8     = @theta:s:0  [load_fast: locals]
        n9     = ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items))  [load_fast: locals]
        n10    = ($builtins.py_inplace_add
                     @theta:s:0
                     ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items)))  [let]
        s      = ($builtins.py_inplace_add
                     @theta:s:0
                     ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items)))  [store_fast: locals]
        n7     = @theta:s:0  [load_fast: locals]
        θ_state_0 = (@theta_0 @state0 @theta:state:0)  [theta_close]
        θ_x_0 = (@theta_0
                    @undef
                    ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items)))  [theta_close]
        θ_s_0 = (@theta_0
                    ($builtins.py_make_int 0)
                    ($builtins.py_inplace_add
                        @theta:s:0
                        ($builtins.py_next_iter
                            @theta:state:0
                            ($builtins.py_get_iter @state0 @param:items))))  [theta_close]
        --- root ---
        (@ret @theta:state:0 @theta:s:0)
        |}]
  end )
