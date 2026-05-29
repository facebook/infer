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


let compare_pipelines source ~proc_name =
  let proc = python_to_proc source ~proc_name in
  (* TextualPeg pipeline *)
  let cc1 = CC.init ~debug:false in
  let textual_result = TextualPeg.convert_proc cc1 proc in
  (* StructuredPeg pipeline *)
  let cc2 = CC.init ~debug:false in
  let structured_result = StructuredPeg.convert_proc cc2 proc in
  match (textual_result, structured_result) with
  | Ok (root1, eq1, _loops1), Ok (root2, eq2, _loops2) ->
      F.printf "--- TextualPeg ---@." ;
      TextualPeg.Equations.pp_thetas cc1 F.std_formatter eq1 ;
      F.printf "root: %a@." (CC.pp_nested_term cc1) root1 ;
      F.printf "--- StructuredPeg ---@." ;
      StructuredPeg.Equations.pp_thetas cc2 F.std_formatter eq2 ;
      F.printf "root: %a@." (CC.pp_nested_term cc2) root2
  | Error msg, _ ->
      F.printf "TextualPeg error: %s@." msg
  | _, Error msg ->
      F.printf "StructuredPeg error: %s@." msg


let%test_module "compare TextualPeg vs StructuredPeg" =
  ( module struct
    (* identical *)
    let%expect_test "straight-line" =
      compare_pipelines {|
def foo():
    return 42
|} ~proc_name:"foo" ;
      [%expect
        {|
        --- TextualPeg ---
        root: (@ret @state0 ($builtins.py_make_int 42))
        --- StructuredPeg ---
        root: (@ret @state0 ($builtins.py_make_int 42))
        |}]


    (* identical *)
    let%expect_test "if-then-else" =
      compare_pipelines
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
        --- TextualPeg ---
        root: (@phi
                  ($builtins.py_bool @param:c)
                  (@ret @state0 ($builtins.py_make_int 1))
                  (@ret @state0 ($builtins.py_make_int 2)))
        --- StructuredPeg ---
        root: (@phi
                  ($builtins.py_bool @param:c)
                  (@ret @state0 ($builtins.py_make_int 1))
                  (@ret @state0 ($builtins.py_make_int 2)))
        |}]


    (* different root structure: TextualPeg uses post-increment i+1, StructuredPeg uses theta placeholder *)
    let%expect_test "while loop" =
      compare_pipelines
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
        --- TextualPeg ---
        θ_state_0 = (@theta_0 @state0 @theta:state:0)
        θ_i_0 = (@theta_0
                     ($builtins.py_make_int 0)
                     ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1)))
        root: (@phi
                  ($builtins.py_bool ($builtins.py_compare_lt ($builtins.py_make_int 0) @param:n))
                  (@ret @theta:state:0 ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1)))
                  (@ret @state0 ($builtins.py_make_int 0)))
        --- StructuredPeg ---
        θ_state_0 = (@theta_0 @state0 @theta:state:0)
        θ_i_0 = (@theta_0
                     ($builtins.py_make_int 0)
                     ($builtins.py_inplace_add @theta:i:0 ($builtins.py_make_int 1)))
        root: (@ret
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


    (* identical roots and thetas: the back-edge env defines the theta step, so no spurious
       @phi from merging the continue branch with the loop-exit branch *)
    let%expect_test "for loop" =
      compare_pipelines
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
        --- TextualPeg ---
        θ_state_0 = (@theta_0 @state0 @theta:state:0)
        θ_x_0 = (@theta_0
                     @undef
                     ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items)))
        θ_s_0 = (@theta_0
                     ($builtins.py_make_int 0)
                     ($builtins.py_inplace_add
                         @theta:s:0
                         ($builtins.py_next_iter
                             @theta:state:0
                             ($builtins.py_get_iter @state0 @param:items))))
        root: (@ret @theta:state:0 @theta:s:0)
        --- StructuredPeg ---
        θ_state_0 = (@theta_0 @state0 @theta:state:0)
        θ_x_0 = (@theta_0
                     @undef
                     ($builtins.py_next_iter @theta:state:0 ($builtins.py_get_iter @state0 @param:items)))
        θ_s_0 = (@theta_0
                     ($builtins.py_make_int 0)
                     ($builtins.py_inplace_add
                         @theta:s:0
                         ($builtins.py_next_iter
                             @theta:state:0
                             ($builtins.py_get_iter @state0 @param:items))))
        root: (@ret @theta:state:0 @theta:s:0)
        |}]


    (* StructuredPeg is simpler: no redundant @phi on 'a' since return y does not depend on x *)
    let%expect_test "sequential ifs" =
      compare_pipelines
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
        ~proc_name:"foo" ;
      [%expect
        {|
        --- TextualPeg ---
        root: (@phi
                  ($builtins.py_bool @param:a)
                  (@phi
                      ($builtins.py_bool @param:b)
                      (@ret @state0 ($builtins.py_make_int 3))
                      (@ret @state0 ($builtins.py_make_int 4)))
                  (@phi
                      ($builtins.py_bool @param:b)
                      (@ret @state0 ($builtins.py_make_int 3))
                      (@ret @state0 ($builtins.py_make_int 4))))
        --- StructuredPeg ---
        root: (@phi
                  ($builtins.py_bool @param:b)
                  (@ret @state0 ($builtins.py_make_int 3))
                  (@ret @state0 ($builtins.py_make_int 4)))
        |}]
  end )
