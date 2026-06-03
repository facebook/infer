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
module Rewrite = CongruenceClosureRewrite

let python_to_module source : T.Module.t =
  let modules = ref [] in
  PyIR.test source ~run:(fun pyir -> modules := PyIR2Textual.mk_module pyir :: !modules) ;
  List.hd_exn !modules


let find_proc (module_ : T.Module.t) name =
  List.find_map_exn module_.decls ~f:(function
    | T.Module.Proc p
      when String.is_suffix
             (F.asprintf "%a" T.QualifiedProcName.pp p.procdecl.qualified_name)
             ~suffix:name ->
        Some p
    | _ ->
        None )


let dump_defaults source ~proc_name =
  let module_ = python_to_module source in
  let proc = find_proc module_ proc_name in
  let defaults = StructuredPeg.extract_defaults module_ proc in
  if IString.Map.is_empty defaults then F.printf "(no defaults)@."
  else IString.Map.iter (fun name exp -> F.printf "%s = %a@." name T.Exp.pp exp) defaults


(* Convert a proc to a PEG with its defaults modelled, and print the root. *)
let peg_with_defaults source ~proc_name =
  let module_ = python_to_module source in
  let proc = find_proc module_ proc_name in
  let defaults = StructuredPeg.extract_defaults module_ proc in
  let cc = CongruenceClosureSolver.init ~debug:false in
  match StructuredPeg.convert_proc ~defaults cc proc with
  | Ok (root, _eqs, _) ->
      F.printf "%a@." (CongruenceClosureSolver.pp_nested_term cc) root
  | Error msg ->
      F.printf "ERROR: %s@." msg


(* ---------- Per-rule tests (rewrite-engine level) ---------- *)

(* The structural B006 rules cannot be isolated with Python source (a single B006 example fires all
   of them together), so each rule is exercised directly: build the left-hand term, apply just that
   rule, and check it became equivalent to the expected right-hand term. The rule text is shared
   with production via TextualPegDiff.b006_named_rules. *)

let mk cc name args = CC.mk_term cc (CC.mk_header cc name) args

let cst cc name = mk cc name []

let check_rule label ~lhs ~rhs =
  let cc = CC.init ~debug:false in
  let rule =
    match
      Rewrite.parse_rule cc
        (List.Assoc.find_exn TextualPegDiff.b006_named_rules label ~equal:String.equal)
    with
    | Ok r ->
        r
    | Error _ ->
        assert false
  in
  let l = lhs cc and r = rhs cc in
  let applied = Rewrite.TestOnly.apply_rule_at cc rule l in
  F.printf "%s: applied=%d equiv=%b@." label applied (CC.is_equiv cc l r)


let%test_module "B006 rewrite rules" =
  ( module struct
    let%expect_test "bool-is: py_bool(x is y) = x is y" =
      check_rule "bool-is"
        ~lhs:(fun cc ->
          mk cc "$builtins.py_bool" [mk cc "$builtins.py_compare_is" [cst cc "x"; cst cc "y"]] )
        ~rhs:(fun cc -> mk cc "$builtins.py_compare_is" [cst cc "x"; cst cc "y"]) ;
      [%expect {| bool-is: applied=1 equiv=true |}]


    let%expect_test "is-distribute: (phi c a b) is None = phi c (a is None) (b is None)" =
      check_rule "is-distribute"
        ~lhs:(fun cc ->
          mk cc "$builtins.py_compare_is"
            [mk cc "@phi" [cst cc "c"; cst cc "a"; cst cc "b"]; cst cc "@None"] )
        ~rhs:(fun cc ->
          mk cc "@phi"
            [ cst cc "c"
            ; mk cc "$builtins.py_compare_is" [cst cc "a"; cst cc "@None"]
            ; mk cc "$builtins.py_compare_is" [cst cc "b"; cst cc "@None"] ] ) ;
      [%expect {| is-distribute: applied=1 equiv=true |}]


    let%expect_test "is-none-none: None is None = true" =
      check_rule "is-none-none"
        ~lhs:(fun cc -> mk cc "$builtins.py_compare_is" [cst cc "@None"; cst cc "@None"])
        ~rhs:(fun cc -> cst cc "@true") ;
      [%expect {| is-none-none: applied=1 equiv=true |}]


    let%expect_test "arg-not-none: (@arg b) is None = false [semi-correct]" =
      check_rule "arg-not-none"
        ~lhs:(fun cc -> mk cc "$builtins.py_compare_is" [mk cc "@arg" [cst cc "b"]; cst cc "@None"])
        ~rhs:(fun cc -> cst cc "@false") ;
      [%expect {| arg-not-none: applied=1 equiv=true |}]


    let%expect_test "bool-id: phi(c, true, false) = c" =
      check_rule "bool-id"
        ~lhs:(fun cc -> mk cc "@phi" [cst cc "c"; cst cc "@true"; cst cc "@false"])
        ~rhs:(fun cc -> cst cc "c") ;
      [%expect {| bool-id: applied=1 equiv=true |}]


    let%expect_test "phi-same-cond: phi(c, a, phi(c, x, b)) = phi(c, a, b)" =
      check_rule "phi-same-cond"
        ~lhs:(fun cc ->
          mk cc "@phi" [cst cc "c"; cst cc "a"; mk cc "@phi" [cst cc "c"; cst cc "x"; cst cc "b"]] )
        ~rhs:(fun cc -> mk cc "@phi" [cst cc "c"; cst cc "a"; cst cc "b"]) ;
      [%expect {| phi-same-cond: applied=1 equiv=true |}]
  end )


let%test_module "B006 parameter modelling" =
  ( module struct
    let%expect_test "list default -> phi(is_default, build_list, arg)" =
      peg_with_defaults {|
def f(b=[]):
    return b
|} ~proc_name:"f" ;
      [%expect {| (@ret @state0 (@phi (@is_default b) $builtins.py_build_list (@arg b))) |}]


    let%expect_test "None default -> phi(is_default, None, arg)" =
      peg_with_defaults {|
def f(b=None):
    return b
|} ~proc_name:"f" ;
      [%expect {| (@ret @state0 (@phi (@is_default b) @None (@arg b))) |}]


    let%expect_test "None default with codemod guard" =
      peg_with_defaults
        {|
def f(b=None):
    if b is None:
        b = []
    return b
|}
        ~proc_name:"f" ;
      [%expect
        {|
        (@ret
            @state0
            (@phi
                ($builtins.py_bool ($builtins.py_compare_is (@phi (@is_default b) @None (@arg b)) @None))
                $builtins.py_build_list
                (@phi (@is_default b) @None (@arg b))))
        |}]
  end )


(* ---------- End-to-end migration checks (Python source) ---------- *)

let check_b006 ?(debug = false) source_old source_new ~proc_name =
  let mold = python_to_module source_old in
  let mnew = python_to_module source_new in
  let pold = find_proc mold proc_name in
  let pnew = find_proc mnew proc_name in
  let defaults_old = StructuredPeg.extract_defaults mold pold in
  let defaults_new = StructuredPeg.extract_defaults mnew pnew in
  let accepted = TextualPegDiff.check_b006_migration ~debug ~defaults_old ~defaults_new pold pnew in
  F.printf "accepted: %b@." accepted


let%test_module "B006 migration" =
  ( module struct
    let%expect_test "list default: trivial body" =
      check_b006
        {|
def f(b=[]):
    return b
|}
        {|
def f(b=None):
    if b is None:
        b = []
    return b
|}
        ~proc_name:"f" ;
      [%expect {| accepted: true |}]


    let%expect_test "list default: body uses b" =
      check_b006
        {|
def f(b=[]):
    b.append(1)
    return b
|}
        {|
def f(b=None):
    if b is None:
        b = []
    b.append(1)
    return b
|}
        ~proc_name:"f" ;
      [%expect {| accepted: true |}]


    let%expect_test "dict default" =
      check_b006
        {|
def f(b={}):
    return b
|}
        {|
def f(b=None):
    if b is None:
        b = {}
    return b
|}
        ~proc_name:"f" ;
      [%expect {| accepted: true |}]


    (* The codemod must preserve the default literal: [] replaced by {} is rejected. *)
    let%expect_test "wrong default literal is rejected" =
      check_b006
        {|
def f(b=[]):
    return b
|}
        {|
def f(b=None):
    if b is None:
        b = {}
    return b
|}
        ~proc_name:"f" ;
      [%expect {| accepted: false |}]


    (* A different body is rejected. *)
    let%expect_test "different body is rejected" =
      check_b006
        {|
def f(b=[]):
    return b
|}
        {|
def f(b=None):
    if b is None:
        b = []
    return 0
|}
        ~proc_name:"f" ;
      [%expect {| accepted: false |}]


    (* ---- Semi-correctness of the arg-not-none rule ----
       These pairs are ACCEPTED, yet they are NOT observationally equivalent: a caller passing
       None explicitly observes a difference. We accept them anyway under the correct-execution
       hypothesis (passing None for a mutable-default parameter is considered misuse). *)

    (* f(None): OLD returns None, NEW returns []. Accepted because (@arg b) is None -> false. *)
    let%expect_test "SEMI-CORRECT: f(None) divergence is accepted" =
      check_b006
        {|
def f(b=[]):
    return b
|}
        {|
def f(b=None):
    if b is None:
        b = []
    return b
|}
        ~proc_name:"f" ;
      [%expect {| accepted: true |}]


    (* OLD `return b is None`: f(None) -> True, f([1]) -> False. NEW always returns False.
       The check accepts (both normalise to False under arg-not-none), masking the f(None) case. *)
    let%expect_test "SEMI-CORRECT: observable None test is accepted" =
      check_b006
        {|
def f(b=[]):
    return b is None
|}
        {|
def f(b=None):
    if b is None:
        b = []
    return b is None
|}
        ~proc_name:"f" ;
      [%expect {| accepted: true |}]
  end )


let%test_module "B006 default extraction" =
  ( module struct
    let%expect_test "mutable list default" =
      dump_defaults {|
def f(b=[]):
    return b
|} ~proc_name:"f" ;
      [%expect {| b = $builtins.py_build_list() |}]


    let%expect_test "None default" =
      dump_defaults {|
def f(b=None):
    return b
|} ~proc_name:"f" ;
      [%expect {| b = $builtins.py_make_none() |}]


    let%expect_test "no default" =
      dump_defaults {|
def f(b):
    return b
|} ~proc_name:"f" ;
      [%expect {| (no defaults) |}]


    let%expect_test "trailing defaults map to last params" =
      dump_defaults {|
def f(a, b, c=1, d=[]):
    return a
|} ~proc_name:"f" ;
      [%expect
        {|
        c = $builtins.py_make_int(1)
        d = $builtins.py_build_list()
        |}]
  end )
