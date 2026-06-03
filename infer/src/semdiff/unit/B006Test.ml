(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module T = Textual

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
