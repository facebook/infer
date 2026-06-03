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
