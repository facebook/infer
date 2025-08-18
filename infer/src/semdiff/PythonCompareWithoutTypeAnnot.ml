(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let () = Py.initialize ~interpreter:Version.python_exe ()

let strip_annotations_code =
  {|
import ast
def strip_type_annotations(tree):
    class TypeAnnotationRemover(ast.NodeTransformer):
        def visit_FunctionDef(self, node):
            node.returns = None
            node.args.args = [ast.arg(arg=a.arg, annotation=None) for a in node.args.args]
            node.args.kwonlyargs = [ast.arg(arg=a.arg, annotation=None) for a in node.args.kwonlyargs]
            return self.generic_visit(node)
        def visit_AnnAssign(self, node):
            # Convert annotated assignment to normal assignment
            return ast.Assign(targets=[node.target], value=node.value)
    return TypeAnnotationRemover().visit(tree)
  |}


let ast_module = Py.Import.import_module "ast"

let main_module = Py.Import.import_module "__main__"

let _ = Py.Run.simple_string strip_annotations_code

let strip_func = Py.Module.get main_module "strip_type_annotations"

let dump_func = Py.Module.get ast_module "dump" |> Py.Callable.to_function

let parse_and_strip source =
  let parse_func = Py.Module.get ast_module "parse" |> Py.Callable.to_function in
  let tree = parse_func [|Py.String.of_string source|] in
  Py.Callable.to_function strip_func [|tree|]


let ast_to_string ast = dump_func [|ast|] |> Py.String.to_string

let compare src1 src2 =
  let ast1 = parse_and_strip src1 in
  let ast2 = parse_and_strip src2 in
  let s1 = ast_to_string ast1 in
  let s2 = ast_to_string ast2 in
  String.equal s1 s2
