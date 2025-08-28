(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let init () =
  let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () in
  let strip_annotations_code =
    {|
import ast
def strip_type_annotations(tree):
    class TypeAnnotationRemover(ast.NodeTransformer):
        def _remove_arg_annotations(self, args):
            return [ast.arg(arg=a.arg, annotation=None) for a in args]
        def visit_FunctionDef(self, node):
            node.returns = None
            node.args.args = self._remove_arg_annotations(node.args.args)
            node.args.kwonlyargs = self._remove_arg_annotations(node.args.kwonlyargs)
            return self.generic_visit(node)
        def visit_AsyncFunctionDef(self, node):
            node.returns = None
            node.args.args = self._remove_arg_annotations(node.args.args)
            node.args.kwonlyargs = self._remove_arg_annotations(node.args.kwonlyargs)
        def visit_AnnAssign(self, node):
            # Convert annotated assignment to normal assignment
            return ast.Assign(targets=[node.target], value=node.value)
        def visit_ImportFrom(self, node):
            return None
        def visit_Import(self, node):
            return None
    return TypeAnnotationRemover().visit(tree)
  |}
  in
  let ast_module = Py.Import.import_module "ast" in
  let main_module = Py.Import.import_module "__main__" in
  let _ = Py.Run.simple_string strip_annotations_code in
  let strip_func = Py.Module.get main_module "strip_type_annotations" in
  let dump_func = Py.Module.get ast_module "dump" |> Py.Callable.to_function_with_keywords in
  (strip_func, ast_module, dump_func)


let parse_and_strip strip_func ast_module source =
  let parse_func = Py.Module.get ast_module "parse" |> Py.Callable.to_function in
  let tree = parse_func [|Py.String.of_string source|] in
  Py.Callable.to_function strip_func [|tree|]


let ast_to_string dump_func ast =
  dump_func [|ast|] [("indent", Py.Int.of_int 4)] |> Py.String.to_string


let rec diff_lines l1 l2 =
  match (l1, l2) with
  | [], [] ->
      []
  | x :: xs, [] ->
      ("- " ^ x) :: diff_lines xs []
  | [], y :: ys ->
      ("+ " ^ y) :: diff_lines [] ys
  | x :: xs, y :: ys ->
      if String.equal x y then diff_lines xs ys else ("- " ^ x) :: ("+ " ^ y) :: diff_lines xs ys


let compare ?(debug = false) src1 src2 =
  let strip_func, ast_module, dump_func = init () in
  let ast1 = parse_and_strip strip_func ast_module src1 in
  let ast2 = parse_and_strip strip_func ast_module src2 in
  let s1 = ast_to_string dump_func ast1 in
  let s2 = ast_to_string dump_func ast2 in
  let lines1 = String.split_on_chars ~on:['\n'] s1 in
  let lines2 = String.split_on_chars ~on:['\n'] s2 in
  let asts_equal = String.equal s1 s2 in
  if debug then (
    let diffs = if asts_equal then [] else diff_lines lines1 lines2 in
    Printf.printf "SemDiff:\n" ;
    List.iter ~f:print_endline diffs ) ;
  asts_equal


let semdiff ?debug previous_file current_file =
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let asts_equal = compare ?debug previous_src current_src in
  if asts_equal then print_endline "ASTs are equal (ignoring type annotations)."
  else print_endline "ASTs differ."
