(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let init () =
  let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () in
  let strip_annotations_code =
    {|
import ast
def transform_ast(tree):
    class ImportStatementRemover(ast.NodeTransformer):
        def visit_ImportFrom(self, node):
            return None
        def visit_Import(self, node):
            return None
    class ClassEqualityToIsInstanceTransformer(ast.NodeTransformer):
        def visit_Compare(self, node):
            # Look for: x.__class__ == Type
            if (isinstance(node.left, ast.Attribute) and
                node.left.attr == '__class__' and
                len(node.ops) == 1 and
                isinstance(node.ops[0], ast.Eq) and
                len(node.comparators) == 1):
                    obj = node.left.value
                    type_node = node.comparators[0]
                    new_node = ast.Call(
                      func=ast.Name(id='isinstance', ctx=ast.Load()),
                      args=[obj, type_node],
                      keywords=[]
                    )
                    return ast.copy_location(new_node, node)
            return self.generic_visit(node)
    return ClassEqualityToIsInstanceTransformer().visit(ImportStatementRemover().visit(tree))

def diff_asts(t1, t2):
    diffs = []

    def collect_lines(node):
        """Collect unique line numbers from a subtree rooted at node."""
        lines = set()
        if isinstance(node, ast.AST):
            if hasattr(node, "lineno"):
                lines.add(node.lineno)
            for field in node._fields:
                value = getattr(node, field, None)
                if isinstance(value, list):
                    for item in value:
                        lines.update(collect_lines(item))
                elif isinstance(value, ast.AST):
                    lines.update(collect_lines(value))
        return lines

    def compare(n1, n2):
        if type(n1) != type(n2):
            # Convert annotated assignment to normal assignment
            if isinstance(n1, ast.Assign) and isinstance(n2, ast.AnnAssign):
                converted_assign = ast.Assign(targets=[n2.target], value=n2.value, lineno=n2.lineno, end_lineno=n2.end_lineno)
                return compare(n1, converted_assign)
            for l1 in collect_lines(n1):
                diffs.append((l1, -1))
            for l2 in collect_lines(n2):
                diffs.append((-1, l2))
            return

        if isinstance(n1, ast.AST):
            for field in n1._fields:
                v1 = getattr(n1, field, None)
                v2 = getattr(n2, field, None)

                if (field == "returns" or field == "annotation") and v1 == None and v2 != None:
                    continue
                if isinstance(v1, list) and isinstance(v2, list):
                    for x1, x2 in zip(v1, v2):
                        compare(x1, x2)

                    for x1 in v1[len(v2):]:
                        for l1 in collect_lines(x1):
                            diffs.append((l1, -1))
                    for x2 in v2[len(v1):]:
                        for l2 in collect_lines(x2):
                            diffs.append((-1, l2))

                elif isinstance(v1, ast.AST) and isinstance(v2, ast.AST):
                    compare(v1, v2)

                elif v1 != v2:
                    for l1 in collect_lines(n1):
                        diffs.append((l1, -1))
                    for l2 in collect_lines(n2):
                        diffs.append((-1, l2))
                    return

    for stmt1, stmt2 in zip(t1.body, t2.body):
        compare(stmt1, stmt2)

    for stmt1 in t1.body[len(t2.body):]:
        for l1 in collect_lines(stmt1):
            diffs.append((l1, -1))
    for stmt2 in t2.body[len(t1.body):]:
        for l2 in collect_lines(stmt2):
            diffs.append((-1, l2))

    return diffs
  |}
  in
  let ast_module = Py.Import.import_module "ast" in
  let main_module = Py.Import.import_module "__main__" in
  let _ = Py.Run.simple_string strip_annotations_code in
  let transform_func = Py.Module.get main_module "transform_ast" in
  let diff_func = Py.Module.get main_module "diff_asts" in
  (transform_func, ast_module, diff_func)


let parse_and_transform transform_func ast_module source =
  let parse_func = Py.Module.get ast_module "parse" |> Py.Callable.to_function in
  let tree = parse_func [|Py.String.of_string source|] in
  Py.Callable.to_function transform_func [|tree|]


let write_output previous_file current_file diffs =
  let out_path = ResultsDir.get_path SemDiff in
  let outcome = if List.is_empty diffs then "equal" else "different" in
  let json =
    `Assoc
      [ ("previous", `String previous_file)
      ; ("current", `String current_file)
      ; ("outcome", `String outcome)
      ; ("diff", `List (List.map ~f:(fun diff -> `String diff) diffs)) ]
  in
  Out_channel.with_file out_path ~f:(fun out_channel -> Yojson.Safe.to_channel out_channel json)


let tuple_to_int_pair (t : Py.Object.t) =
  let lst = Py.Tuple.to_list t in
  let get_some = function
    | Some obj ->
        obj
    | None ->
        L.die InternalError "Expected a value in tuple"
  in
  let a = get_some (List.nth lst 0) in
  let b = get_some (List.nth lst 1) in
  (Py.Int.to_int a, Py.Int.to_int b)


let diff_code (pairs : (int * int) list) (code1 : string) (code2 : string) : string list =
  let lines1 = String.split_on_chars ~on:['\n'] code1 in
  let lines2 = String.split_on_chars ~on:['\n'] code2 in
  let get_line lines n : string =
    if Int.equal n (-1) then ""
    else if n - 1 < List.length lines then List.nth_exn lines (n - 1)
    else ""
  in
  let diffs =
    List.fold_left
      ~f:(fun acc (l1, l2) ->
        let line1 = get_line lines1 l1 in
        let line2 = get_line lines2 l2 in
        let acc = if String.length line1 > 0 then acc @ ["- " ^ line1] else acc in
        let acc = if String.length line2 > 0 then acc @ ["+ " ^ line2] else acc in
        acc )
      ~init:[] pairs
  in
  diffs


let ast_diff ?(debug = false) src1 src2 =
  let transform_func, ast_module, diff_func = init () in
  let ast1 = parse_and_transform transform_func ast_module src1 in
  let ast2 = parse_and_transform transform_func ast_module src2 in
  let diffs_pairs = Py.Callable.to_function diff_func [|ast1; ast2|] in
  let diffs_pairs = Py.List.to_list diffs_pairs |> List.map ~f:tuple_to_int_pair in
  let diffs = diff_code diffs_pairs src1 src2 in
  if debug then Printf.printf "SemDiff:\n%s\n" (String.concat ~sep:"\n" diffs) ;
  diffs


let semdiff previous_file current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs = ast_diff ~debug previous_src current_src in
  write_output previous_file current_file diffs
