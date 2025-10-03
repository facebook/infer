(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type ast_node =
  | Dict of (string * ast_node) list
  | List of ast_node list
  | Str of string
  | Num of int
  | Bool of bool
  | Null
[@@deriving compare, equal]

let init () =
  let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () in
  let strip_annotations_code =
    {|
import ast, json

def node_to_dict(node):
    if isinstance(node, ast.AST):
        result = {"_type": node.__class__.__name__}

        for attr in ("lineno", "end_lineno"):
            if hasattr(node, attr):
                result[attr] = getattr(node, attr)

        for field, value in ast.iter_fields(node):
            result[field] = node_to_dict(value)
        return result
    elif isinstance(node, list):
        return [node_to_dict(x) for x in node]
    else:
        return node  # literals: str, int, None, etc.

def parse_to_json(tree: ast.AST) -> str:
    return json.dumps(node_to_dict(tree))

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
                      func=ast.Name(id='isinstance', ctx=ast.Load(), lineno=node.lineno, end_lineno=node.end_lineno),
                      args=[obj, type_node],
                      keywords=[]
                    )
                    return ast.copy_location(new_node, node)
            return self.generic_visit(node)
    return parse_to_json(ClassEqualityToIsInstanceTransformer().visit(ImportStatementRemover().visit(tree)))

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
  (transform_func, ast_module)


let rec of_yojson (j : Yojson.Safe.t) : ast_node =
  match j with
  | `Assoc fields ->
      Dict (List.map ~f:(fun (k, v) -> (k, of_yojson v)) fields)
  | `List l ->
      List (List.map ~f:of_yojson l)
  | `String s ->
      Str s
  | `Int i ->
      Num i
  | `Bool b ->
      Bool b
  | `Null ->
      Null
  | _ ->
      L.die InternalError "unsupported JSON type"


let parse_and_transform transform_func ast_module source =
  let parse_func = Py.Module.get ast_module "parse" |> Py.Callable.to_function in
  let tree = parse_func [|Py.String.of_string source|] in
  let ast = Py.Callable.to_function transform_func [|tree|] |> Py.String.to_string in
  of_yojson (Yojson.Safe.from_string ast)


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


let get_line_number (fields : (string * ast_node) list) : int =
  match List.Assoc.find ~equal:String.equal fields "lineno" with Some (Num l1) -> l1 | _ -> -1


let ann_assign_to_assign fields : ast_node =
  (* remove annotation, simple, type_comment, change type to assign, target becomes targets = [target] *)
  let assign_fields =
    List.fold
      ~f:(fun acc (k, field_node) ->
        match k with
        | "annotation" | "simple" ->
            acc
        | "_type" ->
            ("_type", Str "Assign") :: acc
        | "target" ->
            ("targets", List [field_node]) :: acc
        | _ ->
            (k, field_node) :: acc )
      ~init:[] fields
  in
  Dict (List.rev (("type_comment", Null) :: assign_fields))


let is_type fields type_name : bool =
  match List.Assoc.find_exn ~equal:String.equal fields "_type" with
  | Str name ->
      String.equal name type_name
  | _ ->
      false


let rec get_diff ?(left_line : int = -1) ?(right_line : int = -1) (n1 : ast_node) (n2 : ast_node) :
    (int * int) list =
  match (n1, n2) with
  | a, b when equal_ast_node a b ->
      []
  | Dict f1, Dict f2 ->
      let left_line = get_line_number f1 in
      let right_line = get_line_number f2 in
      if is_type f1 "Assign" && is_type f2 "AnnAssign" then
        get_diff ~left_line ~right_line n1 (ann_assign_to_assign f2)
      else
        let diffs =
          List.fold_left
            ~f:(fun acc (k, v1) ->
              match List.Assoc.find ~equal:String.equal f2 k with
              | Some v2 when String.equal k "annotation" || String.equal k "returns" -> (
                (* special case for type annotations *)
                match (v1, v2) with
                | Null, Dict _ ->
                    acc
                | _ ->
                    get_diff ~left_line ~right_line v1 v2 @ acc )
              | Some _ when String.equal k "lineno" || String.equal k "end_lineno" ->
                  acc
              | Some v2 ->
                  get_diff ~left_line ~right_line v1 v2 @ acc
              | None ->
                  (left_line, -1) :: acc )
            ~init:[] f1
        in
        let missing_in_left =
          List.filter_map
            ~f:(fun (k, _) ->
              if List.Assoc.mem ~equal:String.equal f1 k then None else Some (-1, right_line) )
            f2
        in
        diffs @ missing_in_left
  | List l1, List l2 ->
      let rec aux acc xs ys =
        match (xs, ys) with
        | x :: xt, y :: yt ->
            aux (get_diff ~left_line ~right_line x y @ acc) xt yt
        | x :: xt, [] ->
            aux ((get_line_number_of_node x, -1) :: acc) xt []
        | [], y :: yt ->
            aux ((-1, get_line_number_of_node y) :: acc) [] yt
        | [], [] ->
            acc
      and get_line_number_of_node = function Dict f -> get_line_number f | _ -> -1 in
      aux [] l1 l2
  | Dict f1, _ ->
      [(get_line_number f1, -1)]
  | _, Dict f2 ->
      [(-1, get_line_number f2)]
  | _ ->
      [(left_line, -1); (-1, right_line)]


let show_diff file1 file2 diffs =
  let lines1 = String.split_on_chars ~on:['\n'] file1
  and lines2 = String.split_on_chars ~on:['\n'] file2 in
  let get_line_content lines n : string =
    if Int.equal n (-1) then ""
    else if n - 1 < List.length lines then List.nth_exn lines (n - 1)
    else ""
  in
  let diffs =
    let diffs_sorted =
      List.sort
        ~compare:(fun (a1, b1) (a2, b2) ->
          match (b1, b2) with
          | -1, -1 ->
              Int.compare a1 a2
          | -1, _ ->
              -1
          | _, -1 ->
              1
          | _, _ -> (
            match Int.compare a1 a2 with 0 -> Int.compare b1 b2 | c -> c ) )
        diffs
    in
    List.fold_left
      ~f:(fun acc (left_line, right_line) ->
        let line1 = get_line_content lines1 left_line in
        let line2 = get_line_content lines2 right_line in
        let acc = if String.length line1 > 0 then acc @ ["- " ^ line1] else acc in
        let acc = if String.length line2 > 0 then acc @ ["+ " ^ line2] else acc in
        acc )
      ~init:[] diffs_sorted
  in
  List.remove_consecutive_duplicates ~equal:String.equal diffs


let ast_diff ?(debug = false) src1 src2 =
  let transform_func, ast_module = init () in
  let ast1 = parse_and_transform transform_func ast_module src1 in
  let ast2 = parse_and_transform transform_func ast_module src2 in
  let diffs = get_diff ast1 ast2 in
  let diffs = show_diff src1 src2 diffs in
  if debug then Printf.printf "SemDiff:\n%s\n" (String.concat ~sep:"\n" diffs) ;
  diffs


let semdiff previous_file current_file =
  let debug = Config.debug_mode in
  let previous_src = In_channel.with_file previous_file ~f:In_channel.input_all in
  let current_src = In_channel.with_file current_file ~f:In_channel.input_all in
  let diffs = ast_diff ~debug previous_src current_src in
  write_output previous_file current_file diffs
