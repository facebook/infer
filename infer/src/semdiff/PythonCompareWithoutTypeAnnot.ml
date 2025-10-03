(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module StringMap = Stdlib.Map.Make (String)

type ast_node =
  | Dict of ast_node StringMap.t
  | List of ast_node list
  | Str of string
  | Num of int
  | Bool of bool
  | Null
[@@deriving compare, equal]

let init () =
  let () = if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () in
  let ast_to_json_code =
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

def parse_to_json(source: str) -> str:
    tree = ast.parse(source)
    return json.dumps(node_to_dict(tree))
  |}
  in
  let main_module = Py.Import.import_module "__main__" in
  let _ = Py.Run.simple_string ast_to_json_code in
  let parse_func = Py.Module.get main_module "parse_to_json" in
  parse_func


let rec of_yojson (j : Yojson.Safe.t) : ast_node =
  match j with
  | `Assoc fields ->
      Dict (StringMap.of_list (List.map ~f:(fun (k, v) -> (k, of_yojson v)) fields))
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


let get_type fields =
  StringMap.find_opt "_type" fields
  |> Option.value_or_thunk ~default:(fun () -> L.die InternalError "Could not find ast node type")


let is_type fields type_name : bool =
  match get_type fields with Str name -> String.equal name type_name | _ -> false


let rec normalize (node : ast_node) : ast_node =
  match node with
  | Dict fields when is_type fields "Import" || is_type fields "ImportFrom" ->
      Null
  | Dict fields when is_type fields "Compare" -> (
      let left_node = StringMap.find_opt "left" fields in
      let left_fields =
        match left_node with Some (Dict fields) -> fields | _ -> StringMap.empty
      in
      match StringMap.find_opt "attr" left_fields with
      | Some (Str "__class__") ->
          let fst_arg =
            match StringMap.find_opt "value" left_fields with
            | Some arg ->
                arg
            | _ ->
                L.die InternalError "Could not find object where __class__ attribute is accessed"
          in
          let snd_arg =
            match StringMap.find_opt "comparators" fields with
            | Some (List [x]) ->
                x
            | _ ->
                L.die InternalError
                  "Could not find rhs type in comparison using __class__ attribute"
          in
          let ctx_node = Option.value (StringMap.find_opt "ctx" left_fields) ~default:Null in
          let lineno = Option.value (StringMap.find_opt "lineno" fields) ~default:Null in
          let end_lineno = Option.value (StringMap.find_opt "end_lineno" fields) ~default:Null in
          let func_node =
            Dict
              (StringMap.of_list
                 [ ("_type", Str "Name")
                 ; ("id", Str "isinstance")
                 ; ("ctx", ctx_node)
                 ; ("lineno", lineno)
                 ; ("end_lineno", end_lineno) ] )
          in
          Dict
            (StringMap.of_list
               [ ("_type", Str "Call")
               ; ("lineno", lineno)
               ; ("end_lineno", end_lineno)
               ; ("func", func_node)
               ; ("args", List [fst_arg; snd_arg])
               ; ("keywords", List []) ] )
      | _ ->
          Dict (StringMap.map (fun v -> normalize v) fields) )
  | Dict fields ->
      Dict (StringMap.map (fun v -> normalize v) fields)
  | List l ->
      List
        (List.filter
           ~f:(fun n -> match n with Null -> false | _ -> true)
           (List.map ~f:normalize l) )
  | other ->
      other


let parse_and_transform parse_func source =
  let parse_func = parse_func |> Py.Callable.to_function in
  let ast = parse_func [|Py.String.of_string source|] |> Py.String.to_string in
  of_yojson (Yojson.Safe.from_string ast) |> normalize


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


let get_line_number (fields : ast_node StringMap.t) : int option =
  match StringMap.find_opt "lineno" fields with Some (Num l1) -> Some l1 | _ -> None


let ann_assign_to_assign fields : ast_node =
  (* remove annotation, simple, type_comment, change type to assign, target becomes targets = [target] *)
  let assign_fields =
    StringMap.fold
      (fun k field_node acc ->
        match k with
        | "annotation" | "simple" ->
            acc
        | "_type" ->
            StringMap.add "_type" (Str "Assign") acc
        | "target" ->
            StringMap.add "targets" (List [field_node]) acc
        | _ ->
            StringMap.add k field_node acc )
      fields StringMap.empty
  in
  Dict (StringMap.add "type_comment" Null assign_fields)


let rec get_diff ?(left_line : int option = None) ?(right_line : int option = None) (n1 : ast_node)
    (n2 : ast_node) : (int option * int option) list =
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
          StringMap.fold
            (fun k v1 acc ->
              match StringMap.find_opt k f2 with
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
                  (left_line, None) :: acc )
            f1 []
        in
        let missing_in_left =
          StringMap.fold
            (fun k _ acc -> if StringMap.mem k f1 then acc else (None, right_line) :: acc)
            f2 []
        in
        diffs @ missing_in_left
  | List l1, List l2 ->
      let rec aux acc xs ys =
        match (xs, ys) with
        | x :: xt, y :: yt ->
            aux (get_diff ~left_line ~right_line x y @ acc) xt yt
        | x :: xt, [] ->
            aux ((get_line_number_of_node x, None) :: acc) xt []
        | [], y :: yt ->
            aux ((None, get_line_number_of_node y) :: acc) [] yt
        | [], [] ->
            acc
      and get_line_number_of_node = function Dict f -> get_line_number f | _ -> None in
      aux [] l1 l2
  | Dict f1, _ ->
      [(get_line_number f1, None)]
  | _, Dict f2 ->
      [(None, get_line_number f2)]
  | _ ->
      [(left_line, None); (None, right_line)]


let show_diff file1 file2 diffs =
  let lines1 = String.split_on_chars ~on:['\n'] file1
  and lines2 = String.split_on_chars ~on:['\n'] file2 in
  let get_line_content lines n : string =
    match n with Some n when n - 1 < List.length lines -> List.nth_exn lines (n - 1) | _ -> ""
  in
  let diffs =
    let diffs_sorted =
      List.sort
        ~compare:(fun (a1, b1) (a2, b2) ->
          match (b1, b2) with
          | None, None ->
              Option.compare Int.compare a1 a2
          | None, _ ->
              -1
          | _, None ->
              1
          | Some v1, Some v2 -> (
            match Option.compare Int.compare a1 a2 with 0 -> Int.compare v1 v2 | c -> c ) )
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
  let parse_func = init () in
  let ast1 = parse_and_transform parse_func src1 in
  let ast2 = parse_and_transform parse_func src2 in
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
