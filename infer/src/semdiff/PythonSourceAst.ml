(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module StringMap = IString.Map

module Node = struct
  type t =
    | Dict of dict
    | List of t list
    | Str of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null
  [@@deriving compare, equal]

  and dict = t StringMap.t

  let type_field_name = "_type"

  let field_args = "args"

  let field_ctx = "ctx"

  let field_end_lineno = "end_lineno"

  let field_func = "func"

  let field_id = "id"

  let field_keywords = "keywords"

  let field_lineno = "lineno"

  let field_value = "value"

  let get_type fields =
    StringMap.find_opt type_field_name fields
    |> Option.value_or_thunk ~default:(fun () -> L.die InternalError "Could not find ast node type")


  let is_type fields type_name : bool =
    match get_type fields with Str name -> String.equal name type_name | _ -> false


  let is_line_number_field field_name =
    String.equal field_name field_lineno || String.equal field_name field_end_lineno


  let is_type_annotation_field field_name =
    String.equal field_name "annotation" || String.equal field_name "returns"


  let get_line_number (fields : dict) : int option =
    match StringMap.find_opt field_lineno fields with Some (Int l1) -> Some l1 | _ -> None


  let get_line_number_of_node = function Dict f -> get_line_number f | _ -> None

  let make_dict_node field_list = Dict (StringMap.of_list field_list)

  let find_field_or_null field_name fields =
    Option.value (StringMap.find_opt field_name fields) ~default:Null


  exception UnsupportedJsonType of Yojson.Safe.t

  let rec of_yojson (j : Yojson.Safe.t) : t =
    match j with
    | `Assoc fields ->
        make_dict_node (List.map ~f:(fun (k, v) -> (k, of_yojson v)) fields)
    | `List l ->
        List (List.map ~f:of_yojson l)
    | `String s ->
        Str s
    | `Int i ->
        Int i
    | `Float f ->
        Float f
    | `Bool b ->
        Bool b
    | `Null ->
        Null
    | `Intlit str ->
        (* Large integers will just be handled as string. It will restrict our abilty to reason on them, except for exact matching *)
        Str str
    | _ ->
        raise (UnsupportedJsonType j)


  let rec to_str ?(indent = 0) (node : t) : string =
    let indent_str = String.make (indent * 2) ' ' in
    let next_indent = indent + 1 in
    let next_indent_str = String.make (next_indent * 2) ' ' in
    match node with
    | Dict fields ->
        "Dict: {"
        ^ StringMap.fold
            (fun k v acc -> acc ^ "\n" ^ next_indent_str ^ k ^ "=" ^ to_str ~indent:next_indent v)
            fields ""
        ^ "\n" ^ indent_str ^ "}"
    | List l ->
        "List: ["
        ^ String.concat ~sep:" "
            (List.map ~f:(fun node -> "\n" ^ next_indent_str ^ to_str ~indent:next_indent node) l)
        ^ "\n" ^ indent_str ^ "]"
    | Str s ->
        "Str: " ^ s
    | Int i ->
        "Int: " ^ Int.to_string i
    | Float f ->
        "Float: " ^ Float.to_string f
    | Bool b ->
        "Bool: " ^ Bool.to_string b
    | Null ->
        "Null"
end

(* Python integration *)
let python_ast_parser_code =
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
    elif isinstance(node, bytes):
        return "<unserializable bytes>"
    elif isinstance(node, complex):
        return {"_type": "complex", "real": node.real, "imag": node.imag}
    elif node is Ellipsis:  # Handle ellipsis
        return "..."
    else:
        return node  # literals: str, int, None, etc.

def parse_to_json(source: str) -> str:
    tree = ast.parse(source)
    return json.dumps(node_to_dict(tree))
  |}


type error = SyntaxError of {filename: string option; py_error: string}

let pp_error fmt = function
  | SyntaxError {filename= Some filename; py_error} ->
      F.fprintf fmt "Syntax error in %s: %s\n" filename py_error
  | SyntaxError {filename= None; py_error} ->
      F.fprintf fmt "Syntax error: %s\n" py_error


let build_parser () =
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  let main_module = Py.Import.import_module "__main__" in
  ignore (Py.Run.simple_string python_ast_parser_code) ;
  let pyobject = Py.Module.get main_module "parse_to_json" in
  fun ?filename source ->
    let parser = Py.Callable.to_function pyobject in
    try
      let ast = parser [|Py.String.of_string source|] |> Py.String.to_string in
      Ok (Node.of_yojson (Yojson.Safe.from_string ast))
    with Py.E (error_type, error_value) as exn ->
      let str_error_type = Py.Object.to_string error_type in
      if String.equal str_error_type "<class 'SyntaxError'>" then
        let py_error = Py.Object.to_string error_value in
        Error (SyntaxError {filename; py_error})
      else raise exn


let iter_files ~f filenames =
  let parse = build_parser () in
  let errors =
    List.fold filenames ~init:[] ~f:(fun errors filename ->
        let source = In_channel.with_file filename ~f:In_channel.input_all in
        match parse ~filename source with
        | Ok node ->
            f node ;
            errors
        | Error error ->
            error :: errors
        | exception Node.UnsupportedJsonType j ->
            L.internal_error "[semdiff] unsupported JSON type in file %s: %a\n" filename
              Yojson.Safe.pp j ;
            errors
        | exception Py.E (error_type, error_value) ->
            L.internal_error "[semdiff] error while parsing file %s:\n  type:%s\n  value: %s\n"
              filename (Py.Object.to_string error_type) (Py.Object.to_string error_value) ;
            errors )
  in
  if List.is_empty errors then Ok () else Error errors


let iter_from_index ~f ~index_filename =
  match Utils.read_file index_filename with
  | Ok lines ->
      iter_files ~f lines
  | Error error ->
      L.die UserError "Error reading the semdiff input files index '%s': %s@." index_filename error
