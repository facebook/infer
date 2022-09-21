(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module YSU = Yojson.Safe.Util
module Ast = ErlangAst
open IOption.Let_syntax

type json = Yojson.Safe.t

type 'a parser = json -> 'a option

let unknown name json =
  L.debug Capture Verbose "ErlangAst.Parse: unknown %s: %s@." name (Yojson.Safe.show json) ;
  None


(* Takes a json of the form `List [j1;...;jn] and returns [f j1;...;f jn]. If skip_errors, then
   elements that fail to parse are filtered out; otherwise, a parsing failure propagates up. *)
let to_list ?(skip_errors = false) ~(f : 'a parser) : 'a list parser =
 fun json ->
  try
    let xs = List.map ~f (YSU.to_list json) in
    if skip_errors then Some (List.filter_opt xs) else Option.all xs
  with YSU.Type_error (error, json) -> unknown error json


let default_or (to_a : 'a parser) : 'a option parser = function
  | `String "default" ->
      Some None
  | a ->
      let* a = to_a a in
      Some (Some a)


let to_loc json : Ast.location option =
  match json with
  | `Int line ->
      Some {Ast.line; col= -1}
  | `List [`List (`String "generated" :: _); `List [`String "location"; `Int line]] ->
      Some {Ast.line; col= -1}
  | `List [`List (`String "generated" :: _); `List [`String "location"; `List [`Int line; `Int col]]]
    ->
      Some {Ast.line; col}
  | `List [`Int line; `Int col] ->
      Some {Ast.line; col}
  | _ ->
      unknown "line" json


let rec kill_singleton_list json =
  match json with
  | `List (`String _ :: _) ->
      json (* an expression *)
  | `List [x] ->
      kill_singleton_list x (* singleton list dropped *)
  | x ->
      x


let one_list json =
  match kill_singleton_list json with
  | `List (`String _ :: _) as json ->
      `List [json]
  | `List _ as json ->
      json (* already (nonsingleton) list *)
  | json ->
      `List [json]


let to_intlit json =
  match json with
  | `Int x ->
      Some (Printf.sprintf "%d" x)
  | `Intlit s ->
      Some s
  | _ ->
      unknown "intlit" json


let to_binary_operator json : Ast.binary_operator option =
  match json with
  | `String "!" ->
      Some Send
  | `String "*" ->
      Some Mul
  | `String "+" ->
      Some Add
  | `String "++" ->
      Some ListAdd
  | `String "-" ->
      Some Sub
  | `String "--" ->
      Some ListSub
  | `String "/" ->
      Some FDiv
  | `String "/=" ->
      Some NotEqual
  | `String "<" ->
      Some Less
  | `String "=/=" ->
      Some ExactlyNotEqual
  | `String "=:=" ->
      Some ExactlyEqual
  | `String "=<" ->
      Some AtMost
  | `String "==" ->
      Some Equal
  | `String ">" ->
      Some Greater
  | `String ">=" ->
      Some AtLeast
  | `String "and" ->
      Some And
  | `String "andalso" ->
      Some AndAlso
  | `String "band" ->
      Some BAnd
  | `String "bor" ->
      Some BOr
  | `String "bsl" ->
      Some Bsl
  | `String "bsr" ->
      Some Bsr
  | `String "bxor" ->
      Some BXor
  | `String "div" ->
      Some IDiv
  | `String "or" ->
      Some Or
  | `String "orelse" ->
      Some OrElse
  | `String "rem" ->
      Some Rem
  | `String "xor" ->
      Some Xor
  | _ ->
      unknown "binary_operator" json


let to_unary_operator json : Ast.unary_operator option =
  match json with
  | `String "-" ->
      Some UMinus
  | `String "bnot" ->
      Some UBNot
  | `String "not" ->
      Some UNot
  | _ ->
      unknown "unary_operator" json


let to_exception json : Ast.exception_ option =
  match json with
  | `List [`String "atom"; _anno; `String atom] ->
      Some (Atom atom)
  | `List [`String "var"; _anno; `String variable] ->
      Some (Pattern variable)
  | _ ->
      unknown "exception" json


let to_arity json : int option =
  match json with
  | `Int arity ->
      Some arity
  | `List [`String "integer"; _anno; `Int arity] ->
      Some arity
  | _ ->
      unknown "arity" json


let to_module_reference json : Ast.module_reference option =
  match json with
  | `List [`String "atom"; _anno; `String name] ->
      Some (ModuleName name)
  | `List [`String "var"; _anno; `String variable] ->
      Some (ModuleVariable variable)
  | `String name ->
      Some (ModuleName name)
  | _ ->
      unknown "module_reference" json


let to_function_reference json : Ast.function_reference option =
  match json with
  | `List [`String "atom"; _anno; `String name] ->
      Some (FunctionName name)
  | `List [`String "var"; _anno; `String variable] ->
      Some (FunctionVariable variable)
  | `String name ->
      Some (FunctionName name)
  | _ ->
      unknown "function_reference" json


let rec to_expression json : Ast.expression option =
  let expr location simple_expression : Ast.expression option =
    Some {location; simple_expression}
  in
  match json with
  | `List [`String "atom"; anno; `Bool atom] ->
      let* loc = to_loc anno in
      expr loc (Literal (Atom (Printf.sprintf "%b" atom)))
  | `List [`String "atom"; anno; `Null] ->
      let* loc = to_loc anno in
      expr loc (Literal (Atom "null"))
  | `List [`String "atom"; anno; `String atom] ->
      let* loc = to_loc anno in
      expr loc (Literal (Atom atom))
  | `List [`String "bc"; anno; expression; qualifiers] ->
      let* loc = to_loc anno in
      let* expression = to_expression expression in
      let* qualifiers = to_list ~f:to_qualifier qualifiers in
      expr loc (BitstringComprehension {expression; qualifiers})
  | `List [`String "bin"; anno; elements] ->
      let* loc = to_loc anno in
      let* elements = to_list ~f:to_bin_element elements in
      expr loc (BitstringConstructor elements)
  | `List [`String "block"; anno; body] ->
      let* loc = to_loc anno in
      let* body = to_body body in
      expr loc (Block body)
  | `List [`String "call"; anno; `List [`String "remote"; _anno2; module_; function_]; args] ->
      let* loc = to_loc anno in
      let* module_ = to_expression module_ in
      let* function_ = to_expression function_ in
      let* args = to_body args in
      expr loc (Call {module_= Some module_; function_; args})
  | `List [`String "call"; anno; function_; args] ->
      let* loc = to_loc anno in
      let* function_ = to_expression function_ in
      let* args = to_body args in
      expr loc (Call {module_= None; function_; args})
  | `List [`String "case"; anno; expression; cases] ->
      let* loc = to_loc anno in
      let* expression = to_expression expression in
      let* cases = to_list ~f:to_case_clause cases in
      expr loc (Case {expression; cases})
  | `List [`String "catch"; anno; expression] ->
      let* loc = to_loc anno in
      let* expression = to_expression expression in
      expr loc (Catch expression)
  | `List [`String "char"; anno; charlit] ->
      let* loc = to_loc anno in
      let* charlit = to_intlit charlit in
      expr loc (Literal (Char charlit))
  | `List [`String "cons"; anno; head; tail] ->
      let* loc = to_loc anno in
      let* head = to_expression head in
      let* tail = to_expression tail in
      expr loc (Cons {head; tail})
  | `List [`String "float"; anno; `Float floatlit] ->
      let* loc = to_loc anno in
      expr loc (Literal (Float floatlit))
  | `List [`String "fun"; anno; `List [`String "clauses"; cases]] ->
      let* loc = to_loc anno in
      let* cases = to_list ~f:to_case_clause cases in
      expr loc (Lambda {name= None; cases; procname= None; captured= None})
  | `List [`String "fun"; anno; `List [`String "function"; function_; arity]] ->
      let* loc = to_loc anno in
      let* function_ = to_function_reference function_ in
      let* arity = to_arity arity in
      expr loc (Fun {module_= ModuleMissing; function_; arity})
  | `List [`String "fun"; anno; `List [`String "function"; module_; function_; arity]] ->
      let* loc = to_loc anno in
      let* module_ = to_module_reference module_ in
      let* function_ = to_function_reference function_ in
      let* arity = to_arity arity in
      expr loc (Fun {module_; function_; arity})
  | `List [`String "if"; anno; cases] ->
      let* loc = to_loc anno in
      let* cases = to_list ~f:to_case_clause cases in
      expr loc (If cases)
  | `List [`String "integer"; anno; intlit] ->
      let* loc = to_loc anno in
      let* intlit = to_intlit intlit in
      expr loc (Literal (Int intlit))
  | `List [`String "lc"; anno; expression; qualifiers] ->
      let* loc = to_loc anno in
      let* expression = to_expression expression in
      let* qualifiers = to_list ~f:to_qualifier qualifiers in
      expr loc (ListComprehension {expression; qualifiers})
  | `List [`String "map"; anno; map; updates] ->
      let* loc = to_loc anno in
      let* map = to_expression map in
      let* updates = to_list ~f:to_association updates in
      expr loc (Map {map= Some map; updates})
  | `List [`String "map"; anno; updates] ->
      let* loc = to_loc anno in
      let* updates = to_list ~f:to_association updates in
      expr loc (Map {map= None; updates})
  | `List [`String "match"; anno; pattern; body] ->
      let* loc = to_loc anno in
      let* pattern = to_expression pattern in
      let* body = to_expression body in
      expr loc (Match {pattern; body})
  | `List [`String "named_fun"; anno; `String name; cases] ->
      let* loc = to_loc anno in
      let* cases = to_list ~f:to_case_clause cases in
      expr loc (Lambda {name= Some name; cases; procname= None; captured= None})
  | `List [`String "nil"; anno] ->
      let* loc = to_loc anno in
      expr loc Nil
  | `List [`String "op"; _anno; `String "+"; argument] ->
      to_expression argument
  | `List [`String "op"; anno; op; argument] ->
      let* loc = to_loc anno in
      let* op = to_unary_operator op in
      let* argument = to_expression argument in
      expr loc (UnaryOperator (op, argument))
  | `List [`String "op"; anno; op; left; right] ->
      let* loc = to_loc anno in
      let* op = to_binary_operator op in
      let* left = to_expression left in
      let* right = to_expression right in
      expr loc (BinaryOperator (left, op, right))
  | `List [`String "receive"; anno; cases; time; handler] ->
      let* loc = to_loc anno in
      let* cases = to_list ~f:to_case_clause cases in
      let* time = to_expression time in
      let* handler = to_body handler in
      expr loc (Receive {cases; timeout= Some {time; handler}})
  | `List [`String "receive"; anno; cases] ->
      let* loc = to_loc anno in
      let* cases = to_list ~f:to_case_clause cases in
      expr loc (Receive {cases; timeout= None})
  | `List [`String "record"; anno; `String name; updates] ->
      let* loc = to_loc anno in
      let* updates = to_list ~f:to_record_update updates in
      expr loc (RecordUpdate {record= None; name; updates})
  | `List [`String "record"; anno; record; `String name; updates] ->
      let* loc = to_loc anno in
      let* record = to_expression record in
      let* updates = to_list ~f:to_record_update updates in
      expr loc (RecordUpdate {record= Some record; name; updates})
  | `List
      [ `String "record_field"
      ; anno
      ; record
      ; `String name
      ; `List [`String "atom"; _anno; `String field] ] ->
      let* loc = to_loc anno in
      let* record = to_expression record in
      expr loc (RecordAccess {record; name; field})
  | `List
      [ `String "record_index"
      ; anno
      ; `String name
      ; `List [`String "atom"; _anno_field; `String field] ] ->
      let* loc = to_loc anno in
      expr loc (RecordIndex {name; field})
  | `List [`String "string"; anno; `List []] ->
      let* loc = to_loc anno in
      expr loc (Literal (String ""))
  | `List [`String "string"; anno; `String s] ->
      let* loc = to_loc anno in
      expr loc (Literal (String s))
  | `List [`String "try"; anno; body; ok_cases; catch_cases; after] ->
      let* loc = to_loc anno in
      let* body = to_body body in
      let* ok_cases = to_list ~f:to_case_clause ok_cases in
      let* catch_cases = to_list ~f:to_catch_clause catch_cases in
      let* after = to_body after in
      expr loc (TryCatch {body; ok_cases; catch_cases; after})
  | `List [`String "tuple"; anno; tuple] ->
      let* loc = to_loc anno in
      let* xs = to_list ~f:to_expression tuple in
      expr loc (Tuple xs)
  | `List [`String "var"; anno; `String variable] ->
      let* loc = to_loc anno in
      expr loc (Variable {vname= variable; scope= None})
  | _ ->
      unknown "expression" json


and to_body json : Ast.expression list option = to_list ~f:to_expression json

and to_association json : Ast.association option =
  match json with
  | `List [`String "map_field_assoc"; _anno; key; value] ->
      let* key = to_expression key in
      let* value = to_expression value in
      Some {Ast.kind= Arrow; key; value}
  | `List [`String "map_field_exact"; _anno; key; value] ->
      let* key = to_expression key in
      let* value = to_expression value in
      Some {Ast.kind= Exact; key; value}
  | _ ->
      unknown "association" json


and to_record_update json : Ast.record_update option =
  match json with
  | `List
      [ `String "record_field"
      ; _anno_update
      ; `List [`String "var"; _anno_field; `String "_"]
      ; expression ] ->
      let* expression = to_expression expression in
      Some {Ast.field= None; expression}
  | `List
      [ `String "record_field"
      ; _anno_update
      ; `List [`String "atom"; _anno_field; `String field]
      ; expression ] ->
      let* expression = to_expression expression in
      Some {Ast.field= Some field; expression}
  | _ ->
      unknown "record_update" json


and to_bin_element json : Ast.bin_element option =
  match json with
  | `List [`String "bin_element"; _anno; expression; size; (* TODO *) _type_specifier_list] ->
      let* expression = to_expression expression in
      let* size = default_or to_expression size in
      Some {Ast.expression; size; types= None}
  | _ ->
      unknown "bin_element" json


and to_catch_pattern json : Ast.catch_pattern option =
  match json with
  | `List
      [ `String "tuple"
      ; _anno
      ; `List [exception_; pattern; `List [`String "var"; _var_anno; `String variable]] ] ->
      let* exception_ = to_exception exception_ in
      let* pattern = to_expression pattern in
      Some {Ast.exception_; pattern; variable}
  | _ ->
      unknown "catch_pattern" json


and to_guards json : Ast.expression list list option =
  let to_guard xs = to_list ~f:to_expression xs in
  to_list ~f:to_guard json


and to_clause : 'pat. 'pat parser -> 'pat Ast.clause parser =
 fun to_pat json ->
  match json with
  | `List [`String "clause"; anno; patterns; guards; body] ->
      let* location = to_loc anno in
      let* patterns = to_list ~f:to_pat patterns in
      let* guards = to_guards guards in
      let body = one_list body in
      let* body = to_body body in
      Some {Ast.location; patterns; guards; body}
  | json ->
      unknown "clause" json


and to_qualifier json : Ast.qualifier option =
  match json with
  | `List [`String "b_generate"; _anno; pattern; expression] ->
      let* pattern = to_expression pattern in
      let* expression = to_expression expression in
      Some (Ast.BitsGenerator {pattern; expression})
  | `List [`String "generate"; _anno; pattern; expression] ->
      let* pattern = to_expression pattern in
      let* expression = to_expression expression in
      Some (Ast.Generator {pattern; expression})
  | filter ->
      let* filter = to_expression filter in
      Some (Ast.Filter filter)


and to_case_clause json : Ast.case_clause option = to_clause to_expression json

and to_catch_clause json : Ast.catch_clause option = to_clause to_catch_pattern json

let to_function ~check_no_module json : Ast.function_ option =
  match json with
  | `List [`String function_; `Int arity] ->
      let function_ = Ast.FunctionName function_ in
      Some {module_= ModuleMissing; function_; arity}
  | `List [`String _module; `String _; `Int _] when check_no_module ->
      unknown "function with unexpected module" json
  | `List [`String _module; `String function_; `Int arity] ->
      (* TODO: assert [_module] is current module *)
      let function_ = Ast.FunctionName function_ in
      Some {module_= ModuleMissing; function_; arity}
  | _ ->
      unknown "function" json


let rec to_record_field json : Ast.record_field option =
  match json with
  | `List [`String "record_field"; _; `List [`String "atom"; _; `String field_name]; expr] ->
      Some {Ast.field_name; initializer_= to_expression expr}
  | `List [`String "record_field"; _; `List [`String "atom"; _; `String field_name]] ->
      Some {Ast.field_name; initializer_= None}
  | `List [`String "typed_record_field"; inner_record_field; _] ->
      to_record_field inner_record_field
  | _ ->
      unknown "record_field" json


let rec to_type json : Ast.type_ option =
  match json with
  | `List [`String "ann_type"; _anno; `List [`List [`String "var"; _anno2; `String _varname]; typ]]
    ->
      to_type typ
  | `List [`String "type"; _anno; `String "any"; `List []]
  | `List [`String "type"; _anno; `String "term"; `List []] ->
      Some Ast.Any
  | `List [`String "type"; _anno; `String "atom"; `List []]
  | `List [`String "type"; _anno; `String "module"; `List []]
  | `List [`String "type"; _anno; `String "node"; `List []] ->
      Some (Ast.Atom Any)
  | `List [`String "atom"; _anno; `String value] ->
      Some (Ast.Atom (Literal value))
  | `List [`String "atom"; _anno; `Bool true] ->
      Some (Ast.Atom (Literal "true"))
  | `List [`String "atom"; _anno; `Bool false] ->
      Some (Ast.Atom (Literal "false"))
  | `List
      [ `String "type"
      ; _anno
      ; `String "binary"
      ; `List
          [ `List [`String "integer"; _anno2; `Int start_size]
          ; `List [`String "integer"; _anno3; `Int segment_size] ] ] ->
      Some (Ast.BitString {start_size; segment_size})
  | `List [`String "type"; _anno; `String "binary"; `List []] ->
      Some (Ast.BitString {start_size= 0; segment_size= 8})
  | `List [`String "type"; _anno; `String "bitstring"; `List []] ->
      Some (Ast.BitString {start_size= 0; segment_size= 1})
  | `List [`String "type"; _anno; `String "arity"; `List []]
  | `List [`String "type"; _anno; `String "byte"; `List []] ->
      Some (Ast.Integer (Range {low= 0; high= 255}))
  | `List [`String "type"; _anno; `String "nonempty_binary"; `List []] ->
      Some (Ast.BitString {start_size= 1; segment_size= 8})
  | `List [`String "type"; _anno; `String "nonempty_bitstring"; `List []] ->
      Some (Ast.BitString {start_size= 1; segment_size= 1})
  | `List [`String "type"; _anno; `String "boolean"; `List []] ->
      Some (Ast.Union [Atom (Literal "true"); Atom (Literal "false")])
  | `List [`String "type"; _anno; `String "identifier"; `List []] ->
      Some (Ast.Union [Pid; Port; Reference])
  | `List [`String "type"; _anno; `String "integer"; `List []] ->
      Some (Ast.Integer Any)
  | `List [`String "integer"; _anno; `Int value] ->
      Some (Ast.Integer (Literal value))
  | `List
      [ `String "type"
      ; _anno
      ; `String "range"
      ; `List
          [`List [`String "integer"; _anno2; `Int low]; `List [`String "integer"; _anno3; `Int high]]
      ] ->
      Some (Ast.Integer (Range {low; high}))
  | `List [`String "type"; _anno; `String "mfa"; `List []] ->
      Some (Ast.Union [Atom Any; Atom Any; Integer (Range {low= 0; high= 255})])
  | `List [`String "type"; _anno; `String "non_neg_integer"; `List []] ->
      Some (Ast.Integer NonNeg)
  | `List [`String "type"; _anno; `String "neg_integer"; `List []] ->
      Some (Ast.Integer Neg)
  | `List [`String "type"; _anno; `String "pos_integer"; `List []] ->
      Some (Ast.Integer Pos)
  | `List [`String "type"; _anno; `String "list"; `List []] ->
      Some (Ast.List (Proper Any))
  | `List [`String "type"; _anno; `String "list"; `List [arg_json]] ->
      let* arg = to_type arg_json in
      Some (Ast.List (Proper arg))
  | `List [`String "type"; _anno; `String "map"; _args_json] ->
      (* TODO: associations *)
      Some Ast.Map
  | `List [`String "type"; _anno; `String "nil"; `List []] ->
      Some Ast.Nil
  | `List [`String "type"; _anno; `String "none"; `List []]
  | `List [`String "type"; _anno; `String "no_return"; `List []] ->
      Some Ast.None
  | `List [`String "type"; _anno; `String "pid"; `List []] ->
      Some Ast.Pid
  | `List [`String "type"; _anno; `String "port"; `List []] ->
      Some Ast.Port
  | `List
      [`String "type"; _anno; `String "record"; `List [`List [`String "atom"; _anno2; `String name]]]
    ->
      Some (Ast.Record name)
  | `List
      [ `String "type"
      ; _anno
      ; `String "record"
      ; `List [`List [`String "atom"; _anno2; `String name]; _fields_json] ] ->
      (* TODO: fields *)
      Some (Ast.Record name)
  | `List [`String "type"; _anno; `String "reference"; `List []] ->
      Some Ast.Reference
  | `List [`String "type"; _anno; `String "string"; `List []] ->
      (* string() is [char()] but we don't support char() yet. *)
      Some (Ast.List (Proper Unsupported))
  | `List [`String "type"; _anno; `String "tuple"; `String "any"] ->
      Some (Ast.Tuple AnySize)
  | `List [`String "type"; _anno; `String "tuple"; args_json] ->
      let* args = to_list ~f:to_type args_json in
      Some (Ast.Tuple (FixedSize args))
  | `List [`String "type"; _anno; `String "union"; args_json] ->
      let* args = to_list ~f:to_type args_json in
      Some (Ast.Union args)
  | `List
      [ `String "remote_type"
      ; _anno
      ; `List
          [ `List [`String "atom"; _anno2; `String module_]
          ; `List [`String "atom"; _anno3; `String type_]
          ; `List [] ] ] ->
      (* TODO: arguments *)
      Some (Ast.Remote {module_; type_})
  | `List [`String "user_type"; _anno; `String typ; `List []] ->
      (* TODO: arguments *)
      Some (Ast.UserDefined typ)
  | `List [`String "var"; _anno; `String name] ->
      Some (Ast.Var name)
  (* TODO: add more types *)
  | _ ->
      let _ = unknown "type" json in
      Some Ast.Unsupported


let to_spec_args json : Ast.type_ list option =
  match json with
  (* We expect a product type with a list, even if there are 0 or 1 arguments. *)
  | `List [`String "type"; _anno; `String "product"; args_json] ->
      to_list ~f:to_type args_json
  | _ ->
      unknown "spec argument types" json


let to_spec_ret : json -> Ast.type_ option = to_type

let to_constraint json : (string * Ast.type_) option =
  match json with
  | `List
      [ `String "type"
      ; _anno
      ; `String "constraint"
      ; `List
          [ `List [`String "atom"; _anno2; `String "is_subtype"]
          ; `List [`List [`String "var"; _anno3; `String var]; type_json] ] ] ->
      let* subtype_of = to_type type_json in
      Some (var, subtype_of)
  | _ ->
      unknown "spec constraint" json


let to_spec_disjunct json : Ast.spec_disjunct option =
  match json with
  | `List [`String "type"; _anno; `String "fun"; `List [args_json; ret_json]] ->
      let* return = to_spec_ret ret_json in
      let* arguments = to_spec_args args_json in
      Some {Ast.arguments; return; constraints= String.Map.empty}
  | `List
      [ `String "type"
      ; _anno
      ; `String "bounded_fun"
      ; `List
          [ `List [`String "type"; _anno2; `String "fun"; `List [args_json; ret_json]]
          ; constraints_json ] ] ->
      let* return = to_spec_ret ret_json in
      let* arguments = to_spec_args args_json in
      let* constr_list = to_list ~f:to_constraint constraints_json in
      let f map (key, data) =
        match Map.add ~key ~data map with
        | `Ok map ->
            map
        | `Duplicate ->
            L.debug Capture Verbose "Ignoring duplicate constraint for type variable %s in %s@." key
              (Yojson.Safe.show json) ;
            map
      in
      let constraints = List.fold ~f ~init:String.Map.empty constr_list in
      Some {Ast.arguments; return; constraints}
  | _ ->
      unknown "spec" json


let to_spec = to_list ~f:to_spec_disjunct

let to_loc_form json : Ast.form option =
  let form location simple_form : Ast.form option = Some {location; simple_form} in
  match json with
  | `List [`String "attribute"; anno; `String "file"; `List [`String path; _anno_file]] ->
      let* loc = to_loc anno in
      form loc (File {path})
  | `List [`String "attribute"; anno; `String "module"; `String module_name] ->
      let* loc = to_loc anno in
      form loc (Module module_name)
  | `List [`String "attribute"; anno; `String "import"; `List [`String module_name; functions]] ->
      let* loc = to_loc anno in
      let* functions = to_list ~f:(to_function ~check_no_module:true) functions in
      form loc (Import {module_name; functions})
  | `List [`String "attribute"; anno; `String "export"; function_] ->
      let* loc = to_loc anno in
      let* func_list = to_list ~f:(to_function ~check_no_module:true) function_ in
      form loc (Export func_list)
  | `List [`String "function"; anno; `String function_; `Int arity; case_clause] ->
      let* loc = to_loc anno in
      let* clauses = to_list ~f:to_case_clause case_clause in
      let function_ : Ast.function_reference = FunctionName function_ in
      let function_ : Ast.function_ = {module_= ModuleMissing; function_; arity} in
      form loc (Function {function_; clauses})
  | `List [`String "attribute"; anno; `String "record"; `List [`String name; fields]] ->
      let* loc = to_loc anno in
      let* field_list = to_list ~f:to_record_field fields in
      form loc (Record {name; fields= field_list})
  | `List [`String "attribute"; anno; `String "spec"; `List [func_json; specs_json]] ->
      let* loc = to_loc anno in
      let* function_ = to_function ~check_no_module:false func_json in
      let* spec = to_spec specs_json in
      form loc (Spec {function_; spec})
  | `List [`String "attribute"; anno; `String "type"; `List [`String name; type_json; `List []]] ->
      let* loc = to_loc anno in
      let* type_ = to_type type_json in
      form loc (Type {name; type_})
  | `List [`String "attribute"; _anno; `String _unknown_attribute; _] ->
      (* TODO *)
      None
  | `List [`String "eof"; _] ->
      None
  | _ ->
      unknown "form" json


let to_module json : Ast.module_ option = to_list ~skip_errors:true ~f:to_loc_form json
