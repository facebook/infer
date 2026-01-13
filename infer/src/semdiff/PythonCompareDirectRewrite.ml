(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Ast = PythonSourceAst
module Var = CongruenceClosureRewrite.Var
module Name = CongruenceClosureRewrite.Var

module Pattern = struct
  type t =
    | Var of Var.t
    | AstNode of Ast.Node.t
    | Node of {name: Name.t; args: (Name.t * t) list}
    | List of t list

  let var v = Var (Var.of_string v)

  let null = AstNode Null

  let str s = AstNode (Str s)

  let int i = AstNode (Int i)

  let list l = List l

  let node name args =
    let name = Name.of_string name in
    let args = List.map args ~f:(fun (name, pattern) -> (Name.of_string name, pattern)) in
    Node {name; args}
end

module Rules = struct
  type rewrite_rule = {lhs: Pattern.t; rhs: Pattern.t}

  type accept_rule = {lhs: Pattern.t; rhs: Pattern.t}

  type t = {ignore: Pattern.t list; rewrite: rewrite_rule list; accept: accept_rule list}
end

let _config : Rules.t =
  let open Pattern in
  { ignore=
      (* we ignore all import statements during comparison *)
      [ node "ImportFrom" [("level", var "L"); ("module", var "M"); ("names", var "N")]
      ; node "Import" [("names", var "N")] ]
  ; rewrite=
      [ { (* an unannotated assignment is turned into an annotated one with Null as type annotation *)
          lhs=
            node "Assign" [("targets", list [var "N"]); ("type_comment", null); ("value", var "V")]
        ; rhs=
            node "AnnAssign"
              [("annotation", null); ("simple", int 1); ("target", var "N"); ("value", var "V")] }
      ; { (* annotated statements are simple or not (see documentation), but we don't care *)
          lhs=
            node "AnnAssign"
              [("annotation", var "A"); ("simple", int 0); ("target", var "N"); ("value", var "V")]
        ; rhs=
            node "AnnAssign"
              [("annotation", var "A"); ("simple", int 1); ("target", var "N"); ("value", var "V")]
        }
      ; { (* if N.class == str <=> if isinstance(N, str) *)
          lhs=
            node "Compare"
              [ ("comparators", list [node "Name" [("ctx", node "Load" []); ("id", str "str")]])
              ; ( "left"
                , node "Attribute"
                    [("attr", str "__class__"); ("ctx", node "Load" []); ("value", var "N")] )
              ; ("ops", list [node "Eq" []]) ]
        ; rhs=
            node "Call"
              [ ("args", list [var "N"; node "Name" [("ctx", node "Load" []); ("id", str "str")]])
              ; ("func", node "Name" [("ctx", node "Load" []); ("id", str "isinstance")])
              ; ("keywords", list []) ] } ]
  ; accept=
      [ { (* if the parent file was not annotated, the new version can be annotated with any type*)
          lhs= null
        ; rhs= var "X" }
      ; { (* if the parent file was annotated with 'Any', we accept 'object' instead *)
          lhs= str "Any"
        ; rhs= str "object" }
      ; { (* if the parent file was annotated with 'Dict[T]', we require 'dict[T]' instead *)
          lhs= node "Name" [("id", str "Dict"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "dict"); ("ctx", var "C")] }
      ; { (* if the parent file was annotated with 'FrozenSet[T]', we require 'frozenset[T]' instead *)
          lhs= node "Name" [("id", str "FrozenSet"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "frozenset"); ("ctx", var "C")] }
      ; { (* if the parent file was annotated with 'List[T]', we require 'list[T]' instead *)
          lhs= node "Name" [("id", str "List"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "list"); ("ctx", var "C")] }
      ; { (* if the parent file was annotated with 'Tuple[T]', we require 'tuple[T]' instead *)
          lhs= node "Name" [("id", str "Tuple"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "tuple"); ("ctx", var "C")] }
      ; { (* if the parent file was annotated with 'Set[T]', we require 'set[T]' instead *)
          lhs= node "Name" [("id", str "Set"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "set"); ("ctx", var "C")] } ] }


let test_ast_diff ~debug src1 src2 =
  (* TODO: implement the new engine here *)
  PythonCompareWithoutTypeAnnot.test_ast_diff ~debug ~test_eqsat:false src1 src2


let semdiff previous_file current_file =
  (* TODO: implement the new engine here *)
  PythonCompareWithoutTypeAnnot.semdiff previous_file current_file
