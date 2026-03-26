(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open SemdiffDirectEngine

let any = Pattern.node "Name" [("ctx", Pattern.node "Load" []); ("id", Pattern.str "Any")]

let is_not_Any arg = Some (Condition.Not (Atom {predicate= Equals; args= [any; arg]}))

let is_not_null arg = Some (Condition.Not (Atom {predicate= Equals; args= [Pattern.null; arg]}))

let does_not_contain_Any arg = Some (Condition.Not (Atom {predicate= Contains; args= [any; arg]}))

let and_ opt1 opt2 =
  match (opt1, opt2) with
  | Some cond1, Some cond2 ->
      Some (Condition.And (cond1, cond2))
  | _, _ ->
      None


let missing_python_type_annotations_config : Rules.t =
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
              [("annotation", null); ("simple", int 1); ("target", var "N"); ("value", var "V")]
        ; condition= None
        ; key= [] }
      ; { (* annotated statements are simple or not (see documentation), but we don't care *)
          lhs=
            node "AnnAssign"
              [("annotation", var "A"); ("simple", int 0); ("target", var "N"); ("value", var "V")]
        ; rhs=
            node "AnnAssign"
              [("annotation", var "A"); ("simple", int 1); ("target", var "N"); ("value", var "V")]
        ; condition= None
        ; key= [] }
      ; { (* if N.class == str ==> if isinstance(N, str) *)
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
              ; ("keywords", list []) ]
        ; condition= None
        ; key= [] } ]
  ; accept=
      [ { (* if the parent file was not annotated, the new version can be annotated with any type, except Any*)
          lhs= null
        ; rhs= var "X"
        ; condition= is_not_Any (var "X")
        ; key= [Name.of_string "returns"; Name.of_string "annotation"] }
      ; { (* if the parent file was annotated', we accept any type as long as it does not contain Any *)
          lhs= var "T1"
        ; rhs= var "T2"
        ; condition= and_ (is_not_null (var "T1")) (does_not_contain_Any (var "T2"))
        ; key= [Name.of_string "returns"; Name.of_string "annotation"] }
      ; { (* if the parent was annotated with Optional[T], we require T | None instead *)
          lhs=
            node "Subscript"
              [ ("value", node "Name" [("id", str "Optional"); ("ctx", node "Load" [])])
              ; ("slice", var "T")
              ; ("ctx", node "Load" []) ]
        ; rhs=
            node "BinOp"
              [ ("left", var "T")
              ; ("op", node "BitOr" [])
              ; ("right", node "Constant" [("kind", null); ("value", null)]) ]
        ; condition= None
        ; key= [] }
      ; { (* if the parent file was annotated with 'Dict[T]', we require 'dict[T]' instead *)
          lhs= node "Name" [("id", str "Dict"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "dict"); ("ctx", var "C")]
        ; condition= None
        ; key= [] }
      ; { (* if the parent file was annotated with 'FrozenSet[T]', we require 'frozenset[T]' instead *)
          lhs= node "Name" [("id", str "FrozenSet"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "frozenset"); ("ctx", var "C")]
        ; condition= None
        ; key= [] }
      ; { (* if the parent file was annotated with 'List[T]', we require 'list[T]' instead *)
          lhs= node "Name" [("id", str "List"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "list"); ("ctx", var "C")]
        ; condition= None
        ; key= [] }
      ; { (* if the parent file was annotated with 'Tuple[T]', we require 'tuple[T]' instead *)
          lhs= node "Name" [("id", str "Tuple"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "tuple"); ("ctx", var "C")]
        ; condition= None
        ; key= [] }
      ; { (* if the parent file was annotated with 'Set[T]', we require 'set[T]' instead *)
          lhs= node "Name" [("id", str "Set"); ("ctx", var "C")]
        ; rhs= node "Name" [("id", str "set"); ("ctx", var "C")]
        ; condition= None
        ; key= [] } ] }
