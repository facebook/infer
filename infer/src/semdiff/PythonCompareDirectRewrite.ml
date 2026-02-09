(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Ast = PythonSourceAst
module Var = CongruenceClosureRewrite.Var
module Name = CongruenceClosureRewrite.Var

module Subst : Stdlib.Map.S with type key = Var.t = Var.Map

module Pattern = struct
  type t =
    | Var of Var.t
    | AstNode of Ast.Node.t
    | Node of {name: Name.t; args: (Name.t * t) list}
    | List of t list
  [@@deriving equal]

  let rec pp fmt = function
    | Var v ->
        Var.pp fmt v
    | AstNode ast ->
        Ast.Node.pp fmt ast
    | Node {name; args} ->
        F.fprintf fmt "%a(%a)" Name.pp name
          (Pp.comma_seq (fun fmt (key, node) -> F.fprintf fmt "%a=%a" Name.pp key pp node))
          args
    | List l ->
        F.fprintf fmt "[%a]" (Pp.comma_seq pp) l


  let var v = Var (Var.of_string v)

  let null = AstNode Null

  let str s = AstNode (Str s)

  let int i = AstNode (Int i)

  let list l = List l

  let node name args =
    let name = Name.of_string name in
    let args = List.map args ~f:(fun (name, pattern) -> (Name.of_string name, pattern)) in
    Node {name; args}


  let vars pattern =
    let rec aux acc = function
      | Var v ->
          Var.Set.add v acc
      | AstNode _ ->
          acc
      | List l ->
          List.fold l ~init:acc ~f:aux
      | Node {args} ->
          List.fold args ~init:acc ~f:(fun acc (_, pattern) -> aux acc pattern)
    in
    aux Var.Set.empty pattern


  exception ImpossibleMatching

  let check b : unit = if not b then raise ImpossibleMatching

  let rec match_rec subst pattern (ast : Ast.Node.t) =
    match (pattern, ast) with
    | Var var, _ -> (
      match Subst.find_opt var subst with
      | Some already_assigned when Ast.Node.equal ast already_assigned ->
          subst
      | Some _ ->
          raise ImpossibleMatching
      | None ->
          Subst.add var ast subst )
    | List patterns, List nodes -> (
      match List.fold2 patterns nodes ~init:subst ~f:match_rec with
      | Ok subst ->
          subst
      | Unequal_lengths ->
          raise ImpossibleMatching )
    | Node {name; args}, Dict dict ->
        let header, nodes = Ast.Node.assoc_of_dict dict in
        check (String.equal header (name :> string)) ;
        let args = List.sort args ~compare:[%compare: Name.t * (t[@ignore])] in
        check (Int.equal (List.length args) (List.length nodes)) ;
        List.fold2_exn args nodes ~init:subst
          ~f:(fun subst ((name : Name.t), pattern) (key, node) ->
            check (String.equal (name :> string) key) ;
            match_rec subst pattern node )
    | AstNode n1, n2 when Ast.Node.equal n1 n2 ->
        subst
    | _, _ ->
        raise ImpossibleMatching


  and match_node pattern ast =
    try Some (match_rec Subst.empty pattern ast) with ImpossibleMatching -> None


  and match_pair (pattern1, pattern2) (ast1, ast2) =
    try
      let subst = match_rec Subst.empty pattern1 ast1 in
      let _subst = match_rec subst pattern2 ast2 in
      true
    with ImpossibleMatching -> false


  let rec to_node ~start_line ~end_line subst = function
    | Var var ->
        Subst.find_opt var subst
        |> Option.value_or_thunk ~default:(fun () -> L.die InternalError "invalid substitution")
    | AstNode node ->
        node
    | List l ->
        Ast.Node.List (List.map l ~f:(to_node ~start_line ~end_line subst))
    | Node {name; args} ->
        let args : (string * Ast.Node.t) list =
          List.map args ~f:(fun ((name : Name.t), pattern) ->
              ((name :> string), to_node ~start_line ~end_line subst pattern) )
        in
        let new_node = Ast.Node.Dict (Ast.Node.dict_of_assoc (name :> string) args) in
        let new_node = Ast.Node.set_node_line_number new_node start_line in
        Ast.Node.set_node_end_line_number new_node end_line
end

module Action = struct
  let ignore node pattern = Option.is_some (Pattern.match_node pattern node)

  let rewrite node ~lhs ~rhs =
    match Pattern.match_node lhs node with
    | Some subst ->
        let start_line = Ast.Node.get_node_line_number node in
        let end_line = Ast.Node.get_node_end_line_number node in
        Pattern.to_node ~start_line ~end_line subst rhs
    | None ->
        node


  let accept ~lhs_node ~rhs_node ~lhs_pattern ~rhs_pattern =
    Pattern.match_pair (lhs_pattern, rhs_pattern) (lhs_node, rhs_node)
end

module Rules = struct
  type rule = {lhs: Pattern.t; rhs: Pattern.t} [@@deriving equal]

  type t = {ignore: Pattern.t list; rewrite: rule list; accept: rule list} [@@deriving equal]

  let vars {ignore; rewrite; accept} =
    let open Var.Set in
    let acc =
      List.fold ignore ~init:empty ~f:(fun acc pattern -> union acc (Pattern.vars pattern))
    in
    let acc =
      List.fold rewrite ~init:acc ~f:(fun acc {lhs; rhs} ->
          union (union acc (Pattern.vars lhs)) (Pattern.vars rhs) )
    in
    List.fold accept ~init:acc ~f:(fun acc {lhs; rhs} ->
        union (union acc (Pattern.vars lhs)) (Pattern.vars rhs) )


  let pp fmt ({ignore; rewrite; accept} as rules) =
    let vars = vars rules in
    let pp_ignore fmt pattern = F.fprintf fmt "ignore(%a)@." Pattern.pp pattern in
    let pp_rewrite fmt {lhs; rhs} =
      F.fprintf fmt "rewrite(@[<hv>lhs=%a,@ rhs=%a@])@." Pattern.pp lhs Pattern.pp rhs
    in
    let pp_accept fmt {lhs; rhs} =
      F.fprintf fmt "accept(@[<hv>lhs=%a, rhs=%a@])@." Pattern.pp lhs Pattern.pp rhs
    in
    F.fprintf fmt "@.vars:" ;
    Var.Set.iter (fun var -> F.fprintf fmt " %a" Var.pp var) vars ;
    F.fprintf fmt "@." ;
    List.iter ignore ~f:(pp_ignore fmt) ;
    F.fprintf fmt "@." ;
    List.iter rewrite ~f:(pp_rewrite fmt) ;
    F.fprintf fmt "@." ;
    List.iter accept ~f:(pp_accept fmt) ;
    F.fprintf fmt "@."
end

module Run = struct
  let dont_ignore {Rules.ignore} node =
    List.for_all ignore ~f:(fun pattern -> not (Action.ignore node pattern))


  let rewrite {Rules.rewrite} node =
    List.fold rewrite ~init:node ~f:(fun node ({lhs; rhs} : Rules.rule) ->
        Action.rewrite node ~lhs ~rhs )


  let accept {Rules.accept} ~left ~right =
    List.exists accept ~f:(fun ({lhs; rhs} : Rules.rule) ->
        Action.accept ~lhs_node:left ~rhs_node:right ~lhs_pattern:lhs ~rhs_pattern:rhs )
end

let zip_and_build_diffs config n1 n2 : Diff.t list =
  let fold_if_same_shape header1 fields1 header2 fields2 ~init ~f =
    let same_type = String.equal header1 header2 in
    if not same_type then None
    else if [%equal: (string * ('a[@ignore])) list] fields1 fields2 then
      (* probably a bit too defensive since Node.Ast will give use sorted keys *)
      Some (List.fold2_exn fields1 fields2 ~init ~f:(fun acc (_, n1) (_, n2) -> f acc n1 n2))
    else None
  in
  let rec zip ~left_line ~right_line acc ~rewritten (n1 : Ast.Node.t) (n2 : Ast.Node.t) :
      Diff.t list =
    match (n1, n2) with
    | a, b when Ast.Node.equal a b || Run.accept config ~left:n1 ~right:n2 ->
        acc
    | Dict f1, Dict f2 ->
        let saved_left_line = left_line in
        let saved_right_line = right_line in
        let left_line = Ast.Node.get_line_number f1 in
        let right_line = Ast.Node.get_line_number f2 in
        let end_left_line = Ast.Node.get_end_line_number f1 in
        let end_right_line = Ast.Node.get_end_line_number f2 in
        let header1, fields1 = Ast.Node.assoc_of_dict f1 in
        let header2, fields2 = Ast.Node.assoc_of_dict f2 in
        fold_if_same_shape header1 fields1 header2 fields2 ~init:acc
          ~f:(zip ~left_line ~right_line ~rewritten:false)
        |> Option.value_or_thunk ~default:(fun () ->
               if rewritten then
                 Diff.append_removed_lines left_line end_left_line
                   (Diff.append_added_lines right_line end_right_line acc)
               else
                 zip ~left_line:saved_left_line ~right_line:saved_right_line ~rewritten:true acc
                   (Run.rewrite config n1) (Run.rewrite config n2) )
    | List l1, List l2 ->
        let l1 = List.filter l1 ~f:(Run.dont_ignore config) in
        let l2 = List.filter l2 ~f:(Run.dont_ignore config) in
        let n1 = List.length l1 in
        let n2 = List.length l2 in
        if Int.equal n1 n2 then
          List.fold2_exn l1 l2 ~f:(zip ~left_line ~right_line ~rewritten:false) ~init:acc
        else if n1 < n2 then
          let l2_start, l2_end = List.split_n l2 n1 in
          let acc =
            List.fold2_exn l1 l2_start ~f:(zip ~left_line ~right_line ~rewritten:false) ~init:acc
          in
          List.fold l2_end ~init:acc ~f:(fun acc node ->
              Diff.append_added_line (Ast.Node.get_node_line_number node) acc )
        else
          let l1_start, l1_end = List.split_n l1 n2 in
          let acc =
            List.fold2_exn l1_start l2 ~f:(zip ~left_line ~right_line ~rewritten:false) ~init:acc
          in
          List.fold l1_end ~init:acc ~f:(fun acc node ->
              Diff.append_removed_line (Ast.Node.get_node_line_number node) acc )
    | Dict f1, _ ->
        Diff.append_removed_line (Ast.Node.get_line_number f1) acc
    | _, Dict f2 ->
        Diff.append_added_line (Ast.Node.get_line_number f2) acc
    | _ ->
        Diff.append_removed_line left_line (Diff.append_added_line right_line acc)
  in
  zip ~left_line:None ~right_line:None ~rewritten:false [] n1 n2


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
              [("annotation", null); ("simple", int 1); ("target", var "N"); ("value", var "V")] }
      ; { (* annotated statements are simple or not (see documentation), but we don't care *)
          lhs=
            node "AnnAssign"
              [("annotation", var "A"); ("simple", int 0); ("target", var "N"); ("value", var "V")]
        ; rhs=
            node "AnnAssign"
              [("annotation", var "A"); ("simple", int 1); ("target", var "N"); ("value", var "V")]
        }
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
              ; ("keywords", list []) ] }
      ; { (*  Optional[T] ==> T | None *)
          lhs=
            node "Subscript"
              [ ("value", node "Name" [("id", str "Optional"); ("ctx", node "Load" [])])
              ; ("slice", var "T")
              ; ("ctx", node "Load" []) ]
        ; rhs=
            node "BinOp"
              [ ("left", var "T")
              ; ("op", node "BitOr" [])
              ; ("right", node "Constant" [("kind", null); ("value", null)]) ] } ]
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


let ast_diff ~debug ~config ?filename1 ?filename2 previous_content current_content =
  let parse = Ast.build_parser () in
  match (parse ?filename:filename1 previous_content, parse ?filename:filename2 current_content) with
  | Error error, _ | Ok _, Error error ->
      L.user_error "%a" PythonSourceAst.pp_error error ;
      []
  | Ok ast1, Ok ast2 ->
      let diffs =
        zip_and_build_diffs config ast1 ast2
        |> Diff.gen_explicit_diffs ~previous_content ~current_content
      in
      if debug then (
        F.printf "AST1: %s\n" (Ast.Node.to_str ast1) ;
        F.printf "AST2: %s\n" (Ast.Node.to_str ast2) ;
        F.printf "SemDiff:\n" ;
        List.iter diffs ~f:(fun diff -> F.printf "%a\n" Diff.pp_explicit diff) ) ;
      diffs
