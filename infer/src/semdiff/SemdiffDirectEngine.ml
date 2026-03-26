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

  let key_matched expected_keys key =
    match (expected_keys, key) with
    | [], _ ->
        true
    | _, None ->
        false
    | keys, Some key ->
        List.exists keys ~f:(Name.equal key)


  let rec match_rec subst expected_keys key pattern (ast : Ast.Node.t) =
    match (pattern, ast) with
    | Var var, _ when key_matched expected_keys key -> (
      match Subst.find_opt var subst with
      | Some already_assigned when Ast.Node.equal ast already_assigned ->
          subst
      | Some _ ->
          raise ImpossibleMatching
      | None ->
          Subst.add var ast subst )
    | List patterns, List nodes -> (
      match
        List.fold2 patterns nodes ~init:subst ~f:(fun subst -> match_rec subst expected_keys None)
      with
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
            match_rec subst expected_keys (Some (Name.of_string key)) pattern node )
    | AstNode n1, n2 when Ast.Node.equal n1 n2 ->
        subst
    | _, _ ->
        raise ImpossibleMatching


  and match_node key pattern ast =
    try Some (match_rec Subst.empty key None pattern ast) with ImpossibleMatching -> None


  and match_pair key expected_keys (pattern1, pattern2) (ast1, ast2) =
    (* let pp_subst fmt subst =
      F.fprintf fmt "{%a}"
        (Pp.comma_seq (fun fmt (name, ast) -> F.fprintf fmt "%a: %a" Name.pp name Ast.Node.pp ast))
        (Subst.bindings subst)
       in *)
    try
      let subst1 = match_rec Subst.empty expected_keys key pattern1 ast1 in
      Some (match_rec subst1 expected_keys key pattern2 ast2)
    with ImpossibleMatching -> None


  let rec to_node ?start_line ?end_line subst = function
    | Var var ->
        Subst.find_opt var subst
        |> Option.value_or_thunk ~default:(fun () -> L.die InternalError "invalid substitution")
    | AstNode node ->
        node
    | List l ->
        Ast.Node.List (List.map l ~f:(to_node ?start_line ?end_line subst))
    | Node {name; args} ->
        let args : (string * Ast.Node.t) list =
          List.map args ~f:(fun ((name : Name.t), pattern) ->
              ((name :> string), to_node ?start_line ?end_line subst pattern) )
        in
        let new_node = Ast.Node.Dict (Ast.Node.dict_of_assoc (name :> string) args) in
        let new_node = Ast.Node.set_node_line_number new_node start_line in
        Ast.Node.set_node_end_line_number new_node end_line
end

module Condition = struct
  type predicate = Contains | Equals [@@deriving equal]

  let pp_predicate fmt = function
    | Equals ->
        F.pp_print_string fmt "equals"
    | Contains ->
        F.pp_print_string fmt "contains"


  let rec contains (node : Ast.Node.t) pattern =
    if Option.is_some (Pattern.match_node [] pattern node) then true
    else
      match node with
      | List nodes ->
          List.exists nodes ~f:(fun node -> contains node pattern)
      | Dict dict ->
          let _header, nodes = Ast.Node.assoc_of_dict dict in
          List.exists nodes ~f:(fun (_key, node) -> contains node pattern)
      | _ ->
          false


  let eval_predicate predicate args subst =
    match (predicate, args) with
    | Contains, [arg1; arg2] ->
        let arg2 = Pattern.to_node subst arg2 in
        contains arg2 arg1
    | Contains, _ ->
        false
    | Equals, [arg1; arg2] ->
        let arg1 = Pattern.to_node subst arg1 in
        let arg2 = Pattern.to_node subst arg2 in
        Ast.Node.equal arg1 arg2
    | Equals, _ ->
        false


  type t = Atom of {predicate: predicate; args: Pattern.t list} | Not of t | And of t * t
  [@@deriving equal]

  let rec pp fmt = function
    | Atom {predicate; args} ->
        F.fprintf fmt "%a(%a)" pp_predicate predicate (Pp.comma_seq Pattern.pp) args
    | Not cond ->
        F.fprintf fmt "not (%a)" pp cond
    | And (cond1, cond2) ->
        F.fprintf fmt "(%a) and (%a)" pp cond1 pp cond2


  let rec eval_cond subst = function
    | Atom {predicate; args} ->
        eval_predicate predicate args subst
    | Not cond ->
        not (eval_cond subst cond)
    | And (cond1, cond2) ->
        eval_cond subst cond1 && eval_cond subst cond2


  let eval opt_condition subst =
    match opt_condition with None -> true | Some cond -> eval_cond subst cond
end

module Action = struct
  let ignore node pattern = Option.is_some (Pattern.match_node [] pattern node)

  let rewrite node ~lhs ~rhs ~condition ~key =
    match Pattern.match_node key lhs node with
    | Some subst when Condition.eval condition subst ->
        let start_line = Ast.Node.get_node_line_number node in
        let end_line = Ast.Node.get_node_end_line_number node in
        Pattern.to_node ?start_line ?end_line subst rhs
    | _ ->
        node


  let accept ~lhs_node ~rhs_node ~lhs_pattern ~rhs_pattern ~condition ~key ~expected_keys =
    match Pattern.match_pair key expected_keys (lhs_pattern, rhs_pattern) (lhs_node, rhs_node) with
    | None ->
        false
    | Some subst ->
        Condition.eval condition subst
end

module Rules = struct
  type rule = {lhs: Pattern.t; rhs: Pattern.t; condition: Condition.t option; key: Name.t list}
  [@@deriving equal]

  type t = {ignore: Pattern.t list; rewrite: rule list; accept: rule list} [@@deriving equal]

  let vars {ignore; rewrite; accept} =
    let open Var.Set in
    let acc =
      List.fold ignore ~init:empty ~f:(fun acc pattern -> union acc (Pattern.vars pattern))
    in
    let rec vars_condition acc cond =
      match (cond : Condition.t) with
      | Atom {args} ->
          List.fold args ~init:acc ~f:(fun acc arg -> union acc (Pattern.vars arg))
      | Not cond ->
          vars_condition acc cond
      | And (cond1, cond2) ->
          vars_condition (vars_condition acc cond1) cond2
    in
    let vars_rule acc {lhs; rhs; condition} =
      match condition with
      | None ->
          union (Pattern.vars rhs) acc |> union (Pattern.vars lhs)
      | Some condition ->
          vars_condition acc condition |> union (Pattern.vars rhs) |> union (Pattern.vars lhs)
    in
    let acc = List.fold rewrite ~init:acc ~f:vars_rule in
    List.fold accept ~init:acc ~f:vars_rule


  let pp_rule fmt {lhs; rhs; condition; key} =
    let pp_condition fmt = function
      | None ->
          ()
      | Some condition ->
          F.fprintf fmt ",@ condition=%a" Condition.pp condition
    in
    let pp_key fmt = function
      | [] ->
          ()
      | l ->
          F.fprintf fmt ",@ key=[%a]"
            (Pp.comma_seq (fun fmt s -> F.fprintf fmt {|"%a"|} Name.pp s))
            l
    in
    F.fprintf fmt "@[<hv>lhs=%a,@ rhs=%a%a%a@]" Pattern.pp lhs Pattern.pp rhs pp_condition condition
      pp_key key


  let pp fmt ({ignore; rewrite; accept} as rules) =
    let vars = vars rules in
    let pp_ignore fmt pattern = F.fprintf fmt "ignore(%a)@." Pattern.pp pattern in
    let pp_rewrite fmt rule = F.fprintf fmt "rewrite(%a)@." pp_rule rule in
    let pp_accept fmt rule = F.fprintf fmt "accept(%a)@." pp_rule rule in
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
    List.fold rewrite ~init:node ~f:(fun node ({lhs; rhs; condition; key} : Rules.rule) ->
        Action.rewrite node ~lhs ~rhs ~condition ~key )


  let accept {Rules.accept} key ~left ~right =
    List.exists accept ~f:(fun ({lhs; rhs; condition; key= expected_keys} : Rules.rule) ->
        Action.accept ~lhs_node:left ~rhs_node:right ~lhs_pattern:lhs ~rhs_pattern:rhs ~condition
          ~key ~expected_keys )
end

let zip_and_build_diffs config n1 n2 : Diff.t list =
  let fold_if_same_shape header1 fields1 header2 fields2 ~init ~f =
    let same_type = String.equal header1 header2 in
    if not same_type then None
    else if [%equal: (string * ('a[@ignore])) list] fields1 fields2 then
      (* probably a bit too defensive since Node.Ast will give use sorted keys *)
      Some
        (List.fold2_exn fields1 fields2 ~init ~f:(fun acc (_, n1) (key, n2) ->
             f (Some (Name.of_string key)) acc n1 n2 ) )
    else None
  in
  let rec zip ~left_line ~right_line (key : Name.t option) acc ~rewritten (n1 : Ast.Node.t)
      (n2 : Ast.Node.t) : Diff.t list =
    match (n1, n2) with
    | a, b when Ast.Node.equal a b || Run.accept config key ~left:n1 ~right:n2 ->
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
                 zip ~left_line:saved_left_line ~right_line:saved_right_line ~rewritten:true key acc
                   (Run.rewrite config n1) (Run.rewrite config n2) )
    | List l1, List l2 ->
        let l1 = List.filter l1 ~f:(Run.dont_ignore config) in
        let l2 = List.filter l2 ~f:(Run.dont_ignore config) in
        let n1 = List.length l1 in
        let n2 = List.length l2 in
        if Int.equal n1 n2 then
          List.fold2_exn l1 l2 ~f:(zip ~left_line ~right_line ~rewritten:false None) ~init:acc
        else if n1 < n2 then
          let l2_start, l2_end = List.split_n l2 n1 in
          let acc =
            List.fold2_exn l1 l2_start
              ~f:(zip ~left_line ~right_line ~rewritten:false None)
              ~init:acc
          in
          List.fold l2_end ~init:acc ~f:(fun acc node ->
              Diff.append_added_line (Ast.Node.get_node_line_number node) acc )
        else
          let l1_start, l1_end = List.split_n l1 n2 in
          let acc =
            List.fold2_exn l1_start l2
              ~f:(zip ~left_line ~right_line ~rewritten:false None)
              ~init:acc
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
  zip ~left_line:None ~right_line:None ~rewritten:false None [] n1 n2


let ast_diff ~debug ~config ~previous_content ~current_content ast1 ast2 =
  let diffs =
    zip_and_build_diffs config ast1 ast2
    |> Diff.gen_explicit_diffs ~previous_content ~current_content
  in
  if debug then (
    F.printf "AST1: %a\n" Ast.Node.pp ast1 ;
    F.printf "AST2: %a\n" Ast.Node.pp ast2 ;
    F.printf "SemDiff:\n" ;
    List.iter diffs ~f:(fun diff -> F.printf "%a\n" Diff.pp_explicit diff) ) ;
  diffs
