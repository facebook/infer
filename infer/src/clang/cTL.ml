(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open CFrontend_utils

(* This module defines a language to define checkers. These checkers
   are intepreted over the AST of the program. A checker is defined by a
   CTL formula which express a condition saying when the checker should
    report a problem *)


(* Label that allows switching from a decl node to a stmt node *)
type transition_decl_to_stmt =
  | Body
  | InitExpr

(* In formulas below prefix
   "E" means "exists a path"
   "A" means "for all path" *)

type t = (* A ctl formula *)
  | True
  | False (* not really necessary but it makes it evaluation faster *)
  | Atomic of Predicates.t
  | Not of t
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | AX of t
  | EX of t
  | AF of t
  | EF of t
  | AG of t
  | EG of t
  | AU of t * t
  | EU of t * t
  | EH of string list * t
  | ET of string list * transition_decl_to_stmt option * t

(* the kind of AST nodes where formulas are evaluated *)
type ast_node =
  | Stmt of Clang_ast_t.stmt
  | Decl of Clang_ast_t.decl

module Debug = struct
  let pp_transition_decl_to_stmt fmt trans_opt =
    let pp_aux fmt trans = match trans with
      | Body -> Format.pp_print_string fmt "Body"
      | InitExpr -> Format.pp_print_string fmt "InitExpr" in
    match trans_opt with
    | Some trans -> pp_aux fmt trans
    | None -> Format.pp_print_string fmt "None"

  let rec pp_formula fmt phi =
    match phi with
    | True -> Format.fprintf fmt "True"
    | False -> Format.fprintf fmt "False"
    | Atomic p -> Predicates.pp_predicate fmt p
    | Not phi -> Format.fprintf fmt "NOT(%a)" pp_formula phi
    | And (phi1, phi2) -> Format.fprintf fmt "(%a AND %a)" pp_formula phi1 pp_formula phi2
    | Or (phi1, phi2) -> Format.fprintf fmt "(%a OR %a)" pp_formula phi1 pp_formula phi2
    | Implies (phi1, phi2) -> Format.fprintf fmt "(%a ==> %a)" pp_formula phi1 pp_formula phi2
    | AX phi -> Format.fprintf fmt "AX(%a)" pp_formula phi
    | EX phi -> Format.fprintf fmt "EX(%a)" pp_formula phi
    | AF phi -> Format.fprintf fmt "AF(%a)" pp_formula phi
    | EF phi -> Format.fprintf fmt "EF(%a)" pp_formula phi
    | AG phi -> Format.fprintf fmt "AG(%a)" pp_formula phi
    | EG phi -> Format.fprintf fmt "EG(%a)" pp_formula phi
    | AU (phi1, phi2) -> Format.fprintf fmt "A[%a UNTIL %a]" pp_formula phi1 pp_formula phi2
    | EU (phi1, phi2) -> Format.fprintf fmt "E[%a UNTIL %a]" pp_formula phi1 pp_formula phi2
    | EH (arglist, phi) -> Format.fprintf fmt "EH[%a](%a)"
                             (Utils.pp_comma_seq Format.pp_print_string) arglist
                             pp_formula phi
    | ET (arglist, trans, phi) -> Format.fprintf fmt "ET[%a][%a](%a)"
                                    (Utils.pp_comma_seq Format.pp_print_string) arglist
                                    pp_transition_decl_to_stmt trans
                                    pp_formula phi

  module EvaluationTracker = struct
    exception Empty_stack of string

    type eval_result = Eval_undefined | Eval_true | Eval_false

    type content = {
      ast_node: ast_node;
      phi: t;
      eval_result: eval_result;
    }

    type node = {
      id: int;
      content: content;
    }

    type tree = Tree of node * (tree list)

    type t = {
      next_id: int;
      eval_stack: tree Stack.t;
      forest: tree list;
    }

    let create_content ast_node phi = {ast_node; phi; eval_result = Eval_undefined}

    let create () = {next_id = 0; eval_stack = Stack.create(); forest = []}

    let eval_begin t content =
      let node = {id = t.next_id; content} in
      let create_subtree root = Tree (root, []) in
      let subtree' = create_subtree node in
      Stack.push subtree' t.eval_stack;
      {t with next_id = t.next_id + 1}

    let eval_end t result =
      let eval_result_of_bool = function
        | true -> Eval_true
        | false -> Eval_false in
      if Stack.is_empty t.eval_stack then
        raise (Empty_stack "Unbalanced number of eval_begin/eval_end invocations");
      let evaluated_tree = match Stack.pop t.eval_stack with
        | Tree ({id = _; content} as node, children) ->
            let content' = {content with eval_result = eval_result_of_bool result} in
            Tree ({node with content = content'}, children) in
      let forest' =
        if Stack.is_empty t.eval_stack then evaluated_tree :: t.forest
        else
          let parent = match Stack.pop t.eval_stack with
              Tree (node, children) -> Tree (node, evaluated_tree :: children) in
          Stack.push parent t.eval_stack;
          t.forest in
      {t with forest = forest'}

    module DottyPrinter = struct
      let dotty_of_ctl_evaluation t =
        let buffer_content buf =
          let result = Buffer.contents buf in
          Buffer.reset buf;
          result in
        let dotty_of_tree cluster_id tree =
          let get_root tree = match tree with Tree (root, _) -> root in
          let get_children tree = match tree with Tree (_, children) -> IList.rev children in
          (* shallow: emit dotty about root node and edges to its children *)
          let shallow_dotty_of_tree tree =
            let root_node = get_root tree in
            let children = get_children tree in
            let edge child_node =
              if root_node.content.ast_node = child_node.content.ast_node then
                Printf.sprintf "%d -> %d [style=dotted]" root_node.id child_node.id
              else
                Printf.sprintf "%d -> %d [style=bold]" root_node.id child_node.id in
            let color =
              match root_node.content.eval_result with
              | Eval_true -> "green"
              | Eval_false -> "red"
              | _ -> failwith "Tree is not fully evaluated" in
            let label =
              let string_of_ast_node an =
                match an with
                | Stmt stmt -> Clang_ast_proj.get_stmt_kind_string stmt
                | Decl decl -> Clang_ast_proj.get_decl_kind_string decl in
              let smart_string_of_formula phi =
                let num_children = IList.length children in
                match phi with
                | And _ when num_children = 2 -> "(...) AND (...)"
                | Or _ when num_children = 2 -> "(...) OR (...)"
                | Implies _ when num_children = 2 -> "(...) ==> (...)"
                | Not _ -> "NOT(...)"
                | _ -> Utils.pp_to_string pp_formula phi in
              Format.sprintf "(%d)\\n%s\\n%s"
                root_node.id
                (Escape.escape_dotty (string_of_ast_node root_node.content.ast_node))
                (Escape.escape_dotty (smart_string_of_formula root_node.content.phi)) in
            let edges =
              let buf = Buffer.create 16 in
              IList.iter
                (fun subtree -> Buffer.add_string buf ((edge (get_root subtree)) ^ "\n"))
                children;
              buffer_content buf in
            Printf.sprintf "%d [label=\"%s\" shape=box color=%s]\n%s\n"
              root_node.id label color edges in
          let rec traverse buf tree =
            Buffer.add_string buf (shallow_dotty_of_tree tree);
            IList.iter (traverse buf) (get_children tree) in
          let buf = Buffer.create 16 in
          traverse buf tree;
          Printf.sprintf "subgraph cluster_%d {\n%s\n}" cluster_id (buffer_content buf) in
        let buf = Buffer.create 16 in
        IList.iteri
          (fun cluster_id tree -> Buffer.add_string buf ((dotty_of_tree cluster_id tree) ^ "\n"))
          (IList.rev t.forest);
        Printf.sprintf "digraph CTL_Evaluation {\n%s\n}\n" (buffer_content buf)
    end
  end
end

let ctl_evaluation_tracker = match Config.debug_mode with
  | true -> Some (ref (Debug.EvaluationTracker.create ()))
  | false -> None

let debug_create_payload ast_node phi =
  match ctl_evaluation_tracker with
  | Some _ -> Some (Debug.EvaluationTracker.create_content ast_node phi)
  | None -> None

let debug_eval_begin payload =
  match ctl_evaluation_tracker, payload with
  | Some tracker, Some payload ->
      tracker := Debug.EvaluationTracker.eval_begin !tracker payload
  | _ -> ()

let debug_eval_end result =
  match ctl_evaluation_tracker with
  | Some tracker ->
      tracker := Debug.EvaluationTracker.eval_end !tracker result
  | None -> ()

let save_dotty_when_in_debug_mode source_file =
  match ctl_evaluation_tracker with
  | Some tracker ->
      let dotty_dir = Config.results_dir // Config.lint_dotty_dir_name in
      create_dir dotty_dir;
      let source_file_basename = Filename.basename (DB.source_file_to_string source_file) in
      let file = dotty_dir // (source_file_basename ^ ".dot") in
      let dotty = Debug.EvaluationTracker.DottyPrinter.dotty_of_ctl_evaluation !tracker in
      with_file file ~f:(fun oc -> output_string oc dotty)
  | _ -> ()

(* Helper functions *)

(* true iff an ast node is a node of type among the list tl *)
let node_has_type tl an =
  let an_str = match an with
    | Decl d -> Clang_ast_proj.get_decl_kind_string d
    | Stmt s -> Clang_ast_proj.get_stmt_kind_string s in
  IList.mem (string_equal) an_str tl

(* given a decl returns a stmt such that decl--->stmt via label trs *)
let transition_from_decl_to_stmt d trs =
  let open Clang_ast_t in
  match trs, d with
  | Some Body, ObjCMethodDecl (_, _, omdi) -> omdi.omdi_body
  | Some Body, FunctionDecl (_, _, _, fdi)
  | Some Body, CXXMethodDecl (_, _, _, fdi,_ )
  | Some Body, CXXConstructorDecl (_, _, _, fdi, _)
  | Some Body, CXXConversionDecl (_, _, _, fdi, _)
  | Some Body, CXXDestructorDecl (_, _, _, fdi, _) -> fdi.fdi_body
  | Some Body, BlockDecl (_, bdi) -> bdi.bdi_body
  | Some InitExpr, VarDecl (_, _ ,_, vdi) -> vdi.vdi_init_expr
  | Some InitExpr, ObjCIvarDecl (_, _, _, fldi, _)
  | Some InitExpr, FieldDecl (_, _, _, fldi)
  | Some InitExpr, ObjCAtDefsFieldDecl (_, _, _, fldi)-> fldi.fldi_init_expr
  | Some InitExpr, CXXMethodDecl _
  | Some InitExpr, CXXConstructorDecl _
  | Some InitExpr, CXXConversionDecl _
  | Some InitExpr, CXXDestructorDecl _ ->
      assert false (* to be done. Requires extending to lists *)
  | Some InitExpr, EnumConstantDecl (_, _, _, ecdi) -> ecdi.ecdi_init_expr
  | _, _ -> None

(* given a stmt returns a decl such that stmt--->decl via label trs
   NOTE: for the moment we don't have any transitions stmt to decl as
   we don't have much experience.
   TBD: the list need to be populated when we know what we need *)
let transition_from_stmt_to_decl st trs =
  match trs, st with
  | _, _ -> None  (* For the moment always no transitions. TBD add transitions *)


(* Evaluation of formulas *)

(* evaluate an atomic formula (i.e. a predicate) on a ast node an and a
   linter context lcxt. That is:  an, lcxt |= pred_name(params) *)
let eval_Atomic pred_name params an lcxt =
  match pred_name, params, an with
  | "call_method", [p1], Stmt st -> Predicates.call_method p1 st
  | "property_name_contains_word", [p1] , Decl d -> Predicates.property_name_contains_word d p1
  | "is_objc_extension", [], _ -> Predicates.is_objc_extension lcxt
  | "is_global_var", [], Decl d -> Predicates.is_syntactically_global_var d
  | "is_const_var", [], Decl d ->  Predicates.is_const_expr_var d
  | "call_function_named", _, Stmt st -> Predicates.call_function_named st params
  | "is_statement_kind", [p1], Stmt st -> Predicates.is_statement_kind st p1
  | "is_declaration_kind", [p1], Decl d -> Predicates.is_declaration_kind d p1
  | "is_strong_property", [], Decl d -> Predicates.is_strong_property d
  | "is_assign_property", [], Decl d -> Predicates.is_assign_property d
  | "is_property_pointer_type", [], Decl d -> Predicates.is_property_pointer_type d
  | "context_in_synchronized_block", [], _ -> Predicates.context_in_synchronized_block lcxt
  | "is_ivar_atomic", [], Stmt st -> Predicates.is_ivar_atomic st
  | "is_method_property_accessor_of_ivar", [], Stmt st ->
      Predicates.is_method_property_accessor_of_ivar st lcxt
  | "is_objc_constructor", [], _ -> Predicates.is_objc_constructor lcxt
  | "is_objc_dealloc", [], _ -> Predicates.is_objc_dealloc lcxt
  | "captures_cxx_references", [], Stmt st -> Predicates.captures_cxx_references st
  | "is_binop_with_kind", [kind], Stmt st -> Predicates.is_binop_with_kind st kind
  | "is_unop_with_kind", [kind], Stmt st -> Predicates.is_unop_with_kind st kind
  | "is_stmt", [stmt_name], Stmt st -> Predicates.is_stmt st stmt_name
  | "isa", [classname], Stmt st -> Predicates.isa st classname
  | _ -> failwith ("ERROR: Undefined Predicate: "^pred_name)

(* st, lcxt |= EF phi  <=>
   st, lcxt |= phi or exists st' in Successors(st): st', lcxt |= EF phi

   That is: a (st, lcxt) satifies EF phi if and only if
   either (st,lcxt) satifies phi or there is a child st' of the node st
   such that (st', lcxt) satifies EF phi
*)
let rec eval_EF_st phi st lcxt =
  let _, succs = Clang_ast_proj.get_stmt_tuple st in
  eval_formula phi (Stmt st) lcxt
  || IList.exists (fun s -> eval_EF phi (Stmt s) lcxt) succs


(* dec, lcxt |= EF phi  <=>
    dec, lcxt |= phi or exists dec' in Successors(dec): dec', lcxt |= EF phi

   This is as eval_EF_st but for decl.
*)
and eval_EF_decl phi dec lcxt =
  eval_formula phi (Decl dec) lcxt ||
  (match Clang_ast_proj.get_decl_context_tuple dec with
   | Some (decl_list, _) ->
       IList.exists (fun d -> eval_EF phi (Decl d) lcxt) decl_list
   | None -> false)

(* an, lcxt |= EF phi  evaluates on decl or stmt depending on an *)
and eval_EF phi an lcxt =
  match an with
  | Stmt st -> eval_EF_st phi st lcxt
  | Decl dec -> eval_EF_decl phi dec lcxt

(* st, lcxt |= EX phi  <=> exists st' in Successors(st): st', lcxt |= phi

   That is: a (st, lcxt) satifies EX phi if and only if
   there exists is a child st' of the node st
   such that (st', lcxt) satifies phi
*)
and eval_EX_st phi st lcxt =
  let _, succs = Clang_ast_proj.get_stmt_tuple st in
  IList.exists (fun s -> eval_formula phi (Stmt s) lcxt) succs

(* dec, lcxt |= EX phi  <=> exists dec' in Successors(dec): dec',lcxt|= phi

   Same as eval_EX_st but for decl.
*)
and eval_EX_decl phi dec lcxt =
  match Clang_ast_proj.get_decl_context_tuple dec with
  | Some (decl_list, _) ->
      IList.exists (fun d -> eval_formula phi (Decl d) lcxt) decl_list
  | None -> false

(* an |= EX phi evaluates on decl/stmt depending on the ast_node an *)
and eval_EX phi an lcxt =
  match an with
  | Stmt st -> eval_EX_st phi st lcxt
  | Decl decl -> eval_EX_decl phi decl lcxt

(* an, lcxt |= E(phi1 U phi2) evaluated using the equivalence
   an, lcxt |= E(phi1 U phi2) <=> an, lcxt |= phi2 or (phi1 and EX(E(phi1 U phi2)))

   That is: a (an,lcxt) satifies E(phi1 U phi2) if and only if
   an,lcxt satifies the formula phi2 or (phi1 and EX(E(phi1 U phi2)))
*)
and eval_EU phi1 phi2 an lcxt =
  let f = Or (phi2, And (phi1, EX (EU (phi1, phi2)))) in
  eval_formula f an lcxt

(* an |= A(phi1 U phi2) evaluated using the equivalence
   an |= A(phi1 U phi2) <=> an |= phi2 or (phi1 and AX(A(phi1 U phi2)))

   Same as EU but for the all path quantifier A
*)
and eval_AU phi1 phi2 an lcxt =
  let f = Or (phi2, And (phi1, AX (AU (phi1, phi2)))) in
  eval_formula f an lcxt

(* Intuitive meaning: (an,lcxt) satifies EH[Classes] phi
   if the node an is among the declaration specified by the list Classes and
   there exists a super class in its hierarchy whose declaration satisfy phi.

   an, lcxt |= EH[Classes] phi <=>
   the node an is in Classes and there exists a declaration d in Hierarchy(an)
   such that d,lcxt |= phi *)
and eval_EH classes phi an lcxt =
  let rec eval_super impl_decl_info =
    match impl_decl_info with
    | Some idi ->
        (match Ast_utils.get_super_ObjCImplementationDecl idi with
         | Some (Clang_ast_t.ObjCImplementationDecl(_, _, _, _, idi') as d) ->
             eval_formula phi (Decl d) lcxt || eval_super (Some idi')
         | _ -> false)
    | None -> false in
  match an with
  | Decl d when node_has_type classes (Decl d) ->
      eval_super (Ast_utils.get_impl_decl_info d)
  | _ -> false

(* an, lcxt |= ET[T][->l]phi <=>
   an is a node among those defined in T and an-l->an'
   ("an transitions" to another node an' via an edge labelled l)
   such that an',lcxt |= phi

   or an is a node among those defined in T, and l is unspecified,
   and an,lcxt |= phi

   or an is not of type in T and exists an' in Successors(an):
      an', lcxt |= ET[T][->l]phi
*)
and eval_ET tl trs phi an lcxt =
  let open Clang_ast_t in
  let evaluate_on_subdeclarations d eval =
    match Clang_ast_proj.get_decl_context_tuple d with
    | None -> false
    | Some (decl_list, _) ->
        IList.exists (fun d' -> eval phi (Decl d') lcxt) decl_list in
  let evaluate_on_substmt st eval =
    let _, stmt_list = Clang_ast_proj.get_stmt_tuple st in
    IList.exists (fun s -> eval phi (Stmt s) lcxt) stmt_list in
  let do_decl d =
    match trs, node_has_type tl (Decl d) with
    | Some _, true ->
        (match transition_from_decl_to_stmt d trs with
         | None -> false
         | Some st -> eval_formula phi (Stmt st) lcxt)
    | None, true -> eval_formula phi (Decl d) lcxt
    | _, false -> evaluate_on_subdeclarations d (eval_ET tl trs) in
  let do_stmt st =
    match trs, node_has_type tl (Stmt st) with
    | Some _, true ->
        (match transition_from_stmt_to_decl st trs with
         | None -> false
         | Some d -> eval_formula phi (Decl d) lcxt)
    | None, true -> eval_formula phi (Stmt st) lcxt
    | _, false -> evaluate_on_substmt st (eval_ET tl trs) in
  match an with
  | Decl d -> do_decl d
  | Stmt BlockExpr(_, _, _, d) ->
      (* From BlockExpr we jump directly to its BlockDecl *)
      eval_ET tl trs phi (Decl d) lcxt
  | Stmt st -> do_stmt st

(* Formulas are evaluated on a AST node an and a linter context lcxt *)
and eval_formula f an lcxt =
  debug_eval_begin (debug_create_payload an f);
  let res = match f with
    | True -> true
    | False -> false
    | Atomic (name, params) -> eval_Atomic name params an lcxt
    | Not f1 -> not (eval_formula f1 an lcxt)
    | And (f1, f2) -> (eval_formula f1 an lcxt) && (eval_formula f2 an lcxt)
    | Or (f1, f2) -> (eval_formula f1 an lcxt) || (eval_formula f2 an lcxt)
    | Implies (f1, f2) ->
        not (eval_formula f1 an lcxt) || (eval_formula f2 an lcxt)
    | AU (f1, f2) -> eval_AU f1 f2 an lcxt
    | EU (f1, f2) -> eval_EU f1 f2 an lcxt
    | EF f1 -> eval_EF f1 an lcxt
    | AF f1 -> eval_formula (AU (True, f1)) an lcxt
    | AG f1 -> eval_formula (Not (EF (Not f1))) an lcxt
    | EX f1 -> eval_EX f1 an lcxt
    | AX f1 -> eval_formula (Not (EX (Not f1))) an lcxt
    | EH (cl, phi) -> eval_EH cl phi an lcxt
    | EG f1 -> (* st |= EG f1 <=> st |= f1 /\ EX EG f1 *)
        eval_formula (And (f1, EX (EG (f1)))) an lcxt
    | ET (tl, sw, phi) -> eval_ET tl sw phi an lcxt in
  debug_eval_end res;
  res
