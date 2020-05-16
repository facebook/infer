(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let pp_ast ~ast_node_to_highlight ?(prettifier = Fn.id) fmt root =
  let open Ctl_parser_types in
  let pp_node_info fmt an =
    let name = Ctl_parser_types.ast_node_name an in
    let typ = Ctl_parser_types.ast_node_type an in
    let cast_kind = Ctl_parser_types.ast_node_cast_kind an in
    Format.fprintf fmt " %s %s %s" name typ cast_kind
  in
  let rec pp_children pp_node wrapper fmt level nodes =
    match nodes with
    | [] ->
        ()
    | node :: nodes ->
        pp_node fmt (wrapper node) level "|-" ;
        pp_children pp_node wrapper fmt level nodes
  in
  let rec pp_ast_aux fmt root level prefix =
    let get_node_name (an : ast_node) =
      match an with
      | Stmt stmt ->
          Clang_ast_proj.get_stmt_kind_string stmt
      | Decl decl ->
          Clang_ast_proj.get_decl_kind_string decl
    in
    let should_highlight =
      match (root, ast_node_to_highlight) with
      | Stmt r, Stmt n ->
          phys_equal r n
      | Decl r, Decl n ->
          phys_equal r n
      | _ ->
          false
    in
    let node_name =
      let node_name = get_node_name root in
      if should_highlight then prettifier node_name else node_name
    in
    let spaces = String.make (level * String.length prefix) ' ' in
    let next_level = level + 1 in
    Format.fprintf fmt "%s%s%s %a@\n" spaces prefix node_name pp_node_info root ;
    match root with
    | Stmt (DeclStmt (_, stmts, ([VarDecl _] as var_decl))) ->
        (* handling special case of DeclStmt with VarDecl: emit the VarDecl node
           then emit the statements in DeclStmt as children of VarDecl. This is
           because despite being equal, the statements inside VarDecl and those
           inside DeclStmt belong to different instances, hence they fail the
           phys_equal check that should colour them *)
        pp_children pp_ast_aux (fun n -> Decl n) fmt next_level var_decl ;
        pp_stmts fmt (next_level + 1) stmts
    | Stmt stmt ->
        let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
        pp_stmts fmt next_level stmts
    | Decl decl ->
        let decls =
          Clang_ast_proj.get_decl_context_tuple decl
          |> Option.map ~f:(fun (decls, _) -> decls)
          |> Option.value ~default:[]
        in
        pp_decls fmt next_level decls
  and pp_stmts fmt level stmts = pp_children pp_ast_aux (fun n -> Stmt n) fmt level stmts
  and pp_decls fmt level decls = pp_children pp_ast_aux (fun n -> Decl n) fmt level decls in
  pp_ast_aux fmt root 0 ""


module EvaluationTracker = struct
  exception Empty_stack of string

  type eval_result = Eval_undefined | Eval_true | Eval_false

  type content =
    { ast_node: Ctl_parser_types.ast_node
    ; phi: CTLTypes.t
    ; lcxt: CLintersContext.context
    ; eval_result: eval_result
    ; witness: Ctl_parser_types.ast_node option }

  type eval_node = {id: int; content: content}

  type tree = Tree of eval_node * tree list

  type ast_node_to_display =
    (* the node can be used to describe further sub calls in the evaluation stack *)
    | Carry_forward of Ctl_parser_types.ast_node
    (* the node cannot be further used to describe sub calls in the evaluation stack *)
    | Last_occurrence of Ctl_parser_types.ast_node

  type t =
    { next_id: int
    ; eval_stack: (tree * ast_node_to_display) Stack.t
    ; forest: tree list
    ; breakpoint_line: int option
    ; debugger_active: bool }

  let create_content ast_node phi lcxt =
    {ast_node; phi; eval_result= Eval_undefined; lcxt; witness= None}


  let create source_file =
    let breakpoint_token = "INFER_BREAKPOINT" in
    let breakpoint_line =
      In_channel.read_lines (SourceFile.to_abs_path source_file)
      |> List.findi ~f:(fun _ line -> String.is_substring line ~substring:breakpoint_token)
      |> Option.map ~f:(fun (i, _) -> i + 1)
    in
    {next_id= 0; eval_stack= Stack.create (); forest= []; breakpoint_line; debugger_active= false}


  let explain t ~eval_node ~ast_node_to_display =
    let open Ctl_parser_types in
    let line_number an =
      let line_of_source_range (sr : Clang_ast_t.source_range) =
        let loc_info, _ = sr in
        loc_info.sl_line
      in
      match an with
      | Stmt stmt ->
          let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
          line_of_source_range stmt_info.si_source_range
      | Decl decl ->
          let decl_info = Clang_ast_proj.get_decl_tuple decl in
          line_of_source_range decl_info.di_source_range
    in
    let stop_and_explain_step () =
      let highlight_style =
        match eval_node.content.eval_result with
        | Eval_undefined ->
            ANSITerminal.[Bold]
        | Eval_true ->
            ANSITerminal.[Bold; green]
        | Eval_false ->
            ANSITerminal.[Bold; red]
      in
      let ast_node_to_highlight = eval_node.content.ast_node in
      let ast_root, is_last_occurrence =
        match ast_node_to_display with
        | Carry_forward n ->
            (n, false)
        | Last_occurrence n ->
            (n, true)
      in
      let witness_str =
        match eval_node.content.witness with
        | Some witness ->
            "\n- witness: "
            ^ Ctl_parser_types.ast_node_kind witness
            ^ " "
            ^ Ctl_parser_types.ast_node_name witness
        | None ->
            ""
      in
      let ast_str =
        Format.asprintf "%a %s"
          (pp_ast ~ast_node_to_highlight ~prettifier:(ANSITerminal.sprintf highlight_style "%s"))
          ast_root witness_str
      in
      L.progress "@\nNode ID: %d\tEvaluation stack level: %d\tSource line-number: %s@\n"
        eval_node.id (Stack.length t.eval_stack)
        (Option.value_map ~default:"Unknown" ~f:string_of_int (line_number ast_node_to_highlight)) ;
      let is_eval_result_undefined =
        match eval_node.content.eval_result with Eval_undefined -> true | _ -> false
      in
      if is_last_occurrence && is_eval_result_undefined then
        L.progress "From this step, a transition to a different part of the AST may follow.@\n" ;
      let phi_str = Format.asprintf "%a" CTLTypes.pp_formula eval_node.content.phi in
      L.progress "CTL Formula: %s@\n@\n" phi_str ;
      L.progress "%s@\n" ast_str ;
      let quit_token = "q" in
      L.progress "Press Enter to continue or type %s to quit... @?" quit_token ;
      match In_channel.input_line_exn In_channel.stdin |> String.lowercase with
      | s when String.equal s quit_token ->
          L.exit 0
      | _ ->
          (* Remove the line at the bottom of terminal with the debug instructions *)
          let open ANSITerminal in
          (* move one line up, as current line is the one generated by pressing enter *)
          move_cursor 0 (-1) ;
          move_bol () ;
          (* move to the beginning of the line *)
          erase Below
      (* erase what follows the cursor's position *)
    in
    match (t.debugger_active, t.breakpoint_line, line_number eval_node.content.ast_node) with
    | false, Some break_point_ln, Some ln when ln >= break_point_ln ->
        L.progress "Attaching debugger at line %d" ln ;
        stop_and_explain_step () ;
        {t with debugger_active= true}
    | true, _, _ ->
        stop_and_explain_step () ;
        t
    | _ ->
        t


  let eval_begin t content =
    let node = {id= t.next_id; content} in
    let create_subtree root = Tree (root, []) in
    let subtree' = create_subtree node in
    let ast_node_from_previous_call =
      match Stack.top t.eval_stack with
      | Some (_, Last_occurrence _) ->
          content.ast_node
      | Some (_, Carry_forward an) ->
          an
      | None ->
          content.ast_node
    in
    let ast_node_to_display =
      if CTLTypes.has_transition content.phi then Last_occurrence ast_node_from_previous_call
      else Carry_forward ast_node_from_previous_call
    in
    Stack.push t.eval_stack (subtree', ast_node_to_display) ;
    let t' = explain t ~eval_node:node ~ast_node_to_display in
    {t' with next_id= t.next_id + 1}


  let eval_end t result =
    let result_bool = Option.is_some result in
    let eval_result_of_bool = function true -> Eval_true | false -> Eval_false in
    if Stack.is_empty t.eval_stack then
      raise (Empty_stack "Unbalanced number of eval_begin/eval_end invocations") ;
    let evaluated_tree, eval_node, ast_node_to_display =
      match Stack.pop_exn t.eval_stack with
      | Tree (({id= _; content} as eval_node), children), ast_node_to_display ->
          let content' =
            {content with eval_result= eval_result_of_bool result_bool; witness= result}
          in
          let eval_node' = {eval_node with content= content'} in
          (Tree (eval_node', children), eval_node', ast_node_to_display)
    in
    let t' = explain t ~eval_node ~ast_node_to_display in
    let forest' =
      if Stack.is_empty t'.eval_stack then evaluated_tree :: t'.forest
      else
        let parent =
          match Stack.pop_exn t'.eval_stack with
          | Tree (node, children), ntd ->
              (Tree (node, evaluated_tree :: children), ntd)
        in
        Stack.push t'.eval_stack parent ;
        t'.forest
    in
    {t' with forest= forest'}


  let equal_ast_node = Poly.equal

  module DottyPrinter = struct
    let dotty_of_ctl_evaluation t =
      let open CTLTypes in
      let open Ctl_parser_types in
      let buffer_content buf =
        let result = Buffer.contents buf in
        Buffer.reset buf ;
        result
      in
      let dotty_of_tree cluster_id tree =
        let get_root tree = match tree with Tree (root, _) -> root in
        let get_children tree = match tree with Tree (_, children) -> List.rev children in
        (* shallow: emit dotty about root node and edges to its children *)
        let shallow_dotty_of_tree tree =
          let root_node = get_root tree in
          let children = get_children tree in
          let edge child_node =
            if equal_ast_node root_node.content.ast_node child_node.content.ast_node then
              Printf.sprintf "%d -> %d [style=dotted]" root_node.id child_node.id
            else Printf.sprintf "%d -> %d [style=bold]" root_node.id child_node.id
          in
          let color =
            match root_node.content.eval_result with
            | Eval_true ->
                "green"
            | Eval_false ->
                "red"
            | _ ->
                L.(die InternalError) "Tree is not fully evaluated"
          in
          let label =
            let string_of_lcxt c =
              match c.CLintersContext.et_evaluation_node with
              | Some s ->
                  "et_evaluation_node = " ^ s
              | _ ->
                  "et_evaluation_node = NONE"
            in
            let string_of_ast_node an =
              match an with
              | Stmt stmt ->
                  Clang_ast_proj.get_stmt_kind_string stmt
              | Decl decl ->
                  Clang_ast_proj.get_decl_kind_string decl
            in
            let smart_string_of_formula phi =
              let num_children = List.length children in
              match phi with
              | And _ when Int.equal num_children 2 ->
                  "(...) AND (...)"
              | Or _ when Int.equal num_children 2 ->
                  "(...) OR (...)"
              | Implies _ when Int.equal num_children 2 ->
                  "(...) ==> (...)"
              | Not _ ->
                  "NOT(...)"
              | _ ->
                  Format.asprintf "%a" CTLTypes.pp_formula phi
            in
            Format.sprintf "(%d)\\n%s\\n%s\\n%s" root_node.id
              (Escape.escape_dotty (string_of_ast_node root_node.content.ast_node))
              (Escape.escape_dotty (string_of_lcxt root_node.content.lcxt))
              (Escape.escape_dotty (smart_string_of_formula root_node.content.phi))
          in
          let edges =
            let buf = Buffer.create 16 in
            List.iter
              ~f:(fun subtree -> Buffer.add_string buf (edge (get_root subtree) ^ "\n"))
              children ;
            buffer_content buf
          in
          Printf.sprintf "%d [label=\"%s\" shape=box color=%s]\n%s\n" root_node.id label color edges
        in
        let rec traverse buf tree =
          Buffer.add_string buf (shallow_dotty_of_tree tree) ;
          List.iter ~f:(traverse buf) (get_children tree)
        in
        let buf = Buffer.create 16 in
        traverse buf tree ;
        Printf.sprintf "subgraph cluster_%d {\n%s\n}" cluster_id (buffer_content buf)
      in
      let buf = Buffer.create 16 in
      List.iteri
        ~f:(fun cluster_id tree -> Buffer.add_string buf (dotty_of_tree cluster_id tree ^ "\n"))
        (List.rev t.forest) ;
      Printf.sprintf "digraph CTL_Evaluation {\n%s\n}\n" (buffer_content buf)
  end
end
