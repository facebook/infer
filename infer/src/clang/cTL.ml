(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Ctl_parser_types
module L = Logging

(* This module defines a language to define checkers. These checkers
   are intepreted over the AST of the program. A checker is defined by a
   CTL formula which express a condition saying when the checker should
    report a problem *)
(* Transition labels used for example to switch from decl to stmt *)
type transitions =
  | Body  (** decl to stmt *)
  | InitExpr  (** decl to stmt *)
  | Super  (** decl to decl *)
  | Parameters  (** decl to decl *)
  | Cond
  | PointerToDecl  (** stmt to decl *)
  | Protocol  (** decl to decl *)

(* In formulas below prefix
   "E" means "exists a path"
   "A" means "for all path" *)

type t =
  (* A ctl formula *)
  | True
  | False
  (* not really necessary but it makes it evaluation faster *)
  | Atomic of CPredicates.t
  | Not of t
  | And of t * t
  | Or of t * t
  | Implies of t * t
  | InNode of ALVar.alexp list * t
  | AX of transitions option * t
  | EX of transitions option * t
  | AF of transitions option * t
  | EF of transitions option * t
  | AG of transitions option * t
  | EG of transitions option * t
  | AU of transitions option * t * t
  | EU of transitions option * t * t
  | EH of ALVar.alexp list * t
  | ET of ALVar.alexp list * transitions option * t
  | ETX of ALVar.alexp list * transitions option * t

let has_transition phi =
  match phi with
  | True
  | False
  | Atomic _
  | Not _
  | And (_, _)
  | Or (_, _)
  | Implies (_, _)
  | InNode (_, _)
  | EH (_, _)
   -> false
  | AX (trans_opt, _)
  | AF (trans_opt, _)
  | AG (trans_opt, _)
  | AU (trans_opt, _, _)
  | EX (trans_opt, _)
  | EF (trans_opt, _)
  | EG (trans_opt, _)
  | EU (trans_opt, _, _)
  | ET (_, trans_opt, _)
  | ETX (_, trans_opt, _)
   -> Option.is_some trans_opt

(* "set" clauses are used for defining mandatory variables that will be used
   by when reporting issues: eg for defining the condition.

   "desc" clauses are used for defining the error message,
   the suggestion, the severity.

   "let" clauses are used to define temporary formulas which are then
   used to abbreviate the another formula. For example

   let f = a And B

   set formula  = f OR f

   set message = "bla"

*)

type clause =
  | CLet of ALVar.formula_id * ALVar.t list * t
  (* Let clause: let id = definifion;  *)
  | CSet of ALVar.keyword * t
  (* Set clause: set id = definition *)
  | CDesc of ALVar.keyword * string
  (* Description clause eg: set message = "..." *)
  | CPath of [`WhitelistPath | `BlacklistPath] * ALVar.t list

type ctl_checker =
  {id: string; (* Checker's id *)
  definitions: clause list (* A list of let/set definitions *)}

type al_file =
  { import_files: string list
  ; global_macros: clause list
  ; global_paths: (string * ALVar.alexp list) list
  ; checkers: ctl_checker list }

let equal_ast_node = Poly.( = )

module Debug = struct
  let pp_transition fmt trans_opt =
    let pp_aux fmt trans =
      match trans with
      | Body
       -> Format.pp_print_string fmt "Body"
      | InitExpr
       -> Format.pp_print_string fmt "InitExpr"
      | Super
       -> Format.pp_print_string fmt "Super"
      | Parameters
       -> Format.pp_print_string fmt "Parameters"
      | Cond
       -> Format.pp_print_string fmt "Cond"
      | Protocol
       -> Format.pp_print_string fmt "Protocol"
      | PointerToDecl
       -> Format.pp_print_string fmt "PointerToDecl"
    in
    match trans_opt with Some trans -> pp_aux fmt trans | None -> Format.pp_print_string fmt "_"

  (* a flag to print more or less in the dotty graph *)
  let full_print = true

  let rec pp_formula fmt phi =
    let nodes_to_string nl = List.map ~f:ALVar.alexp_to_string nl in
    match phi with
    | True
     -> Format.fprintf fmt "True"
    | False
     -> Format.fprintf fmt "False"
    | Atomic p
     -> CPredicates.pp_predicate fmt p
    | Not phi
     -> if full_print then Format.fprintf fmt "NOT(%a)" pp_formula phi
        else Format.fprintf fmt "NOT(...)"
    | And (phi1, phi2)
     -> if full_print then Format.fprintf fmt "(%a AND %a)" pp_formula phi1 pp_formula phi2
        else Format.fprintf fmt "(... AND ...)"
    | Or (phi1, phi2)
     -> if full_print then Format.fprintf fmt "(%a OR %a)" pp_formula phi1 pp_formula phi2
        else Format.fprintf fmt "(... OR ...)"
    | Implies (phi1, phi2)
     -> Format.fprintf fmt "(%a ==> %a)" pp_formula phi1 pp_formula phi2
    | InNode (nl, phi)
     -> Format.fprintf fmt "IN-NODE %a: (%a)" (Pp.comma_seq Format.pp_print_string)
          (nodes_to_string nl) pp_formula phi
    | AX (trs, phi)
     -> Format.fprintf fmt "AX[->%a](%a)" pp_transition trs pp_formula phi
    | EX (trs, phi)
     -> Format.fprintf fmt "EX[->%a](%a)" pp_transition trs pp_formula phi
    | AF (trs, phi)
     -> Format.fprintf fmt "AF[->%a](%a)" pp_transition trs pp_formula phi
    | EF (trs, phi)
     -> Format.fprintf fmt "EF[->%a](%a)" pp_transition trs pp_formula phi
    | AG (trs, phi)
     -> Format.fprintf fmt "AG[->%a](%a)" pp_transition trs pp_formula phi
    | EG (trs, phi)
     -> Format.fprintf fmt "EG[->%a](%a)" pp_transition trs pp_formula phi
    | AU (trs, phi1, phi2)
     -> Format.fprintf fmt "A[->%a][%a UNTIL %a]" pp_transition trs pp_formula phi1 pp_formula phi2
    | EU (trs, phi1, phi2)
     -> Format.fprintf fmt "E[->%a][%a UNTIL %a]" pp_transition trs pp_formula phi1 pp_formula phi2
    | EH (arglist, phi)
     -> Format.fprintf fmt "EH[%a](%a)" (Pp.comma_seq Format.pp_print_string)
          (nodes_to_string arglist) pp_formula phi
    | ET (arglist, trans, phi)
     -> Format.fprintf fmt "ET[%a][%a](%a)" (Pp.comma_seq Format.pp_print_string)
          (nodes_to_string arglist) pp_transition trans pp_formula phi
    | ETX (arglist, trans, phi)
     -> Format.fprintf fmt "ETX[%a][%a](%a)" (Pp.comma_seq Format.pp_print_string)
          (nodes_to_string arglist) pp_transition trans pp_formula phi

  let pp_ast ~ast_node_to_highlight ?(prettifier= Fn.id) fmt root =
    let pp_node_info fmt an =
      let name = Ctl_parser_types.ast_node_name an in
      let typ = Ctl_parser_types.ast_node_type an in
      let cast_kind = Ctl_parser_types.ast_node_cast_kind an in
      Format.fprintf fmt " %s %s %s" name typ cast_kind
    in
    let rec pp_children pp_node wrapper fmt level nodes =
      match nodes with
      | []
       -> ()
      | node :: nodes
       -> pp_node fmt (wrapper node) level "|-" ;
          pp_children pp_node wrapper fmt level nodes
    in
    let rec pp_ast_aux fmt root level prefix =
      let get_node_name (an: ast_node) =
        match an with
        | Stmt stmt
         -> Clang_ast_proj.get_stmt_kind_string stmt
        | Decl decl
         -> Clang_ast_proj.get_decl_kind_string decl
      in
      let should_highlight =
        match (root, ast_node_to_highlight) with
        | Stmt r, Stmt n
         -> phys_equal r n
        | Decl r, Decl n
         -> phys_equal r n
        | _
         -> false
      in
      let node_name =
        let node_name = get_node_name root in
        if should_highlight then prettifier node_name else node_name
      in
      let spaces = String.make (level * String.length prefix) ' ' in
      let next_level = level + 1 in
      Format.fprintf fmt "%s%s%s %a@\n" spaces prefix node_name pp_node_info root ;
      match root with
      | Stmt DeclStmt (_, stmts, ([(VarDecl _)] as var_decl))
       -> (* handling special case of DeclStmt with VarDecl: emit the VarDecl node
              then emit the statements in DeclStmt as children of VarDecl. This is
              because despite being equal, the statements inside VarDecl and those
              inside DeclStmt belong to different instances, hence they fail the
              phys_equal check that should colour them *)
          pp_children pp_ast_aux (fun n -> Decl n) fmt next_level var_decl ;
          pp_stmts fmt (next_level + 1) stmts
      | Stmt stmt
       -> let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
          pp_stmts fmt next_level stmts
      | Decl decl
       -> let decls =
            Clang_ast_proj.get_decl_context_tuple decl |> Option.map ~f:(fun (decls, _) -> decls)
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
      {ast_node: ast_node; phi: t; lcxt: CLintersContext.context; eval_result: eval_result}

    type eval_node = {id: int; content: content}

    type tree = Tree of eval_node * tree list

    type ast_node_to_display =
      (* the node can be used to describe further sub calls in the evaluation stack *)
      | Carry_forward of ast_node
      (* the node cannot be further used to describe sub calls in the evaluation stack *)
      | Last_occurrence of ast_node

    type t =
      { next_id: int
      ; eval_stack: (tree * ast_node_to_display) Stack.t
      ; forest: tree list
      ; breakpoint_line: int option
      ; debugger_active: bool }

    let create_content ast_node phi lcxt = {ast_node; phi; eval_result= Eval_undefined; lcxt}

    let create source_file =
      let breakpoint_token = "INFER_BREAKPOINT" in
      let breakpoint_line =
        In_channel.read_lines (SourceFile.to_abs_path source_file)
        |> List.findi ~f:(fun _ line -> String.is_substring line ~substring:breakpoint_token)
        |> Option.map ~f:(fun (i, _) -> i + 1)
      in
      {next_id= 0; eval_stack= Stack.create (); forest= []; breakpoint_line; debugger_active= false}

    let explain t ~eval_node ~ast_node_to_display =
      let line_number an =
        let line_of_source_range (sr: Clang_ast_t.source_range) =
          let loc_info, _ = sr in
          loc_info.sl_line
        in
        match an with
        | Stmt stmt
         -> let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
            line_of_source_range stmt_info.si_source_range
        | Decl decl
         -> let decl_info = Clang_ast_proj.get_decl_tuple decl in
            line_of_source_range decl_info.di_source_range
      in
      let stop_and_explain_step () =
        let highlight_style =
          match eval_node.content.eval_result with
          | Eval_undefined
           -> ANSITerminal.([Bold])
          | Eval_true
           -> ANSITerminal.([Bold; green])
          | Eval_false
           -> ANSITerminal.([Bold; red])
        in
        let ast_node_to_highlight = eval_node.content.ast_node in
        let ast_root, is_last_occurrence =
          match ast_node_to_display with
          | Carry_forward n
           -> (n, false)
          | Last_occurrence n
           -> (n, true)
        in
        let ast_str =
          Format.asprintf "%a"
            (pp_ast ~ast_node_to_highlight ~prettifier:(ANSITerminal.sprintf highlight_style "%s"))
            ast_root
        in
        L.progress "@\nNode ID: %d\tEvaluation stack level: %d\tSource line-number: %s@\n"
          eval_node.id (Stack.length t.eval_stack)
          (Option.value_map ~default:"Unknown" ~f:string_of_int (line_number ast_node_to_highlight)) ;
        let is_eval_result_undefined =
          match eval_node.content.eval_result with Eval_undefined -> true | _ -> false
        in
        if is_last_occurrence && is_eval_result_undefined then
          L.progress "From this step, a transition to a different part of the AST may follow.@\n" ;
        let phi_str = Format.asprintf "%a" pp_formula eval_node.content.phi in
        L.progress "CTL Formula: %s@\n@\n" phi_str ;
        L.progress "%s@\n" ast_str ;
        let quit_token = "q" in
        L.progress "Press Enter to continue or type %s to quit... @?" quit_token ;
        match In_channel.input_line_exn In_channel.stdin |> String.lowercase with
        | s when String.equal s quit_token
         -> exit 0
        | _
         -> (* Remove the line at the bottom of terminal with the debug instructions *)
            let open ANSITerminal in
            (* move one line up, as current line is the one generated by pressing enter *)
            move_cursor 0 (-1) ; move_bol () ; (* move to the beginning of the line *)
                                               erase Below
        (* erase what follows the cursor's position *)
      in
      match (t.debugger_active, t.breakpoint_line, line_number eval_node.content.ast_node) with
      | false, Some break_point_ln, Some ln when ln >= break_point_ln
       -> L.progress "Attaching debugger at line %d" ln ;
          stop_and_explain_step () ;
          {t with debugger_active= true}
      | true, _, _
       -> stop_and_explain_step () ; t
      | _
       -> t

    let eval_begin t content =
      let node = {id= t.next_id; content} in
      let create_subtree root = Tree (root, []) in
      let subtree' = create_subtree node in
      let ast_node_from_previous_call =
        match Stack.top t.eval_stack with
        | Some (_, Last_occurrence _)
         -> content.ast_node
        | Some (_, Carry_forward an)
         -> an
        | None
         -> content.ast_node
      in
      let ast_node_to_display =
        if has_transition content.phi then Last_occurrence ast_node_from_previous_call
        else Carry_forward ast_node_from_previous_call
      in
      Stack.push t.eval_stack (subtree', ast_node_to_display) ;
      let t' = explain t ~eval_node:node ~ast_node_to_display in
      {t' with next_id= t.next_id + 1}

    let eval_end t result =
      let eval_result_of_bool = function true -> Eval_true | false -> Eval_false in
      if Stack.is_empty t.eval_stack then
        raise (Empty_stack "Unbalanced number of eval_begin/eval_end invocations") ;
      let evaluated_tree, eval_node, ast_node_to_display =
        match Stack.pop_exn t.eval_stack
        with Tree (({id= _; content} as eval_node), children), ast_node_to_display ->
          let content' = {content with eval_result= eval_result_of_bool result} in
          let eval_node' = {eval_node with content= content'} in
          (Tree (eval_node', children), eval_node', ast_node_to_display)
      in
      let t' = explain t ~eval_node ~ast_node_to_display in
      let forest' =
        if Stack.is_empty t'.eval_stack then evaluated_tree :: t'.forest
        else
          let parent =
            match Stack.pop_exn t'.eval_stack
            with Tree (node, children), ntd -> (Tree (node, evaluated_tree :: children), ntd)
          in
          Stack.push t'.eval_stack parent ; t'.forest
      in
      {t' with forest= forest'}

    module DottyPrinter = struct
      let dotty_of_ctl_evaluation t =
        let buffer_content buf =
          let result = Buffer.contents buf in
          Buffer.reset buf ; result
        in
        let dotty_of_tree cluster_id tree =
          let get_root tree =
            match tree
            with Tree (root, _) -> root
          in
          let get_children tree =
            match tree
            with Tree (_, children) -> List.rev children
          in
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
              | Eval_true
               -> "green"
              | Eval_false
               -> "red"
              | _
               -> failwith "Tree is not fully evaluated"
            in
            let label =
              let string_of_lcxt c =
                match c.CLintersContext.et_evaluation_node with
                | Some s
                 -> "et_evaluation_node = " ^ s
                | _
                 -> "et_evaluation_node = NONE"
              in
              let string_of_ast_node an =
                match an with
                | Stmt stmt
                 -> Clang_ast_proj.get_stmt_kind_string stmt
                | Decl decl
                 -> Clang_ast_proj.get_decl_kind_string decl
              in
              let smart_string_of_formula phi =
                let num_children = List.length children in
                match phi with
                | And _ when Int.equal num_children 2
                 -> "(...) AND (...)"
                | Or _ when Int.equal num_children 2
                 -> "(...) OR (...)"
                | Implies _ when Int.equal num_children 2
                 -> "(...) ==> (...)"
                | Not _
                 -> "NOT(...)"
                | _
                 -> Format.asprintf "%a" pp_formula phi
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
            Printf.sprintf "%d [label=\"%s\" shape=box color=%s]\n%s\n" root_node.id label color
              edges
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
end

let print_checker c =
  L.(debug Linters Medium) "@\n-------------------- @\n" ;
  L.(debug Linters Medium) "@\nChecker name: %s@\n" c.id ;
  List.iter
    ~f:(fun d ->
      match d with
      | CSet (keyword, phi)
       -> let cn_str = ALVar.keyword_to_string keyword in
          L.(debug Linters Medium) "    %s=  @\n    %a@\n@\n" cn_str Debug.pp_formula phi
      | CLet (exp, _, phi)
       -> let cn_str = ALVar.formula_id_to_string exp in
          L.(debug Linters Medium) "    %s=  @\n    %a@\n@\n" cn_str Debug.pp_formula phi
      | CDesc (keyword, s)
       -> let cn_str = ALVar.keyword_to_string keyword in
          L.(debug Linters Medium) "    %s=  @\n    %s@\n@\n" cn_str s
      | CPath (paths_keyword, paths)
       -> let keyword =
            match paths_keyword with `WhitelistPath -> "whitelist_path" | _ -> "blacklist_path"
          in
          let paths_str = String.concat ~sep:"," (List.map ~f:ALVar.alexp_to_string paths) in
          L.(debug Linters Medium) "    %s=  @\n    %s@\n@\n" keyword paths_str)
    c.definitions ;
  L.(debug Linters Medium) "@\n-------------------- @\n"

let ctl_evaluation_tracker = ref None

let create_ctl_evaluation_tracker source_file =
  match (Config.linters_developer_mode, !ctl_evaluation_tracker) with
  | true, None
   -> ctl_evaluation_tracker := Some (Debug.EvaluationTracker.create source_file)
  | true, _
   -> failwith "A CTL evaluation tracker has already been created"
  | _
   -> ()

let debug_create_payload ast_node phi lcxt =
  match !ctl_evaluation_tracker with
  | Some _
   -> Some (Debug.EvaluationTracker.create_content ast_node phi lcxt)
  | None
   -> None

let debug_eval_begin payload =
  match (!ctl_evaluation_tracker, payload) with
  | Some tracker, Some payload
   -> ctl_evaluation_tracker := Some (Debug.EvaluationTracker.eval_begin tracker payload)
  | _
   -> ()

let debug_eval_end result =
  match !ctl_evaluation_tracker with
  | Some tracker
   -> ctl_evaluation_tracker := Some (Debug.EvaluationTracker.eval_end tracker result)
  | None
   -> ()

let save_dotty_when_in_debug_mode source_file =
  match !ctl_evaluation_tracker with
  | Some tracker
   -> let dotty_dir = Config.results_dir ^/ Config.lint_dotty_dir_name in
      Utils.create_dir dotty_dir ;
      let source_file_basename = Filename.basename (SourceFile.to_abs_path source_file) in
      let file = dotty_dir ^/ source_file_basename ^ ".dot" in
      let dotty = Debug.EvaluationTracker.DottyPrinter.dotty_of_ctl_evaluation tracker in
      Utils.with_file_out file ~f:(fun oc -> Out_channel.output_string oc dotty)
  | _
   -> ()

(* Helper functions *)

let get_successor_nodes an =
  (* get_decl_of_stmt get declarations that are directly embedded
     as immediate children (i.e. distance 1) of an stmt (i.e., no transition).
     TBD: check if a dual is needed for get_stmt_of_decl
  *)
  let get_decl_of_stmt st =
    match st with Clang_ast_t.BlockExpr (_, _, _, d) -> [Decl d] | _ -> []
  in
  match an with
  | Stmt st
   -> let _, succs_st = Clang_ast_proj.get_stmt_tuple st in
      let succs = List.map ~f:(fun s -> Stmt s) succs_st in
      succs @ get_decl_of_stmt st
  | Decl dec ->
    match Clang_ast_proj.get_decl_context_tuple dec with
    | Some (decl_list, _)
     -> List.map ~f:(fun d -> Decl d) decl_list
    | None
     -> []

let node_to_string an =
  match an with
  | Decl d
   -> Clang_ast_proj.get_decl_kind_string d
  | Stmt s
   -> Clang_ast_proj.get_stmt_kind_string s

let node_to_unique_string_id an =
  match an with
  | Decl d
   -> let di = Clang_ast_proj.get_decl_tuple d in
      Clang_ast_proj.get_decl_kind_string d ^ string_of_int di.Clang_ast_t.di_pointer
  | Stmt s
   -> let si, _ = Clang_ast_proj.get_stmt_tuple s in
      Clang_ast_proj.get_stmt_kind_string s ^ string_of_int si.Clang_ast_t.si_pointer

(* true iff an ast node is a node of type among the list tl *)
let node_has_type tl an =
  let an_alexp = ALVar.Const (node_to_string an) in
  List.mem ~equal:ALVar.equal tl an_alexp

(* given a decl returns a stmt such that decl--->stmt via label trs *)
let transition_decl_to_stmt d trs =
  let open Clang_ast_t in
  let temp_res =
    match (trs, d) with
    | Body, ObjCMethodDecl (_, _, omdi)
     -> omdi.omdi_body
    | Body, FunctionDecl (_, _, _, fdi)
    | Body, CXXMethodDecl (_, _, _, fdi, _)
    | Body, CXXConstructorDecl (_, _, _, fdi, _)
    | Body, CXXConversionDecl (_, _, _, fdi, _)
    | Body, CXXDestructorDecl (_, _, _, fdi, _)
     -> fdi.fdi_body
    | Body, BlockDecl (_, bdi)
     -> bdi.bdi_body
    | InitExpr, VarDecl (_, _, _, vdi)
     -> vdi.vdi_init_expr
    | InitExpr, ObjCIvarDecl (_, _, _, fldi, _)
    | InitExpr, FieldDecl (_, _, _, fldi)
    | InitExpr, ObjCAtDefsFieldDecl (_, _, _, fldi)
     -> fldi.fldi_init_expr
    | InitExpr, CXXMethodDecl _
    | InitExpr, CXXConstructorDecl _
    | InitExpr, CXXConversionDecl _
    | InitExpr, CXXDestructorDecl _
     -> assert false (* to be done. Requires extending to lists *)
    | InitExpr, EnumConstantDecl (_, _, _, ecdi)
     -> ecdi.ecdi_init_expr
    | _, _
     -> None
  in
  match temp_res with Some st -> [Stmt st] | _ -> []

let transition_decl_to_decl_via_super d =
  let decl_opt_to_ast_node_opt d_opt = match d_opt with Some d' -> [Decl d'] | None -> [] in
  let do_ObjCImplementationDecl d =
    match CAst_utils.get_impl_decl_info d with
    | Some idi
     -> decl_opt_to_ast_node_opt (CAst_utils.get_super_ObjCImplementationDecl idi)
    | None
     -> []
  in
  match d with
  | Clang_ast_t.ObjCImplementationDecl _
   -> do_ObjCImplementationDecl d
  | Clang_ast_t.ObjCInterfaceDecl (_, _, _, _, idi)
   -> decl_opt_to_ast_node_opt (CAst_utils.get_decl_opt_with_decl_ref idi.otdi_super)
  | _
   -> []

let transition_decl_to_decl_via_protocol d =
  let open Clang_ast_t in
  let get_nodes dr =
    match CAst_utils.get_decl dr.dr_decl_pointer with Some d -> Some (Decl d) | None -> None
  in
  match d with
  | Clang_ast_t.ObjCProtocolDecl (_, _, _, _, opdi)
   -> List.filter_map ~f:get_nodes opdi.opcdi_protocols
  | _
   -> []

let transition_stmt_to_stmt_via_condition st =
  let open Clang_ast_t in
  match st with
  | IfStmt (_, _ :: _ :: cond :: _)
  | ConditionalOperator (_, cond :: _, _)
  | ForStmt (_, [_; _; cond; _; _])
  | WhileStmt (_, [_; cond; _])
   -> [Stmt cond]
  | _
   -> []

let transition_stmt_to_decl_via_pointer stmt =
  let open Clang_ast_t in
  match stmt with
  | ObjCMessageExpr (_, _, _, obj_c_message_expr_info) -> (
    match CAst_utils.get_decl_opt obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
    | Some decl
     -> [Decl decl]
    | None
     -> [] )
  | DeclRefExpr (_, _, _, decl_ref_expr_info) -> (
    match CAst_utils.get_decl_opt_with_decl_ref decl_ref_expr_info.Clang_ast_t.drti_decl_ref with
    | Some decl
     -> [Decl decl]
    | None
     -> [] )
  | _
   -> []

let transition_decl_to_decl_via_parameters dec =
  let open Clang_ast_t in
  match dec with
  | ObjCMethodDecl (_, _, omdi)
   -> List.map ~f:(fun d -> Decl d) omdi.omdi_parameters
  | _
   -> []

(* given a node an returns a list of nodes an' such that an transition to an' via label trans *)
let next_state_via_transition an trans =
  match (an, trans) with
  | Decl d, Super
   -> transition_decl_to_decl_via_super d
  | Decl d, Parameters
   -> transition_decl_to_decl_via_parameters d
  | Decl d, InitExpr | Decl d, Body
   -> transition_decl_to_stmt d trans
  | Decl d, Protocol
   -> transition_decl_to_decl_via_protocol d
  | Stmt st, Cond
   -> transition_stmt_to_stmt_via_condition st
  | Stmt st, PointerToDecl
   -> transition_stmt_to_decl_via_pointer st
  | _, _
   -> []

(* Evaluation of formulas *)
(* evaluate an atomic formula (i.e. a predicate) on a ast node an and a
   linter context lcxt. That is:  an, lcxt |= pred_name(params) *)
let rec eval_Atomic _pred_name args an lcxt =
  let pred_name = ALVar.formula_id_to_string _pred_name in
  match (pred_name, args, an) with
  | "call_class_method", [c; m], an
   -> CPredicates.call_class_method an c m
  | "call_function", [m], an
   -> CPredicates.call_function an m
  | "call_instance_method", [c; m], an
   -> CPredicates.call_instance_method an c m
  | "call_method", [m], an
   -> CPredicates.call_method an m
  | "captures_cxx_references", [], _
   -> CPredicates.captures_cxx_references an
  | "context_in_synchronized_block", [], _
   -> CPredicates.context_in_synchronized_block lcxt
  | "declaration_has_name", [decl_name], an
   -> CPredicates.declaration_has_name an decl_name
  | "declaration_ref_name", [decl_name], an
   -> CPredicates.declaration_ref_name an decl_name
  | "decl_unavailable_in_supported_ios_sdk", [], an
   -> CPredicates.decl_unavailable_in_supported_ios_sdk lcxt an
  | "class_unavailable_in_supported_ios_sdk", [], an
   -> CPredicates.class_unavailable_in_supported_ios_sdk lcxt an
  | "has_cast_kind", [name], an
   -> CPredicates.has_cast_kind an name
  | "has_type", [typ], an
   -> CPredicates.has_type an typ
  | "isa", [classname], an
   -> CPredicates.isa an classname
  | "is_assign_property", [], an
   -> CPredicates.is_assign_property an
  | "is_binop_with_kind", [kind], an
   -> CPredicates.is_binop_with_kind an kind
  | "is_class", [cname], an
   -> CPredicates.is_class an cname
  | "is_const_var", [], an
   -> CPredicates.is_const_expr_var an
  | "is_enum_constant", [cname], an
   -> CPredicates.is_enum_constant an cname
  | "is_global_var", [], an
   -> CPredicates.is_syntactically_global_var an
  | "is_ivar_atomic", [], an
   -> CPredicates.is_ivar_atomic an
  | "is_method_property_accessor_of_ivar", [], an
   -> CPredicates.is_method_property_accessor_of_ivar an lcxt
  | "is_node", [nodename], an
   -> CPredicates.is_node an nodename
  | "is_objc_constructor", [], _
   -> CPredicates.is_objc_constructor lcxt
  | "is_objc_dealloc", [], _
   -> CPredicates.is_objc_dealloc lcxt
  | "is_objc_extension", [], _
   -> CPredicates.is_objc_extension lcxt
  | "is_objc_interface_named", [name], an
   -> CPredicates.is_objc_interface_named an name
  | "is_property_pointer_type", [], an
   -> CPredicates.is_property_pointer_type an
  | "is_strong_property", [], an
   -> CPredicates.is_strong_property an
  | "is_unop_with_kind", [kind], an
   -> CPredicates.is_unop_with_kind an kind
  | "method_return_type", [typ], an
   -> CPredicates.method_return_type an typ
  | "within_responds_to_selector_block", [], an
   -> CPredicates.within_responds_to_selector_block lcxt an
  | "objc_method_has_nth_parameter_of_type", [num; typ], an
   -> CPredicates.objc_method_has_nth_parameter_of_type an num typ
  | "using_namespace", [namespace], an
   -> CPredicates.using_namespace an namespace
  | "is_at_selector_with_name", [name], an
   -> CPredicates.is_at_selector_with_name an name
  | "has_type_subprotocol_of", [protname], an
   -> CPredicates.has_type_subprotocol_of an protname
  | _
   -> failwith ("ERROR: Undefined Predicate or wrong set of arguments: '" ^ pred_name ^ "'")

(* an, lcxt |= EF phi  <=>
   an, lcxt |= phi or exists an' in Successors(st): an', lcxt |= EF phi

   That is: a (an, lcxt) satifies EF phi if and only if
   either (an,lcxt) satifies phi or there is a child an' of the node an
   such that (an', lcxt) satifies EF phi
*)
and eval_EF phi an lcxt trans =
  match (trans, an) with
  | Some _, _
   -> (* Using equivalence EF[->trans] phi = phi OR EX[->trans](EF[->trans] phi)*)
      let phi' = Or (phi, EX (trans, EF (trans, phi))) in
      eval_formula phi' an lcxt
  | None, _
   -> eval_formula phi an lcxt
      || List.exists ~f:(fun an' -> eval_EF phi an' lcxt trans) (get_successor_nodes an)

(* an, lcxt |= EX phi  <=> exists an' in Successors(st): an', lcxt |= phi

   That is: a (an, lcxt) satifies EX phi if and only if
   there exists is a child an' of the node an
   such that (an', lcxt) satifies phi
*)
and eval_EX phi an lcxt trans =
  let succs =
    match trans with Some l -> next_state_via_transition an l | None -> get_successor_nodes an
  in
  List.exists ~f:(fun an' -> eval_formula phi an' lcxt) succs

(* an, lcxt |= E(phi1 U phi2) evaluated using the equivalence
   an, lcxt |= E(phi1 U phi2) <=> an, lcxt |= phi2 or (phi1 and EX(E(phi1 U phi2)))

   That is: a (an,lcxt) satifies E(phi1 U phi2) if and only if
   an,lcxt satifies the formula phi2 or (phi1 and EX(E(phi1 U phi2)))
*)
and eval_EU phi1 phi2 an lcxt trans =
  let f = Or (phi2, And (phi1, EX (trans, EU (trans, phi1, phi2)))) in
  eval_formula f an lcxt

(* an |= A(phi1 U phi2) evaluated using the equivalence
   an |= A(phi1 U phi2) <=> an |= phi2 or (phi1 and AX(A(phi1 U phi2)))

   Same as EU but for the all path quantifier A
*)
and eval_AU phi1 phi2 an lcxt trans =
  let f = Or (phi2, And (phi1, AX (trans, AU (trans, phi1, phi2)))) in
  eval_formula f an lcxt

(* an, lcxt |= InNode[node_type_list] phi <=>
   an is a node of type in node_type_list and an satifies phi
*)
and in_node node_type_list phi an lctx =
  let holds_for_one_node n =
    match lctx.CLintersContext.et_evaluation_node with
    | Some id
     -> String.equal id (node_to_unique_string_id an) && eval_formula phi an lctx
    | None
     -> node_has_type [n] an && eval_formula phi an lctx
  in
  List.exists ~f:holds_for_one_node node_type_list

(* Intuitive meaning: (an,lcxt) satifies EH[Classes] phi
   if the node an is among the declaration specified by the list Classes and
   there exists a super class in its hierarchy whose declaration satisfy phi.

   an, lcxt |= EH[Classes] phi <=>
   the node an is in Classes and there exists a declaration d in Hierarchy(an)
   such that d,lcxt |= phi *)
and eval_EH classes phi an lcxt =
  (* Define EH[Classes] phi = ET[Classes](EF[->Super] phi) *)
  let f = ET (classes, None, EX (Some Super, EF (Some Super, phi))) in
  eval_formula f an lcxt

(* an, lcxt |= ET[T][->l]phi <=>
   eventually we reach a node an' such that an' is among the types defined in T
   and:

   an'-l->an''
   ("an' transitions" to another node an'' via an edge labelled l)
   and an'',lcxt |= phi

   or l is unspecified and an,lcxt |= phi
*)
and eval_ET tl trs phi an lcxt =
  let f =
    match trs with
    | Some _
     -> EF (None, InNode (tl, EX (trs, phi)))
    | None
     -> EF (None, InNode (tl, phi))
  in
  eval_formula f an lcxt

and eval_ETX tl trs phi an lcxt =
  let lcxt', tl' =
    match (lcxt.CLintersContext.et_evaluation_node, node_has_type tl an) with
    | None, true
     -> let an_alexp = ALVar.Const (node_to_string an) in
        ( {lcxt with CLintersContext.et_evaluation_node= Some (node_to_unique_string_id an)}
        , [an_alexp] )
    | _, _
     -> (lcxt, tl)
  in
  let f =
    match trs with
    | Some _
     -> EF (None, InNode (tl', EX (trs, phi)))
    | None
     -> EF (None, InNode (tl', phi))
  in
  eval_formula f an lcxt'

(* Formulas are evaluated on a AST node an and a linter context lcxt *)
and eval_formula f an lcxt =
  debug_eval_begin (debug_create_payload an f lcxt) ;
  let res =
    match f with
    | True
     -> true
    | False
     -> false
    | Atomic (name, params)
     -> eval_Atomic name params an lcxt
    | Not f1
     -> not (eval_formula f1 an lcxt)
    | And (f1, f2)
     -> eval_formula f1 an lcxt && eval_formula f2 an lcxt
    | Or (f1, f2)
     -> eval_formula f1 an lcxt || eval_formula f2 an lcxt
    | Implies (f1, f2)
     -> not (eval_formula f1 an lcxt) || eval_formula f2 an lcxt
    | InNode (node_type_list, f1)
     -> in_node node_type_list f1 an lcxt
    | AU (trans, f1, f2)
     -> eval_AU f1 f2 an lcxt trans
    | EU (trans, f1, f2)
     -> eval_EU f1 f2 an lcxt trans
    | EF (trans, f1)
     -> eval_EF f1 an lcxt trans
    | AF (trans, f1)
     -> eval_formula (AU (trans, True, f1)) an lcxt
    | AG (trans, f1)
     -> eval_formula (Not (EF (trans, Not f1))) an lcxt
    | EX (trans, f1)
     -> eval_EX f1 an lcxt trans
    | AX (trans, f1)
     -> eval_formula (Not (EX (trans, Not f1))) an lcxt
    | EH (cl, phi)
     -> eval_EH cl phi an lcxt
    | EG (trans, f1)
     -> (* st |= EG f1 <=> st |= f1 /\ EX EG f1 *)
        eval_formula (And (f1, EX (trans, EG (trans, f1)))) an lcxt
    | ET (tl, sw, phi)
     -> eval_ET tl sw phi an lcxt
    | ETX (tl, sw, phi)
     -> eval_ETX tl sw phi an lcxt
  in
  debug_eval_end res ; res
