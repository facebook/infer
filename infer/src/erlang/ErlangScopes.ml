(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Ast = ErlangAst
module Env = ErlangEnvironment
module L = Logging

(* One frame in the stack of scopes *)
type scope = {procname: Procname.t; locals: String.Set.t; captured: Pvar.Set.t}

(* Data structure we pass around for giving unique names to lambdas (per function).
   Initial plan was to use line+column, but if a macro expands to multiple lambdas,
   that is not unique. *)
type lambda_name_counter = {funcname: string; mutable counter: int}

let lookup_var (scopes : scope list) (vname : string) : Procname.t option =
  List.find_map scopes ~f:(fun {procname; locals} ->
      if String.Set.mem locals vname then Some procname else None )


let push_scope =
  let warned = ref false in
  fun scopes procname ->
    if (not !warned) && List.length scopes > 100 then (
      L.debug Capture Verbose "@[Many nested scopes: translation might be slow@." ;
      warned := true ) ;
    {procname; locals= String.Set.empty; captured= Pvar.Set.empty} :: scopes


let pop_scope scopes =
  match scopes with hd :: tl -> (hd, tl) | [] -> L.die InternalError "Cannot pop scope."


let top_scope scopes = match scopes with hd :: _ -> hd | [] -> L.die InternalError "No top scope."

let assert_empty scopes =
  if not (List.is_empty scopes) then L.die InternalError "Expected empty stack of scopes."


let create_unique_lambda_name (lambda_cntr : lambda_name_counter) lambda_name_opt =
  let lambda_name =
    match lambda_name_opt with Some name -> Printf.sprintf "_%s" name | None -> ""
  in
  let name =
    Printf.sprintf "anon_fun%s_%d_of_%s" lambda_name lambda_cntr.counter lambda_cntr.funcname
  in
  lambda_cntr.counter <- lambda_cntr.counter + 1 ;
  name


let rec annotate_expression (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    ({simple_expression; _} : Ast.expression) =
  (* Most constructs just do the traversal recursively.
     We only have to treat lambdas and variables in a special way. *)
  match simple_expression with
  | BinaryOperator (e1, _, e2) ->
      annotate_expression_list env lambda_cntr scopes [e1; e2]
  | BitstringConstructor elems ->
      List.fold_left
        ~f:(fun scopes (e : Ast.bin_element) ->
          let scopes = annotate_expression env lambda_cntr scopes e.expression in
          annotate_expression_opt env lambda_cntr scopes e.size )
        ~init:scopes elems
  | Block body ->
      annotate_expression_list env lambda_cntr scopes body
  | Call {module_; function_; args} ->
      let scopes = annotate_expression_opt env lambda_cntr scopes module_ in
      let scopes = annotate_expression env lambda_cntr scopes function_ in
      annotate_expression_list env lambda_cntr scopes args
  | Case {expression; cases} ->
      let scopes = annotate_expression env lambda_cntr scopes expression in
      annotate_clauses env lambda_cntr scopes cases
  | Catch expression ->
      annotate_expression env lambda_cntr scopes expression
  | Cons {head; tail} ->
      annotate_expression_list env lambda_cntr scopes [head; tail]
  | If clauses ->
      annotate_clauses env lambda_cntr scopes clauses
  | ListComprehension {expression; qualifiers} | BitstringComprehension {expression; qualifiers} ->
      (* TODO: support local variables in list/bitstring comprehensions: T105967634 *)
      let scopes = annotate_expression env lambda_cntr scopes expression in
      List.fold_left ~f:(annotate_qualifier env lambda_cntr) ~init:scopes qualifiers
  | Literal _ | Nil | RecordIndex _ | Fun _ ->
      scopes
  | Map {map; updates} ->
      let scopes = annotate_expression_opt env lambda_cntr scopes map in
      List.fold_left ~f:(annotate_association env lambda_cntr) ~init:scopes updates
  | MapComprehension {expression; qualifiers} ->
      (* TODO: support local variables in map comprehensions: T105967634 *)
      let scopes = annotate_expression env lambda_cntr scopes expression.key in
      let scopes = annotate_expression env lambda_cntr scopes expression.value in
      List.fold_left ~f:(annotate_qualifier env lambda_cntr) ~init:scopes qualifiers
  | Match {pattern; body} | MaybeMatch {pattern; body} ->
      annotate_expression_list env lambda_cntr scopes [pattern; body]
  | Maybe {body; else_cases} ->
      (* Variables bound in the body can't be seen in the else clauses.
         Variables bound in body / else clauses can't be seen outside. *)
      let maybe_scopes = annotate_expression_list env lambda_cntr scopes body in
      let else_scopes = annotate_clauses env lambda_cntr scopes else_cases in
      (* Only take captured variables into our final result. *)
      let hd, tl = pop_scope scopes in
      let captured = Pvar.Set.union hd.captured (top_scope maybe_scopes).captured in
      let captured = Pvar.Set.union captured (top_scope else_scopes).captured in
      {hd with captured} :: tl
  | Receive {cases; timeout} ->
      (* Process clauses and timeout independently and then merge as
         if the timeout was also just one of the clauses. *)
      let clause_scopes = List.map ~f:(annotate_one_clause env lambda_cntr scopes) cases in
      let timeout_scopes =
        match timeout with
        | None ->
            []
        | Some t ->
            let scopes = annotate_expression env lambda_cntr scopes t.time in
            [annotate_expression_list env lambda_cntr scopes t.handler]
      in
      merge_scopes ~into:scopes (timeout_scopes @ clause_scopes)
  | RecordAccess {record; _} ->
      annotate_expression env lambda_cntr scopes record
  | RecordUpdate {record; updates; _} ->
      let scopes = annotate_expression_opt env lambda_cntr scopes record in
      List.fold_left ~f:(annotate_record_update env lambda_cntr) ~init:scopes updates
  | TryCatch {body; ok_cases; catch_cases; after} ->
      (* OK cases can see variables from the body, but otherwise everything is independent *)
      let body_scopes = annotate_expression_list env lambda_cntr scopes body in
      let ok_scopes = annotate_clauses env lambda_cntr body_scopes ok_cases in
      let catch_scopes = annotate_catch_clauses env lambda_cntr scopes catch_cases in
      let after_scopes = annotate_expression_list env lambda_cntr scopes after in
      (* All variables are unsafe outside of try, so we just merge the captured vars *)
      let hd, tl = pop_scope scopes in
      let captured = hd.captured in
      let captured = Pvar.Set.union captured (top_scope ok_scopes).captured in
      let captured = Pvar.Set.union captured (top_scope catch_scopes).captured in
      let captured = Pvar.Set.union captured (top_scope after_scopes).captured in
      {hd with captured} :: tl
  | Tuple exprs ->
      annotate_expression_list env lambda_cntr scopes exprs
  | UnaryOperator (_, e) ->
      annotate_expression env lambda_cntr scopes e
  | Lambda lambda -> (
      let arity =
        match lambda.cases with
        | c :: _ ->
            List.length c.Ast.patterns
        | _ ->
            L.die InternalError "Lambda has no clauses, cannot determine arity"
      in
      (* A fresh id is created even for named lambdas. *)
      let function_name = create_unique_lambda_name lambda_cntr lambda.name in
      let procname = Procname.make_erlang ~module_name:env.current_module ~function_name ~arity in
      lambda.procname <- Some procname ;
      let scopes = push_scope scopes procname in
      let scopes = annotate_clauses env lambda_cntr scopes lambda.cases in
      let popped, scopes = pop_scope scopes in
      lambda.captured <- Some popped.captured ;
      (* Propagate captured to current scope. *)
      match scopes with
      | hd :: tl ->
          {hd with captured= Pvar.Set.union popped.captured hd.captured} :: tl
      | [] ->
          L.die InternalError "No scope found during lambda annotation." )
  | Variable v -> (
    match scopes with
    | hd :: tl -> (
        if String.Set.mem hd.locals v.vname then (
          (* Known local var *)
          v.scope <- Some hd.procname ;
          scopes )
        else
          (* Check if it's captured from outside *)
          match lookup_var tl v.vname with
          | Some procname ->
              let pvar = Pvar.mk (Mangled.from_string v.vname) procname in
              v.scope <- Some procname ;
              {hd with captured= Pvar.Set.add pvar hd.captured} :: tl
          | None ->
              (* It's a local we see here first *)
              v.scope <- Some hd.procname ;
              {hd with locals= String.Set.add hd.locals v.vname} :: tl )
    | [] ->
        L.die InternalError "No scope found during variable annotation." )


and annotate_expression_opt (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (maybe_expr : Ast.expression option) =
  match maybe_expr with
  | Some expr ->
      annotate_expression env lambda_cntr scopes expr
  | None ->
      scopes


and annotate_expression_list (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (exprs : Ast.expression list) =
  List.fold_left ~f:(annotate_expression env lambda_cntr) ~init:scopes exprs


and annotate_qualifier (env : (_, _) Env.t) lambda_cntr (scopes : scope list) (q : Ast.qualifier) =
  match q with
  | BitsGenerator {pattern; expression} ->
      annotate_expression_list env lambda_cntr scopes [pattern; expression]
  | Filter expression ->
      annotate_expression env lambda_cntr scopes expression
  | Generator {pattern; expression} ->
      annotate_expression_list env lambda_cntr scopes [pattern; expression]
  | MapGenerator {pattern; expression} ->
      annotate_expression_list env lambda_cntr scopes [pattern.key; pattern.value; expression]


and annotate_association (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    ({key; value; _} : Ast.association) =
  annotate_expression_list env lambda_cntr scopes [key; value]


and annotate_record_update (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (u : Ast.record_update) =
  annotate_expression env lambda_cntr scopes u.expression


and annotate_clauses (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (clauses : Ast.case_clause list) =
  (* Process each clause independently *)
  let subscopes = List.map ~f:(annotate_one_clause env lambda_cntr scopes) clauses in
  merge_scopes ~into:scopes subscopes


and annotate_catch_clauses (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (clauses : Ast.catch_clause list) =
  (* Process each clause independently *)
  let subscopes = List.map ~f:(annotate_one_catch_clause env lambda_cntr scopes) clauses in
  merge_scopes ~into:scopes subscopes


(* Merge the top frames  of a list of scope stacks into the top frame of a given scope stack. *)
and merge_scopes ~into (scopes : scope list list) =
  if List.is_empty scopes then into
  else
    let top_scopes = List.map ~f:top_scope scopes in
    (* Merge results: intersect locals, union captured  *)
    let merge (acc : scope) (s : scope) =
      { acc with
        locals= String.Set.inter acc.locals s.locals
      ; captured= Pvar.Set.union acc.captured s.captured }
    in
    let merged = List.reduce_exn ~f:merge top_scopes in
    (* Add to top scope *)
    let top_scope, tl_scopes = pop_scope into in
    { top_scope with
      locals= String.Set.union top_scope.locals merged.locals
    ; captured= Pvar.Set.union top_scope.captured merged.captured }
    :: tl_scopes


and annotate_one_clause (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (clause : Ast.case_clause) =
  let scopes = annotate_expression_list env lambda_cntr scopes clause.patterns in
  let scopes =
    List.fold_left ~f:(annotate_expression_list env lambda_cntr) ~init:scopes clause.guards
  in
  let scopes = annotate_expression_list env lambda_cntr scopes clause.body in
  scopes


and annotate_one_catch_clause (env : (_, _) Env.t) lambda_cntr (scopes : scope list)
    (clause : Ast.catch_clause) =
  let scopes =
    List.fold_left
      ~f:(fun scopes (p : Ast.catch_pattern) -> annotate_expression env lambda_cntr scopes p.pattern)
      ~init:scopes clause.patterns
  in
  let scopes =
    List.fold_left ~f:(annotate_expression_list env lambda_cntr) ~init:scopes clause.guards
  in
  let scopes = annotate_expression_list env lambda_cntr scopes clause.body in
  scopes


let annotate_one_function (env : (_, _) Env.t) function_ clauses =
  let _, name = Env.func_procname env function_ in
  let funcname =
    Procname.to_string name |> String.substr_replace_all ~pattern:"/" ~with_:"#"
    (* / confuses debug output *)
  in
  let scopes : scope list = push_scope [] name in
  let lambda_cntr = {funcname; counter= 0} in
  (* Process each clause independently *)
  List.iter
    ~f:(fun clause ->
      let scopes = annotate_one_clause env lambda_cntr scopes clause in
      let _, scopes = pop_scope scopes in
      assert_empty scopes )
    clauses


let annotate_scopes env module_ =
  let f {Ast.simple_form; _} =
    match simple_form with
    | Function {function_; clauses} ->
        annotate_one_function env function_ clauses
    | _ ->
        ()
  in
  List.iter module_ ~f
