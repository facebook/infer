(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type scope_kind =
  | Breakable  (** loop or switch statement within which it's ok to [break;] *)
  | Compound  (** inside a CompoundStmt *)
  | InitialScope  (** should be only one of these at the bottom of the stack *)
[@@deriving compare]

let equal_scope_kind = [%compare.equal: scope_kind]

let string_of_kind = function
  | Compound ->
      "Compound"
  | Breakable ->
      "Breakable"
  | InitialScope ->
      "InitialScope"


(** the [current] scope is distinguished to make it easy to add elements to it *)
type 'a scope = {current: 'a list; current_kind: scope_kind; outers: ('a list * scope_kind) list}

(** executes [f] in new scope where [kind] has been pushed on top *)
let in_ kind scope ~f =
  L.debug Capture Verbose "<@[<v2>%s|@," (string_of_kind kind) ;
  let scope =
    {current= []; current_kind= kind; outers= (scope.current, scope.current_kind) :: scope.outers}
  in
  let scope, x = f scope in
  let (current, current_kind), outers =
    match scope.outers with [] -> assert false | top :: rest -> (top, rest)
  in
  L.debug Capture Verbose "@]@;/%s>" (string_of_kind scope.current_kind) ;
  (scope.current, {current; current_kind; outers}, x)


let rev_append xs scope = {scope with current= List.rev_append xs scope.current}

let collect_until kind scope =
  let rec aux kind rev_vars_to_destroy = function
    | [] ->
        assert false
    | (in_scope, kind') :: outers ->
        let rev_vars_to_destroy = List.rev_append in_scope rev_vars_to_destroy in
        if equal_scope_kind kind' kind then List.rev rev_vars_to_destroy
        else aux kind rev_vars_to_destroy outers
  in
  aux kind [] ((scope.current, scope.current_kind) :: scope.outers)


let breaks_control_flow = function
  | Clang_ast_t.(ReturnStmt _ | BreakStmt _ | ContinueStmt _) ->
      true
  | _ ->
      false


module Variables = struct
  let pp_var_decl f = function
    | Clang_ast_t.VarDecl (_, {ni_name}, _, _) ->
        Format.pp_print_string f ni_name
    | _ ->
        assert false


  type scope =
    { outer_scope: Clang_ast_t.stmt list  (** statements that are under the new scope *)
    ; breakable_scope: Clang_ast_t.stmt list
          (** the body of a loop or switch statement that defines the scope that [BreakStmt] and
              [ContinueStmt] will exit *)
    ; swallow_destructors: bool
          (** That scope does not generate destructor calls (eg because it ends in an instruction
              that will already do so like [ReturnStmt]). We still want to generate a scope to catch
              variables declared in that scope and avoid them being destroyed elsewhere. *)
    }

  (** get which statements define a variable scope and possible a breakable scope *)
  let get_scopes stmt =
    let is_compound_stmt_ending_in_control_flow_break stmt =
      match (stmt : Clang_ast_t.stmt) with
      | CompoundStmt (_, stmt_list) ->
          List.last stmt_list |> Option.exists ~f:breaks_control_flow
      | _ ->
          false
    in
    match (stmt : Clang_ast_t.stmt) with
    | CompoundStmt (_, stmt_list) | IfStmt (_, stmt_list, _) ->
        Some
          { outer_scope= stmt_list
          ; breakable_scope= []
          ; swallow_destructors= is_compound_stmt_ending_in_control_flow_break stmt }
    | CXXForRangeStmt
        ( _
        , [ _init
          (* TODO: ignored here because ignored in [CTrans] *)
          ; iterator_decl
          ; begin_stmt
          ; end_stmt
          ; exit_cond
          ; increment
          ; assign_current_index
          ; loop_body ] ) ->
        Some
          { outer_scope= [iterator_decl; begin_stmt; end_stmt; exit_cond; increment]
          ; breakable_scope= [assign_current_index; loop_body]
          ; swallow_destructors= false }
    | ObjCForCollectionStmt (_, [item; items; body]) ->
        Some {outer_scope= [item; items]; breakable_scope= [body]; swallow_destructors= false}
    | ForStmt (_stmt_info, [init; decl_stmt; condition; increment; body]) ->
        Some
          { outer_scope= [init; decl_stmt; condition; increment]
          ; breakable_scope= [body]
          ; swallow_destructors= false }
    | DoStmt (_, [body; condition]) | WhileStmt (_, [condition; body]) ->
        Some {outer_scope= [condition]; breakable_scope= [body]; swallow_destructors= false}
    | WhileStmt (_, [decls; condition; body]) ->
        Some {outer_scope= [decls; condition]; breakable_scope= [body]; swallow_destructors= false}
    | SwitchStmt (stmt_info, _stmt_list, switch_stmt_info) ->
        let condition =
          CAst_utils.get_stmt_exn switch_stmt_info.Clang_ast_t.ssi_cond
            stmt_info.Clang_ast_t.si_source_range
        in
        let body =
          CAst_utils.get_stmt_exn switch_stmt_info.Clang_ast_t.ssi_body
            stmt_info.Clang_ast_t.si_source_range
        in
        let cond_var = switch_stmt_info.Clang_ast_t.ssi_cond_var in
        Some
          { outer_scope= condition :: Option.to_list cond_var
          ; breakable_scope= [body]
          ; swallow_destructors= false }
    | _ ->
        None


  let rec visit_stmt stmt ((scope, map) as scope_map) =
    L.debug Capture Verbose "%a{%a}@;"
      (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
      stmt (Pp.seq ~sep:"," pp_var_decl) scope.current ;
    match (stmt : Clang_ast_t.stmt) with
    | ReturnStmt (stmt_info, _)
    | BreakStmt (stmt_info, _)
    | ContinueStmt (stmt_info, _) (* TODO: GotoStmt *) ->
        let break_until =
          match stmt with Clang_ast_t.ReturnStmt _ -> InitialScope | _ -> Breakable
        in
        let vars_to_destroy = collect_until break_until scope in
        L.debug Capture Verbose "~[%d:%a]" stmt_info.Clang_ast_t.si_pointer
          (Pp.seq ~sep:"," pp_var_decl) vars_to_destroy ;
        let map =
          ClangPointers.Map.set map ~key:stmt_info.Clang_ast_t.si_pointer ~data:vars_to_destroy
        in
        (scope, map)
    | DeclStmt (_, _, decl_list) ->
        let new_vars =
          List.filter decl_list ~f:(function Clang_ast_t.VarDecl _ -> true | _ -> false)
        in
        (* the reverse order is the one we want to destroy the variables in at the end of the scope
        *)
        L.debug Capture Verbose "+%a@," (Pp.seq ~sep:"," pp_var_decl) new_vars ;
        (rev_append new_vars scope, map)
    | _ -> (
        let stmt_info, stmt_list = Clang_ast_proj.get_stmt_tuple stmt in
        match get_scopes stmt with
        | None ->
            visit_stmt_list stmt_list scope_map
        | Some {outer_scope; breakable_scope; swallow_destructors} ->
            with_scope ~inject_destructors:(not swallow_destructors) Compound
              stmt_info.Clang_ast_t.si_pointer scope ~f:(fun scope ->
                let scope_map = visit_stmt_list outer_scope (scope, map) in
                match breakable_scope with
                | [] ->
                    scope_map
                | _ :: _ as body ->
                    let body_ptr =
                      List.last_exn body |> Clang_ast_proj.get_stmt_tuple
                      |> function {Clang_ast_t.si_pointer}, _ -> si_pointer
                    in
                    let scope, map = scope_map in
                    with_scope Breakable ~inject_destructors:false body_ptr scope ~f:(fun scope ->
                        visit_stmt_list body (scope, map) ) ) )


  and with_scope ?(inject_destructors = true) kind pointer scope ~f =
    let vars_to_destroy, scope, map = in_ kind scope ~f:(fun scope -> f scope) in
    let map =
      if inject_destructors then (
        L.debug Capture Verbose "~[%d:%a]" pointer (Pp.seq ~sep:"," pp_var_decl) vars_to_destroy ;
        ClangPointers.Map.set map ~key:pointer ~data:vars_to_destroy )
      else (
        L.debug Capture Verbose "~[%d:skip]" pointer ;
        map )
    in
    (scope, map)


  and visit_stmt_list stmt_list scope_map =
    List.fold stmt_list ~init:scope_map ~f:(fun scope_map stmt ->
        L.debug Capture Verbose "@;" ; visit_stmt stmt scope_map )


  let empty_scope = {current= []; current_kind= InitialScope; outers= []}

  let compute_vars_to_destroy_map body =
    visit_stmt body (empty_scope, ClangPointers.Map.empty) |> snd
end

module CXXTemporaries = struct
  let rec visit_stmt_aux context stmt temporaries =
    match (stmt : Clang_ast_t.stmt) with
    | MaterializeTemporaryExpr
        ( stmt_info
        , stmt_list
        , expr_info
        , { mtei_decl_ref=
              (* C++ temporaries bound to a const reference see their lifetimes extended to that of
                 the reference *)
              None } ) ->
        let pvar, typ = CVar_decl.materialize_cpp_temporary context stmt_info expr_info in
        L.debug Capture Verbose "+%a@," (Pvar.pp Pp.text) pvar ;
        let temporaries = (pvar, typ, expr_info.ei_qual_type) :: temporaries in
        visit_stmt_list context stmt_list temporaries
    | ExprWithCleanups _ ->
        (* huho, we're stepping on someone else's toes (eg, a lambda literal); stop accumulating *)
        temporaries
    | ConditionalOperator _
    | BinaryOperator (_, _, _, {boi_kind= `LAnd | `LOr | `LT | `GT | `LE | `GE | `EQ | `NE}) ->
        (* Do not destroy temporaries created under a conditional operator. This is incorrect but
           better than destroying temporaries that are created in only one branch unconditionally
           after the conditional.

           Note that destroying the variable inside the branch of the conditional would also be
           incorrect since the conditional operator may be only part of the enclosing full
           expression.

           Example of tricky case: [foo(x?y:z, w)] or [cond && y] where [y] generates a C++
           temporary. *)
        temporaries
    | LambdaExpr _ ->
        (* do not analyze the code of another function *) temporaries
    | _ ->
        let _, stmt_list = Clang_ast_proj.get_stmt_tuple stmt in
        visit_stmt_list context stmt_list temporaries


  and visit_stmt context stmt temporaries =
    L.debug Capture Verbose "<@[<hv2>%a|@,"
      (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
      stmt ;
    let r = visit_stmt_aux context stmt temporaries in
    L.debug Capture Verbose "@]@;/%a>" (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string) stmt ;
    r


  and visit_stmt_list context stmt_list temporaries =
    List.fold stmt_list ~init:temporaries ~f:(fun temporaries stmt ->
        L.debug Capture Verbose "@;" ;
        visit_stmt context stmt temporaries )


  let get_destroyable_temporaries context stmt_list = visit_stmt_list context stmt_list []
end
