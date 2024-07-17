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
[@@deriving compare, equal]

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


module CXXTemporaries = struct
  (** This function has basically two modes depending on whether [bound_to_decl] is set or not. If
      set, we look for temporaries bound to the decl passed as argument. If not set, we look for
      temporaries not bound to any decls. *)
  let rec visit_stmt_aux ~bound_to_decl (context : CContext.t) (stmt : Clang_ast_t.stmt) ~marker
      temporaries =
    match stmt with
    | MaterializeTemporaryExpr (stmt_info, stmt_list, expr_info, _)
    | CXXBindTemporaryExpr (stmt_info, stmt_list, expr_info, _) ->
        (* whether we want to count this temporary or not *)
        let should_accumulate =
          match (bound_to_decl, stmt) with
          (* looking for a temporary bound to a decl, keeping only those bound to that particular
             decl *)
          | ( Some var_decl_pointer
            , MaterializeTemporaryExpr (_, _, _, {mtei_decl_ref= Some {dr_decl_pointer}}) ) ->
              Int.equal var_decl_pointer dr_decl_pointer
          (* looking for a temporary bound to a decl, skipping those not bound to a decl *)
          | Some _, MaterializeTemporaryExpr (_, _, _, {mtei_decl_ref= None})
          | Some _, CXXBindTemporaryExpr _
          (* not looking for a temporary bound to a decl, skipping those bound to a decl *)
          | None, MaterializeTemporaryExpr (_, _, _, {mtei_decl_ref= Some _}) ->
              false
          (* not looking for a temporary bound to a decl, keeping those not bound to a decl *)
          | ( None
            , (MaterializeTemporaryExpr (_, _, _, {mtei_decl_ref= None}) | CXXBindTemporaryExpr _) )
            ->
              true
          | _ ->
              L.die InternalError "Impossible: got bound_to_decl:%a and decl=%a" (Pp.option Int.pp)
                bound_to_decl
                (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
                stmt
        in
        let temporaries =
          if should_accumulate then
            let pvar, typ = CVar_decl.materialize_cpp_temporary context stmt_info expr_info in
            if Option.is_none bound_to_decl && Typ.is_unique_pointer typ then
              (* HACK: We avoid destructing [unique_ptr] when its bound declaration is unclear. In
                 that case, the [unique_ptr] object can be *passed* to a function as an argument,
                 sometimes via copy-elision, but Infer cannot understand excatly when the
                 copy-elision happens, so which resulted in FPs of use after lifetime after
                 injecting destructor calls at incorrect places. *)
              temporaries
            else (
              L.debug Capture Verbose "+%a:%a@," (Pvar.pp Pp.text) pvar (Typ.pp Pp.text) typ ;
              let marker =
                Option.map marker ~f:(fun if_kind ->
                    let marker_pvar =
                      Pvar.mk_tmp "_temp_marker_" (Procdesc.get_proc_name context.procdesc)
                    in
                    L.debug Capture Verbose "Attaching marker %a to %a@," (Pvar.pp Pp.text)
                      marker_pvar (Pvar.pp Pp.text) pvar ;
                    (marker_pvar, if_kind) )
              in
              CContext.CXXTemporarySet.add
                {CContext.pvar; typ; qual_type= expr_info.ei_qual_type; marker}
                temporaries )
          else temporaries
        in
        visit_stmt_list ~bound_to_decl context stmt_list ~marker temporaries
    | ConditionalOperator (_, [cond; then_; else_], _) ->
        (* temporaries created in branches need instrumentation markers to remember if they have
           been created or not during the evaluation of the expression *)
        visit_stmt ~bound_to_decl context cond ~marker temporaries
        |> visit_stmt ~bound_to_decl context then_ ~marker:(Some (Sil.Ik_bexp {terminated= true}))
        |> visit_stmt ~bound_to_decl context else_ ~marker:(Some (Sil.Ik_bexp {terminated= true}))
    | BinaryOperator (_, [lhs; rhs], _, {boi_kind= `LAnd | `LOr}) ->
        (* similarly to above, due to possible short-circuiting we are not sure that the RHS of [a
           && b] and [a || b] will be executed *)
        visit_stmt ~bound_to_decl context lhs ~marker temporaries
        |> visit_stmt ~bound_to_decl context rhs ~marker:(Some Sil.Ik_land_lor)
    | LambdaExpr _ ->
        (* do not analyze the code of another function *) temporaries
    | ExprWithCleanups _ when Option.is_none bound_to_decl ->
        (* huho, we're stepping on someone else's toes (eg, a lambda literal); stop here unless we
           are looking for the temporary bound to a specific lvalue [bound_to_decl] (because then we
           are not already in the [ExprWithCleanups _] that might contain the temporary, and we will
           check the ref the temporary is bound to so there is no chance of destroying the wrong C++
           temporary that way) *)
        temporaries
    | CoroutineBodyStmt _ | CoawaitExpr _ | CoreturnStmt _ ->
        (* ignore coroutines stuff for the moment *)
        temporaries
    | _ ->
        let _, stmt_list = Clang_ast_proj.get_stmt_tuple stmt in
        visit_stmt_list ~bound_to_decl context stmt_list ~marker temporaries


  and visit_stmt ~bound_to_decl context stmt ~marker temporaries =
    L.debug Capture Verbose "<@[<hv2>%a|@,"
      (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
      stmt ;
    let r = visit_stmt_aux ~bound_to_decl context stmt ~marker temporaries in
    L.debug Capture Verbose "@]@;/%a>" (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string) stmt ;
    r


  and visit_stmt_list ~bound_to_decl context stmt_list ~marker temporaries =
    List.fold stmt_list ~init:temporaries ~f:(fun temporaries stmt ->
        L.debug Capture Verbose "@;" ;
        visit_stmt ~bound_to_decl context stmt ~marker temporaries )


  let get_temporaries ~bound_to_decl context stmt_list =
    let temporaries =
      visit_stmt_list ~bound_to_decl context stmt_list ~marker:None CContext.CXXTemporarySet.empty
    in
    L.debug Capture Verbose "@\n" ;
    temporaries


  (** {2 Interface} *)

  let get_destroyable_temporaries context stmt_list =
    get_temporaries ~bound_to_decl:None context stmt_list


  let get_temporaries_bound_to_decl context decl stmt_list =
    get_temporaries ~bound_to_decl:(Some decl) context stmt_list
end

module Variables = struct
  type scope =
    { outer_scope: Clang_ast_t.stmt list  (** statements that are under the new scope *)
    ; breakable_scope: Clang_ast_t.stmt list
          (** the body of a loop or switch statement that defines the scope that [BreakStmt] and
              [ContinueStmt] will exit *)
    ; swallow_destructors: bool
          (** That scope does not generate destructor calls (eg because it ends in an instruction
              that will already do so like [ReturnStmt]). We still want to generate a scope to catch
              variables declared in that scope and avoid them being destroyed elsewhere. *) }

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
        , [ _init (* TODO: ignored here because ignored in [CTrans] *)
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


  let rec visit_stmt context stmt ((scope, map) as scope_map) =
    L.debug Capture Verbose "%a{%a}@;"
      (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
      stmt
      (Pp.seq ~sep:"," CContext.pp_var_to_destroy)
      scope.current ;
    match (stmt : Clang_ast_t.stmt) with
    | ReturnStmt (stmt_info, _)
    | BreakStmt (stmt_info, _)
    | ContinueStmt (stmt_info, _) (* TODO: GotoStmt *) ->
        let break_until =
          match stmt with Clang_ast_t.ReturnStmt _ -> InitialScope | _ -> Breakable
        in
        let vars_to_destroy = collect_until break_until scope in
        L.debug Capture Verbose "~[%d:%a]" stmt_info.Clang_ast_t.si_pointer
          (Pp.seq ~sep:"," CContext.pp_var_to_destroy)
          vars_to_destroy ;
        let map =
          ClangPointers.Map.set map ~key:stmt_info.Clang_ast_t.si_pointer ~data:vars_to_destroy
        in
        (scope, map)
    | DeclStmt (_, stmts, decl_list) ->
        let to_destroy =
          List.concat_map decl_list ~f:(function
            | Clang_ast_t.VarDecl (({di_pointer}, _, _, {vdi_is_static_local= false}) as var_decl)
              ->
                (* C++ temporaries bound to a const reference see their lifetimes extended to that
                   of the reference; collect these cases here so they get destroyed at the same time
                   that (in reality just before) the lvalues they are bound to get destroyed *)
                let temporaries_in_extended_scope =
                  CXXTemporaries.get_temporaries_bound_to_decl context di_pointer stmts
                  |> CContext.CXXTemporarySet.elements
                  |> List.map ~f:(fun temp -> CContext.CXXTemporary temp)
                in
                CContext.VarDecl var_decl :: temporaries_in_extended_scope
            | _ ->
                [] )
        in
        L.debug Capture Verbose "+%a@," (Pp.seq ~sep:"," CContext.pp_var_to_destroy) to_destroy ;
        (* the reverse order is the one we want to destroy the variables in at the end of the scope
           *)
        (rev_append to_destroy scope, map)
    | _ -> (
        let stmt_info, stmt_list = Clang_ast_proj.get_stmt_tuple stmt in
        match get_scopes stmt with
        | None ->
            visit_stmt_list context stmt_list scope_map
        | Some {outer_scope; breakable_scope; swallow_destructors} ->
            with_scope ~inject_destructors:(not swallow_destructors) Compound
              stmt_info.Clang_ast_t.si_pointer scope ~f:(fun scope ->
                let scope_map = visit_stmt_list context outer_scope (scope, map) in
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
                        visit_stmt_list context body (scope, map) ) ) )


  and with_scope ?(inject_destructors = true) kind pointer scope ~f =
    let vars_to_destroy, scope, map = in_ kind scope ~f in
    let map =
      if inject_destructors then (
        L.debug Capture Verbose "~[%d:%a]" pointer
          (Pp.seq ~sep:"," CContext.pp_var_to_destroy)
          vars_to_destroy ;
        ClangPointers.Map.set map ~key:pointer ~data:vars_to_destroy )
      else (
        L.debug Capture Verbose "~[%d:skip]" pointer ;
        map )
    in
    (scope, map)


  and visit_stmt_list context stmt_list scope_map =
    List.fold stmt_list ~init:scope_map ~f:(fun scope_map stmt ->
        L.debug Capture Verbose "@;" ;
        visit_stmt context stmt scope_map )


  let empty_scope = {current= []; current_kind= InitialScope; outers= []}

  let compute_vars_to_destroy_map context body =
    let scope_map = visit_stmt context body (empty_scope, ClangPointers.Map.empty) |> snd in
    L.debug Capture Verbose "@\n" ;
    scope_map
end
