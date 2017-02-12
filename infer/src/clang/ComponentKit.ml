(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

let get_source_range an =
  match an with
  | Ctl_parser_types.Decl decl ->
      let decl_info = Clang_ast_proj.get_decl_tuple decl in
      decl_info.Clang_ast_t.di_source_range
  | Ctl_parser_types.Stmt stmt ->
      let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
      stmt_info.Clang_ast_t.si_source_range

let is_in_main_file translation_unit_context an =
  let file_opt = (fst (get_source_range an)).Clang_ast_t.sl_file in
  match file_opt with
  | None ->
      false
  | Some source_file ->
      SourceFile.equal source_file translation_unit_context.CFrontend_config.source_file

let is_ck_context (context: CLintersContext.context) an =
  context.is_ck_translation_unit
  && is_in_main_file context.translation_unit_context an
  && CGeneral_utils.is_objc_extension context.translation_unit_context


(** Recursively go up the inheritance hierarchy of a given ObjCInterfaceDecl.
    (Returns false on decls other than that one.) *)
let is_component_or_controller_if decl =
  let open CFrontend_config in
  CAst_utils.is_objc_if_descendant decl [ckcomponent_cl; ckcomponentcontroller_cl]

(** True if it's an objc class impl that extends from CKComponent or
    CKComponentController, false otherwise *)
let rec is_component_or_controller_descendant_impl decl =
  match decl with
  | Clang_ast_t.ObjCImplementationDecl _ ->
      is_component_or_controller_if (CAst_utils.get_super_if (Some decl))
  | Clang_ast_t.LinkageSpecDecl (_, decl_list, _) ->
      contains_ck_impl decl_list
  | _ -> false

(** Returns true if the passed-in list of decls contains an
    ObjCImplementationDecl of a descendant of CKComponent or
    CKComponentController.

    Does not recurse into hierarchy. *)
and contains_ck_impl decl_list =
  List.exists ~f:is_component_or_controller_descendant_impl decl_list

(** An easy way to fix the component kit best practice
    http://componentkit.org/docs/avoid-local-variables.html

    Local variables that are const or const pointers by definition cannot be
    assigned to after declaration, which means the entire class of bugs stemming
    from value mutation after assignment are gone.

    Note we want const pointers, not mutable pointers to const instances.

    OK:

    ```
    const int a;
    int *const b;
    NSString *const c;
    const int *const d;
    ```

    Not OK:

    ```
    const int *z;
    const NSString *y;
    ``` *)
let mutable_local_vars_advice context an =
  let rec get_referenced_type (qual_type: Clang_ast_t.qual_type) : Clang_ast_t.decl option =
    let typ_opt = CAst_utils.get_desugared_type qual_type.qt_type_ptr in
    match (typ_opt : Clang_ast_t.c_type option) with
    | Some ObjCInterfaceType (_, decl_ptr)
    | Some RecordType (_, decl_ptr) -> CAst_utils.get_decl decl_ptr
    | Some PointerType (_, inner_qual_type)
    | Some ObjCObjectPointerType (_, inner_qual_type)
    | Some LValueReferenceType (_, inner_qual_type) -> get_referenced_type inner_qual_type
    | _ -> None in

  let is_of_whitelisted_type qual_type =
    let cpp_whitelist = ["CKComponentScope"; "FBTrackingNodeScope"; "FBTrackingCodeScope"] in
    let objc_whitelist = ["NSError"] in
    match get_referenced_type qual_type with
    | Some CXXRecordDecl (_, ndi, _, _, _, _, _, _) ->
        List.mem ~equal:String.equal cpp_whitelist ndi.ni_name
    | Some ObjCInterfaceDecl (_, ndi, _, _, _) ->
        List.mem ~equal:String.equal objc_whitelist ndi.ni_name
    | _ -> false in

  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.VarDecl(decl_info, named_decl_info, qual_type, _) as decl)->
      let is_const_ref = match CAst_utils.get_type qual_type.qt_type_ptr with
        | Some LValueReferenceType (_, {Clang_ast_t.qt_is_const}) ->
            qt_is_const
        | _ -> false in
      let is_const = qual_type.qt_is_const || is_const_ref in
      let condition = is_ck_context context an
                      && (not (CAst_utils.is_syntactically_global_var decl))
                      && (not is_const)
                      && not (is_of_whitelisted_type qual_type)
                      && not decl_info.di_is_implicit in
      if condition then
        CTL.True, Some {
          CIssue.name = "MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE";
          severity = Exceptions.Kadvice;
          mode = CIssue.On;
          description = "Local variable '" ^ named_decl_info.ni_name
                        ^ "' should be const to avoid reassignment";
          suggestion = Some "Add a const (after the asterisk for pointer types).";
          loc = CFrontend_checkers.location_from_dinfo context decl_info
        }
      else CTL.False, None
  | _ -> CTL.False, None (* Should only be called with a VarDecl *)


(** Catches functions that should be composite components.
    http://componentkit.org/docs/break-out-composites.html

    Any static function that returns a subclass of CKComponent will be flagged. *)
let component_factory_function_advice context an =
  let is_component_if decl =
    CAst_utils.is_objc_if_descendant decl [CFrontend_config.ckcomponent_cl] in

  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.FunctionDecl (decl_info, _, (qual_type: Clang_ast_t.qual_type), _)) ->
      let objc_interface =
        CAst_utils.type_ptr_to_objc_interface qual_type.qt_type_ptr in
      let condition =
        is_ck_context context an && is_component_if objc_interface in
      if condition then
        CTL.True, Some {
          CIssue.name = "COMPONENT_FACTORY_FUNCTION";
          severity = Exceptions.Kadvice;
          mode = CIssue.On;
          description = "Break out composite components";
          suggestion = Some (
              "Prefer subclassing CKCompositeComponent to static helper functions \
               that return a CKComponent subclass."
            );
          loc = CFrontend_checkers.location_from_dinfo context decl_info
        }
      else CTL.False, None
  | _ -> CTL.False, None (* Should only be called with FunctionDecl *)

(** Components should not inherit from each other. They should instead
    inherit from CKComponent, CKCompositeComponent, or
    CKStatefulViewComponent. (Similar rule applies to component controllers.) *)
let component_with_unconventional_superclass_advice context an =
  let check_interface if_decl =
    match if_decl with
    | Clang_ast_t.ObjCInterfaceDecl (_, _, _, _, _) ->
        if is_component_or_controller_if (Some if_decl) then
          let superclass_name = match CAst_utils.get_super_if (Some if_decl) with
            | Some Clang_ast_t.ObjCInterfaceDecl (_, named_decl_info, _, _, _) ->
                Some named_decl_info.ni_name
            | _ -> None in
          let has_conventional_superclass =
            let open CFrontend_config in
            match superclass_name with
            | Some name when List.mem ~equal:String.equal
                  [
                    ckcomponent_cl;
                    ckcomponentcontroller_cl;
                    "CKCompositeComponent";
                    "CKStatefulViewComponent";
                    "CKStatefulViewComponentController";
                    "NTNativeTemplateComponent"
                  ]
                  name -> true
            | _ -> false in
          let condition =
            is_component_or_controller_if (Some if_decl)
            && not has_conventional_superclass in
          if condition then
            CTL.True, Some {
              CIssue.name = "COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS";
              severity = Exceptions.Kadvice;
              mode = CIssue.On;
              description = "Never Subclass Components";
              suggestion = Some (
                  "Instead, create a new subclass of CKCompositeComponent."
                );
              loc = CFrontend_checkers.location_from_decl context if_decl
            }
          else
            CTL.False, None
        else
          CTL.False, None
    | _ -> assert false in
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCImplementationDecl (_, _, _, _, impl_decl_info)) ->
      let if_decl_opt =
        CAst_utils.get_decl_opt_with_decl_ref impl_decl_info.oidi_class_interface in
      if Option.is_some if_decl_opt && is_ck_context context an then
        check_interface (Option.value_exn if_decl_opt)
      else
        CTL.False, None
  | _ -> CTL.False, None

(** Components should only have one factory method.

    (They could technically have none if they re-use the parent class's factory
    method.)

    We care about ones that are declared in the interface. In other words, if
    additional factory methods are implementation-only, the rule doesn't catch
    it. While its existence is probably not good, I can't think of any reason
    there would be factory methods that aren't exposed outside of a class is
    not useful if there's only one public factory method.

    Given n factory methods, the rule should emit n-1 issues. Each issue's
    location should point to the method declaration. *)
let component_with_multiple_factory_methods_advice context an =
  let is_unavailable_attr attr = match attr with
    | Clang_ast_t.UnavailableAttr _ -> true
    | _ -> false in
  let is_available_factory_method if_decl (decl: Clang_ast_t.decl) =
    let attrs = match decl with
      | ObjCMethodDecl (decl_info, _, _) -> decl_info.Clang_ast_t.di_attributes
      | _ -> assert false in
    let unavailable_attrs = (List.filter ~f:is_unavailable_attr attrs) in
    let is_available = Int.equal (IList.length unavailable_attrs) 0 in
    (CAst_utils.is_objc_factory_method if_decl decl) && is_available in

  let check_interface if_decl =
    match if_decl with
    | Clang_ast_t.ObjCInterfaceDecl (_, _, decls, _, _) ->
        let factory_methods = List.filter ~f:(is_available_factory_method if_decl) decls in
        CTL.True, IList.map (fun meth_decl -> {
              CIssue.name = "COMPONENT_WITH_MULTIPLE_FACTORY_METHODS";
              severity = Exceptions.Kadvice;
              mode = CIssue.On;
              description = "Avoid Overrides";
              suggestion =
                Some "Instead, always expose all parameters in a single \
                      designated initializer and document which are optional.";
              loc = CFrontend_checkers.location_from_decl context meth_decl
            }) (IList.drop_first 1 factory_methods)
    | _ -> assert false in
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCImplementationDecl (_, _, _, _, impl_decl_info)) ->
      let if_decl_opt =
        CAst_utils.get_decl_opt_with_decl_ref impl_decl_info.oidi_class_interface in
      (match if_decl_opt with
       | Some d when is_ck_context context an -> check_interface d
       | _ -> CTL.False, [])
  | _ -> CTL.False, []

let in_ck_class (context: CLintersContext.context) =
  Option.value_map ~f:is_component_or_controller_descendant_impl ~default:false
    context.current_objc_impl
  && CGeneral_utils.is_objc_extension context.translation_unit_context

(** Components shouldn't have side-effects in its initializer.

    http://componentkit.org/docs/no-side-effects.html

    The only current way we look for side-effects is by looking for
    asynchronous execution (dispatch_async, dispatch_after) and execution that
    relies on other threads (dispatch_sync). Other side-effects, like reading
    of global variables, is not checked by this analyzer, although still an
    infraction of the rule. *)
let rec _component_initializer_with_side_effects_advice
    (context: CLintersContext.context) call_stmt =
  let condition =
    in_ck_class context
    && context.in_objc_static_factory_method
    && (match context.current_objc_impl with
        | Some d -> is_in_main_file context.translation_unit_context (Ctl_parser_types.Decl d)
        | None -> false) in
  if condition then
    match call_stmt with
    | Clang_ast_t.ImplicitCastExpr (_, stmt :: _, _, _) ->
        _component_initializer_with_side_effects_advice context stmt
    | Clang_ast_t.DeclRefExpr (_, _, _, decl_ref_expr_info) ->
        let refs = [decl_ref_expr_info.drti_decl_ref;
                    decl_ref_expr_info.drti_found_decl_ref] in
        (match List.find_map ~f:CAst_utils.name_of_decl_ref_opt refs with
         | Some "dispatch_after"
         | Some "dispatch_async"
         | Some "dispatch_sync" ->
             CTL.True, Some {
               CIssue.name = "COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS";
               severity = Exceptions.Kadvice;
               mode = CIssue.On;
               description = "No Side-effects";
               suggestion = Some "Your +new method should not modify any \
                                  global variables or global state.";
               loc = CFrontend_checkers.location_from_stmt context call_stmt
             }
         | _ ->
             CTL.False, None)
    | _->
        CTL.False, None
  else
    CTL.False, None

let component_initializer_with_side_effects_advice
    (context: CLintersContext.context) an =
  match an with
  | Ctl_parser_types.Stmt (CallExpr (_, called_func_stmt :: _, _))  ->
      _component_initializer_with_side_effects_advice context called_func_stmt
  | _ -> CTL.False, None (* only to be called in CallExpr *)

(** Returns one issue per line of code, with the column set to 0.

    This still needs to be in infer b/c only files that have a valid component
    kit class impl should be analyzed. *)
let component_file_line_count_info (context: CLintersContext.context) dec =
  let condition = Config.compute_analytics && context.is_ck_translation_unit in
  match dec with
  | Ctl_parser_types.Decl Clang_ast_t.TranslationUnitDecl _ when condition ->
      let source_file =
        context.translation_unit_context.CFrontend_config.source_file in
      let line_count = SourceFile.line_count source_file in
      CTL.True, IList.map (fun i -> {
            CIssue.name = "COMPONENT_FILE_LINE_COUNT";
            severity = Exceptions.Kinfo;
            mode = CIssue.Off;
            description = "Line count analytics";
            suggestion = None;
            loc = {
              Location.line = i;
              Location.col = 0;
              Location.file = source_file
            }
          }
        ) (IList.range 1 line_count)
  | _ -> CTL.False, []

(** Computes a component file's cyclomatic complexity.

    Somewhat borrowed from
    https://github.com/oclint/oclint/blob/5889b5ec168185513ba69ce83821ea1cc8e63fbe
    /oclint-metrics/lib/CyclomaticComplexityMetric.cpp *)
let component_file_cyclomatic_complexity_info (context: CLintersContext.context) an =
  let is_cyclo_stmt stmt = match stmt with
    | Clang_ast_t.IfStmt _
    | Clang_ast_t.ForStmt _
    | Clang_ast_t.ObjCForCollectionStmt _
    | Clang_ast_t.CXXForRangeStmt _
    | Clang_ast_t.WhileStmt _
    | Clang_ast_t.DoStmt _
    | Clang_ast_t.CaseStmt _
    | Clang_ast_t.ObjCAtCatchStmt _
    | Clang_ast_t.CXXCatchStmt _
    | Clang_ast_t.ConditionalOperator _ -> true
    | Clang_ast_t.BinaryOperator (_, _, _, boi) ->
        List.mem ~equal:(=) [`LAnd; `LOr] boi.Clang_ast_t.boi_kind
    | _ -> false in
  let cyclo_loc_opt an = match an with
    | Ctl_parser_types.Stmt stmt when (Config.compute_analytics
                                       && is_cyclo_stmt stmt
                                       && is_ck_context context an) ->
        Some (CFrontend_checkers.location_from_stmt context stmt)
    | Ctl_parser_types.Decl (Clang_ast_t.TranslationUnitDecl _ as d)
      when Config.compute_analytics && context.is_ck_translation_unit ->
        Some (CFrontend_checkers.location_from_decl context d)
    | _ -> None in
  match cyclo_loc_opt an with
  | Some loc ->
      CTL.True, Some {
        CIssue.name = "COMPONENT_FILE_CYCLOMATIC_COMPLEXITY";
        severity = Exceptions.Kinfo;
        mode = CIssue.Off;
        description = "Cyclomatic Complexity Incremental Marker";
        suggestion = None;
        loc = loc
      }
  | _ -> CTL.False, None
