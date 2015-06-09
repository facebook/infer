(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Utility module for retrieving types *)

open Utils
open Clang_ast_t
open CFrontend_utils
open CFrontend_utils.General_utils
module L = Logging

let get_function_return_type s =
  let regexp = Str.regexp_string " (" in
  let matches = try let _ = Str.search_forward regexp s 0 in true with Not_found -> false in
  let regexp' =
    if matches then regexp
    else Str.regexp_string "(" in (* match e.g. "char *()" *)
  let buf = Str.split regexp' s in
  match buf with
  | ret:: _ ->
      let ret'= String.trim ret in
      Printing.log_out ~fmt:"return type ='%s'@." ret';
      ret'
  | _ -> assert false

let get_type qt =
  match qt.Clang_ast_t.qt_desugared with
  | Some t -> t
  | _ -> qt.Clang_ast_t.qt_raw

let get_type_from_expr_info ei =
  ei.Clang_ast_t.ei_qual_type

let lookup_var_type context pvar =
  let formals = Cfg.Procdesc.get_formals context.CContext.procdesc in
  let locals = Cfg.Procdesc.get_locals context.CContext.procdesc in
  try
    let s, t = list_find (fun (s, t) -> s = (Sil.pvar_to_string pvar)) formals in
    Printing.log_out ~fmt:"When looking for type of variable '%s' " (Sil.pvar_to_string pvar);
    Printing.log_out ~fmt:"found '%s' in formals.@." (Sil.typ_to_string t);
    t
  with Not_found ->
      try
        let s, t = list_find (fun (s, t) -> Mangled.equal (Sil.pvar_get_name pvar) s) locals in
        Printing.log_out ~fmt:"When looking for type of variable '%s' " (Sil.pvar_to_string pvar);
        Printing.log_out ~fmt:"found '%s' in locals.@." (Sil.typ_to_string t);
        t
      with Not_found ->
          try
            let typ = CGlobal_vars.var_get_typ (CGlobal_vars.find (Sil.pvar_get_name pvar)) in
            Printing.log_out ~fmt:"When looking for type of variable '%s'" (Sil.pvar_to_string pvar);
            Printing.log_out ~fmt:" found '%s' in globals.@." (Sil.typ_to_string typ);
            typ
          with Not_found ->
              Printing.log_err
                ~fmt:"WARNING: Variable '%s' not found in local+formal when looking for its type. Returning void.\n%!"
                (Sil.pvar_to_string pvar);
              Sil.Tvoid

(* Extract the type out of a statement. This is useful when the statement  *)
(* denotes actually an expression                                          *)
let extract_type_from_stmt s =
  match s with
  | BinaryConditionalOperator(_, _, expr_info) | ConditionalOperator(_, _, expr_info)
  | AddrLabelExpr(_, _, expr_info, _) | ArraySubscriptExpr(_, _, expr_info)
  | ArrayTypeTraitExpr(_, _, expr_info) | AsTypeExpr(_, _, expr_info)
  | AtomicExpr(_, _, expr_info) | BinaryOperator(_, _, expr_info, _)
  | CompoundAssignOperator(_, _, expr_info, _, _)
  | BlockExpr(_, _, expr_info, _) | CXXBindTemporaryExpr (_, _ , expr_info, _)
  | CXXBoolLiteralExpr (_, _, expr_info, _) | CXXConstructExpr (_, _, expr_info, _)
  | CXXTemporaryObjectExpr (_, _, expr_info, _) | CXXDefaultArgExpr (_, _, expr_info)
  | CXXDefaultInitExpr (_, _, expr_info) | CXXDeleteExpr (_, _, expr_info)
  | CXXDependentScopeMemberExpr (_, _, expr_info) | CXXNewExpr (_, _, expr_info)
  | CXXNoexceptExpr (_, _, expr_info) | CXXNullPtrLiteralExpr (_, _, expr_info)
  | CXXPseudoDestructorExpr (_, _, expr_info) | CXXScalarValueInitExpr (_, _, expr_info)
  | CXXStdInitializerListExpr (_, _, expr_info) | CXXThisExpr (_, _, expr_info)
  | CXXThrowExpr (_, _, expr_info) | CXXTypeidExpr (_, _, expr_info)
  | CXXUnresolvedConstructExpr (_, _, expr_info) | CXXUuidofExpr (_, _, expr_info)
  | CallExpr (_, _, expr_info) | CUDAKernelCallExpr (_, _, expr_info)
  | CXXMemberCallExpr (_, _, expr_info) | CXXOperatorCallExpr (_, _, expr_info)
  | UserDefinedLiteral (_, _, expr_info) | CStyleCastExpr (_, _, expr_info, _, _)
  | CXXFunctionalCastExpr (_, _, expr_info, _, _) | CXXConstCastExpr (_, _, expr_info, _, _, _)
  | CXXDynamicCastExpr (_, _, expr_info, _, _, _) | CXXReinterpretCastExpr(_, _, expr_info, _, _, _)
  | CXXStaticCastExpr (_, _, expr_info, _, _, _) | ObjCBridgedCastExpr (_, _, expr_info, _, _)
  | ImplicitCastExpr (_, _, expr_info, _) | CharacterLiteral (_, _, expr_info, _ )
  | ChooseExpr (_, _, expr_info) | CompoundLiteralExpr (_, _, expr_info)
  | ConvertVectorExpr (_, _, expr_info) | DeclRefExpr (_, _, expr_info, _)
  | DependentScopeDeclRefExpr (_, _, expr_info) | DesignatedInitExpr (_, _, expr_info)
  | ExprWithCleanups (_, _, expr_info, _) | ExpressionTraitExpr (_, _, expr_info)
  | ExtVectorElementExpr (_, _, expr_info) | FloatingLiteral (_, _, expr_info, _)
  | FunctionParmPackExpr (_, _, expr_info) | GNUNullExpr (_, _, expr_info)
  | GenericSelectionExpr (_, _, expr_info) | ImaginaryLiteral (_, _, expr_info)
  | ImplicitValueInitExpr (_, _, expr_info)
  | InitListExpr (_, _, expr_info) | IntegerLiteral(_, _, expr_info, _)
  | LambdaExpr (_, _, expr_info, _) | MSPropertyRefExpr (_, _, expr_info)
  | MaterializeTemporaryExpr (_, _, expr_info, _)
  | MemberExpr (_, _, expr_info, _) | ObjCArrayLiteral (_, _, expr_info)
  | ObjCBoolLiteralExpr (_, _, expr_info , _) | ObjCBoxedExpr (_, _, expr_info, _)
  | ObjCDictionaryLiteral (_, _, expr_info) | ObjCEncodeExpr (_, _, expr_info, _)
  | ObjCIndirectCopyRestoreExpr (_, _, expr_info) | ObjCIsaExpr (_, _, expr_info)
  | ObjCIvarRefExpr(_, _, expr_info, _) | ObjCMessageExpr(_, _, expr_info, _)
  | ObjCPropertyRefExpr(_, _, expr_info, _) | ObjCProtocolExpr (_, _, expr_info, _)
  | ObjCSelectorExpr (_, _, expr_info, _) | ObjCStringLiteral (_, _, expr_info)
  | ObjCSubscriptRefExpr(_, _, expr_info, _) | OffsetOfExpr (_, _, expr_info)
  | OpaqueValueExpr(_, _, expr_info, _) | UnresolvedLookupExpr (_, _, expr_info, _, _)
  | UnresolvedMemberExpr (_, _, expr_info, _) | PackExpansionExpr (_, _, expr_info)
  | ParenExpr (_, _, expr_info) | ParenListExpr (_, _, expr_info)
  | PredefinedExpr (_, _, expr_info, _) | PseudoObjectExpr (_, _, expr_info)
  | ShuffleVectorExpr (_, _, expr_info) | SizeOfPackExpr (_, _, expr_info)
  | StmtExpr (_, _, expr_info) | StringLiteral (_, _, expr_info, _)
  | SubstNonTypeTemplateParmExpr (_, _, expr_info)
  | SubstNonTypeTemplateParmPackExpr (_, _, expr_info)
  | TypeTraitExpr (_, _, expr_info) | UnaryExprOrTypeTraitExpr (_, _, expr_info, _)
  | UnaryOperator(_, _, expr_info, _)
  | VAArgExpr (_, _, expr_info) -> expr_info.Clang_ast_t.ei_qual_type
  | _ -> (* For the other case we cannot get the type info *)
      Printing.log_err ~fmt:"WARNING: Could not get type of statement '%s'\n%!" (Clang_ast_j.string_of_stmt s);
      assert false

let get_desugared_type t =
  match t.Clang_ast_t.qt_desugared with
  | Some t' -> t'
  | _ -> assert false

(* Remove the work 'struct' from a type name. Used to avoid repetition when typename are constructed*)
(* E.g. 'struct struct s' *)
let cut_struct_union s =
  Printing.log_out ~fmt:"Cutting '%s'@." s;
  let buf = Str.split (Str.regexp "[ \t]+") s in
  match buf with
  | "struct":: l (*-> Printing.string_from_list l *)
  | "class":: l
  | "union":: l -> string_from_list l
  | _ -> s

let get_name_from_struct s =
  match s with
  | Sil.Tstruct(_, _, _, Some n, _, _, _) -> n
  | _ -> assert false

let rec get_type_list nn ll =
  match ll with
  | [] -> []
  | (n, t):: ll' -> (* Printing.log_out ">>>>>Searching for type '%s'. Seen '%s'.@." nn n; *)
      if n = nn then (
        Printing.log_out ~fmt:">>>>>>>>>>>>>>>>>>>>>>>NOW Found, Its type is: '%s'@." (Sil.typ_to_string t);
        [t]
      ) else get_type_list nn ll'

let add_pointer_to_typ typ =
  Sil.Tptr(typ, Sil.Pk_pointer)

let remove_pointer_to_typ typ =
  match typ with
  | Sil.Tptr(typ, Sil.Pk_pointer) -> typ
  | _ -> typ

let classname_of_type typ =
  match typ with
  | Sil.Tvar (Sil.TN_csu (_, name) )
  | Sil.Tstruct(_, _, _, (Some name), _, _, _)
  | Sil.Tvar (Sil.TN_typedef name) -> Mangled.to_string name
  | Sil.Tfun _ -> CFrontend_config.objc_object
  | _ -> (Printing.log_out
          ~fmt:"Classname of type cannot be extracted in type %s" (Sil.typ_to_string typ)); assert false

let get_raw_qual_type_decl_ref_exp_info decl_ref_expr_info =
  match decl_ref_expr_info.Clang_ast_t.drti_decl_ref with
  | Some d ->
      (match d.Clang_ast_t.dr_qual_type with
        | Some qt -> Some qt.Clang_ast_t.qt_raw
        | None -> None)
  | None -> None

(* Iterates over the tenv to find the value of the enumeration constant    *)
(* using its name Here we assume that the enumeration constant have        *)
(* different names. Note: this assumption may not be true all the time. So *)
(* need to be careful and give name that cane ensure uniqueness. In case   *)
(* of repeated names it gets the last.                                     *)
let search_enum_type_by_name tenv name =
  let found = ref None in
  let mname = Mangled.from_string name in
  let f tn typ =
    match typ with
    | Sil.Tenum enum_constants ->
        list_iter (fun (c, v) -> if Mangled.equal c mname then found:= Some v else ()) enum_constants
    | _ -> () in
  Sil.tenv_iter f tenv;
  !found

let mk_classname n =
  Sil.TN_csu (Sil.Class, Mangled.from_string n)

let is_class typ =
  match typ with
  | Sil.Tptr( Sil.Tstruct(_, _, _, (Some name), _, _, _), _)
  | Sil.Tptr( Sil.Tvar (Sil.TN_csu (_, name) ), _) ->
      (Mangled.to_string name) = CFrontend_config.objc_class
  | _ -> false
