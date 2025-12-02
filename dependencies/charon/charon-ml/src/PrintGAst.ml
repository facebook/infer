(** Pretty-printing for generic AST (ULLBC and LLBC) *)

open Types
open TypesUtils
open GAst
open PrintUtils
open PrintTypes
open PrintExpressions

let any_decl_id_to_string (id : any_decl_id) : string =
  match id with
  | IdFun id -> FunDeclId.to_string id
  | IdGlobal id -> GlobalDeclId.to_string id
  | IdType id -> TypeDeclId.to_string id
  | IdTraitDecl id -> TraitDeclId.to_string id
  | IdTraitImpl id -> TraitImplId.to_string id

let fn_operand_to_string (env : 'a fmt_env) (op : fn_operand) : string =
  match op with
  | FnOpRegular func -> fn_ptr_to_string env func
  | FnOpMove p -> "move " ^ place_to_string env p

let call_to_string (env : 'a fmt_env) (indent : string) (call : call) : string =
  let func = fn_operand_to_string env call.func in
  let args = List.map (operand_to_string env) call.args in
  let args = "(" ^ String.concat ", " args ^ ")" in
  let dest = place_to_string env call.dest in
  indent ^ dest ^ " := move " ^ func ^ args

let assertion_to_string (env : 'a fmt_env) (indent : string) (a : assertion) :
    string =
  let cond = operand_to_string env a.cond in
  if a.expected then indent ^ "assert(" ^ cond ^ ")"
  else indent ^ "assert(¬" ^ cond ^ ")"

(** Small helper *)
let fun_sig_with_name_to_string (env : 'a fmt_env) (indent : string)
    (indent_incr : string) (attribute : string option) (name : string option)
    (args : local list option) (sg : fun_sig) : string =
  let ty_to_string = ty_to_string env in

  (* Unsafe keyword *)
  let unsafe = if sg.is_unsafe then "unsafe " else "" in

  (* Generics and predicates *)
  let params, clauses =
    predicates_and_trait_clauses_to_string env indent indent_incr sg.generics
  in
  let params =
    if params = [] then "" else "<" ^ String.concat ", " params ^ ">"
  in

  (* Return type *)
  let ret_ty = sg.output in
  let ret_ty = if ty_is_unit ret_ty then "" else " -> " ^ ty_to_string ret_ty in

  (* Arguments *)
  let args =
    match args with
    | None ->
        let args = List.map ty_to_string sg.inputs in
        String.concat ", " args
    | Some args ->
        let args = List.combine args sg.inputs in
        let args =
          List.map
            (fun (var, rty) -> local_to_string var ^ " : " ^ ty_to_string rty)
            args
        in
        String.concat ", " args
  in

  (* Put everything together *)
  let attribute =
    match attribute with
    | None -> ""
    | Some attr -> attr ^ " "
  in
  let name =
    match name with
    | None -> ""
    | Some name -> " " ^ name
  in
  indent ^ attribute ^ unsafe ^ "fn" ^ name ^ params ^ "(" ^ args ^ ")" ^ ret_ty
  ^ clauses

let fun_sig_to_string (env : 'a fmt_env) (indent : string)
    (indent_incr : string) (sg : fun_sig) : string =
  fun_sig_with_name_to_string env indent indent_incr None None None sg

let gfun_decl_to_string (env : 'a fmt_env) (indent : string)
    (indent_incr : string)
    (body_to_string : 'a fmt_env -> string -> string -> 'body -> string)
    (def : 'body gfun_decl) : string =
  (* Locally update the environment *)
  let env = fmt_env_update_generics_and_preds env def.signature.generics in

  let sg = def.signature in

  (* Function name *)
  let name = name_to_string env def.item_meta.name in

  (* We print the declaration differently if it is opaque (no body) or transparent
   * (we have access to a body) *)
  match def.body with
  | None ->
      fun_sig_with_name_to_string env indent indent_incr (Some "opaque")
        (Some name) None sg
  | Some body ->
      (* Locally update the environment *)
      let locals = List.map (fun v -> (v.index, v.name)) body.locals.locals in
      let env = { env with locals } in

      (* Arguments *)
      let inputs = GAstUtils.locals_get_input_vars body.locals in

      (* All the locals (with erased regions) *)
      let locals =
        List.map
          (fun var ->
            indent ^ indent_incr ^ local_to_string var ^ " : "
            ^ ty_to_string env var.var_ty
            ^ ";")
          body.locals.locals
      in
      let locals = String.concat "\n" locals in

      (* Body *)
      let body =
        body_to_string env (indent ^ indent_incr) indent_incr body.body
      in

      (* Put everything together *)
      fun_sig_with_name_to_string env indent indent_incr None (Some name)
        (Some inputs) sg
      ^ indent ^ "\n{\n" ^ locals ^ "\n\n" ^ body ^ "\n" ^ indent ^ "}"

let trait_decl_to_string (env : 'a fmt_env) (indent : string)
    (indent_incr : string) (def : trait_decl) : string =
  (* Locally update the environment *)
  let env = fmt_env_update_generics_and_preds env def.generics in

  let ty_to_string = ty_to_string env in

  (* Name *)
  let name = name_to_string env def.item_meta.name in

  (* Generics and predicates *)
  let params, clauses =
    predicates_and_trait_clauses_to_string env indent indent_incr def.generics
  in
  let params =
    if params = [] then "" else "<" ^ String.concat ", " params ^ ">"
  in

  let indent1 = indent ^ indent_incr in

  let items =
    let parent_clauses =
      List.map
        (fun clause ->
          indent1 ^ "parent_clause_"
          ^ TraitClauseId.to_string clause.clause_id
          ^ " : "
          ^ trait_clause_to_string env clause
          ^ "\n")
        def.parent_clauses
    in
    let consts =
      List.map
        (fun (name, ty) ->
          let ty = ty_to_string ty in
          indent1 ^ "const " ^ name ^ " : " ^ ty ^ "\n")
        def.consts
    in
    let types =
      List.map (fun name -> indent1 ^ "type " ^ name ^ "\n") def.types
    in
    let methods =
      List.map
        (fun ((name, f) : _ * fun_decl_ref binder) ->
          indent1 ^ "fn " ^ name ^ " : "
          ^ fun_decl_id_to_string env f.binder_value.id
          ^ "\n")
        def.methods
    in
    let items = List.concat [ parent_clauses; consts; types; methods ] in
    if items = [] then "" else "\n{\n" ^ String.concat "" items ^ "}"
  in

  "trait " ^ name ^ params ^ clauses ^ items

let trait_impl_to_string (env : 'a fmt_env) (indent : string)
    (indent_incr : string) (def : trait_impl) : string =
  (* Locally update the environment *)
  let env = fmt_env_update_generics_and_preds env def.generics in

  let ty_to_string = ty_to_string env in

  (* Name *)
  let name = name_to_string env def.item_meta.name in

  (* Generics and predicates *)
  let params, clauses =
    predicates_and_trait_clauses_to_string env indent indent_incr def.generics
  in
  let params =
    if params = [] then "" else "<" ^ String.concat ", " params ^ ">"
  in

  let indent1 = indent ^ indent_incr in

  let items =
    (* The parent clauses are given by the trait refs of the implemented trait *)
    let parent_clauses =
      Collections.List.mapi
        (fun i trait_ref ->
          indent1 ^ "parent_clause" ^ string_of_int i ^ " = "
          ^ trait_ref_to_string env trait_ref
          ^ "\n")
        def.parent_trait_refs
    in
    let consts =
      List.map
        (fun (name, gref) ->
          let gref = global_decl_ref_to_string env gref in
          indent1 ^ "const " ^ name ^ " = " ^ gref ^ "\n")
        def.consts
    in
    let types =
      List.map
        (fun (name, ty) ->
          indent1 ^ "type " ^ name ^ " = " ^ ty_to_string ty ^ "\n")
        def.types
    in
    let env_method ((name, f) : _ * fun_decl_ref binder) =
      indent1 ^ "fn " ^ name ^ " : "
      ^ fun_decl_id_to_string env f.binder_value.id
      ^ "\n"
    in
    let methods = List.map env_method def.methods in
    let items = List.concat [ parent_clauses; consts; types; methods ] in
    if items = [] then "" else "\n{\n" ^ String.concat "" items ^ "}"
  in

  let impl_trait = trait_decl_ref_to_string env def.impl_trait in
  "impl" ^ params ^ " " ^ name ^ params ^ " : " ^ impl_trait ^ clauses ^ items
