open PrintUtils
open Types
open TypesUtils
open LlbcAst
open PrintTypes
open PrintValues
open PrintExpressions

type fmt_env = block PrintUtils.fmt_env

(** Pretty-printing for LLBC AST (generic functions) *)
module Ast = struct
  include PrintGAst

  let rec statement_to_string (env : fmt_env) (indent : string)
      (indent_incr : string) (st : statement) : string =
    raw_statement_to_string env indent indent_incr st.content

  and raw_statement_to_string (env : fmt_env) (indent : string)
      (indent_incr : string) (st : raw_statement) : string =
    match st with
    | Assign (p, rv) ->
        indent ^ place_to_string env p ^ " := " ^ rvalue_to_string env rv
    | SetDiscriminant (p, variant_id) ->
        (* TODO: improve this to lookup the variant name by using the def id *)
        indent ^ "set_discriminant(" ^ place_to_string env p ^ ", "
        ^ VariantId.to_string variant_id
        ^ ")"
    | CopyNonOverlapping { src; dst; count } ->
        indent ^ "copy_non_overlapping(" ^ operand_to_string env src ^ ", "
        ^ operand_to_string env dst ^ ", "
        ^ operand_to_string env count
        ^ ")"
    | StorageLive var_id ->
        indent ^ "storage_live " ^ local_id_to_string env var_id
    | StorageDead var_id ->
        indent ^ "storage_dead " ^ local_id_to_string env var_id
    | Deinit p -> indent ^ "deinit " ^ place_to_string env p
    | Drop (p, _) -> indent ^ "drop " ^ place_to_string env p
    | Assert a -> assertion_to_string env indent a
    | Call call -> call_to_string env indent call
    | Abort (Panic _) -> indent ^ "panic"
    | Abort UndefinedBehavior -> indent ^ "undefined_behavior"
    | Abort UnwindTerminate -> indent ^ "unwind_terminate"
    | Return -> indent ^ "return"
    | Break i -> indent ^ "break " ^ string_of_int i
    | Continue i -> indent ^ "continue " ^ string_of_int i
    | Nop -> indent ^ "nop"
    | Switch switch -> (
        match switch with
        | If (op, true_st, false_st) ->
            let op = operand_to_string env op in
            let inner_indent = indent ^ indent_incr in
            let inner_to_string =
              block_to_string env inner_indent indent_incr
            in
            let true_st = inner_to_string true_st in
            let false_st = inner_to_string false_st in
            indent ^ "if (" ^ op ^ ") {\n" ^ true_st ^ "\n" ^ indent ^ "}\n"
            ^ indent ^ "else {\n" ^ false_st ^ "\n" ^ indent ^ "}"
        | SwitchInt (op, _ty, branches, otherwise) ->
            let op = operand_to_string env op in
            let indent1 = indent ^ indent_incr in
            let indent2 = indent1 ^ indent_incr in
            let inner_to_string2 = block_to_string env indent2 indent_incr in
            let branches =
              List.map
                (fun (svl, be) ->
                  let svl =
                    List.map (fun sv -> "| " ^ scalar_value_to_string sv) svl
                  in
                  let svl = String.concat " " svl in
                  indent ^ svl ^ " => {\n" ^ inner_to_string2 be ^ "\n"
                  ^ indent1 ^ "}")
                branches
            in
            let branches = String.concat "\n" branches in
            let branches =
              branches ^ "\n" ^ indent1 ^ "_ => {\n"
              ^ inner_to_string2 otherwise ^ "\n" ^ indent1 ^ "}"
            in
            indent ^ "switch (" ^ op ^ ") {\n" ^ branches ^ "\n" ^ indent ^ "}"
        | Match (p, branches, otherwise) ->
            let p = place_to_string env p in
            let indent1 = indent ^ indent_incr in
            let indent2 = indent1 ^ indent_incr in
            let inner_to_string2 = block_to_string env indent2 indent_incr in
            let branches =
              List.map
                (fun (svl, be) ->
                  let svl =
                    List.map (fun sv -> "| " ^ VariantId.to_string sv) svl
                  in
                  let svl = String.concat " " svl in
                  indent ^ svl ^ " => {\n" ^ inner_to_string2 be ^ "\n"
                  ^ indent1 ^ "}")
                branches
            in
            let branches = String.concat "\n" branches in
            let otherwise =
              match otherwise with
              | None -> ""
              | Some otherwise ->
                  "\n" ^ indent1 ^ "_ => {\n" ^ inner_to_string2 otherwise
                  ^ "\n" ^ indent1 ^ "}"
            in
            let branches = branches ^ otherwise in
            indent ^ "match (" ^ p ^ ") {\n" ^ branches ^ "\n" ^ indent ^ "}")
    | Loop loop_blk ->
        indent ^ "loop {\n"
        ^ block_to_string env (indent ^ indent_incr) indent_incr loop_blk
        ^ "\n" ^ indent ^ "}"
    | Error s -> indent ^ "ERROR(' " ^ s ^ "')"

  and block_to_string (env : fmt_env) (indent : string) (indent_incr : string)
      (b : block) : string =
    String.concat ";\n"
      (List.map (statement_to_string env indent indent_incr) b.statements)

  let fun_sig_to_string (env : fmt_env) (indent : string) (indent_incr : string)
      (sg : fun_sig) : string =
    fun_sig_to_string env indent indent_incr sg

  let fun_decl_to_string (env : fmt_env) (indent : string)
      (indent_incr : string) (def : fun_decl) : string =
    gfun_decl_to_string env indent indent_incr block_to_string def

  let global_decl_to_string (env : fmt_env) (indent : string)
      (_indent_incr : string) (def : global_decl) : string =
    (* Locally update the generics and the predicates *)
    let env = fmt_env_update_generics_and_preds env def.generics in
    let params, clauses =
      predicates_and_trait_clauses_to_string env "" "  " def.generics
    in
    let params =
      if params <> [] then "<" ^ String.concat ", " params ^ ">" else ""
    in

    (* Global name *)
    let name = name_to_string env def.item_meta.name in

    (* Type *)
    let ty = ty_to_string env def.ty in

    let body_id = fun_decl_id_to_string env def.body in
    indent ^ "global " ^ name ^ params ^ clauses ^ " : " ^ ty ^ " = " ^ body_id
end

(** Pretty-printing for ASTs (functions based on a declaration context) *)
module Crate = struct
  open Ast

  let crate_to_fmt_env (crate : crate) : fmt_env =
    { crate; generics = []; locals = [] }

  let crate_fun_decl_to_string (m : crate) (d : fun_decl) : string =
    let env = crate_to_fmt_env m in
    fun_decl_to_string env "" "  " d

  let crate_to_string (m : crate) : string =
    let env = crate_to_fmt_env m in

    (* The types *)
    let type_decls =
      List.map
        (fun (_, d) -> type_decl_to_string env d)
        (TypeDeclId.Map.bindings m.type_decls)
    in

    (* The globals *)
    let global_decls =
      List.map
        (fun (_, d) -> global_decl_to_string env "" "  " d)
        (GlobalDeclId.Map.bindings m.global_decls)
    in

    (* The functions *)
    let fun_decls =
      List.map
        (fun (_, d) -> fun_decl_to_string env "" "  " d)
        (FunDeclId.Map.bindings m.fun_decls)
    in

    (* The trait declarations *)
    let trait_decls =
      List.map
        (fun (_, d) -> trait_decl_to_string env "" "  " d)
        (TraitDeclId.Map.bindings m.trait_decls)
    in

    (* The trait implementations *)
    let trait_impls =
      List.map
        (fun (_, d) -> trait_impl_to_string env "" "  " d)
        (TraitImplId.Map.bindings m.trait_impls)
    in

    (* Put everything together *)
    let all_defs =
      List.concat
        [ type_decls; global_decls; trait_decls; trait_impls; fun_decls ]
    in
    String.concat "\n\n" all_defs
end
