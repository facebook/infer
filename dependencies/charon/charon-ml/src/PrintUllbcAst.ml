open PrintUtils
open Types
open TypesUtils
open UllbcAst
open PrintTypes
open PrintValues
open PrintExpressions

type fmt_env = blocks PrintUtils.fmt_env

(** Pretty-printing for ULLBC AST (generic functions) *)
module Ast = struct
  include PrintGAst

  let rec statement_to_string (env : fmt_env) (indent : string) (st : statement)
      : string =
    raw_statement_to_string env indent st.content

  and raw_statement_to_string (env : fmt_env) (indent : string)
      (st : raw_statement) : string =
    match st with
    | Assign (p, rv) ->
        indent ^ place_to_string env p ^ " := " ^ rvalue_to_string env rv
    | SetDiscriminant (p, variant_id) ->
        (* TODO: improve this to lookup the variant name by using the def id
           (we are missing the def id here) *)
        indent ^ "set_discriminant(" ^ place_to_string env p ^ ", "
        ^ variant_id_to_pretty_string variant_id
        ^ ")"
    | Assert a -> assertion_to_string env indent a
    | StorageLive var_id ->
        indent ^ "storage_live " ^ local_id_to_string env var_id
    | StorageDead var_id ->
        indent ^ "storage_dead " ^ local_id_to_string env var_id
    | Deinit p -> indent ^ "deinit " ^ place_to_string env p
    | Drop (p, _) -> indent ^ "drop " ^ place_to_string env p
    | CopyNonOverlapping { src; dst; count } ->
        indent ^ "copy_non_overlapping(" ^ operand_to_string env src ^ ", "
        ^ operand_to_string env dst ^ ", "
        ^ operand_to_string env count
        ^ ")"
    | Nop -> "nop"

  let switch_to_string (indent : string) (tgt : switch) : string =
    match tgt with
    | If (b0, b1) ->
        let b0 = block_id_to_string b0 in
        let b1 = block_id_to_string b1 in
        indent ^ "[true -> " ^ b0 ^ "; false -> " ^ b1 ^ "]"
    | SwitchInt (_int_ty, branches, otherwise) ->
        let branches =
          List.map
            (fun (sv, bid) ->
              scalar_value_to_string sv ^ " -> " ^ block_id_to_string bid ^ "; ")
            branches
        in
        let branches = String.concat "" branches in
        let otherwise = "_ -> " ^ block_id_to_string otherwise in
        indent ^ "[" ^ branches ^ otherwise ^ "]"

  let rec terminator_to_string (env : fmt_env) (indent : string)
      (st : terminator) : string =
    raw_terminator_to_string env indent st.content

  and raw_terminator_to_string (env : fmt_env) (indent : string)
      (st : raw_terminator) : string =
    match st with
    | Goto bid -> indent ^ "goto " ^ block_id_to_string bid
    | Switch (op, tgts) ->
        indent ^ "switch " ^ operand_to_string env op
        ^ switch_to_string indent tgts
    | Call (call, tgt, unwind) ->
        call_to_string env indent call
        ^ " -> " ^ block_id_to_string tgt ^ "(unwind:"
        ^ block_id_to_string unwind ^ ")"
    | Abort _ -> indent ^ "panic"
    | Return -> indent ^ "return"
    | UnwindResume -> indent ^ "unwind_continue"

  let block_to_string (env : fmt_env) (indent : string) (indent_incr : string)
      (id : BlockId.id) (block : block) : string =
    let indent1 = indent ^ indent_incr in
    let statements =
      List.map
        (fun st -> statement_to_string env indent1 st ^ ";\n")
        block.statements
    in
    let terminator = terminator_to_string env indent1 block.terminator in
    indent ^ block_id_to_string id ^ " {\n"
    ^ String.concat "" statements
    ^ terminator ^ ";\n" ^ indent ^ "}"

  let blocks_to_string (env : fmt_env) (indent : string) (indent_incr : string)
      (blocks : block list) : string =
    let blocks = BlockId.mapi (block_to_string env indent indent_incr) blocks in
    String.concat "\n\n" blocks

  let fun_decl_to_string (env : fmt_env) (indent : string)
      (indent_incr : string) (def : fun_decl) : string =
    gfun_decl_to_string env indent indent_incr blocks_to_string def

  let global_decl_to_string (env : fmt_env) (indent : string)
      (indent_incr : string) (def : global_decl) : string =
    (* Locally update the generics and the predicates *)
    let env = fmt_env_update_generics_and_preds env def.generics in
    let params, clauses =
      predicates_and_trait_clauses_to_string env "" "  " def.generics
    in
    let params =
      if params <> [] then "<" ^ String.concat ", " params ^ ">" else ""
    in

    let name = name_to_string env def.item_meta.name in
    let ty = ty_to_string env def.ty in

    let body_id = fun_decl_id_to_string env def.body in
    indent ^ "global " ^ name ^ params ^ clauses ^ " : " ^ ty ^ " = " ^ body_id
end

(** Pretty-printing for ASTs (functions based on a declaration context) *)
module Crate = struct
  open Ast

  let crate_to_fmt_env (crate : crate) : fmt_env =
    { crate; generics = []; locals = [] }

  let crate_to_string (m : crate) : string =
    let env : fmt_env = crate_to_fmt_env m in

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
