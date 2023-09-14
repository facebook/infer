(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

let func_name_from_typename ~stem = function "t" -> stem | s -> stem ^ "_" ^ s

let normalize_from_typename = func_name_from_typename ~stem:"normalize"

let normalize_of_longident ~loc ?(suffix = "") lid =
  let ident =
    match lid with
    | Lident "string" ->
        (* [HashNormalizer.StringNormalizer.normalize] *)
        Ldot (Ldot (Lident "HashNormalizer", "StringNormalizer"), "normalize" ^ suffix)
    | Lident typename ->
        (* [t]/[x] is not enclosed in a module *)
        Lident (normalize_from_typename typename ^ suffix)
    | Ldot (l, typename) ->
        (* [M.t]/[M.x] *)
        Ldot (Ldot (l, "Normalizer"), normalize_from_typename typename ^ suffix)
    | _ ->
        Format.eprintf "Could not parse ident: %a@\n" Common.pp_longident lid ;
        assert false
  in
  Loc.make ~loc ident


(* ident `A.B.C.normalize`/`A.B.C.normalize_opt` from the type `A.B.C.t`/`A.B.C.t option` *)
let normalize_of_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_constr (l, []) ->
      (* monomorphic type *)
      normalize_of_longident ~loc l.txt
  | Ptyp_constr ({txt= Lident "option"}, [{ptyp_desc= Ptyp_constr (l, [])}]) ->
      (* option type *)
      normalize_of_longident ~loc ~suffix:"_opt" l.txt
  | Ptyp_constr ({txt= Lident "list"}, [{ptyp_desc= Ptyp_constr (l, [])}]) ->
      (* option type *)
      normalize_of_longident ~loc ~suffix:"_list" l.txt
  | Ptyp_constr _
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_arrow _
  | Ptyp_tuple _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_variant _
  | Ptyp_poly _
  | Ptyp_package _
  | Ptyp_extension _ ->
      assert false


(* [let name = body] where [name] = normalization function from type name *)
let make_normalize_function ~loc typ_name body =
  let func_name = normalize_from_typename typ_name.txt in
  Common.make_function ~loc func_name body


(* [let var' = (normalize_of_core_type typ) var in acc] *)
let let_varprime_equal_f_var_expr ~loc acc var typ =
  let lhs_pat = Ast_helper.Pat.var ~loc (Loc.make ~loc (var ^ "'")) in
  let var_expr = Common.make_ident_exp ~loc var in
  let f = normalize_of_core_type ~loc typ |> Ast_helper.Exp.ident ~loc in
  [%expr
    let [%p lhs_pat] = [%e f] [%e var_expr] in
    [%e acc]]


(* [phys_equal var var'] *)
let var_phys_equal_varprime ~loc var =
  let var_expr = Common.make_ident_exp ~loc var in
  let varprime_expr = Common.make_ident_exp ~loc (var ^ "'") in
  [%expr phys_equal [%e var_expr] [%e varprime_expr]]


(* [if (phys_equal a a' && phys_equal b b' && ...) then t else else_exp] *)
let if_phys_equal_then_t ~loc ~else_exp vars =
  let guard = List.map vars ~f:(var_phys_equal_varprime ~loc) |> Common.conjunction ~loc in
  [%expr if [%e guard] then t else [%e else_exp]]


(* [M.normalize (t : core_type)] *)
let create_normalize_initializer ~loc t_expr core_type =
  let func_lid = normalize_of_core_type ~loc core_type in
  let func_name = Ast_helper.Exp.ident ~loc func_lid in
  [%expr [%e func_name] [%e t_expr]]


(* [Field.normalize field] *)
let create_normalize_field_initializer ~loc (ld : label_declaration) =
  let rhs_exp = Common.make_ident_exp ~loc ld.pld_name.txt in
  create_normalize_initializer ~loc rhs_exp ld.pld_type


let should_normalize_type core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr ({txt= Lident ("bool" | "char" | "float" | "int" | "unit")}, _) ->
      false
  | _ ->
      true


let tuple_pattern_and_body ~loc ~constructor core_types =
  if List.is_empty core_types then (Ast_helper.Pat.any ~loc (), [%expr t])
  else if List.for_all core_types ~f:(Fn.non should_normalize_type) then
    (* just return [t] *)
    (Ast_helper.Pat.any ~loc (), [%expr t])
  else
    let vars = List.mapi core_types ~f:(fun i _ -> Printf.sprintf "x%d" i) in
    let vars_patterns =
      List.map vars ~f:(fun var -> Loc.make ~loc var |> Ast_helper.Pat.var ~loc)
    in
    let vars_pattern =
      match vars_patterns with [pattern] -> pattern | _ -> Ast_helper.Pat.tuple ~loc vars_patterns
    in
    let return_result =
      (* [(x0, x1', x2, ...)] where a variable is primed if it's normalizable *)
      List.map2_exn vars core_types ~f:(fun var typ ->
          (if should_normalize_type typ then var ^ "'" else var) |> Common.make_ident_exp ~loc )
    in
    let return_tuple =
      (match return_result with [exp] -> exp | _ -> Ast_helper.Exp.tuple ~loc return_result)
      |> constructor
    in
    let normalizable_vars, normalizable_typs =
      List.zip_exn vars core_types
      |> List.filter ~f:(fun (_var, typ) -> should_normalize_type typ)
      |> List.unzip
    in
    let physeq_guarded = if_phys_equal_then_t ~loc normalizable_vars ~else_exp:return_tuple in
    let rhs =
      List.fold2_exn normalizable_vars normalizable_typs ~init:physeq_guarded
        ~f:(let_varprime_equal_f_var_expr ~loc)
    in
    (vars_pattern, rhs)


let normalize_tuple_impl ~loc ptype_name core_types =
  let vars_pattern, rhs = tuple_pattern_and_body ~loc ~constructor:Fn.id core_types in
  let rhs_expr =
    [%expr
      let [%p vars_pattern] = t in
      [%e rhs]]
  in
  make_normalize_function ~loc ptype_name [%expr fun t -> [%e rhs_expr]]


let record_pattern_and_body ~loc ~constructor (lds : label_declaration list) =
  let normalizable_names_types =
    List.filter_map lds ~f:(fun ld ->
        Option.some_if (should_normalize_type ld.pld_type) (ld.pld_name.txt, ld.pld_type) )
  in
  if List.is_empty normalizable_names_types then (Ast_helper.Pat.any ~loc (), [%expr t])
  else
    let rhs_exps =
      List.map lds ~f:(fun ld ->
          (if should_normalize_type ld.pld_type then ld.pld_name.txt ^ "'" else ld.pld_name.txt)
          |> Common.make_ident_exp ~loc )
    in
    let record_exp = Common.create_record ~loc lds rhs_exps |> constructor in
    let guarded =
      if_phys_equal_then_t ~loc (List.unzip normalizable_names_types |> fst) ~else_exp:record_exp
    in
    let final_expr =
      List.fold normalizable_names_types ~init:guarded ~f:(fun acc (var, typ) ->
          let_varprime_equal_f_var_expr ~loc acc var typ )
    in
    let lid_pattern_list =
      List.map lds ~f:(fun ld ->
          (Common.make_longident ~loc ld.pld_name.txt, Ast_helper.Pat.var ~loc ld.pld_name) )
    in
    let record_pattern = Ast_helper.Pat.record ~loc lid_pattern_list Closed in
    (record_pattern, final_expr)


let normalize_record_impl ~loc ptype_name (lds : label_declaration list) =
  let pattern, final_expr = record_pattern_and_body ~loc ~constructor:Fn.id lds in
  let body =
    [%expr
      let [%p pattern] = t in
      [%e final_expr]]
  in
  make_normalize_function ~loc ptype_name [%expr fun t -> [%e body]]


let normalize_passthrough_impl ~loc ptype_name manifest_type =
  let normalize_name = normalize_from_typename ptype_name.txt in
  Common.generate_passthrough_function ~loc normalize_of_core_type normalize_name manifest_type


let make_variant_case ~loc (constructor_declaration : constructor_declaration) : case =
  let constructor_ident = Common.make_longident ~loc constructor_declaration.pcd_name.txt in
  let constructor expr = Ast_helper.Exp.construct ~loc constructor_ident (Some expr) in
  let pattern, rhs =
    match constructor_declaration.pcd_args with
    | Pcstr_tuple [] ->
        (None, [%expr t])
    | Pcstr_tuple core_types ->
        let pattern, rhs = tuple_pattern_and_body ~loc ~constructor core_types in
        (Some pattern, rhs)
    | Pcstr_record label_declarations ->
        let pattern, rhs = record_pattern_and_body ~loc ~constructor label_declarations in
        (Some pattern, rhs)
  in
  let case_pattern = Ast_helper.Pat.construct ~loc constructor_ident pattern in
  Ast_helper.Exp.case case_pattern rhs


let normalize_variant_impl ~loc ptype_name (constructor_declarations : constructor_declaration list)
    =
  let case_list = List.map constructor_declarations ~f:(make_variant_case ~loc) in
  let match_expr = Ast_helper.Exp.match_ ~loc [%expr t] case_list in
  let body = [%expr fun t -> [%e match_expr]] in
  make_normalize_function ~loc ptype_name body


let hash_normalize ~loc typ_name =
  let norm_name = normalize_from_typename typ_name.txt in
  let norm_name_expr = Common.make_ident_exp ~loc norm_name in
  let equal_name_expr =
    func_name_from_typename ~stem:"equal" typ_name.txt |> Common.make_ident_exp ~loc
  in
  let hash_name_expr =
    func_name_from_typename ~stem:"hash" typ_name.txt |> Common.make_ident_exp ~loc
  in
  let ct = Ast_helper.Typ.constr ~loc (Common.make_longident ~loc typ_name.txt) [] in
  let body =
    [%expr
      let module T = struct
        type nonrec t = [%t ct]

        let equal = [%e equal_name_expr]

        let hash = [%e hash_name_expr]
      end in
      let module H = Caml.Hashtbl.Make (T) in
      let table : T.t H.t = H.create 11 in
      let () = HashNormalizer.register_reset (fun () -> H.reset table) in
      fun t ->
        match H.find_opt table t with
        | Some t' ->
            t'
        | None ->
            let normalized = [%e norm_name_expr] t in
            H.add table normalized normalized ;
            normalized]
  in
  let hash_norm_name = "hash_" ^ norm_name in
  Common.make_function ~loc hash_norm_name body


let hash_normalize_opt ~loc typ_name =
  let hash_norm_name = "hash_" ^ normalize_from_typename typ_name.txt in
  let hash_norm_name_expr = Common.make_ident_exp ~loc hash_norm_name in
  let body =
    [%expr
      function
      | Some _ as some_t ->
          IOption.map_changed ~equal:phys_equal ~f:[%e hash_norm_name_expr] some_t
      | None ->
          None]
  in
  Common.make_function ~loc (hash_norm_name ^ "_opt") body


let hash_normalize_list ~loc typ_name =
  let hash_norm_name = "hash_" ^ normalize_from_typename typ_name.txt in
  let hash_norm_name_expr = Common.make_ident_exp ~loc hash_norm_name in
  let body = [%expr fun ts -> IList.map_changed ts ~equal:phys_equal ~f:[%e hash_norm_name_expr]] in
  Common.make_function ~loc (hash_norm_name ^ "_list") body


let hash_normalize_functions ~loc typ_name =
  [hash_normalize ~loc typ_name; hash_normalize_opt ~loc typ_name; hash_normalize_list ~loc typ_name]


let normalize_type_declaration ~loc (td : type_declaration) =
  match td with
  | {ptype_kind= Ptype_record fields; ptype_name} ->
      normalize_record_impl ~loc ptype_name fields :: hash_normalize_functions ~loc ptype_name
  | {ptype_kind= Ptype_abstract; ptype_manifest= Some {ptyp_desc= Ptyp_tuple core_types}; ptype_name}
    ->
      normalize_tuple_impl ~loc ptype_name core_types :: hash_normalize_functions ~loc ptype_name
  | {ptype_kind= Ptype_variant constructor_declarations; ptype_name} ->
      normalize_variant_impl ~loc ptype_name constructor_declarations
      :: hash_normalize_functions ~loc ptype_name
  | { ptype_kind= Ptype_abstract
    ; ptype_manifest= Some ({ptyp_desc= Ptyp_constr _} as manifest_type)
    ; ptype_name } ->
      (* passthrough case like `let nonrec t = t` *)
      normalize_passthrough_impl ~loc ptype_name manifest_type
      :: hash_normalize_functions ~loc ptype_name
  | {ptype_loc; _} ->
      let ext = Location.error_extensionf ~loc:ptype_loc "Cannot derive functions for this type" in
      [Ast_builder.Default.pstr_extension ~loc ext []]


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(normalize_type_declaration ~loc) |> List.concat


let _ =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "normalize" ~str_type_decl
