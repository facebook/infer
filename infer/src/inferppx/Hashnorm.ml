(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

exception BadType of extension

let func_name_from_typename ~stem = function "t" -> stem | s -> stem ^ "_" ^ s

let hash_normalize_from_typename = func_name_from_typename ~stem:"hash_normalize"

let hashtable_find_opt_stem = "hash_normalize_find_opt"

let hashtable_add_stem = "hash_normalize_add"

let hash_normalize_of_longident ~loc ?(suffix = "") lid =
  let ident =
    match lid with
    | Lident "string" ->
        (* [HashNormalizer.String.hash_normalize] *)
        Ldot (Ldot (Lident "HashNormalizer", "String"), "hash_normalize" ^ suffix)
    | Lident "int64" ->
        Ldot (Ldot (Lident "HashNormalizer", "Int64"), "hash_normalize" ^ suffix)
    | Lident typename ->
        (* [t]/[x] is not enclosed in a module *)
        Lident (hash_normalize_from_typename typename ^ suffix)
    | Ldot (l, typename) ->
        (* [M.t]/[M.x] *)
        Ldot (l, hash_normalize_from_typename typename ^ suffix)
    | _ ->
        raise
          (BadType
             (Location.error_extensionf ~loc "Could not parse ident: %a@\n" Common.pp_longident lid)
          )
  in
  Loc.make ~loc ident


(* ident `A.B.C.normalize`/`A.B.C.normalize_opt` from the type `A.B.C.t`/`A.B.C.t option` *)
let hash_normalize_of_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_constr (l, []) ->
      (* monomorphic type *)
      hash_normalize_of_longident ~loc l.txt
  | Ptyp_constr ({txt= Lident "option"}, [{ptyp_desc= Ptyp_constr (l, [])}]) ->
      (* option type *)
      hash_normalize_of_longident ~loc ~suffix:"_opt" l.txt
  | Ptyp_constr ({txt= Lident "list"}, [{ptyp_desc= Ptyp_constr (l, [])}]) ->
      (* option type *)
      hash_normalize_of_longident ~loc ~suffix:"_list" l.txt
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
      raise
        (BadType
           (Location.error_extensionf ~loc "Could not get normalizer for type %s@\n"
              (string_of_core_type ct) ) )


(* [let var' = (hash_normalize_of_core_type typ) var in acc] *)
let let_varprime_equal_f_var_expr ~loc acc var typ =
  let lhs_pat = Ast_helper.Pat.var ~loc (Loc.make ~loc (var ^ "'")) in
  let var_expr = Common.make_ident_exp ~loc var in
  let f = hash_normalize_of_core_type ~loc typ |> Ast_helper.Exp.ident ~loc in
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


let rec should_normalize_type core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr ({txt= Lident ("bool" | "char" | "float" | "int" | "unit")}, _) ->
      false
  | Ptyp_constr ({txt= Lident ("option" | "list")}, [type_param]) ->
      should_normalize_type type_param
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


let normalize_tuple_impl ~loc core_types =
  let vars_pattern, rhs = tuple_pattern_and_body ~loc ~constructor:Fn.id core_types in
  [%expr
    fun t ->
      let [%p vars_pattern] = t in
      [%e rhs]]


let ignore_attribute =
  Attribute.declare "ignore" Attribute.Context.label_declaration Ast_pattern.(pstr nil) ()


let record_pattern_and_body ~loc ~constructor (lds : label_declaration list) =
  let should_normalize ld =
    Option.is_none (Attribute.get ignore_attribute ld) && should_normalize_type ld.pld_type
  in
  let normalizable_names_types =
    List.filter_map lds ~f:(fun ld ->
        Option.some_if (should_normalize ld) (ld.pld_name.txt, ld.pld_type) )
  in
  if List.is_empty normalizable_names_types then (Ast_helper.Pat.any ~loc (), [%expr t])
  else
    let rhs_exps =
      List.map lds ~f:(fun ld ->
          (if should_normalize ld then ld.pld_name.txt ^ "'" else ld.pld_name.txt)
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


let normalize_record_impl ~loc (lds : label_declaration list) =
  let pattern, final_expr = record_pattern_and_body ~loc ~constructor:Fn.id lds in
  [%expr
    fun t ->
      let [%p pattern] = t in
      [%e final_expr]]


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


let normalize_variant_impl ~loc (constructor_declarations : constructor_declaration list) =
  let case_list = List.map constructor_declarations ~f:(make_variant_case ~loc) in
  let match_expr = Ast_helper.Exp.match_ ~loc [%expr t] case_list in
  [%expr fun t -> [%e match_expr]]


let normalize ~loc (td : type_declaration) =
  match td with
  | {ptype_kind= Ptype_record fields} ->
      normalize_record_impl ~loc fields
  | {ptype_kind= Ptype_abstract; ptype_manifest= Some {ptyp_desc= Ptyp_tuple core_types}} ->
      normalize_tuple_impl ~loc core_types
  | {ptype_kind= Ptype_variant constructor_declarations} ->
      normalize_variant_impl ~loc constructor_declarations
  | _ ->
      raise (BadType (Location.error_extensionf ~loc "Cannot derive functions for this type"))


let make_function_value_binding ~loc name body =
  Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc (Loc.make ~loc name)) body


let make_passthrough ~loc (td : type_declaration) =
  match td with
  | {ptype_kind= Ptype_abstract; ptype_manifest= Some ({ptyp_desc= Ptyp_constr _} as manifest_type)}
    ->
      (* passthrough case like `let nonrec t = t` *)
      Some (Ast_helper.Exp.ident ~loc (hash_normalize_of_core_type ~loc manifest_type))
  | _ ->
      None


let maybe_make_hashtable_api ~loc td =
  let hashtable_api ~loc (td : type_declaration) =
    let equal_name_expr =
      func_name_from_typename ~stem:"equal" td.ptype_name.txt |> Common.make_ident_exp ~loc
    in
    let hash_name_expr =
      func_name_from_typename ~stem:"hash" td.ptype_name.txt |> Common.make_ident_exp ~loc
    in
    let ct = Ast_helper.Typ.constr ~loc (Common.make_longident ~loc td.ptype_name.txt) [] in
    let body =
      [%expr
        let module H = Caml.Hashtbl.Make (struct
          type nonrec t = [%t ct]

          let equal = [%e equal_name_expr]

          let hash = [%e hash_name_expr]
        end) in
        let table : [%t ct] H.t = H.create 11 in
        let () = HashNormalizer.register_reset (fun () -> H.reset table) in
        ((fun t -> H.find_opt table t), fun t -> H.add table t t)]
    in
    let make_pattern stem =
      Ast_helper.Pat.var ~loc @@ Loc.make ~loc @@ func_name_from_typename ~stem td.ptype_name.txt
    in
    let make_patterns ls = List.map ~f:make_pattern ls in
    let pattern =
      Ast_helper.Pat.tuple ~loc (make_patterns [hashtable_find_opt_stem; hashtable_add_stem])
    in
    Ast_helper.Str.value ~loc Nonrecursive [Ast_helper.Vb.mk ~loc pattern body]
  in
  if make_passthrough ~loc td |> Option.is_some then None else Some (hashtable_api ~loc td)


let hash_normalize ~loc (td : type_declaration) =
  let hash_norm_name = hash_normalize_from_typename td.ptype_name.txt in
  let body =
    match make_passthrough ~loc td with
    | Some e ->
        e
    | None ->
        let normalize = normalize ~loc td in
        let hashtable_find_opt =
          Common.make_ident_exp ~loc
          @@ func_name_from_typename ~stem:hashtable_find_opt_stem td.ptype_name.txt
        in
        let hashtable_add =
          Common.make_ident_exp ~loc
          @@ func_name_from_typename ~stem:hashtable_add_stem td.ptype_name.txt
        in
        [%expr
          let normalize = [%e normalize] in
          fun t ->
            match [%e hashtable_find_opt] t with
            | Some t' ->
                t'
            | None ->
                let normalized = normalize t in
                [%e hashtable_add] normalized ;
                normalized]
  in
  make_function_value_binding ~loc hash_norm_name body


let hash_normalize_opt ~loc typ_name =
  let hash_norm_name = hash_normalize_from_typename typ_name.txt in
  let hash_norm_name_expr = Common.make_ident_exp ~loc hash_norm_name in
  let body =
    [%expr
      function
      | Some _ as some_t ->
          IOption.map_changed ~equal:phys_equal ~f:[%e hash_norm_name_expr] some_t
      | None ->
          None]
  in
  make_function_value_binding ~loc (hash_norm_name ^ "_opt") body


let hash_normalize_list ~loc typ_name =
  let hash_norm_name = hash_normalize_from_typename typ_name.txt in
  let hash_norm_name_expr = Common.make_ident_exp ~loc hash_norm_name in
  let body = [%expr fun ts -> IList.map_changed ~equal:phys_equal ~f:[%e hash_norm_name_expr] ts] in
  make_function_value_binding ~loc (hash_norm_name ^ "_list") body


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt in
  let process_type_declaration (td : type_declaration) =
    let loc = td.ptype_loc in
    [ hash_normalize ~loc td
    ; hash_normalize_opt ~loc td.ptype_name
    ; hash_normalize_list ~loc td.ptype_name ]
  in
  try
    List.filter_map type_declarations ~f:(maybe_make_hashtable_api ~loc)
    @ [ List.map type_declarations ~f:process_type_declaration
        |> List.concat
        |> Ast_helper.Str.value ~loc Recursive ]
  with BadType ext -> [Ast_builder.Default.pstr_extension ~loc ext []]


(* does not work for polymorphic types *)
let type_of_type_decl (td : type_declaration) : core_type =
  let loc = td.ptype_loc in
  Ast_helper.Typ.constr ~loc (Common.make_longident ~loc td.ptype_name.txt) []


let opt_type_of_type_decl (td : type_declaration) : core_type =
  let loc = td.ptype_loc in
  Ast_helper.Typ.constr ~loc
    (Common.make_longident ~loc "option")
    [Ast_helper.Typ.constr ~loc (Common.make_longident ~loc td.ptype_name.txt) []]


let list_type_of_type_decl (td : type_declaration) : core_type =
  let loc = td.ptype_loc in
  Ast_helper.Typ.constr ~loc
    (Common.make_longident ~loc "list")
    [Ast_helper.Typ.constr ~loc (Common.make_longident ~loc td.ptype_name.txt) []]


(* [fun_name : typ -> typ] *)
let fun_type_of_type_decl ~loc fun_name (typ : core_type) : value_description =
  Ast_helper.Val.mk ~loc fun_name (Ast_helper.Typ.arrow ~loc Nolabel typ typ)


let make_all_fun_types_of_type_decl (td : type_declaration) : value_description list =
  let loc = td.ptype_loc in
  let root = hash_normalize_from_typename td.ptype_name.txt in
  [ type_of_type_decl td |> fun_type_of_type_decl ~loc (root |> Loc.make ~loc)
  ; opt_type_of_type_decl td |> fun_type_of_type_decl ~loc (root ^ "_opt" |> Loc.make ~loc)
  ; list_type_of_type_decl td |> fun_type_of_type_decl ~loc (root ^ "_list" |> Loc.make ~loc) ]


let make_sig ~loc ~path:_ (_rec_flag, tds) : signature =
  let vds = List.map tds ~f:make_all_fun_types_of_type_decl |> List.concat in
  List.map vds ~f:(fun vd -> Ast_helper.Sig.value ~loc vd)


let _ =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Ppxlib.Deriving.Generator.make_noarg make_sig in
  Deriving.add "normalize" ~str_type_decl ~sig_type_decl
