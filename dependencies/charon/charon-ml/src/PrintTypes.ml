(** Pretty-printing for types *)

include PrintValues
open Meta
open Types
open TypesUtils
open GAst
open PrintUtils

let region_param_to_string (rv : region_param) : string =
  match rv.name with
  | Some name -> name
  | None -> RegionId.to_string rv.index

let g_region_group_to_string (rid_to_string : 'rid -> string)
    (id_to_string : 'id -> string) (gr : ('rid, 'id) g_region_group) : string =
  let { id; regions; parents } = gr in
  "{ id: " ^ id_to_string id ^ "; regions: ["
  ^ String.concat ", " (List.map rid_to_string regions)
  ^ "]; parents: ["
  ^ String.concat ", " (List.map id_to_string parents)
  ^ "] }"

let region_var_group_to_string (gr : region_var_group) : string =
  g_region_group_to_string RegionId.to_string RegionGroupId.to_string gr

let region_var_groups_to_string (gl : region_var_groups) : string =
  String.concat "\n" (List.map region_var_group_to_string gl)

let ref_kind_to_string (rk : ref_kind) : string =
  match rk with
  | RMut -> "Mut"
  | RShared -> "Shared"

let builtin_ty_to_string (_ : builtin_ty) : string = "Box"

let de_bruijn_var_to_pretty_string show_varid var : string =
  match var with
  | Bound (dbid, varid) -> show_de_bruijn_id dbid ^ "_" ^ show_varid varid
  | Free varid -> show_varid varid

let region_db_var_to_pretty_string (var : region_db_var) : string =
  "'" ^ de_bruijn_var_to_pretty_string RegionId.to_string var

let type_db_var_to_pretty_string (var : type_db_var) : string =
  "T@" ^ de_bruijn_var_to_pretty_string TypeVarId.to_string var

let type_var_id_to_pretty_string (id : type_var_id) : string =
  "T@" ^ TypeVarId.to_string id

let type_param_to_string (tv : type_param) : string = tv.name

let const_generic_var_id_to_pretty_string (id : const_generic_var_id) : string =
  "C@" ^ ConstGenericVarId.to_string id

let const_generic_db_var_to_pretty_string (var : const_generic_db_var) : string
    =
  "C@" ^ de_bruijn_var_to_pretty_string ConstGenericVarId.to_string var

let const_generic_param_to_string (v : const_generic_param) : string = v.name

let trait_clause_id_to_pretty_string (id : trait_clause_id) : string =
  "TraitClause@" ^ TraitClauseId.to_string id

let trait_db_var_to_pretty_string (var : trait_db_var) : string =
  "TraitClause@" ^ de_bruijn_var_to_pretty_string TraitClauseId.to_string var

let trait_clause_id_to_string _ id = trait_clause_id_to_pretty_string id

let type_decl_id_to_pretty_string (id : type_decl_id) : string =
  "TypeDecl@" ^ TypeDeclId.to_string id

let fun_decl_id_to_pretty_string (id : FunDeclId.id) : string =
  "FunDecl@" ^ FunDeclId.to_string id

let global_decl_id_to_pretty_string (id : GlobalDeclId.id) : string =
  "GlobalDecl@" ^ GlobalDeclId.to_string id

let trait_decl_id_to_pretty_string (id : trait_decl_id) : string =
  "TraitDecl@" ^ TraitDeclId.to_string id

let trait_impl_id_to_pretty_string (id : trait_impl_id) : string =
  "TraitImpl@" ^ TraitImplId.to_string id

let variant_id_to_pretty_string (id : variant_id) : string =
  "Variant@" ^ VariantId.to_string id

let field_id_to_pretty_string (id : field_id) : string =
  "Field@" ^ FieldId.to_string id

let lookup_var_in_env (env : 'a fmt_env)
    (find_in : generic_params -> 'id -> 'b option) (var : 'id de_bruijn_var) :
    'b option =
  if List.length env.generics == 0 then None
  else
    let dbid, varid =
      match var with
      | Bound (dbid, varid) -> (dbid, varid)
      | Free varid ->
          let len = List.length env.generics in
          let dbid = len - 1 in
          (dbid, varid)
    in
    match List.nth_opt env.generics dbid with
    | None -> None
    | Some generics -> begin
        match find_in generics varid with
        | None -> None
        | Some r -> Some r
      end

let region_db_var_to_string (env : 'a fmt_env) (var : region_db_var) : string =
  (* Note that the regions are not necessarily ordered following their indices *)
  let find (generics : generic_params) varid =
    List.find_opt (fun (v : region_param) -> v.index = varid) generics.regions
  in
  match lookup_var_in_env env find var with
  | None -> region_db_var_to_pretty_string var
  | Some r -> region_param_to_string r

let type_db_var_to_string (env : 'a fmt_env) (var : type_db_var) : string =
  let find (generics : generic_params) varid =
    List.find_opt (fun (v : type_param) -> v.index = varid) generics.types
  in
  match lookup_var_in_env env find var with
  | None -> type_db_var_to_pretty_string var
  | Some r -> type_param_to_string r

let const_generic_db_var_to_string (env : 'a fmt_env)
    (var : const_generic_db_var) : string =
  let find (generics : generic_params) varid =
    List.find_opt
      (fun (v : const_generic_param) -> v.index = varid)
      generics.const_generics
  in
  match lookup_var_in_env env find var with
  | None -> const_generic_db_var_to_pretty_string var
  | Some r -> const_generic_param_to_string r

let trait_db_var_to_string (env : 'a fmt_env) (var : trait_db_var) : string =
  let find (generics : generic_params) varid =
    List.find_opt
      (fun (v : trait_param) -> v.clause_id = varid)
      generics.trait_clauses
  in
  match lookup_var_in_env env find var with
  | None -> trait_db_var_to_pretty_string var
  | Some r -> trait_clause_id_to_pretty_string r.clause_id

let region_to_string (env : 'a fmt_env) (r : region) : string =
  match r with
  | RStatic -> "'static"
  | RErased -> "'_"
  | RBody id -> "°" ^ RegionId.to_string id
  | RVar var -> region_db_var_to_string env var

let region_binder_to_string (value_to_string : 'a fmt_env -> 'c -> string)
    (env : 'a fmt_env) (rb : 'c region_binder) : string =
  let env = fmt_env_push_regions env rb.binder_regions in
  let value = value_to_string env rb.binder_value in
  match rb.binder_regions with
  | [] -> value
  | _ ->
      "for <"
      ^ String.concat "," (List.map region_param_to_string rb.binder_regions)
      ^ "> " ^ value

let rec type_id_to_string (env : 'a fmt_env) (id : type_id) : string =
  match id with
  | TAdtId id -> type_decl_id_to_string env id
  | TTuple -> ""
  | TBuiltin aty -> (
      match aty with
      | TBox -> "alloc::boxed::Box"
      | TStr -> "str")

and type_decl_id_to_string env def_id =
  (* We don't want the printing functions to crash if the crate is partial *)
  match TypeDeclId.Map.find_opt def_id env.crate.type_decls with
  | None -> type_decl_id_to_pretty_string def_id
  | Some def -> name_to_string env def.item_meta.name

and type_decl_ref_to_string (env : 'a fmt_env) (tref : type_decl_ref) : string =
  match tref.id with
  | TTuple ->
      let params, trait_refs = generic_args_to_strings env tref.generics in
      "(" ^ String.concat ", " params ^ ")"
  | id ->
      let id = type_id_to_string env id in
      let generics = generic_args_to_string env tref.generics in
      id ^ generics

and fun_decl_id_to_string (env : 'a fmt_env) (id : FunDeclId.id) : string =
  match FunDeclId.Map.find_opt id env.crate.fun_decls with
  | None -> fun_decl_id_to_pretty_string id
  | Some def -> name_to_string env def.item_meta.name

and fun_decl_ref_to_string (env : 'a fmt_env) (fn : fun_decl_ref) : string =
  let fun_id = fun_decl_id_to_string env fn.id in
  let generics = generic_args_to_string env fn.generics in
  fun_id ^ generics

and global_decl_id_to_string env def_id =
  match GlobalDeclId.Map.find_opt def_id env.crate.global_decls with
  | None -> global_decl_id_to_pretty_string def_id
  | Some def -> name_to_string env def.item_meta.name

and global_decl_ref_to_string (env : 'a fmt_env) (gr : global_decl_ref) : string
    =
  let global_id = global_decl_id_to_string env gr.id in
  let generics = generic_args_to_string env gr.generics in
  global_id ^ generics

and trait_decl_id_to_string env id =
  match TraitDeclId.Map.find_opt id env.crate.trait_decls with
  | None -> trait_decl_id_to_pretty_string id
  | Some def -> name_to_string env def.item_meta.name

and trait_impl_id_to_string env id =
  match TraitImplId.Map.find_opt id env.crate.trait_impls with
  | None -> trait_impl_id_to_pretty_string id
  | Some def -> name_to_string env def.item_meta.name

and trait_impl_ref_to_string (env : 'a fmt_env) (iref : trait_impl_ref) : string
    =
  let impl = trait_impl_id_to_string env iref.id in
  let generics = generic_args_to_string env iref.generics in
  impl ^ generics

and provenance_to_string (env : 'a fmt_env) (pv : provenance) : string =
  match pv with
  | ProvGlobal gref -> "prov_global(" ^ global_decl_ref_to_string env gref ^ ")"
  | ProvFunction fn_ref -> "prov_fn(" ^ fun_decl_ref_to_string env fn_ref ^ ")"
  | ProvUnknown -> "prov_unknown"

and byte_to_string (env : 'a fmt_env) (cv : byte) : string =
  match cv with
  | Uninit -> "uninit"
  | Value b -> string_of_int b
  | Provenance (p, i) ->
      provenance_to_string env p ^ "[" ^ string_of_int i ^ "]"

and const_aggregate_to_string (env : 'a fmt_env) (tref : type_decl_ref)
    opt_variant_id (fields : constant_expr list) : string =
  let fields = List.map (constant_expr_to_string env) fields in

  match tref.id with
  | TTuple -> "(" ^ String.concat ", " fields ^ ")"
  | TAdtId def_id ->
      let adt_name = type_decl_id_to_string env def_id in
      let variant_name =
        match opt_variant_id with
        | None -> adt_name
        | Some variant_id ->
            adt_name ^ "::" ^ adt_variant_to_string env def_id variant_id
      in
      let fields =
        match adt_field_names env def_id opt_variant_id with
        | None -> "(" ^ String.concat ", " fields ^ ")"
        | Some field_names ->
            let fields = List.combine field_names fields in
            let fields =
              List.map
                (fun (field, value) -> field ^ " = " ^ value ^ ";")
                fields
            in
            let fields = String.concat " " fields in
            "{ " ^ fields ^ " }"
      in
      variant_name ^ " " ^ fields
  | TBuiltin _ -> raise (Failure "Unreachable")

and constant_expr_to_string (env : 'a fmt_env) (cv : constant_expr) : string =
  match cv.kind with
  | CLiteral lit -> literal_to_string lit
  | CVar var -> const_generic_db_var_to_string env var
  | CTraitConst (trait_ref, const_name) ->
      let trait_ref = trait_ref_to_string env trait_ref in
      trait_ref ^ const_name
  | CVTableRef trait_ref ->
      let trait_ref = trait_ref_to_string env trait_ref in
      "&vtable_of(" ^ trait_ref ^ ")"
  | CFnDef fn_ptr | CFnPtr fn_ptr -> fn_ptr_to_string env fn_ptr
  | CRawMemory bytes ->
      "RawMemory(["
      ^ String.concat ", " (List.map (byte_to_string env) bytes)
      ^ "])"
  | COpaque reason -> "Opaque(" ^ reason ^ ")"
  | CAdt (variant_id, fields) -> begin
      match cv.ty with
      | TAdt tref -> const_aggregate_to_string env tref variant_id fields
      | _ -> "malformed constant"
    end
  | CArray fields ->
      "["
      ^ String.concat ", " (List.map (constant_expr_to_string env) fields)
      ^ "]"
  | CGlobal gref -> global_decl_ref_to_string env gref
  | CPtrNoProvenance n -> "(" ^ Z.to_string n ^ " as *const _)"
  | CRef (c, _) -> "&" ^ constant_expr_to_string env c
  | CPtr (ref_kind, c, _) ->
      let ref_kind =
        match ref_kind with
        | RShared -> "&raw const"
        | RMut -> "&raw mut"
      in
      ref_kind ^ constant_expr_to_string env c

and builtin_fun_id_to_string (aid : builtin_fun_id) : string =
  match aid with
  | BoxNew -> "alloc::boxed::Box::new"
  | ArrayToSliceShared -> "@ArrayToSliceShared"
  | ArrayToSliceMut -> "@ArrayToSliceMut"
  | ArrayRepeat -> "@ArrayRepeat"
  | Index { is_array; mutability; is_range } ->
      let ty = if is_array then "Array" else "Slice" in
      let op = if is_range then "SubSlice" else "Index" in
      let mutability = ref_kind_to_string mutability in
      "@" ^ ty ^ op ^ mutability
  | PtrFromParts mut ->
      let mut = if mut = RMut then "Mut" else "" in
      "@PtrFromParts" ^ mut

and fun_id_to_string (env : 'a fmt_env) (fid : fun_id) : string =
  match fid with
  | FRegular fid -> fun_decl_id_to_string env fid
  | FBuiltin aid -> builtin_fun_id_to_string aid

and fn_ptr_kind_to_string (env : 'a fmt_env) (r : fn_ptr_kind) : string =
  match r with
  | TraitMethod (trait_ref, method_name, _) ->
      trait_ref_to_string env trait_ref ^ "::" ^ method_name
  | FunId fid -> fun_id_to_string env fid

and fn_ptr_to_string (env : 'a fmt_env) (ptr : fn_ptr) : string =
  let generics = generic_args_to_string env ptr.generics in
  fn_ptr_kind_to_string env ptr.kind ^ generics

and ty_to_string (env : 'a fmt_env) (ty : ty) : string =
  match ty with
  | TAdt tref -> type_decl_ref_to_string env tref
  | TVar tv -> type_db_var_to_string env tv
  | TNever -> "!"
  | TLiteral lit_ty -> literal_type_to_string lit_ty
  | TTraitType (trait_ref, type_name) ->
      let trait_ref = trait_ref_to_string env trait_ref in
      trait_ref ^ "::" ^ type_name
  | TRef (r, rty, ref_kind) -> (
      match ref_kind with
      | RMut ->
          "&" ^ region_to_string env r ^ " mut (" ^ ty_to_string env rty ^ ")"
      | RShared ->
          "&" ^ region_to_string env r ^ " (" ^ ty_to_string env rty ^ ")")
  | TRawPtr (rty, ref_kind) -> (
      match ref_kind with
      | RMut -> "*mut " ^ ty_to_string env rty
      | RShared -> "*const " ^ ty_to_string env rty)
  | TFnPtr binder ->
      let env = fmt_env_push_regions env binder.binder_regions in
      let { inputs; output; _ } = binder.binder_value in
      let inputs =
        "(" ^ String.concat ", " (List.map (ty_to_string env) inputs) ^ ") -> "
      in
      inputs ^ ty_to_string env output
  | TFnDef f ->
      let env = fmt_env_push_regions env f.binder_regions in
      let fn = fn_ptr_to_string env f.binder_value in
      fn
  | TDynTrait pred ->
      let regions, clauses =
        generic_params_to_strings env pred.binder.binder_params
      in
      let reg_str =
        match regions with
        | [] -> ""
        | r :: _ -> " + " ^ r
      in
      "dyn (" ^ String.concat " + " clauses ^ reg_str ^ ")"
  | TArray (ty, len) ->
      "[" ^ ty_to_string env ty ^ "; " ^ constant_expr_to_string env len ^ "]"
  | TSlice ty -> "[" ^ ty_to_string env ty ^ "]"
  | TPtrMetadata ty -> "PtrMetadata(" ^ ty_to_string env ty ^ ")"
  | TError msg -> "type_error (\"" ^ msg ^ "\")"

(** Return two lists:
    - one for the regions, types, const generics
    - one for the trait refs *)
and generic_args_to_strings (env : 'a fmt_env) (generics : generic_args) :
    string list * string list =
  let { regions; types; const_generics; trait_refs } = generics in
  let regions = List.map (region_to_string env) regions in
  let types = List.map (ty_to_string env) types in
  let cgs = List.map (constant_expr_to_string env) const_generics in
  let params = List.flatten [ regions; types; cgs ] in
  let trait_refs = List.map (trait_ref_to_string env) trait_refs in
  (params, trait_refs)

and generic_args_to_string (env : 'a fmt_env) (generics : generic_args) : string
    =
  let params, trait_refs = generic_args_to_strings env generics in
  let params =
    if params = [] then "" else "<" ^ String.concat ", " params ^ ">"
  in
  let trait_refs =
    if trait_refs = [] then "" else "[" ^ String.concat ", " trait_refs ^ "]"
  in
  params ^ trait_refs

and trait_ref_kind_to_string (env : 'a fmt_env)
    (implements : trait_decl_ref region_binder option) (kind : trait_ref_kind) :
    string =
  match kind with
  | Self -> "Self"
  | TraitImpl impl_ref -> trait_impl_ref_to_string env impl_ref
  | BuiltinOrAuto _ ->
      region_binder_to_string trait_decl_ref_to_string env
        (Option.get implements)
  | Clause id -> trait_db_var_to_string env id
  | ParentClause (tref, clause_id) ->
      let inst_id = trait_ref_to_string env tref in
      let clause_id = trait_clause_id_to_string env clause_id in
      "parent(" ^ inst_id ^ ")::" ^ clause_id
  | ItemClause (tref, name, clause_id) ->
      let inst_id = trait_ref_to_string env tref in
      let clause_id = trait_clause_id_to_string env clause_id in
      "item(" ^ inst_id ^ ")::" ^ name ^ "::" ^ clause_id
  | Dyn ->
      let trait =
        region_binder_to_string trait_decl_ref_to_string env
          (Option.get implements)
      in
      "dyn(" ^ trait ^ ")"
  | UnknownTrait msg -> "UNKNOWN(" ^ msg ^ ")"

and trait_ref_to_string (env : 'a fmt_env) (tr : trait_ref) : string =
  trait_ref_kind_to_string env (Some tr.trait_decl_ref) tr.kind

and trait_decl_ref_to_string (env : 'a fmt_env) (tr : trait_decl_ref) : string =
  let trait_id = trait_decl_id_to_string env tr.id in
  let generics = generic_args_to_string env tr.generics in
  trait_id ^ generics

and impl_elem_to_string (env : 'a fmt_env) (elem : impl_elem) : string =
  match elem with
  | ImplElemTy bound_ty ->
      (* Locally replace the generics and the predicates *)
      let env = fmt_env_push_generics_and_preds env bound_ty.binder_params in
      ty_to_string env bound_ty.binder_value
  | ImplElemTrait impl_id -> begin
      match TraitImplId.Map.find_opt impl_id env.crate.trait_impls with
      | None -> trait_impl_id_to_string env impl_id
      | Some impl -> (
          (* Locally replace the generics and the predicates *)
          let env = fmt_env_push_generics_and_preds env impl.generics in
          (* Put the first type argument aside (it gives the type for which we
             implement the trait) *)
          let { id; generics } : trait_decl_ref = impl.impl_trait in
          match generics.types with
          | ty :: types -> begin
              let ty, types = Collections.List.pop generics.types in
              let generics = { generics with types } in
              let tr : trait_decl_ref = { id; generics } in
              let ty = ty_to_string env ty in
              let tr = trait_decl_ref_to_string env tr in
              tr ^ " for " ^ ty
            end
          (* When monomorphizing, traits no longer take a `Self` argument, it's stored in the name *)
          | [] -> trait_decl_ref_to_string env impl.impl_trait)
    end

and path_elem_to_string (env : 'a fmt_env) (e : path_elem) : string =
  match e with
  | PeIdent (s, d) ->
      let d =
        if d = Disambiguator.zero then "" else "#" ^ Disambiguator.to_string d
      in
      s ^ d
  | PeImpl impl -> "{" ^ impl_elem_to_string env impl ^ "}"
  | PeInstantiated binder ->
      let env = fmt_env_push_generics_and_preds env binder.binder_params in
      let explicits, _ = generic_args_to_strings env binder.binder_value in
      "<" ^ String.concat ", " explicits ^ ">"

and name_to_string (env : 'a fmt_env) (n : name) : string =
  let name = List.map (path_elem_to_string env) n in
  String.concat "::" name

and raw_attribute_to_string (attr : raw_attribute) : string =
  let args =
    match attr.args with
    | None -> ""
    | Some args -> "(" ^ args ^ ")"
  in
  attr.path ^ args

and trait_param_to_string (env : 'a fmt_env) (clause : trait_param) : string =
  let clause_id = trait_clause_id_to_string env clause.clause_id in
  let trait =
    region_binder_to_string trait_decl_ref_to_string env clause.trait
  in
  "[" ^ clause_id ^ "]: " ^ trait

and generic_params_to_strings (env : 'a fmt_env) (generics : generic_params) :
    string list * string list =
  let ({ regions; types; const_generics; trait_clauses; _ } : generic_params) =
    generics
  in
  let regions = List.map region_param_to_string regions in
  let types = List.map type_param_to_string types in
  let cgs = List.map const_generic_param_to_string const_generics in
  let params = List.flatten [ regions; types; cgs ] in
  let trait_clauses = List.map (trait_param_to_string env) trait_clauses in
  (params, trait_clauses)

and adt_variant_to_string (env : 'a fmt_env) (def_id : TypeDeclId.id)
    (variant_id : VariantId.id) : string =
  match TypeDeclId.Map.find_opt def_id env.crate.type_decls with
  | None ->
      type_decl_id_to_pretty_string def_id
      ^ "::"
      ^ variant_id_to_pretty_string variant_id
  | Some def -> begin
      match def.kind with
      | Enum variants ->
          let variant = VariantId.nth variants variant_id in
          name_to_string env def.item_meta.name ^ "::" ^ variant.variant_name
      | _ -> raise (Failure "Unreachable")
    end

and adt_field_names (env : 'a fmt_env) (def_id : TypeDeclId.id)
    (opt_variant_id : VariantId.id option) : string list option =
  match TypeDeclId.Map.find_opt def_id env.crate.type_decls with
  | None -> None
  | Some { kind = Opaque; _ } -> None
  | Some def ->
      let fields = type_decl_get_fields def opt_variant_id in
      (* There are two cases: either all the fields have names, or none
         of them has names *)
      let has_names =
        List.exists (fun f -> Option.is_some f.field_name) fields
      in
      if has_names then
        let fields = List.map (fun f -> Option.get f.field_name) fields in
        Some fields
      else None

let field_to_string env (f : field) : string =
  match f.field_name with
  | Some field_name -> field_name ^ " : " ^ ty_to_string env f.field_ty
  | None -> ty_to_string env f.field_ty

let variant_to_string env (v : variant) : string =
  v.variant_name ^ "("
  ^ String.concat ", " (List.map (field_to_string env) v.fields)
  ^ ")"

let trait_type_constraint_to_string (env : 'a fmt_env)
    (ttc : trait_type_constraint) : string =
  let { trait_ref; type_name; ty } = ttc in
  let trait_ref = trait_ref_to_string env trait_ref in
  let ty = ty_to_string env ty in
  trait_ref ^ "::" ^ type_name ^ " = " ^ ty

(** Helper to format "where" clauses *)
let clauses_to_string (indent : string) (indent_incr : string)
    (clauses : string list) : string =
  if clauses = [] then ""
  else
    let env_clause s = indent ^ indent_incr ^ s ^ "," in
    let clauses = List.map env_clause clauses in
    "\n" ^ indent ^ "where\n" ^ String.concat "\n" clauses

(** Helper to format "where" clauses *)
let predicates_and_trait_clauses_to_string (env : 'a fmt_env) (indent : string)
    (indent_incr : string) (generics : generic_params) : string list * string =
  let params, trait_clauses = generic_params_to_strings env generics in
  let region_to_string = region_to_string env in
  let regions_outlive =
    let outlive_to_string _ (x, y) =
      region_to_string x ^ " : " ^ region_to_string y
    in
    List.map
      (region_binder_to_string outlive_to_string env)
      generics.regions_outlive
  in
  let types_outlive =
    let outlive_to_string _ (x, y) =
      ty_to_string env x ^ " : " ^ region_to_string y
    in
    List.map
      (region_binder_to_string outlive_to_string env)
      generics.types_outlive
  in
  let trait_type_constraints =
    List.map
      (region_binder_to_string trait_type_constraint_to_string env)
      generics.trait_type_constraints
  in
  (* Split between the inherited clauses and the local clauses *)
  let clauses =
    clauses_to_string indent indent_incr
      (List.concat
         [
           trait_clauses; regions_outlive; types_outlive; trait_type_constraints;
         ])
  in
  (params, clauses)

let type_decl_to_string (env : 'a fmt_env) (def : type_decl) : string =
  (* Locally update the generics and the predicates *)
  let env = fmt_env_push_generics_and_preds env def.generics in
  let params, clauses =
    predicates_and_trait_clauses_to_string env "" "  " def.generics
  in

  let name = name_to_string env def.item_meta.name in
  let params =
    if params <> [] then "<" ^ String.concat ", " params ^ ">" else ""
  in
  match def.kind with
  | Struct fields ->
      if fields <> [] then
        let fields =
          String.concat ""
            (List.map (fun f -> "\n  " ^ field_to_string env f ^ ",") fields)
        in
        "struct " ^ name ^ params ^ clauses ^ "\n{" ^ fields ^ "\n}"
      else "struct " ^ name ^ params ^ clauses ^ "{}"
  | Enum variants ->
      let variants =
        List.map (fun v -> "|  " ^ variant_to_string env v) variants
      in
      let variants = String.concat "\n" variants in
      "enum " ^ name ^ params ^ clauses ^ "\n  =\n" ^ variants
  | Union fields ->
      if fields <> [] then
        let fields =
          String.concat ""
            (List.map (fun f -> "\n  " ^ field_to_string env f ^ ",") fields)
        in
        "union " ^ name ^ params ^ clauses ^ "\n{" ^ fields ^ "\n}"
      else "union " ^ name ^ params ^ clauses ^ "{}"
  | Alias ty -> "type " ^ name ^ params ^ clauses ^ " = " ^ ty_to_string env ty
  | Opaque -> "opaque type " ^ name ^ params ^ clauses
  | TDeclError err -> "error(\"" ^ err ^ "\")"

let adt_field_to_string (env : 'a fmt_env) (def_id : TypeDeclId.id)
    (opt_variant_id : VariantId.id option) (field_id : FieldId.id) :
    string option =
  match TypeDeclId.Map.find_opt def_id env.crate.type_decls with
  | None -> None
  | Some { kind = Opaque; _ } -> None
  | Some def ->
      let fields = type_decl_get_fields def opt_variant_id in
      let field = FieldId.nth fields field_id in
      field.field_name
