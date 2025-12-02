(** This file implements various substitution utilities to instantiate types,
    function bodies, etc. *)

open Types
open TypesUtils
open GAstUtils
open Expressions
open LlbcAst

(* TODO: use Core.Fn.compose *)
let compose f g x = f (g x)

let ( let* ) o f =
  match o with
  | Some x -> f x
  | None -> None

(* A substitution that takes a full variable as input *)
type subst = {
  r_subst : RegionId.id de_bruijn_var -> region;
      (** Remark: this might be called with bound regions with a negative
          DeBruijn index. A negative DeBruijn index means that the region is
          locally bound. *)
  ty_subst : TypeVarId.id de_bruijn_var -> ty;
  cg_subst : ConstGenericVarId.id de_bruijn_var -> const_generic;
      (** Substitution from *local* trait clause to trait instance *)
  tr_subst : TraitClauseId.id de_bruijn_var -> trait_instance_id;
      (** Substitution for the [Self] trait instance *)
  tr_self : trait_instance_id;
}

(* A substitution that applies to a specific binder. Use it with
   `subst_at_binder_zero` or `subst_free_vars` to get a real `subst`. *)
type single_binder_subst = {
  r_sb_subst : RegionId.id -> region;
  ty_sb_subst : TypeVarId.id -> ty;
  cg_sb_subst : ConstGenericVarId.id -> const_generic;
  tr_sb_subst : TraitClauseId.id -> trait_instance_id;
      (** Substitution for the [Self] trait instance *)
  tr_sb_self : trait_instance_id;
}

let empty_subst : subst =
  {
    r_subst = (fun var -> RVar var);
    ty_subst = (fun var -> TVar var);
    cg_subst = (fun var -> CgVar var);
    tr_subst = (fun var -> Clause var);
    tr_self = Self;
  }

(** The do-nothing substitution when used with `subst_free_vars` *)
let empty_bound_sb_subst : single_binder_subst =
  {
    r_sb_subst = compose empty_subst.r_subst zero_db_var;
    ty_sb_subst = compose empty_subst.ty_subst zero_db_var;
    cg_sb_subst = compose empty_subst.cg_subst zero_db_var;
    tr_sb_subst = compose empty_subst.tr_subst zero_db_var;
    tr_sb_self = empty_subst.tr_self;
  }

(** The do-nothing substitution when used with `subst_at_binder_zero` *)
let empty_free_sb_subst : single_binder_subst =
  let free x = Free x in
  {
    r_sb_subst = compose empty_subst.r_subst free;
    ty_sb_subst = compose empty_subst.ty_subst free;
    cg_sb_subst = compose empty_subst.cg_subst free;
    tr_sb_subst = compose empty_subst.tr_subst free;
    tr_sb_self = empty_subst.tr_self;
  }

let error_sb_subst : single_binder_subst =
  let error _ = failwith "Unexpected bound variable" in
  {
    r_sb_subst = compose empty_subst.r_subst error;
    ty_sb_subst = compose empty_subst.ty_subst error;
    cg_sb_subst = compose empty_subst.cg_subst error;
    tr_sb_subst = compose empty_subst.tr_subst error;
    tr_sb_self = empty_subst.tr_self;
  }

(** Substitute the free variables. *)
let subst_free_vars (subst : single_binder_subst) : subst =
  let subst_free subst nosubst = function
    | Free id -> subst id
    | var -> nosubst var
  in
  {
    r_subst = subst_free subst.r_sb_subst empty_subst.r_subst;
    ty_subst = subst_free subst.ty_sb_subst empty_subst.ty_subst;
    cg_subst = subst_free subst.cg_sb_subst empty_subst.cg_subst;
    tr_subst = subst_free subst.tr_sb_subst empty_subst.tr_subst;
    tr_self = subst.tr_sb_self;
  }

(** Substitute the variables bound by the currently innermost (level 0) binder.
*)
let subst_at_binder_zero (subst : single_binder_subst) : subst =
  let subst_if_zero subst nosubst = function
    | Bound (dbid, id) when dbid = 0 -> subst id
    | var -> nosubst var
  in
  {
    r_subst = subst_if_zero subst.r_sb_subst empty_subst.r_subst;
    ty_subst = subst_if_zero subst.ty_sb_subst empty_subst.ty_subst;
    cg_subst = subst_if_zero subst.cg_sb_subst empty_subst.cg_subst;
    tr_subst = subst_if_zero subst.tr_sb_subst empty_subst.tr_subst;
    tr_self = subst.tr_sb_self;
  }

(** Substitute the variables bound by the current (level 0) binder, and shift
    variables to remove the current binder level. *)
let subst_remove_binder_zero (subst : single_binder_subst) : subst =
  let subst_remove_zero subst nosubst = function
    | Bound (dbid, id) when dbid = 0 -> subst id
    | Bound (dbid, varid) when dbid > 0 -> nosubst (Bound (dbid - 1, varid))
    | var -> nosubst var
  in
  {
    r_subst = subst_remove_zero subst.r_sb_subst empty_subst.r_subst;
    ty_subst = subst_remove_zero subst.ty_sb_subst empty_subst.ty_subst;
    cg_subst = subst_remove_zero subst.cg_sb_subst empty_subst.cg_subst;
    tr_subst = subst_remove_zero subst.tr_sb_subst empty_subst.tr_subst;
    tr_self = subst.tr_sb_self;
  }

(** Move a whole expression under one level of binder. *)
let move_under_binder_subst : subst =
  let shift = function
    | Bound (dbid, var) -> Bound (dbid + 1, var)
    | Free _ as var -> var
  in
  {
    r_subst = compose empty_subst.r_subst shift;
    ty_subst = compose empty_subst.ty_subst shift;
    cg_subst = compose empty_subst.cg_subst shift;
    tr_subst = compose empty_subst.tr_subst shift;
    tr_self = empty_subst.tr_self;
  }

(** Visitor that shifts all bound variables by the given delta *)
let st_shift_visitor =
  object (self)
    inherit [_] map_statement
    method! visit_de_bruijn_id delta dbid = dbid + delta
  end

(* Shift the the substitution under one binder. *)
let shift_subst (subst : subst) : subst =
  (* We decrement the input because the variables we encounter will be bound
     deeper. We shift the output so that it's valid at the new depth we're
     substituting it into. *)
  {
    r_subst =
      compose
        (st_shift_visitor#visit_region 1)
        (compose subst.r_subst decr_db_var);
    ty_subst =
      compose (st_shift_visitor#visit_ty 1) (compose subst.ty_subst decr_db_var);
    cg_subst =
      compose
        (st_shift_visitor#visit_const_generic 1)
        (compose subst.cg_subst decr_db_var);
    tr_subst =
      compose
        (st_shift_visitor#visit_trait_instance_id 1)
        (compose subst.tr_subst decr_db_var);
    tr_self = subst.tr_self;
  }

(** Visitor that applies the given substitution *)
let st_substitute_visitor =
  object (self)
    inherit [_] map_statement

    method! visit_binder visit_value (subst : subst) x =
      (* Note that we don't visit the bound variables. *)
      let { binder_params; binder_value } = x in
      (* Crucial: we shift the substitution to be valid under this binder. *)
      let subst = shift_subst subst in
      let binder_params = self#visit_generic_params subst binder_params in
      let binder_value = visit_value subst binder_value in
      { binder_params; binder_value }

    method! visit_region_binder visit_value (subst : subst) x =
      (* Note that we don't visit the bound variables. *)
      let { binder_regions; binder_value } = x in
      (* Crucial: we shift the substitution to be valid under this binder. *)
      let subst = shift_subst subst in
      let binder_regions =
        self#visit_list self#visit_region_var subst binder_regions
      in
      let binder_value = visit_value subst binder_value in
      { binder_regions; binder_value }

    method! visit_RVar (subst : subst) var = subst.r_subst var
    method! visit_TVar (subst : subst) var = subst.ty_subst var
    method! visit_CgVar (subst : subst) var = subst.cg_subst var
    method! visit_Clause (subst : subst) var = subst.tr_subst var
    method! visit_Self (subst : subst) = subst.tr_self
  end

(** Substitute types variables and regions in a type. *)
let ty_substitute (subst : subst) (ty : ty) : ty =
  st_substitute_visitor#visit_ty subst ty

let trait_ref_substitute (subst : subst) (tr : trait_ref) : trait_ref =
  st_substitute_visitor#visit_trait_ref subst tr

let trait_decl_ref_substitute (subst : subst) (tr : trait_decl_ref) :
    trait_decl_ref =
  st_substitute_visitor#visit_trait_decl_ref subst tr

let trait_instance_id_substitute (subst : subst) (tr : trait_instance_id) :
    trait_instance_id =
  st_substitute_visitor#visit_trait_instance_id subst tr

let generic_args_substitute (subst : subst) (g : generic_args) : generic_args =
  st_substitute_visitor#visit_generic_args subst g

(** Substitute the predicates (clauses, outlives predicates, etc) inside these
    generic params. This leaves the list of parameters (regions, types and
    const_generics) untouched. *)
let predicates_substitute (subst : subst) (p : generic_params) : generic_params
    =
  let visitor = st_substitute_visitor in
  let {
    regions;
    types;
    const_generics;
    trait_clauses;
    regions_outlive;
    types_outlive;
    trait_type_constraints;
  } =
    p
  in
  (* We leave the three lists of parameters untouched *)
  {
    regions;
    types;
    const_generics;
    trait_clauses = List.map (visitor#visit_trait_clause subst) trait_clauses;
    regions_outlive =
      List.map
        (visitor#visit_region_binder
           (visitor#visit_outlives_pred visitor#visit_region
              visitor#visit_region)
           subst)
        regions_outlive;
    types_outlive =
      List.map
        (visitor#visit_region_binder
           (visitor#visit_outlives_pred visitor#visit_ty visitor#visit_region)
           subst)
        types_outlive;
    trait_type_constraints =
      List.map
        (visitor#visit_region_binder visitor#visit_trait_type_constraint subst)
        trait_type_constraints;
  }

let erase_regions_subst : subst =
  { empty_subst with r_subst = (fun _ -> RErased) }

(** Erase the region variables in a type *)
let erase_regions (ty : ty) : ty = ty_substitute erase_regions_subst ty

let trait_ref_erase_regions (tr : trait_ref) : trait_ref =
  trait_ref_substitute erase_regions_subst tr

let trait_instance_id_erase_regions (tr : trait_instance_id) : trait_instance_id
    =
  trait_instance_id_substitute erase_regions_subst tr

let generic_args_erase_regions (tr : generic_args) : generic_args =
  generic_args_substitute erase_regions_subst tr

(** Erase the regions in a type and perform a substitution *)
let erase_regions_substitute_types (subst : subst) (ty : ty) : ty =
  let subst = { subst with r_subst = (fun _ -> RErased) } in
  ty_substitute subst ty

(** Move the value out of the binder by shifting relevant binding levels. Errors
    if a variable bound in this binder is found. *)
let extract_from_binder (substituer : subst -> 'a -> 'a)
    (bound_val : 'a region_binder) : 'a =
  let subst = subst_remove_binder_zero error_sb_subst in
  substituer subst bound_val.binder_value

(** Substitute the free regions corresponding to each `var_id` with the
    corresponding provided region. *)
let make_region_subst (var_ids : RegionId.id list) (regions : region list) :
    RegionId.id -> region =
  let map = RegionId.Map.of_list (List.combine var_ids regions) in
  fun varid -> RegionId.Map.find varid map

let make_region_subst_from_vars (vars : region_var list) (regions : region list)
    : RegionId.id -> region =
  make_region_subst (List.map (fun (x : region_var) -> x.index) vars) regions

(** Create a type substitution from a list of type variable ids and a list of
    types (with which to substitute the type variable ids) *)
let make_type_subst (var_ids : TypeVarId.id list) (tys : ty list) :
    TypeVarId.id -> ty =
  let map = TypeVarId.Map.of_list (List.combine var_ids tys) in
  fun varid -> TypeVarId.Map.find varid map

let make_type_subst_from_vars (vars : type_var list) (tys : ty list) :
    TypeVarId.id -> ty =
  make_type_subst (List.map (fun (x : type_var) -> x.index) vars) tys

(** Create a const generic substitution from a list of const generic variable
    ids and a list of const generics (with which to substitute the const generic
    variable ids) *)
let make_const_generic_subst (var_ids : ConstGenericVarId.id list)
    (cgs : const_generic list) : ConstGenericVarId.id -> const_generic =
  let map = ConstGenericVarId.Map.of_list (List.combine var_ids cgs) in
  fun varid -> ConstGenericVarId.Map.find varid map

let make_const_generic_subst_from_vars (vars : const_generic_var list)
    (cgs : const_generic list) : ConstGenericVarId.id -> const_generic =
  make_const_generic_subst
    (List.map (fun (x : const_generic_var) -> x.index) vars)
    cgs

(** Create a trait substitution from a list of trait clause ids and a list of
    trait refs *)
let make_trait_subst (var_ids : TraitClauseId.id list)
    (trs : trait_instance_id list) : TraitClauseId.id -> trait_instance_id =
  let map = TraitClauseId.Map.of_list (List.combine var_ids trs) in
  fun varid -> TraitClauseId.Map.find varid map

let make_trait_subst_from_clauses (clauses : trait_clause list)
    (trs : trait_ref list) : TraitClauseId.id -> trait_instance_id =
  make_trait_subst
    (List.map (fun (x : trait_clause) -> x.clause_id) clauses)
    (List.map (fun (x : trait_ref) -> x.trait_id) trs)

let make_sb_subst_from_generics (params : generic_params) (args : generic_args)
    : single_binder_subst =
  let r_sb_subst = make_region_subst_from_vars params.regions args.regions in
  let ty_sb_subst = make_type_subst_from_vars params.types args.types in
  let cg_sb_subst =
    make_const_generic_subst_from_vars params.const_generics args.const_generics
  in
  let tr_sb_subst =
    make_trait_subst_from_clauses params.trait_clauses args.trait_refs
  in
  { r_sb_subst; ty_sb_subst; cg_sb_subst; tr_sb_subst; tr_sb_self = Self }

let make_subst_from_generics (params : generic_params) (args : generic_args) :
    subst =
  subst_free_vars (make_sb_subst_from_generics params args)

let make_subst_from_generics_erase_regions (params : generic_params)
    (generics : generic_args) : subst =
  let generics = generic_args_erase_regions generics in
  let subst = make_subst_from_generics params generics in
  { subst with r_subst = (fun _ -> RErased) }

(** Instantiate the type variables in an ADT definition, and return, for every
    variant, the list of the types of its fields. *)
let type_decl_get_instantiated_variants_fields_types (def : type_decl)
    (generics : generic_args) : (VariantId.id option * ty list) list =
  let subst = make_subst_from_generics def.generics generics in
  let (variants_fields : (VariantId.id option * field list) list) =
    match def.kind with
    | Enum variants ->
        List.mapi (fun i v -> (Some (VariantId.of_int i), v.fields)) variants
    | Struct fields | Union fields -> [ (None, fields) ]
    | Alias _ | Opaque | TDeclError _ ->
        raise
          (Failure
             ("Can't retrieve the variants of non-adt type: "
             ^ show_name def.item_meta.name))
  in
  List.map
    (fun (id, fields) ->
      (id, List.map (fun f -> ty_substitute subst f.field_ty) fields))
    variants_fields

(** Instantiate the type variables in an ADT definition, and return the list of
    types of the fields for the chosen variant. *)
let type_decl_get_instantiated_field_types (def : type_decl)
    (opt_variant_id : VariantId.id option) (generics : generic_args) : ty list =
  (* Check that there are no clauses - otherwise we might need
     to normalize the types *)
  assert (def.generics.trait_clauses = []);
  let subst = make_subst_from_generics def.generics generics in
  let fields = type_decl_get_fields def opt_variant_id in
  List.map (fun f -> ty_substitute subst f.field_ty) fields

(** Same as [type_decl_get_instantiated_field_types], but also erases the
    regions *)
let type_decl_get_instantiated_field_etypes (def : type_decl)
    (opt_variant_id : VariantId.id option) (generics : generic_args) : ty list =
  let types =
    type_decl_get_instantiated_field_types def opt_variant_id generics
  in
  List.map erase_regions types

(** Apply a type substitution to a place *)
let place_substitute (subst : subst) (p : place) : place =
  (* There is in fact nothing to do *)
  st_substitute_visitor#visit_place subst p

(** Apply a type substitution to an operand *)
let operand_substitute (subst : subst) (op : operand) : operand =
  st_substitute_visitor#visit_operand subst op

(** Apply a type substitution to an rvalue *)
let rvalue_substitute (subst : subst) (rv : rvalue) : rvalue =
  st_substitute_visitor#visit_rvalue subst rv

(** Apply a type substitution to an assertion *)
let assertion_substitute (subst : subst) (a : assertion) : assertion =
  st_substitute_visitor#visit_assertion subst a

(** Apply a type substitution to a call *)
let call_substitute (subst : subst) (call : call) : call =
  st_substitute_visitor#visit_call subst call

(** Apply a type substitution to a statement *)
let block_substitute (subst : subst) (blk : block) : block =
  st_substitute_visitor#visit_block subst blk

(** Apply a type substitution to a function body. Return the local variables and
    the body. *)
let fun_body_substitute_in_body (subst : subst) (body : fun_body) :
    local list * block =
  let locals =
    List.map
      (fun (v : local) -> { v with var_ty = ty_substitute subst v.var_ty })
      body.locals.locals
  in
  let body = block_substitute subst body.body in
  (locals, body)

let trait_type_constraint_substitute (subst : subst)
    (ttc : trait_type_constraint) : trait_type_constraint =
  let { trait_ref; type_name; ty } = ttc in
  let visitor = st_substitute_visitor in
  let trait_ref = visitor#visit_trait_ref subst trait_ref in
  let ty = visitor#visit_ty subst ty in
  { trait_ref; type_name; ty }

(** Substitute variable identifiers in a type *)
let block_substitute_ids (ty_subst : TypeVarId.id -> TypeVarId.id)
    (cg_subst : ConstGenericVarId.id -> ConstGenericVarId.id) (ty : ty) : ty =
  let visitor =
    object
      inherit [_] map_ty
      method visit_'r _ r = r
      method! visit_type_var_id _ id = ty_subst id
      method! visit_const_generic_var_id _ id = cg_subst id
    end
  in
  visitor#visit_ty () ty

(** Remove this binder by substituting the provided arguments for each bound
    variable. The `substitutor` argument must be the appropriate
    `st_substitute_visitor` method. *)
let apply_args_to_binder (args : generic_args) (substitutor : subst -> 'a -> 'a)
    (binder : 'a binder) : 'a =
  substitutor
    (subst_remove_binder_zero
       (make_sb_subst_from_generics binder.binder_params args))
    binder.binder_value

(** Remove this binder by substituting the provided arguments for each bound
    variable. The `substitutor` argument must be the appropriate
    `st_substitute_visitor` method. *)
let apply_args_to_item_binder (tr_self : trait_instance_id)
    (args : generic_args) (substitutor : subst -> 'a -> 'a)
    (binder : 'a item_binder) : 'a =
  let subst = make_sb_subst_from_generics binder.item_binder_params args in
  let subst = { subst with tr_sb_self = tr_self } in
  substitutor (subst_free_vars subst) binder.item_binder_value

(** Merge two levels of binders into a single one that binds the concatenated
    params. Useful for consumers that don't want to have to handle method
    binders. *)
let fuse_binders (substitutor : subst -> 'a -> 'a)
    (binder : 'a binder item_binder) : 'a item_binder =
  let outer_params = binder.item_binder_params in
  let inner_params = binder.item_binder_value.binder_params in
  let bound_val = binder.item_binder_value.binder_value in

  (* Variables bound in the inner binder are `Bound`. We make them all `Free`
     variables, shifting indices to avoid overlap with the variables bound in
     the outer binder. *)
  let shift_region_varid varid =
    RegionId.of_int
      (RegionId.to_int varid + List.length outer_params.regions_outlive)
  in
  let shift_ty_varid varid =
    TypeVarId.of_int (TypeVarId.to_int varid + List.length outer_params.types)
  in
  let shift_cg_varid varid =
    ConstGenericVarId.of_int
      (ConstGenericVarId.to_int varid + List.length outer_params.const_generics)
  in
  let shift_clause_varid varid =
    TraitClauseId.of_int
      (TraitClauseId.to_int varid + List.length outer_params.trait_clauses)
  in

  (* Replace bound variables with free variables that don't overlap with
     existing ones. *)
  let subst =
    subst_remove_binder_zero
      {
        r_sb_subst = compose empty_free_sb_subst.r_sb_subst shift_region_varid;
        ty_sb_subst = compose empty_free_sb_subst.ty_sb_subst shift_ty_varid;
        cg_sb_subst = compose empty_free_sb_subst.cg_sb_subst shift_cg_varid;
        tr_sb_subst = compose empty_free_sb_subst.tr_sb_subst shift_clause_varid;
        tr_sb_self = empty_subst.tr_self;
      }
  in
  let bound_val = substitutor subst bound_val in
  (* Shift the inner params too, as predicates etc may refer to bound types. *)
  let inner_params =
    st_substitute_visitor#visit_generic_params subst inner_params
  in

  (* Finally, merge the two levels. *)
  let shift_region_var (var : region_var) =
    { var with index = shift_region_varid var.index }
  in
  let shift_ty_var (var : type_var) =
    { var with index = shift_ty_varid var.index }
  in
  let shift_cg_var (var : const_generic_var) =
    { var with index = shift_cg_varid var.index }
  in
  let shift_clause_var (var : trait_clause) =
    { var with clause_id = shift_clause_varid var.clause_id }
  in
  let params =
    {
      regions =
        outer_params.regions @ List.map shift_region_var inner_params.regions;
      types = outer_params.types @ List.map shift_ty_var inner_params.types;
      const_generics =
        outer_params.const_generics
        @ List.map shift_cg_var inner_params.const_generics;
      trait_clauses =
        outer_params.trait_clauses
        @ List.map shift_clause_var inner_params.trait_clauses;
      regions_outlive =
        outer_params.regions_outlive @ inner_params.regions_outlive;
      types_outlive = outer_params.types_outlive @ inner_params.types_outlive;
      trait_type_constraints =
        outer_params.trait_type_constraints
        @ inner_params.trait_type_constraints;
    }
  in
  { item_binder_params = params; item_binder_value = bound_val }

(** Helper *)
let instantiate_method (trait_self : trait_instance_id)
    (item_generics : generic_args) (method_generics : generic_args)
    (bound_fn : fun_decl_ref binder item_binder) : fun_decl_ref =
  let bound_fn =
    apply_args_to_item_binder trait_self item_generics
      (st_substitute_visitor#visit_binder
         st_substitute_visitor#visit_fun_decl_ref)
      bound_fn
  in
  apply_args_to_binder method_generics st_substitute_visitor#visit_fun_decl_ref
    bound_fn

(** Helper *)
let instantiate_trait_method (trait_ref : trait_ref) =
  let trait_generics = trait_ref.trait_decl_ref.binder_value.generics in
  let trait_self = trait_ref.trait_id in
  instantiate_method trait_self trait_generics

(** Like lookup_trait_decl_method, but also correctly substitutes the generics.
*)
let lookup_and_subst_trait_decl_method (tdecl : trait_decl)
    (name : trait_item_name) (trait_ref : trait_ref)
    (method_generics : generic_args) : fun_decl_ref option =
  Option.map
    (instantiate_trait_method trait_ref method_generics)
    (lookup_trait_decl_method tdecl name)

(** Like lookup_trait_impl_method, but also correctly substitutes the generics.
*)
let lookup_and_subst_trait_impl_method (timpl : trait_impl)
    (name : trait_item_name) (impl_generics : generic_args)
    (method_generics : generic_args) : fun_decl_ref option =
  Option.map
    (instantiate_method Self impl_generics method_generics)
    (lookup_trait_impl_method timpl name)

(* Lookup the signature of a method. This returns a signature bound in two
   levels of binders: one for the trait generics and one for the method
   generics. Returns [None] if the trait or method declarations could not be
   found.
*)
let lookup_method_sig (crate : 'a gcrate) (trait_id : trait_decl_id)
    (name : trait_item_name) : fun_sig binder item_binder option =
  let* tdecl = TraitDeclId.Map.find_opt trait_id crate.trait_decls in
  let* {
         item_binder_params : generic_params = trait_params;
         item_binder_value : fun_decl_ref binder = bound_method;
       } =
    lookup_trait_decl_method tdecl name
  in
  let method_decl_id = bound_method.binder_value.id in
  let* method_decl =
    LlbcAst.FunDeclId.Map.find_opt method_decl_id crate.fun_decls
  in
  (* Substitute the signature to be valid under the binder. *)
  let signature =
    st_substitute_visitor#visit_fun_sig
      (make_subst_from_generics method_decl.signature.generics
         bound_method.binder_value.generics)
      method_decl.signature
  in
  (* Rebind everything *)
  let bound_sig =
    { binder_params = bound_method.binder_params; binder_value = signature }
  in
  Some { item_binder_params = trait_params; item_binder_value = bound_sig }

(* Like [lookup_method_sig], but with no binder shenanigans: the returns
   fun_sig takes as parameters the concatenation of trait generics and method
   generics. *)
let lookup_flat_method_sig (crate : 'a gcrate) (trait_id : trait_decl_id)
    (name : trait_item_name) : fun_sig option =
  let* bound_sig = lookup_method_sig crate trait_id name in
  let bound_sig = fuse_binders st_substitute_visitor#visit_fun_sig bound_sig in
  let s =
    { bound_sig.item_binder_value with generics = bound_sig.item_binder_params }
  in
  Some s

(* Lookup the signature of a `Ty::FnDef`. *)
let lookup_fndef_sig (crate : 'a gcrate) (fn_def : fn_ptr region_binder) :
    (ty list * ty) region_binder option =
  match fn_def.binder_value.func with
  | FunId (FRegular fun_decl_id) ->
      let* fun_decl =
        LlbcAst.FunDeclId.Map.find_opt fun_decl_id crate.fun_decls
      in
      (* Substitute the signature to be valid under the binder. *)
      let fn_sig =
        st_substitute_visitor#visit_fun_sig
          (make_subst_from_generics fun_decl.signature.generics
             fn_def.binder_value.generics)
          fun_decl.signature
      in
      (* Rebind everything *)
      Some
        {
          binder_regions = fn_def.binder_regions;
          binder_value = (fn_sig.inputs, fn_sig.output);
        }
  | _ -> None

(* Construct a set of generic arguments in the scope of `params` that matches
   `params` and feeds each required parameter with itself. E.g. given
   parameters for `<T, U> where U: PartialEq<T>`, the arguments would be `<T,
   U>[@TraitClause0]`. This uses `Bound` variables; we could define the same
   for `Free` variables if needed.
*)
let bound_identity_args (params : generic_params) : generic_args =
  (* Reuse the basic id->val mappings *)
  let s = empty_bound_sb_subst in
  {
    regions =
      List.map
        (fun (var : _ indexed_var) -> s.r_sb_subst var.index)
        params.regions;
    types =
      List.map
        (fun (var : _ indexed_var) -> s.ty_sb_subst var.index)
        params.types;
    const_generics =
      List.map
        (fun (var : const_generic_var) -> s.cg_sb_subst var.index)
        params.const_generics;
    trait_refs =
      List.map
        (fun (clause : trait_clause) ->
          let trait_id = s.tr_sb_subst clause.clause_id in
          { trait_id; trait_decl_ref = clause.trait })
        params.trait_clauses;
  }

(* Bind the predicate with no higher-kinded regions to make it into a poly predicate. *)
let trait_decl_ref_to_poly_trait_decl_ref (pred : trait_decl_ref) :
    trait_decl_ref region_binder =
  {
    binder_value =
      st_substitute_visitor#visit_trait_decl_ref move_under_binder_subst pred;
    binder_regions = [];
  }
