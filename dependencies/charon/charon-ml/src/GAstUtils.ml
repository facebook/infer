open Types
open GAst

(** Small utility: list the transitive parents of a region var group. We don't
    do that in an efficient manner, but it doesn't matter.

    This list *doesn't* include the current region. *)
let rec list_ancestor_region_groups (regions_hierarchy : region_var_groups)
    (gid : RegionGroupId.id) : RegionGroupId.Set.t =
  let rg = RegionGroupId.nth regions_hierarchy gid in
  let parents =
    List.fold_left
      (fun s gid ->
        (* Compute the parents *)
        let parents = list_ancestor_region_groups regions_hierarchy gid in
        (* Parents U current region *)
        let parents = RegionGroupId.Set.add gid parents in
        (* Make the union with the accumulator *)
        RegionGroupId.Set.union s parents)
      RegionGroupId.Set.empty rg.parents
  in
  parents

(** Small utility: same as {!list_ancestor_region_groups}, but returns an
    ordered list. *)
let list_ordered_ancestor_region_groups (regions_hierarchy : region_var_groups)
    (gid : RegionGroupId.id) : RegionGroupId.id list =
  let pset = list_ancestor_region_groups regions_hierarchy gid in
  let parents =
    List.filter
      (fun (rg : region_var_group) -> RegionGroupId.Set.mem rg.id pset)
      regions_hierarchy
  in
  let parents = List.map (fun (rg : region_var_group) -> rg.id) parents in
  parents

let locals_get_input_vars (locals : locals) : local list =
  let args = List.tl locals.locals in
  Collections.List.prefix locals.arg_count args

let fun_body_get_input_vars (fbody : 'body gexpr_body) : local list =
  locals_get_input_vars fbody.locals

(** Like `binder` but for the free variables bound by the generics of an item.
    This is not present in the charon ast but returned by helpers so we don't
    forget to substitute. Use `Substitute.apply_args_to_item_binder` to get the
    correctly-substituted inner value. *)
type 'a item_binder = {
  item_binder_params : generic_params;
  item_binder_value : 'a;
}
[@@deriving show, ord]

(** Lookup a method in this trait decl. The two levels of binders in the output
    reflect that there are two binding levels: the trait generics and the method
    generics. *)
let lookup_trait_decl_method (tdecl : trait_decl) (name : trait_item_name) :
    fun_decl_ref binder item_binder option =
  Option.map
    (fun (_, bound_fn) ->
      { item_binder_params = tdecl.generics; item_binder_value = bound_fn })
    (List.find_opt (fun (s, _) -> s = name) tdecl.methods)

(** Lookup a method in this trait impl. The two levels of binders in the output
    reflect that there are two binding levels: the impl generics and the method
    generics. *)
let lookup_trait_impl_method (timpl : trait_impl) (name : trait_item_name) :
    fun_decl_ref binder item_binder option =
  Option.map
    (fun (_, bound_fn) ->
      { item_binder_params = timpl.generics; item_binder_value = bound_fn })
    (List.find_opt (fun (s, _) -> s = name) timpl.methods)

let g_declaration_group_to_list (g : 'a g_declaration_group) : 'a list =
  match g with
  | RecGroup ids -> ids
  | NonRecGroup id -> [ id ]

let g_declaration_group_map (f : 'a -> 'a) (g : 'a g_declaration_group) :
    'a g_declaration_group =
  match g with
  | RecGroup ids -> RecGroup (List.map f ids)
  | NonRecGroup id -> NonRecGroup (f id)

let g_declaration_group_iter (f : 'a -> unit) (g : 'a g_declaration_group) :
    unit =
  let ids = g_declaration_group_to_list g in
  List.iter f ids

(** List all the ids in this declaration group. *)
let declaration_group_to_list (g : declaration_group) : any_decl_id list =
  match g with
  | FunGroup g -> List.map (fun id -> IdFun id) (g_declaration_group_to_list g)
  | TypeGroup g ->
      List.map (fun id -> IdType id) (g_declaration_group_to_list g)
  | TraitDeclGroup g ->
      List.map (fun id -> IdTraitDecl id) (g_declaration_group_to_list g)
  | GlobalGroup g ->
      List.map (fun id -> IdGlobal id) (g_declaration_group_to_list g)
  | TraitImplGroup g ->
      List.map (fun id -> IdTraitImpl id) (g_declaration_group_to_list g)
  | MixedGroup g -> g_declaration_group_to_list g

(** Split a module's declarations between types, functions and globals *)
let split_declarations (decls : declaration_group list) :
    type_declaration_group list
    * fun_declaration_group list
    * global_declaration_group list
    * trait_declaration_group list
    * trait_impl_group list
    * mixed_declaration_group list =
  let rec split decls =
    match decls with
    | [] -> ([], [], [], [], [], [])
    | d :: decls' -> (
        let types, funs, globals, trait_decls, trait_impls, mixeds =
          split decls'
        in
        match d with
        | TypeGroup decl ->
            (decl :: types, funs, globals, trait_decls, trait_impls, mixeds)
        | FunGroup decl ->
            (types, decl :: funs, globals, trait_decls, trait_impls, mixeds)
        | GlobalGroup decl ->
            (types, funs, decl :: globals, trait_decls, trait_impls, mixeds)
        | TraitDeclGroup decl ->
            (types, funs, globals, decl :: trait_decls, trait_impls, mixeds)
        | TraitImplGroup decl ->
            (types, funs, globals, trait_decls, decl :: trait_impls, mixeds)
        | MixedGroup decls ->
            (types, funs, globals, trait_decls, trait_impls, decls :: mixeds))
  in
  split decls

(** Split a module's declarations into three maps from type/fun/global ids to
    declaration groups. *)
let split_declarations_to_group_maps (decls : declaration_group list) :
    type_declaration_group TypeDeclId.Map.t
    * fun_declaration_group FunDeclId.Map.t
    * global_declaration_group GlobalDeclId.Map.t
    * trait_declaration_group TraitDeclId.Map.t
    * trait_impl_group TraitImplId.Map.t
    * mixed_declaration_group list =
  let module G (M : Map.S) = struct
    let add_group (map : M.key g_declaration_group M.t)
        (group : M.key g_declaration_group) : M.key g_declaration_group M.t =
      List.fold_left
        (fun map id -> M.add id group map)
        map
        (g_declaration_group_to_list group)

    let create_map (groups : M.key g_declaration_group list) :
        M.key g_declaration_group M.t =
      List.fold_left add_group M.empty groups
  end in
  let types, funs, globals, trait_decls, trait_impls, mixed_groups =
    split_declarations decls
  in
  let module TG = G (TypeDeclId.Map) in
  let types = TG.create_map types in
  let module FG = G (FunDeclId.Map) in
  let funs = FG.create_map funs in
  let module GG = G (GlobalDeclId.Map) in
  let globals = GG.create_map globals in
  let module TDG = G (TraitDeclId.Map) in
  let trait_decls = TDG.create_map trait_decls in
  let module TIG = G (TraitImplId.Map) in
  let trait_impls = TIG.create_map trait_impls in
  (types, funs, globals, trait_decls, trait_impls, mixed_groups)

module OrderedAnyDeclId : Collections.OrderedType with type t = any_decl_id =
struct
  type t = any_decl_id

  let compare = compare_any_decl_id
  let to_string = show_any_decl_id
  let pp_t fmt x = Format.pp_print_string fmt (show_any_decl_id x)
  let show_t = show_any_decl_id
end

module AnyDeclIdSet = Collections.MakeSet (OrderedAnyDeclId)
module AnyDeclIdMap = Collections.MakeMap (OrderedAnyDeclId)

let any_decl_id_to_kind_name (id : any_decl_id) : string =
  match id with
  | IdType _ -> "type decl"
  | IdFun _ -> "fun decl"
  | IdGlobal _ -> "global decl"
  | IdTraitDecl _ -> "trait decl"
  | IdTraitImpl _ -> "trait impl"

let g_declaration_group_filter_map (f : 'a -> 'a option)
    (g : 'a g_declaration_group) : 'a g_declaration_group option =
  match g with
  | NonRecGroup id -> begin
      match f id with
      | Some id -> Some (NonRecGroup id)
      | None -> None
    end
  | RecGroup ids ->
      let ids = List.filter_map f ids in
      if ids = [] then None else Some (RecGroup ids)

class ['self] filter_decl_id =
  object (self : 'self (* inherit [_] VisitorsRuntime.iter *))
    method visit_type_decl_id _ (id : TypeDeclId.id) = Some id
    method visit_fun_decl_id _ (id : FunDeclId.id) = Some id
    method visit_global_decl_id _ (id : GlobalDeclId.id) = Some id
    method visit_trait_decl_id _ (id : TraitDeclId.id) = Some id
    method visit_trait_impl_id _ (id : TraitImplId.id) = Some id

    method visit_any_decl_id (env : 'a) (id : any_decl_id) : any_decl_id option
        =
      match id with
      | IdType id ->
          Option.map (fun id -> IdType id) (self#visit_type_decl_id env id)
      | IdFun id ->
          Option.map (fun id -> IdFun id) (self#visit_fun_decl_id env id)
      | IdGlobal id ->
          Option.map (fun id -> IdGlobal id) (self#visit_global_decl_id env id)
      | IdTraitDecl id ->
          Option.map
            (fun id -> IdTraitDecl id)
            (self#visit_trait_decl_id env id)
      | IdTraitImpl id ->
          Option.map
            (fun id -> IdTraitImpl id)
            (self#visit_trait_impl_id env id)

    method visit_type_declaration_group env (g : type_declaration_group) :
        type_declaration_group option =
      g_declaration_group_filter_map (self#visit_type_decl_id env) g

    method visit_fun_declaration_group env (g : fun_declaration_group) :
        fun_declaration_group option =
      g_declaration_group_filter_map (self#visit_fun_decl_id env) g

    method visit_global_declaration_group env (g : global_declaration_group) :
        global_declaration_group option =
      g_declaration_group_filter_map (self#visit_global_decl_id env) g

    method visit_trait_declaration_group env (g : trait_declaration_group) :
        trait_declaration_group option =
      g_declaration_group_filter_map (self#visit_trait_decl_id env) g

    method visit_trait_impl_group env (g : trait_impl_group) :
        trait_impl_group option =
      g_declaration_group_filter_map (self#visit_trait_impl_id env) g

    method visit_mixed_declaration_group env (g : mixed_declaration_group) :
        mixed_declaration_group option =
      g_declaration_group_filter_map (self#visit_any_decl_id env) g

    method visit_declaration_group env (g : declaration_group) :
        declaration_group option =
      match g with
      | TypeGroup g ->
          Option.map
            (fun g -> TypeGroup g)
            (self#visit_type_declaration_group env g)
      | FunGroup g ->
          Option.map
            (fun g -> FunGroup g)
            (self#visit_fun_declaration_group env g)
      | GlobalGroup g ->
          Option.map
            (fun g -> GlobalGroup g)
            (self#visit_global_declaration_group env g)
      | TraitDeclGroup g ->
          Option.map
            (fun g -> TraitDeclGroup g)
            (self#visit_trait_declaration_group env g)
      | TraitImplGroup g ->
          Option.map
            (fun g -> TraitImplGroup g)
            (self#visit_trait_impl_group env g)
      | MixedGroup g ->
          Option.map
            (fun g -> MixedGroup g)
            (self#visit_mixed_declaration_group env g)

    method visit_declaration_groups env (gl : declaration_group list) :
        declaration_group list =
      List.filter_map (self#visit_declaration_group env) gl
  end
