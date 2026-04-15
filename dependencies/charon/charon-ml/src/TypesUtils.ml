open Collections
open Types
open Utils
open ValuesUtils

module RegionOrderedType : OrderedType with type t = region = struct
  type t = region

  let compare = compare_region
  let to_string = show_region
  let pp_t = pp_region
  let show_t = show_region
end

module RegionMap = Collections.MakeMap (RegionOrderedType)
module RegionSet = Collections.MakeSet (RegionOrderedType)

(** Like `binder` but for the free variables bound by the generics of an item.
    This is not present in the charon ast but returned by helpers so we don't
    forget to substitute. Use `Substitute.apply_args_to_item_binder` to get the
    correctly-substituted inner value. *)
type 'a item_binder = {
  item_binder_params : generic_params;
  item_binder_value : 'a;
}
[@@deriving show, ord]

type bound_fun_sig = fun_sig item_binder

let to_name (ls : string list) : name =
  List.map (fun s -> PeIdent (s, Disambiguator.zero)) ls

let as_ident (e : path_elem) : string =
  match e with
  | PeIdent (s, _) -> s
  | _ -> raise (Failure "Unexpected")

let type_decl_is_opaque (d : type_decl) : bool =
  match d.kind with
  | Opaque -> true
  | _ -> false

(** Retrieve the list of fields for the given variant of a
    {!Charon.Types.type_decl}.

    Raises [Invalid_argument] if the arguments are incorrect. *)
let type_decl_get_fields (def : type_decl)
    (opt_variant_id : VariantId.id option) : field list =
  match (def.kind, opt_variant_id) with
  | Enum variants, Some variant_id -> (VariantId.nth variants variant_id).fields
  | Struct fields, None -> fields
  | Union fields, None -> fields
  | (Enum _ | Struct _ | Union _), _ ->
      let opt_variant_id =
        match opt_variant_id with
        | None -> "None"
        | Some _ -> "Some"
      in
      raise
        (Invalid_argument
           ("The variant id should be [Some] if and only if the definition is \
             an enumeration:\n\
             - def: " ^ show_type_decl def ^ "\n- opt_variant_id: "
          ^ opt_variant_id))
  | _ ->
      raise
        (Invalid_argument
           ("Can't get the list of fields of this adt:\n" ^ show_type_decl def))

let type_decl_is_enum (def : type_decl) : bool =
  match def.kind with
  | Enum _ -> true
  | _ -> false

(** Return [true] if a {!type:Charon.Types.ty} is actually [unit] *)
let ty_is_unit (ty : ty) : bool =
  match ty with
  | TAdt
      {
        id = TTuple;
        generics =
          { regions = []; types = []; const_generics = []; trait_refs = _ };
      } -> true
  | _ -> false

let ty_as_opt_adt (ty : ty) : (type_id * generic_args) option =
  match ty with
  | TAdt tref -> Some (tref.id, tref.generics)
  | _ -> None

let ty_is_adt (ty : ty) : bool = Option.is_some (ty_as_opt_adt ty)

let ty_as_adt (ty : ty) : type_id * generic_args =
  match ty_as_opt_adt ty with
  | Some (id, generics) -> (id, generics)
  | None -> raise (Failure "Unreachable")

let ty_as_builtin_adt_opt (ty : ty) : (builtin_ty * generic_args) option =
  match ty with
  | TAdt { id = TBuiltin id; generics } -> Some (id, generics)
  | _ -> None

let ty_is_builtin_adt (ty : ty) : bool =
  Option.is_some (ty_as_builtin_adt_opt ty)

let ty_as_builtin_adt (ty : ty) : builtin_ty * generic_args =
  match ty_as_builtin_adt_opt ty with
  | Some (id, generics) -> (id, generics)
  | None -> raise (Failure "Unreachable")

let ty_as_opt_array (ty : ty) : (ty * constant_expr) option =
  match ty with
  | TArray (ty, len) -> Some (ty, len)
  | _ -> None

let ty_is_array (ty : ty) : bool = Option.is_some (ty_as_opt_array ty)

let ty_as_array (ty : ty) : ty * constant_expr =
  match ty_as_opt_array ty with
  | Some (ty, n) -> (ty, n)
  | None -> raise (Failure "Unreachable")

let ty_as_opt_slice (ty : ty) : ty option =
  match ty with
  | TSlice ty -> Some ty
  | _ -> None

let ty_is_slice (ty : ty) : bool = Option.is_some (ty_as_opt_slice ty)

let ty_as_slice (ty : ty) : ty =
  match ty_as_opt_slice ty with
  | Some ty -> ty
  | None -> raise (Failure "Unreachable")

let ty_as_ref (ty : ty) : region * ty * ref_kind =
  match ty with
  | TRef (r, ref_ty, kind) -> (r, ref_ty, kind)
  | _ -> raise (Failure "Unreachable")

let ty_is_custom_adt (ty : ty) : bool =
  match ty with
  | TAdt { id = TAdtId _; _ } -> true
  | _ -> false

let ty_as_custom_adt (ty : ty) : TypeDeclId.id * generic_args =
  match ty with
  | TAdt { id = TAdtId id; generics } -> (id, generics)
  | _ -> raise (Failure "Unreachable")

let ty_as_literal (ty : ty) : literal_type =
  match ty with
  | TLiteral lty -> lty
  | _ -> raise (Failure "Unreachable")

let literal_as_integer (literal : literal_type) : integer_type =
  match literal with
  | TInt ty -> Signed ty
  | TUInt ty -> Unsigned ty
  | _ -> raise (Failure "Unreachable")

let ty_as_integer (ty : ty) : integer_type =
  literal_as_integer (ty_as_literal ty)

let integer_as_literal (int_ty : integer_type) : literal_type =
  match int_ty with
  | Signed int_ty -> TInt int_ty
  | Unsigned int_ty -> TUInt int_ty

let constant_expr_as_literal (c : constant_expr) : Values.literal =
  match c.kind with
  | CLiteral v -> v
  | _ -> raise (Failure "Unreachable")

let trait_instance_id_as_trait_impl (id : trait_ref_kind) :
    trait_impl_id * generic_args =
  match id with
  | TraitImpl impl_ref -> (impl_ref.id, impl_ref.generics)
  | _ -> raise (Failure "Unreachable")

(* Make a debruijn variable of index 0 *)
let zero_db_var (varid : 'id) : 'id de_bruijn_var = Bound (0, varid)

let free_var_of_db_var (var : 'id de_bruijn_var) : 'id option =
  match var with
  | Bound _ -> None
  | Free id -> Some id

let decr_db_var : 'id de_bruijn_var -> 'id de_bruijn_var = function
  | Free id -> Free id
  | Bound (dbid, id) -> Bound (dbid - 1, id)

let incr_db_var : 'id de_bruijn_var -> 'id de_bruijn_var = function
  | Free id -> Free id
  | Bound (dbid, id) -> Bound (dbid + 1, id)

let empty_generic_args : generic_args =
  { regions = []; types = []; const_generics = []; trait_refs = [] }

let mk_generic_args (regions : region list) (types : ty list)
    (const_generics : constant_expr list) (trait_refs : trait_ref list) :
    generic_args =
  { regions; types; const_generics; trait_refs }

let mk_generic_args_from_types (types : ty list) : generic_args =
  { regions = []; types; const_generics = []; trait_refs = [] }

let empty_generic_params : generic_params =
  {
    regions = [];
    types = [];
    const_generics = [];
    trait_clauses = [];
    regions_outlive = [];
    types_outlive = [];
    trait_type_constraints = [];
  }

let generic_args_of_params span (generics : generic_params) : generic_args =
  let regions =
    List.map (fun (v : region_param) -> RVar (Free v.index)) generics.regions
  in
  let types =
    List.map (fun (v : type_param) -> TVar (Free v.index)) generics.types
  in
  let const_generics =
    List.map
      (fun (v : const_generic_param) ->
        { kind = CVar (Free v.index); ty = v.ty })
      generics.const_generics
  in
  let trait_refs =
    List.map
      (fun (c : trait_param) ->
        { kind = Clause (Free c.clause_id); trait_decl_ref = c.trait })
      generics.trait_clauses
  in
  { regions; types; const_generics; trait_refs }

(** The unit type *)
let mk_unit_ty : ty = TAdt { id = TTuple; generics = empty_generic_args }

(** The usize type *)
let mk_usize_ty : ty = TLiteral (TUInt Usize)

let ty_as_opt_box (box_ty : ty) : ty option =
  match box_ty with
  | TAdt { id = TBuiltin TBox; generics = { types = [ boxed_ty ]; _ } } ->
      Some boxed_ty
  | _ -> None

let ty_is_box (box_ty : ty) : bool = Option.is_some (ty_as_opt_box box_ty)

(** Deconstruct a type of the form [Box<T>] to retrieve the [T] inside *)
let ty_as_box (box_ty : ty) : ty =
  match ty_as_opt_box box_ty with
  | Some ty -> ty
  | None -> raise (Failure "Not a boxed type")

(** Deconstruct a type of the form [&T] or [&mut T] to retrieve the [T] (and the
    borrow kind, etc.) *)
let ty_get_ref (ty : ty) : region * ty * ref_kind =
  match ty with
  | TRef (r, ty, ref_kind) -> (r, ty, ref_kind)
  | _ -> raise (Failure "Not a ref type")

let mk_ref_ty (r : region) (ty : ty) (ref_kind : ref_kind) : ty =
  TRef (r, ty, ref_kind)

(** Make a box type *)
let mk_box_ty (ty : ty) : ty =
  TAdt { id = TBuiltin TBox; generics = mk_generic_args_from_types [ ty ] }

(* TODO: move region set manipulation to aeneas *)

(** Check if a free region is in a set of regions.

    This function should be used on non-erased and non-bound regions. For
    sanity, we raise exceptions if this is not the case. *)
let region_in_set ?(allow_erased = false) (r : region) (rset : RegionId.Set.t) :
    bool =
  match r with
  | RStatic -> false
  | RErased | RBody _ ->
      if allow_erased then false
      else
        raise
          (Failure "region_in_set shouldn't be called on erased or body regions")
  | RVar (Bound _) ->
      raise (Failure "region_in_set shouldn't be called on bound regions")
  | RVar (Free id) -> RegionId.Set.mem id rset

let region_is_free (r : region) : bool =
  match r with
  | RVar (Free _) -> true
  | _ -> false

let region_is_erased (r : region) : bool =
  match r with
  | RErased -> true
  | _ -> false

let generic_args_lengths (args : generic_args) : int * int * int * int =
  let { regions; types; const_generics; trait_refs } = args in
  ( List.length regions,
    List.length types,
    List.length const_generics,
    List.length trait_refs )

let generic_params_lengths (args : generic_params) : int * int * int * int =
  let { regions; types; const_generics; trait_clauses; _ } = args in
  ( List.length regions,
    List.length types,
    List.length const_generics,
    List.length trait_clauses )

(** Computes the variant from the tag.

    If the [tag] does not correspond to any valid discriminant but there is a
    niche, the resulting [VariantId] will be for the untagged
    variant[TagEncoding::Niche::untagged_variant]. *)
let get_variant_from_tag ptr_size ty_decl (tag : Values.scalar_value) =
  let ( let* ) = Option.bind in
  let* layout = ty_decl.layout in
  let* discr_layout = layout.discriminant_layout in
  match ty_decl.kind with
  | Enum variants -> begin
      match variants with
      | [] -> None
      | hd_variant :: _ -> (
          let discr = hd_variant.discriminant in
          let rec find_mapi f i = function
            | [] -> None
            | v :: tl ->
                if f i v then Some (VariantId.of_int i)
                else find_mapi f (i + 1) tl
          in

          match discr_layout.encoding with
          | Direct -> begin
              assert (discr_layout.tag_ty = Scalars.get_ty tag);
              let discr =
                match integer_type_of_literal discr with
                | Some intty -> begin
                    match
                      Scalars.mk_scalar ptr_size intty (Scalars.get_val tag)
                    with
                    | Ok sv -> Some (Values.VScalar sv)
                    | Error _ -> None
                  end
                | None -> None
              in
              find_mapi (fun i v -> Some v.discriminant = discr) 0 variants
            end
          | Niche untagged_var -> begin
              match
                find_mapi
                  (fun id v -> Some tag = v.tag)
                  0 layout.variant_layouts
              with
              | Some id -> Some id
              | None -> Some untagged_var
            end)
    end
  | _ -> None
