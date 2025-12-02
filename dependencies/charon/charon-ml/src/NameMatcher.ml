(** Utilities to identify Rust definitions by matching on their names.

    Identifying Rust definitions is non trivial because of:
    - the impl blocks, which are identified by their types
    - trait instances, which don't have a name (and which we identify with trait
      references)

    For this reason, we define:
    - a small pattern matching language for Rust names
    - a parser for this language
    - matchers which check if a name matches a pattern
    - helpers to derive patterns from names (useful when one identifies some
      external functions that need custom treatment, as it avoids writing
      patterns by hand)

    Here are some examples of patterns:
    - ["core::mem::replace"]: the function [core::mem::replace]
    - ["alloc::vec::{alloc::vec::Vec<@>}::push"]: the function [push] in any
      impl block of type [alloc::vec::Vec<T>], where T is a type variable. Note
      that ["@"] means that this matches any (type) variable. In case we need
      stronger constraints, we can name those variables: "@T". All the
      occurrences of ["@T"] must match the same variable (ex.: ["Foo<@T, @T>"]
      would match [Foo<U, U>] but not [Foo<T, U>]).
    - the ["@"] syntax is used both for types and const generics. For
      regions/lifetimes, we use ["'"]: ["&'a mut @T"]
    - for the types we put inside blocks, we have syntax for arrays, slices, and
      references:
    - ["[@T; @N]"]: slice
    - ["&'R mut @T"]: mutable reference

    Remark: [Box] is treated as a primitive type, which means that one only
    needs to type ["Box"] (instead of ["alloc::boxed::Box"] - though the latter
    also works). *)

(* The "raw" name matcher patterns *)

include Name_matcher_parser.Ast
include Name_matcher_parser.Interface
module T = Types
module E = Expressions

let log = Logging.name_matcher_logger

(*
 * Convert patterns to strings
 *)
type target_kind =
  | TkPattern  (** Generate a string which can be parsed as a pattern *)
  | TkPretty  (** Pretty printing *)
  | TkName  (** A name for code extraction (for instance for trait instances) *)

type print_config = { tgt : target_kind }

let literal_to_string (c : print_config) (l : literal) : string =
  match l with
  | LInt v -> Z.to_string v
  | LBool b -> Bool.to_string b
  | LChar x -> (
      match c.tgt with
      | TkPattern ->
          (* TODO: we can't use the syntax 'x' for now because of lifetimes *)
          raise (Failure "TODO")
      | TkPretty -> "'" ^ String.make 1 x ^ "'"
      | TkName -> String.make 1 x)

let region_var_to_string (c : print_config) (v : var option) : string =
  match c.tgt with
  | TkPattern | TkPretty -> (
      match v with
      | None -> "'_"
      | Some (VarName n) -> "'" ^ n
      | Some (VarIndex id) -> "'" ^ string_of_int id)
  | TkName -> (
      match v with
      | None -> ""
      | Some (VarName n) -> StringUtils.capitalize_first_letter n
      | Some (VarIndex id) -> string_of_int id)

let region_to_string (c : print_config) (r : region) : string =
  match r with
  | RStatic -> (
      match c.tgt with
      | TkPattern | TkPretty -> "'static"
      | TkName -> "Static")
  | RVar v -> region_var_to_string c v

let opt_var_to_string (c : print_config) (v : var option) : string =
  match c.tgt with
  | TkPattern -> (
      match v with
      | None -> "@"
      | Some (VarName n) -> "@" ^ n
      | Some (VarIndex id) -> "@" ^ string_of_int id)
  | TkPretty | TkName -> (
      (* Below: when generating names, we shouldn't use the None or VarIndex cases *)
      match v with
      | None -> "P"
      | Some (VarName n) -> n
      | Some (VarIndex id) -> "P" ^ string_of_int id)

let disambiguator_to_string (c : print_config) (d : int) : string =
  if d = 0 then ""
  else
    match c.tgt with
    | TkPattern | TkPretty -> "#" ^ string_of_int d
    | TkName -> "_" ^ string_of_int d

let rec pattern_to_string (c : print_config) (p : pattern) : string =
  let sep =
    match c.tgt with
    | TkPattern | TkPretty -> "::"
    | TkName -> ""
  in
  String.concat sep (List.map (pattern_elem_to_string c) p)

and pattern_elem_to_string (c : print_config) (e : pattern_elem) : string =
  match e with
  | PIdent (s, d, g) ->
      let d = disambiguator_to_string c d in
      s ^ d ^ generic_args_to_string c g
  | PImpl ty -> (
      let ty = expr_to_string c ty in
      match c.tgt with
      | TkPattern | TkPretty -> "{" ^ ty ^ "}"
      | TkName -> ty)

and expr_to_string (c : print_config) (e : expr) : string =
  match e with
  | EComp pat -> pattern_to_string c pat
  | EPrimAdt (id, generics) -> (
      match id with
      | TTuple -> (
          let generics = List.map (generic_arg_to_string c) generics in
          match c.tgt with
          | TkPattern | TkPretty -> "(" ^ String.concat ", " generics ^ ")"
          | TkName -> "Tuple" ^ String.concat "" generics)
      | TArray -> (
          match generics with
          | [ ty; cg ] -> (
              let ty = generic_arg_to_string c ty in
              let cg = generic_arg_to_string c cg in
              match c.tgt with
              | TkPattern | TkPretty -> "[" ^ ty ^ "; " ^ cg ^ "]"
              | TkName -> "Array" ^ ty ^ cg)
          | _ -> raise (Failure "Ill-formed pattern"))
      | TSlice -> (
          match generics with
          | [ ty ] -> (
              let ty = generic_arg_to_string c ty in
              match c.tgt with
              | TkPattern | TkPretty -> "[" ^ ty ^ "]"
              | TkName -> "Slice" ^ ty)
          | _ -> raise (Failure "Ill-formed pattern")))
  | ERef (r, ty, rk) ->
      let rk =
        match rk with
        | RMut -> "mut "
        | RShared -> ""
      in
      "&" ^ region_to_string c r ^ " " ^ rk ^ expr_to_string c ty
  | EVar v -> opt_var_to_string c v
  | EArrow (inputs, out) -> (
      let inputs = List.map (expr_to_string c) inputs in
      let out = Option.map (expr_to_string c) out in
      match c.tgt with
      | TkPattern | TkPretty ->
          let out =
            match out with
            | None -> ""
            | Some out -> " -> " ^ out
          in
          "fn (" ^ String.concat ", " inputs ^ ")" ^ out
      | TkName -> (
          "Arrow" ^ String.concat "" inputs
          ^
          match out with
          | None -> ""
          | Some out -> out))
  | ERawPtr (mut, ty) -> (
      match c.tgt with
      | TkPattern | TkPretty ->
          let mut =
            match mut with
            | Mut -> "*mut"
            | Not -> "*const"
          in
          mut ^ " " ^ expr_to_string c ty
      | TkName ->
          let mut =
            match mut with
            | Mut -> "RawPtrMut"
            | Not -> "RawPtrConst"
          in
          mut ^ expr_to_string c ty)

and generic_arg_to_string (c : print_config) (g : generic_arg) : string =
  match g with
  | GExpr e -> expr_to_string c e
  | GValue l -> (
      let l = literal_to_string c l in
      match c.tgt with
      | TkPattern | TkPretty -> l
      | TkName -> StringUtils.capitalize_first_letter l)
  | GRegion r -> region_to_string c r

and generic_args_to_string (c : print_config) (generics : generic_args) : string
    =
  if generics = [] then ""
  else
    let generics = List.map (generic_arg_to_string c) generics in
    match c.tgt with
    | TkPattern | TkPretty -> "<" ^ String.concat ", " generics ^ ">"
    | TkName -> String.concat "" generics

(*
 * Match a name
 *)

module VarOrderedType : Collections.OrderedType with type t = var = struct
  type t = var

  let compare = compare_var
  let to_string x = show_var x
  let pp_t fmt x = Format.pp_print_string fmt (show_var x)
  let show_t x = show_var x
end

module VarMap = Collections.MakeMap (VarOrderedType)

(** Context to lookup definitions *)
type 'fun_body ctx = { crate : 'fun_body GAst.gcrate }

let ctx_from_crate crate = { crate }
let ctx_to_fmt_env { crate } = PrintUtils.of_crate crate

(** Match configuration *)
type match_config = {
  map_vars_to_vars : bool;
      (** If true, only allow matching variables to variables.

          This is important when matching names: if the pattern is
          [alloc::boxed::{Box<@T>}::new], we only want to match names where [@T]
          is a variable. For instance, we wouldn't want to match
          [alloc::boxed::{Box<u32>}::new] (if it existed...). However, we might
          want to match instantiations (i.e., for which [@T] is matched to
          [usize]) when matching function calls inside bodies. *)
  match_with_trait_decl_refs : bool;
      (** If true, when matching trait refs, use the implemented trait decl
          refs, otherwise match the name of the implementations.

          For instance, if it is set to true, you can identify a call to
          [std::ops::Index<usize>::index] for [Vec<T>] with the name:
          ["std::ops::Index<Vec<@T>, usize>::index"]. Otherwise, you will have
          to refer to the [index] function in the proper [impl] block for [Vec].
      *)
}

(** Mapped expressions.

    The {!MRegion} variant is used when matching generics. *)
type mexpr = MTy of T.ty | MCg of T.const_generic | MRegion of T.region
[@@deriving show]

type region_map = {
  regions : T.region VarMap.t ref;  (** The map for "regular" regions *)
  bound_regions : T.region T.RegionId.Map.t ref list;
      (** The stack of maps for bound regions.

          Note that the stack itself is not inside a reference: this allows us
          not to pop it when we go outside a bound regions group. *)
}

(* Small helper to store the mappings from variables to expressions *)
type maps = {
  rmap : region_map;  (** Regions maps. *)
  vmap : mexpr VarMap.t ref;
      (** Variables map (accounts both for the types and const generics) *)
}

let mk_empty_region_map () =
  { regions = ref VarMap.empty; bound_regions = [ ref T.RegionId.Map.empty ] }

let mk_empty_maps () =
  { rmap = mk_empty_region_map (); vmap = ref VarMap.empty }

let maps_push_bound_regions_group (m : maps) : maps =
  let rmap =
    {
      m.rmap with
      bound_regions = ref T.RegionId.Map.empty :: m.rmap.bound_regions;
    }
  in
  { m with rmap }

(** Push a group of bound regions if the group is non-empty - TODO: make this
    more precise *)
let maps_push_bound_regions_group_if_nonempty (m : maps) (regions : 'a list) :
    maps =
  match regions with
  | [] -> m
  | _ -> maps_push_bound_regions_group m

(** Update a map and check that there are no incompatible constraints at the
    same time. *)
let update_map (find_opt : 'a -> 'm -> 'b option) (add : 'a -> 'b -> 'm -> 'm)
    (m : 'm ref) (id : 'a) (v : 'b) : bool =
  match find_opt id !m with
  | None ->
      (* Simply update *)
      m := add id v !m;
      true
  | Some v' ->
      (* Check the binding *)
      v = v'

let update_rmap (c : match_config) (m : maps) (id : var) (v : T.region) : bool =
  (* When it comes to matching, we treat erased regions like variables. *)
  let is_var =
    match v with
    | RVar _ | RErased -> true
    | RStatic -> false
  in
  if c.map_vars_to_vars && not is_var then false
  else
    match v with
    | RVar var ->
        let dbid, varid =
          match var with
          | Bound (dbid, varid) -> (dbid, varid)
          | Free varid -> (List.length m.rmap.bound_regions - 1, varid)
        in
        begin
          match List.nth_opt m.rmap.bound_regions dbid with
          | None -> raise (Failure "Unexpected region variable")
          | Some brmap ->
              update_map T.RegionId.Map.find_opt T.RegionId.Map.add brmap varid
                v
        end
    | _ -> update_map VarMap.find_opt VarMap.add m.rmap.regions id v

let update_tmap (c : match_config) (m : maps) (id : var) (v : T.ty) : bool =
  let is_var =
    match v with
    | TVar _ -> true
    | _ -> false
  in
  if c.map_vars_to_vars && not is_var then false
  else update_map VarMap.find_opt VarMap.add m.vmap id (MTy v)

let update_cmap (c : match_config) (m : maps) (id : var) (v : T.const_generic) :
    bool =
  let is_var =
    match v with
    | CgVar _ -> true
    | _ -> false
  in
  if c.map_vars_to_vars && not is_var then false
  else update_map VarMap.find_opt VarMap.add m.vmap id (MCg v)

let opt_update_rmap (c : match_config) (m : maps) (id : var option)
    (v : T.region) : bool =
  match id with
  | None -> true
  | Some id -> update_rmap c m id v

let opt_update_tmap (c : match_config) (m : maps) (id : var option) (v : T.ty) :
    bool =
  match id with
  | None -> true
  | Some id -> update_tmap c m id v

let opt_update_cmap (c : match_config) (m : maps) (id : var option)
    (v : T.const_generic) : bool =
  match id with
  | None -> true
  | Some id -> update_cmap c m id v

(** Pay attention when updating the names because we use this function for
    several purposes:
    - to match patterns with literal types
    - to convert patterns to strings which can be parsed as patterns
    - to convert patterns to string for printing/name generation *)
let literal_type_to_string (ty : T.literal_type) : string =
  match ty with
  | TBool -> "bool"
  | TChar -> "char"
  | TInt ty -> (
      match ty with
      | Isize -> "isize"
      | I8 -> "i8"
      | I16 -> "i16"
      | I32 -> "i32"
      | I64 -> "i64"
      | I128 -> "i128")
  | TUInt ty -> (
      match ty with
      | Usize -> "usize"
      | U8 -> "u8"
      | U16 -> "u16"
      | U32 -> "u32"
      | U64 -> "u64"
      | U128 -> "u128")
  | TFloat ty -> (
      match ty with
      | F16 -> "f16"
      | F32 -> "f32"
      | F64 -> "f64"
      | F128 -> "f128")

(** Match a pattern with a region.

    Region true and update the maps if the match is successful, return false
    otherwise. *)
let match_region (c : match_config) (m : maps) (id : region) (v : T.region) :
    bool =
  match (id, v) with
  | RStatic, RStatic -> true
  | RVar id, (RVar _ | RErased) ->
      (* When it comes to matching, we treat erased regions like variables *)
      opt_update_rmap c m id v
  | RVar id, _ -> if c.map_vars_to_vars then false else opt_update_rmap c m id v
  | _ -> false

let match_ref_kind (prk : ref_kind) (rk : T.ref_kind) : bool =
  match (prk, rk) with
  | RMut, RMut | RShared, RShared -> true
  | _ -> false

let match_literal (pl : literal) (l : Values.literal) : bool =
  match (pl, l) with
  | LInt pv, VScalar v -> pv = Scalars.get_val v
  | LBool pv, VBool v -> pv = v
  | LChar pv, VChar v -> Uchar.of_char pv = v
  | _ -> false

let rec match_name_with_generics (ctx : 'fun_body ctx) (c : match_config)
    ?(m : maps = mk_empty_maps ()) (p : pattern) (n : T.name)
    (g : T.generic_args) : bool =
  match (p, n) with
  | [], [] ->
      raise
        (Failure
           "match_name_with_generics: attempt to match empty names and patterns")
      (* We shouldn't get there: the names/patterns should be non empty *)
  | [], [ PeMonomorphized _ ] -> true (* We ignored monomorphizeds *)
  | [ PIdent (pid, pd, pg) ], [ PeIdent (id, d) ] ->
      log#ldebug
        (lazy
          ("match_name_with_generics: last ident:" ^ "\n- pid: " ^ pid
         ^ "\n- id: " ^ id));
      (* We reached the end: match the generics.
         We have to generate an empty map. *)
      pid = id
      && T.Disambiguator.of_int pd = d
      && match_generic_args ctx c m pg g
  | [ PImpl pty ], [ PeImpl impl ] -> (
      (* We can get there when matching a prefix of the name with a pattern *)
      (* We have to distinguish two cases:
         - the impl is an inherent impl (linked to a type)
         - the impl is a trait impl
      *)
      match impl with
      | ImplElemTy bound_ty ->
          match_expr_with_ty ctx c (mk_empty_maps ()) pty bound_ty.binder_value
          && g = TypesUtils.empty_generic_args
      | ImplElemTrait impl_id ->
          match_expr_with_trait_impl_id ctx c pty impl_id
          && g = TypesUtils.empty_generic_args)
  | PIdent (pid, pd, pg) :: p, PeIdent (id, d) :: n ->
      (* This is not the end: check that the generics are empty *)
      pid = id
      && T.Disambiguator.of_int pd = d
      && pg = []
      && match_name_with_generics ctx c p n g
  | PImpl pty :: p, PeImpl impl :: n -> (
      (* We have to distinguish two cases:
         - the impl is an inherent impl (linked to a type)
         - the impl is a trait impl
      *)
      match impl with
      | ImplElemTy bound_ty ->
          match_expr_with_ty ctx c (mk_empty_maps ()) pty bound_ty.binder_value
          && match_name_with_generics ctx c p n g
      | ImplElemTrait impl_id ->
          match_expr_with_trait_impl_id ctx c pty impl_id
          && match_name_with_generics ctx c p n g)
  | _ -> false

and match_name (ctx : 'fun_body ctx) (c : match_config) (p : pattern)
    (n : T.name) : bool =
  match_name_with_generics ctx c p n TypesUtils.empty_generic_args

and match_pattern_with_type_id (ctx : 'fun_body ctx) (c : match_config)
    (m : maps) (pid : pattern) (id : T.type_id) (generics : T.generic_args) :
    bool =
  match id with
  | TAdtId id ->
      (* Lookup the type decl and match the name *)
      let d = T.TypeDeclId.Map.find id ctx.crate.type_decls in
      match_name_with_generics ctx c ~m pid d.item_meta.name generics
  | TTuple -> false
  | TBuiltin id -> (
      match (id, pid) with
      | ( TBox,
          ( [ PIdent ("Box", _, pgenerics) ]
          | [
              PIdent ("alloc", _, []);
              PIdent ("boxed", _, []);
              PIdent ("Box", _, pgenerics);
            ] ) ) -> match_generic_args ctx c m pgenerics generics
      | TStr, [ PIdent ("str", _, []) ] ->
          generics = TypesUtils.empty_generic_args
      | _ -> false)

and match_pattern_with_literal_type (pty : pattern) (ty : T.literal_type) : bool
    =
  let ty = literal_type_to_string ty in
  match pty with
  | [ PIdent (ty', _, []) ] when ty = ty' -> true
  | _ -> false

and match_primitive_adt (pid : primitive_adt) (id : T.type_id) : bool =
  match (pid, id) with
  | TTuple, TTuple | TArray, TBuiltin TArray | TSlice, TBuiltin TSlice -> true
  | _ -> false

and match_expr_with_ty (ctx : 'fun_body ctx) (c : match_config) (m : maps)
    (pty : expr) (ty : T.ty) : bool =
  match (pty, ty) with
  | EComp pid, TAdt tref ->
      match_pattern_with_type_id ctx c m pid tref.id tref.generics
  | EComp pid, TLiteral lit -> match_pattern_with_literal_type pid lit
  | EPrimAdt (pid, pgenerics), TAdt tref ->
      match_primitive_adt pid tref.id
      && match_generic_args ctx c m pgenerics tref.generics
  | ERef (pr, pty, prk), TRef (r, ty, rk) ->
      match_region c m pr r
      && match_expr_with_ty ctx c m pty ty
      && match_ref_kind prk rk
  | EVar v, _ -> opt_update_tmap c m v ty
  | EComp pid, TTraitType (trait_ref, type_name) ->
      match_trait_type ctx c m pid trait_ref type_name
  | EArrow (pinputs, pout), TFnPtr binder -> begin
      (* Push a region group in the map, if necessary - TODO: make this more precise *)
      let m =
        maps_push_bound_regions_group_if_nonempty m binder.binder_regions
      in
      let inputs, output = binder.binder_value in
      (* Match *)
      List.for_all2 (match_expr_with_ty ctx c m) pinputs inputs
      &&
      match pout with
      | None -> output = TypesUtils.mk_unit_ty
      | Some pout -> match_expr_with_ty ctx c m pout output
    end
  | ERawPtr (Mut, pty), TRawPtr (ty, RMut) -> match_expr_with_ty ctx c m pty ty
  | ERawPtr (Not, pty), TRawPtr (ty, RShared) ->
      match_expr_with_ty ctx c m pty ty
  | _ -> false

and match_expr_with_trait_impl_id (ctx : 'fun_body ctx) (c : match_config)
    (ptr : expr) (impl_id : T.TraitImplId.id) : bool =
  (* Lookup the trait implementation *)
  let impl = T.TraitImplId.Map.find impl_id ctx.crate.trait_impls in
  (* Lookup the trait declaration *)
  let d = T.TraitDeclId.Map.find impl.impl_trait.id ctx.crate.trait_decls in
  (* Match *)
  match ptr with
  | EComp pid ->
      match_name_with_generics ctx c pid d.item_meta.name
        impl.impl_trait.generics
  | EPrimAdt _ | ERef _ | EVar _ | EArrow _ | ERawPtr _ -> false

and match_trait_decl_ref (ctx : 'fun_body ctx) (c : match_config) (m : maps)
    (pid : pattern) (tr : T.trait_decl_ref T.region_binder) : bool =
  (* Lookup the trait declaration *)
  let d = T.TraitDeclId.Map.find tr.binder_value.id ctx.crate.trait_decls in
  (* Push a region group in the map, if necessary - TODO: make this more precise *)
  let m = maps_push_bound_regions_group_if_nonempty m tr.binder_regions in
  (* Match the trait decl ref *)
  match_name_with_generics ctx c ~m pid d.item_meta.name
    tr.binder_value.generics

and match_trait_decl_ref_item (ctx : 'fun_body ctx) (c : match_config)
    (m : maps) (pid : pattern) (tr : T.trait_decl_ref T.region_binder)
    (item_name : string) (generics : T.generic_args) : bool =
  if c.match_with_trait_decl_refs then
    (* We match the trait decl ref *)
    (* We split the pattern between the trait decl ref and the associated item name *)
    let pid, pitem_name = Collections.List.pop_last pid in
    (* Match the trait ref *)
    match_trait_decl_ref ctx c m pid tr
    &&
    (* Match the item name *)
    match pitem_name with
    | PIdent (pitem_name, pd, pgenerics) ->
        pitem_name = item_name && pd = 0
        && match_generic_args ctx c (mk_empty_maps ()) pgenerics generics
    | _ -> false
  else raise (Failure "Unimplemented")

and match_trait_type (ctx : 'fun_body ctx) (c : match_config) (m : maps)
    (pid : pattern) (tr : T.trait_ref) (type_name : string) : bool =
  match_trait_decl_ref_item ctx c m pid tr.trait_decl_ref type_name
    TypesUtils.empty_generic_args

and match_generic_args (ctx : 'fun_body ctx) (c : match_config) (m : maps)
    (pgenerics : generic_args) (generics : T.generic_args) : bool =
  log#ldebug
    (lazy
      (let fmt_env = ctx_to_fmt_env ctx in
       "match_generic_args: " ^ "\n- pgenerics: "
       ^ generic_args_to_string { tgt = TkPattern } pgenerics
       ^ "\n- generics: "
       ^ PrintTypes.generic_args_to_string fmt_env generics));
  let merged_generics =
    List.concat
      [
        List.map (fun x -> MRegion x) generics.regions;
        List.map (fun x -> MTy x) generics.types;
        List.map (fun x -> MCg x) generics.const_generics;
      ]
  in
  if List.length pgenerics = 0 then true (* Generics can be omitted *)
  else begin
    (* Regions can be omitted *)
    let any_region_pat =
      List.exists
        (function
          | GRegion _ -> true
          | _ -> false)
        pgenerics
    in
    let pgenerics =
      if (not (List.length generics.regions = 0)) && not any_region_pat then
        List.append
          (List.map (fun _ -> GRegion (RVar None)) generics.regions)
          pgenerics
      else pgenerics
    in
    if List.length pgenerics = List.length merged_generics then
      List.for_all2 (match_generic_arg ctx c m) pgenerics merged_generics
    else false
  end

and match_generic_arg (ctx : 'fun_body ctx) (c : match_config) (m : maps)
    (pg : generic_arg) (g : mexpr) : bool =
  log#ldebug
    (lazy
      ("match_generic_arg: " ^ "\n- pg: "
      ^ generic_arg_to_string { tgt = TkPattern } pg
      ^ "\n- g: " ^ show_mexpr g));
  match (pg, g) with
  | GRegion pr, MRegion r -> match_region c m pr r
  | GExpr e, MTy ty -> match_expr_with_ty ctx c m e ty
  | GExpr e, MCg cg -> match_expr_with_const_generic ctx c m e cg
  | GValue v, MCg (CgValue cg) -> match_literal v cg
  | _ -> false

and match_expr_with_const_generic (ctx : 'fun_body ctx) (c : match_config)
    (m : maps) (pcg : expr) (cg : T.const_generic) : bool =
  match (pcg, cg) with
  | EVar pv, _ -> opt_update_cmap c m pv cg
  | EComp pat, CgGlobal gid ->
      (* Lookup the decl and match the name *)
      let d = T.GlobalDeclId.Map.find gid ctx.crate.global_decls in
      match_name ctx c pat d.item_meta.name
  | _ -> false

let builtin_fun_id_to_string (fid : T.builtin_fun_id) : string =
  match fid with
  | BoxNew -> "alloc::boxed::{Box<@T, alloc::alloc::Global>}::new"
  | ArrayToSliceShared -> "ArrayToSliceShared"
  | ArrayToSliceMut -> "ArrayToSliceMut"
  | ArrayRepeat -> "ArrayRepeat"
  | Index { is_array; mutability; is_range } ->
      let ty = if is_array then "Array" else "Slice" in
      let op = if is_range then "SubSlice" else "Index" in
      let mutability = PrintTypes.ref_kind_to_string mutability in
      ty ^ op ^ mutability
  | PtrFromParts mut ->
      let mut = if mut = RMut then "_mut" else "" in
      "std::ptr::from_raw_parts" ^ mut

let match_fn_ptr (ctx : 'fun_body ctx) (c : match_config) (p : pattern)
    (func : T.fn_ptr) : bool =
  match func.func with
  | FunId (FBuiltin fid) -> (
      let to_name (s : string list) : T.name =
        List.map (fun s -> T.PeIdent (s, T.Disambiguator.of_int 0)) s
      in
      match fid with
      | BoxNew -> (
          (* Slightly annoying because of the impl block.
             TODO: we could use the functions which check if two patterns
             are convertible. But we would need to update them (convertible
             is too strong, we simply need unification).
          *)
          match p with
          | [
           PIdent ("alloc", _, g0);
           PIdent ("boxed", _, g1);
           PImpl (EComp box_impl);
           PIdent ("new", _, g2);
          ] -> (
              g0 = [] && g1 = []
              && match_generic_args ctx c (mk_empty_maps ()) g2 func.generics
              &&
              match box_impl with
              | [ PIdent ("Box", _, _) ]
              | [
                  PIdent ("alloc", _, []);
                  PIdent ("boxed", _, []);
                  PIdent ("Box", _, _);
                ] -> true
              | _ -> false)
          | _ -> false)
      | _ ->
          let name = builtin_fun_id_to_string fid in
          match_name_with_generics ctx c p (to_name [ name ]) func.generics)
  | FunId (FRegular fid) ->
      let d = Types.FunDeclId.Map.find fid ctx.crate.fun_decls in
      (* Match the pattern on the name of the function. *)
      let match_function_name =
        match_name_with_generics ctx c p d.item_meta.name func.generics
      in
      (* Match the pattern on the trait implementation and method name, if applicable. *)
      let match_trait_ref =
        match d.kind with
        | TraitImplItem (_, trait_ref, method_name, _)
          when c.match_with_trait_decl_refs ->
            (* FIXME: this is a hack to circumvent the fact that sometimes
               Charon does not retrieve the proper number of parameters:
               before doing the substitution, check that the number of generic
               arguments matches the number of generic parameters.
            *)
            if
              TypesUtils.generic_params_lengths d.signature.generics
              = TypesUtils.generic_args_lengths func.generics
            then
              let subst =
                Substitute.make_subst_from_generics d.signature.generics
                  func.generics
              in
              let trait_ref =
                Substitute.trait_decl_ref_substitute subst trait_ref
              in
              (* TODO: recover the method generics somehow *)
              let method_generics = TypesUtils.empty_generic_args in
              match_trait_decl_ref_item ctx c (mk_empty_maps ()) p
                { binder_value = trait_ref; binder_regions = [] }
                method_name method_generics
            else false
        | _ -> false
      in
      match_function_name || match_trait_ref
  | TraitMethod (tr, method_name, _) ->
      match_trait_decl_ref_item ctx c (mk_empty_maps ()) p tr.trait_decl_ref
        method_name func.generics

let mk_name_with_generics_matcher (ctx : 'fun_body ctx) (c : match_config)
    (pat : string) : T.name -> T.generic_args -> bool =
  let pat = parse_pattern pat in
  match_name_with_generics ctx c pat

let mk_name_matcher (ctx : 'fun_body ctx) (c : match_config) (pat : string) :
    T.name -> bool =
  let pat = parse_pattern pat in
  match_name ctx c pat

(*
 * Helpers to convert names to patterns
 *)

(* Maps from variable ids to option pattern variable ids *)
type vars_map = {
  rmap : var option T.RegionId.Map.t;
  tmap : var option T.TypeVarId.Map.t;
  cmap : var option T.ConstGenericVarId.Map.t;
}

let empty_vars_map =
  {
    rmap = T.RegionId.Map.empty;
    tmap = T.TypeVarId.Map.empty;
    cmap = T.ConstGenericVarId.Map.empty;
  }

(* We use this to store the constraints maps. Note that we have a stack of
   maps, with one level per binder. *)
type constraints = vars_map list

let empty_constraints = [ empty_vars_map ]

let ref_kind_to_pattern (rk : T.ref_kind) : ref_kind =
  match rk with
  | RMut -> RMut
  | RShared -> RShared

let lookup_var_in_maps (m : constraints)
    (lookup : 'id -> vars_map -> 'a option option) (var : 'id T.de_bruijn_var) :
    'a option =
  let dbid, varid =
    match var with
    | Bound (dbid, varid) -> (dbid, varid)
    | Free varid -> (List.length m - 1, varid)
  in
  match List.nth_opt m dbid with
  | None -> None
  | Some map -> (
      match lookup varid map with
      | Some r -> r
      | None -> None)

let region_to_pattern (m : constraints) (r : T.region) : region =
  match r with
  | RVar var ->
      RVar
        (lookup_var_in_maps m
           (fun varid map -> T.RegionId.Map.find_opt varid map.rmap)
           var)
  | RStatic -> RStatic
  | RErased ->
      (* We do get there when converting function pointers (when we try to
         detect specific function calls) to patterns. *)
      RVar None

let type_var_to_pattern (m : constraints) (var : T.type_db_var) : var option =
  lookup_var_in_maps m
    (fun varid map -> T.TypeVarId.Map.find_opt varid map.tmap)
    var

let const_generic_var_to_pattern (m : constraints)
    (var : T.const_generic_db_var) : var option =
  lookup_var_in_maps m
    (fun varid map -> T.ConstGenericVarId.Map.find_opt varid map.cmap)
    var

let constraints_map_compute_regions_map (regions : T.region_var list) :
    var option T.RegionId.Map.t =
  let fresh_id (gen : int ref) : int =
    let id = !gen in
    gen := id + 1;
    id
  in
  let rid_gen = ref 0 in
  T.RegionId.Map.of_list
    (List.map
       (fun (r : T.region_var) ->
         let v =
           match r.name with
           | None -> VarIndex (fresh_id rid_gen)
           | Some name -> VarName name
         in
         (r.index, Some v))
       regions)

let compute_constraints_map (generics : T.generic_params) : constraints =
  let rmap = constraints_map_compute_regions_map generics.regions in
  let tmap =
    T.TypeVarId.Map.of_list
      (List.map
         (fun (x : T.type_var) -> (x.index, Some (VarName x.name)))
         generics.types)
  in
  let cmap =
    T.ConstGenericVarId.Map.of_list
      (List.map
         (fun (x : T.const_generic_var) -> (x.index, Some (VarName x.name)))
         generics.const_generics)
  in
  [ { rmap; tmap; cmap } ]

let constraints_map_push_regions_map (m : constraints)
    (regions : T.region_var list) : constraints =
  let rmap = constraints_map_compute_regions_map regions in
  { empty_vars_map with rmap } :: m

(** Push a regions map to the constraints map, if the group of regions is
    non-empty - TODO: do something more precise *)
let constraints_map_push_regions_map_if_nonempty (m : constraints)
    (regions : T.region_var list) : constraints =
  match regions with
  | [] -> m
  | _ -> constraints_map_push_regions_map m regions

type to_pat_config = {
  tgt : target_kind;
  use_trait_decl_refs : bool;  (** See {!match_with_trait_decl_refs} *)
}

let literal_type_to_pattern (c : to_pat_config) (lit : T.literal_type) : expr =
  let lit = literal_type_to_string lit in
  let lit =
    match c.tgt with
    | TkPattern | TkPretty -> lit
    | TkName -> StringUtils.capitalize_first_letter lit
  in
  EComp [ PIdent (lit, 0, []) ]

let literal_to_pattern (_c : to_pat_config) (lit : Values.literal) : literal =
  match lit with
  | VScalar sv -> LInt (Scalars.get_val sv)
  | VBool v -> LBool v
  | VChar v when Uchar.is_char v -> LChar (Uchar.to_char v)
  | VChar _ ->
      raise (Failure "Can't convert non-ASCII character literal to pattern")
  | VFloat _ | VStr _ | VByteStr _ ->
      raise
        (Failure "Float, string and byte string literals are not valid in names")

let rec name_with_generic_args_to_pattern_aux (ctx : 'fun_body ctx)
    (c : to_pat_config) (n : T.name) (generics : generic_args option) : pattern
    =
  match n with
  | [] -> raise (Failure "Empty names are not valid")
  | [ e ] -> path_elem_with_generic_args_to_pattern ctx c e generics
  | e :: n ->
      path_elem_with_generic_args_to_pattern ctx c e None
      @ name_with_generic_args_to_pattern_aux ctx c n generics

and name_to_pattern_aux (ctx : 'fun_body ctx) (c : to_pat_config) (n : T.name) :
    pattern =
  name_with_generic_args_to_pattern_aux ctx c n None

and path_elem_with_generic_args_to_pattern (ctx : 'fun_body ctx)
    (c : to_pat_config) (e : T.path_elem) (generics : generic_args option) :
    pattern_elem list =
  match e with
  | PeIdent (s, d) -> begin
      let d = T.Disambiguator.to_int d in
      match generics with
      | None -> [ PIdent (s, d, []) ]
      | Some args -> [ PIdent (s, d, args) ]
    end
  | PeImpl impl -> [ impl_elem_to_pattern ctx c impl ]
  | PeMonomorphized _ -> []

and impl_elem_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (impl : T.impl_elem) : pattern_elem =
  match impl with
  | ImplElemTy bound_ty ->
      PImpl (ty_to_pattern ctx c bound_ty.binder_params bound_ty.binder_value)
  | ImplElemTrait impl_id ->
      let impl = T.TraitImplId.Map.find impl_id ctx.crate.trait_impls in
      PImpl (trait_decl_ref_to_pattern ctx c impl.generics impl.impl_trait)

and trait_decl_ref_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (params : T.generic_params) (tr : T.trait_decl_ref) : expr =
  (* Compute the constraints map *)
  let m = compute_constraints_map params in
  let generics = generic_args_to_pattern ctx c m tr.generics in
  (* Lookup the declaration *)
  let d = T.TraitDeclId.Map.find tr.id ctx.crate.trait_decls in
  EComp
    (name_with_generic_args_to_pattern_aux ctx c d.item_meta.name
       (Some generics))

and ty_to_pattern_aux (ctx : 'fun_body ctx) (c : to_pat_config)
    (m : constraints) (ty : T.ty) : expr =
  match ty with
  | TAdt tref -> (
      let generics = generic_args_to_pattern ctx c m tref.generics in
      match tref.id with
      | TAdtId id ->
          (* Lookup the declaration *)
          let d = T.TypeDeclId.Map.find id ctx.crate.type_decls in
          EComp
            (name_with_generic_args_to_pattern_aux ctx c d.item_meta.name
               (Some generics))
      | TTuple -> EPrimAdt (TTuple, generics)
      | TBuiltin TArray -> EPrimAdt (TArray, generics)
      | TBuiltin TSlice -> EPrimAdt (TSlice, generics)
      | TBuiltin TBox -> EComp [ PIdent ("Box", 0, generics) ]
      | TBuiltin TStr -> EComp [ PIdent ("str", 0, generics) ])
  | TVar v -> EVar (type_var_to_pattern m v)
  | TLiteral lit -> literal_type_to_pattern c lit
  | TRef (r, ty, rk) ->
      ERef
        ( region_to_pattern m r,
          ty_to_pattern_aux ctx c m ty,
          ref_kind_to_pattern rk )
  | TTraitType (trait_ref, type_name) ->
      let name =
        trait_ref_item_with_generics_to_pattern ctx c m trait_ref type_name
          TypesUtils.empty_generic_args
      in
      EComp name
  | TFnPtr binder ->
      (* Push a regions map if necessary - TODO: make this more precise *)
      let m =
        constraints_map_push_regions_map_if_nonempty m binder.binder_regions
      in
      let inputs, output = binder.binder_value in
      let inputs = List.map (ty_to_pattern_aux ctx c m) inputs in
      let output =
        if output = TypesUtils.mk_unit_ty then None
        else Some (ty_to_pattern_aux ctx c m output)
      in
      EArrow (inputs, output)
  | TError _ -> EVar None
  | TRawPtr (ty, RMut) -> ERawPtr (Mut, ty_to_pattern_aux ctx c m ty)
  | TRawPtr (ty, RShared) -> ERawPtr (Not, ty_to_pattern_aux ctx c m ty)
  | _ ->
      let fmt_env = ctx_to_fmt_env ctx in
      raise
        (Failure
           ("Can't convert type to pattern: "
           ^ PrintTypes.ty_to_string fmt_env ty))

and trait_ref_item_with_generics_to_pattern (ctx : 'fun_body ctx)
    (c : to_pat_config) (m : constraints) (trait_ref : T.trait_ref)
    (item_name : string) (item_generics : T.generic_args) : pattern =
  if c.use_trait_decl_refs then
    let trait_decl_ref = trait_ref.trait_decl_ref in
    let d =
      T.TraitDeclId.Map.find trait_decl_ref.binder_value.id
        ctx.crate.trait_decls
    in
    (* Push a regions map if necessary - TODO: make this more precise *)
    let m =
      constraints_map_push_regions_map_if_nonempty m
        trait_decl_ref.binder_regions
    in
    let g =
      generic_args_to_pattern ctx c m trait_decl_ref.binder_value.generics
    in
    let name =
      name_with_generic_args_to_pattern_aux ctx c d.item_meta.name (Some g)
    in
    let item_generics = generic_args_to_pattern ctx c m item_generics in
    let name = name @ [ PIdent (item_name, 0, item_generics) ] in
    name
  else raise (Failure "TODO")

and ty_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (params : T.generic_params) (ty : T.ty) : expr =
  (* Compute the constraints map *)
  let m = compute_constraints_map params in
  (* Convert the type *)
  ty_to_pattern_aux ctx c m ty

and const_generic_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (m : constraints) (cg : T.const_generic) : generic_arg =
  match cg with
  | CgVar v -> GExpr (EVar (const_generic_var_to_pattern m v))
  | CgValue v -> GValue (literal_to_pattern c v)
  | CgGlobal gid ->
      let d = T.GlobalDeclId.Map.find gid ctx.crate.global_decls in
      let n = name_to_pattern_aux ctx c d.item_meta.name in
      GExpr (EComp n)

and generic_args_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (m : constraints) (generics : T.generic_args) : generic_args =
  let ({ regions; types; const_generics; trait_refs = _ } : T.generic_args) =
    generics
  in
  let regions = List.map (region_to_pattern m) regions in
  let types = List.map (ty_to_pattern_aux ctx c m) types in
  let const_generics =
    List.map (const_generic_to_pattern ctx c m) const_generics
  in
  List.concat
    [
      List.map (fun x -> GRegion x) regions;
      List.map (fun x -> GExpr x) types;
      const_generics;
    ]

let name_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config) (n : T.name) :
    pattern =
  (* Convert the name to a pattern *)
  let pat = name_to_pattern_aux ctx c n in
  (* Sanity check: the name should match the pattern *)
  assert (
    c.tgt = TkName
    || match_name ctx
         {
           map_vars_to_vars = true;
           match_with_trait_decl_refs = c.use_trait_decl_refs;
         }
         pat n);
  (* Return *)
  pat

(** We use the [params] to compute proper names for the variables. Note that it
    is safe to provide empty generic parameters. *)
let name_with_generics_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (params : T.generic_params) (n : T.name) (args : T.generic_args) : pattern =
  (* Convert the name to a pattern *)
  let pat =
    let m = compute_constraints_map params in
    let args = generic_args_to_pattern ctx c m args in
    name_with_generic_args_to_pattern_aux ctx c n (Some args)
  in
  (* Sanity check: the name should match the pattern *)
  assert (
    c.tgt = TkName
    || match_name_with_generics ctx
         {
           map_vars_to_vars = true;
           match_with_trait_decl_refs = c.use_trait_decl_refs;
         }
         pat n args);
  (* Return *)
  pat

(** We use the [params] to compute proper names for the variables. Note that it
    is safe to provide empty generic parameters. *)
let fn_ptr_to_pattern (ctx : 'fun_body ctx) (c : to_pat_config)
    (params : T.generic_params) (func : T.fn_ptr) : pattern =
  (* Convert the function pointer to a pattern *)
  let m = compute_constraints_map params in
  let args = generic_args_to_pattern ctx c m func.generics in
  let pat =
    match func.func with
    | FunId (FBuiltin fid) -> (
        match fid with
        | BoxNew ->
            let var = Some (VarName "T") in
            let box_impl =
              [
                PIdent ("alloc", 0, []);
                PIdent ("boxed", 0, []);
                PIdent ("Box", 0, [ GExpr (EVar var) ]);
              ]
            in
            [
              PIdent ("alloc", 0, []);
              PIdent ("boxed", 0, []);
              PImpl (EComp box_impl);
              PIdent ("new", 0, args);
            ]
        | _ ->
            let fid = builtin_fun_id_to_string fid in
            [ PIdent (fid, 0, args) ])
    | FunId (FRegular fid) ->
        let d = Types.FunDeclId.Map.find fid ctx.crate.fun_decls in
        name_with_generic_args_to_pattern_aux ctx c d.item_meta.name (Some args)
    | TraitMethod (tr, method_name, _) ->
        trait_ref_item_with_generics_to_pattern ctx c m tr method_name
          func.generics
  in
  (* Sanity check *)
  log#ldebug
    (lazy
      (let fmt_env = ctx_to_fmt_env ctx in
       "fn_ptr_to_pattern:" ^ "\n- fn_ptr: "
       ^ PrintTypes.fn_ptr_to_string fmt_env func
       ^ "\n- pattern: "
       ^ pattern_to_string { tgt = TkPattern } pat));
  assert (
    c.tgt = TkName
    || match_fn_ptr ctx
         {
           map_vars_to_vars = true;
           match_with_trait_decl_refs = c.use_trait_decl_refs;
         }
         pat func);
  (* Return *)
  pat

(*
 * Check if two patterns are convertible, and compute the common "convertible"
 * suffix.
 *)

type conv_config = {
  equiv : bool;
      (** If [true], check if the patterns are equivalent, otherwise check if
          the first is convertible to the second one *)
}

type inj_map = { m0 : var VarMap.t; m1 : var VarMap.t }

let empty_inj_map = { m0 = VarMap.empty; m1 = VarMap.empty }

type conv_map = { rmap : inj_map; vmap : inj_map }

let empty_conv_map = { rmap = empty_inj_map; vmap = empty_inj_map }

open Result

let ( let* ) o f =
  match o with
  | Error e -> Error e
  | Ok x -> f x

let gen_var_convertible (c : conv_config) (m : inj_map) (v0 : var) (v1 : var) :
    (inj_map, unit) result =
  if c.equiv then
    (* We are checking for equivalence: use the two maps *)
    match (VarMap.find_opt v0 m.m0, VarMap.find_opt v1 m.m1) with
    | None, None ->
        let m0 = VarMap.add v0 v1 m.m0 in
        let m1 = VarMap.add v1 v0 m.m0 in
        Ok { m0; m1 }
    | Some v1', Some v0' -> if v1 = v1' && v0 = v0' then Ok m else Error ()
    | _ -> Error ()
  else
    (* We are only checking for convertibility: only use the first map *)
    match VarMap.find_opt v0 m.m0 with
    | None ->
        let m0 = VarMap.add v0 v1 m.m0 in
        Ok { m with m0 }
    | Some v1' -> if v1 = v1' then Ok m else Error ()

let region_convertible (c : conv_config) (m : conv_map) (r0 : region)
    (r1 : region) : (conv_map, unit) result =
  match (r0, r1) with
  | RStatic, RStatic -> Ok m
  | RVar None, RVar None -> Ok m
  | RVar (Some r0), RVar (Some r1) ->
      let* rmap = gen_var_convertible c m.rmap r0 r1 in
      Ok { m with rmap }
  | _ -> Error ()

let var_convertible (c : conv_config) (m : conv_map) (v0 : var) (v1 : var) :
    (conv_map, unit) result =
  let* vmap = gen_var_convertible c m.vmap v0 v1 in
  Ok { m with vmap }

let opt_var_convertible (c : conv_config) (m : conv_map) (v0 : var option)
    (v1 : var option) : (conv_map, unit) result =
  match (v0, v1) with
  | None, None -> Ok m
  | Some v0, Some v1 -> var_convertible c m v0 v1
  | _ -> Error ()

(** Return the common prefix, and the divergent suffixes.

    The conv map is optional:
    - if [Some] it means we are analyzing an Impl pattern elem
    - if [None] it means we are not inside an Impl pattern elem *)
let rec pattern_common_prefix_aux (c : conv_config) (m : conv_map option)
    (p0 : pattern) (p1 : pattern) :
    pattern * conv_map option * pattern * pattern =
  match (p0, p1) with
  | [], _ | _, [] -> ([], m, p0, p1)
  | e0 :: tp0, e1 :: tp1 -> (
      match pattern_elem_convertible_aux c m e0 e1 with
      | Error _ -> ([], m, p0, p1)
      | Ok m ->
          let pre, m, p0, p1 = pattern_common_prefix_aux c m tp0 tp1 in
          (e0 :: pre, m, p0, p1))

(** We use the result type because otherwise we have options of options, which
    is confusing. *)
and pattern_elem_convertible_aux (c : conv_config) (m : conv_map option)
    (p0 : pattern_elem) (p1 : pattern_elem) : (conv_map option, unit) result =
  match (p0, p1) with
  | PIdent (s0, d0, g0), PIdent (s1, d1, g1) ->
      if s0 = s1 && d0 = d1 then
        match m with
        | None ->
            (* No map: we are not inside an impl block.
               We must check that there are no variables in the elements,
               that is they are convertible with an empty map *)
            let* m = generic_args_convertible_aux c empty_conv_map g0 g1 in
            if m = empty_conv_map then Ok None else Error ()
        | Some m ->
            (* There is a map: we are inside an impl block *)
            let* nm = generic_args_convertible_aux c m g0 g1 in
            Ok (Some nm)
      else Error ()
  | PImpl e0, PImpl e1 ->
      let nm = empty_conv_map in
      let* _ = expr_convertible_aux c nm e0 e1 in
      Ok m
  | _ -> Error ()

and expr_convertible_aux (c : conv_config) (m : conv_map) (e0 : expr)
    (e1 : expr) : (conv_map, unit) result =
  match (e0, e1) with
  | EComp p0, EComp p1 ->
      let _, nm, p0, p1 = pattern_common_prefix_aux c (Some m) p0 p1 in
      if p0 = [] && p1 = [] then Ok (Option.get nm) else Error ()
  | EPrimAdt (a0, g0), EPrimAdt (a1, g1) ->
      if a0 = a1 then generic_args_convertible_aux c m g0 g1 else Error ()
  | ERef (r0, e0, rk0), ERef (r1, e1, rk1) ->
      if rk0 = rk1 then
        let* m = region_convertible c m r0 r1 in
        expr_convertible_aux c m e0 e1
      else Error ()
  | EVar v0, EVar v1 -> opt_var_convertible c m v0 v1
  | EArrow (inputs0, None), EArrow (inputs1, None) ->
      exprl_convertible_aux c m inputs0 inputs1
  | EArrow (inputs0, Some out0), EArrow (inputs1, Some out1) ->
      let* m = exprl_convertible_aux c m inputs0 inputs1 in
      expr_convertible_aux c m out0 out1
  | ERawPtr (mut0, ty0), ERawPtr (mut1, ty1) ->
      if mut0 = mut1 then expr_convertible_aux c m ty0 ty1 else Error ()
  | _ -> Error ()

and exprl_convertible_aux (c : conv_config) (m : conv_map) (e0 : expr list)
    (e1 : expr list) : (conv_map, unit) result =
  match (e0, e1) with
  | [], [] -> Ok m
  | x0 :: e0, x1 :: e1 ->
      let* m = expr_convertible_aux c m x0 x1 in
      exprl_convertible_aux c m e0 e1
  | _ -> Error ()

and generic_args_convertible_aux (c : conv_config) (m : conv_map)
    (g0 : generic_args) (g1 : generic_args) : (conv_map, unit) result =
  match (g0, g1) with
  | [], [] -> Ok m
  | x0 :: g0, x1 :: g1 ->
      let* m = generic_arg_convertible_aux c m x0 x1 in
      generic_args_convertible_aux c m g0 g1
  | _ -> Error ()

and generic_arg_convertible_aux (c : conv_config) (m : conv_map)
    (g0 : generic_arg) (g1 : generic_arg) : (conv_map, unit) result =
  match (g0, g1) with
  | GExpr e0, GExpr e1 -> expr_convertible_aux c m e0 e1
  | GValue lit0, GValue lit1 -> if lit0 = lit1 then Ok m else Error ()
  | GRegion r0, GRegion r1 -> region_convertible c m r0 r1
  | _ -> Error ()

(** Return the common prefix, and the divergent suffixes.

    The conv map is optional:
    - if [Some] it means we are analyzing an Impl pattern elem
    - if [None] it means we are not inside an Impl pattern elem *)
let pattern_common_prefix (c : conv_config) (p0 : pattern) (p1 : pattern) :
    pattern * pattern * pattern =
  let pre, _, p0, p1 = pattern_common_prefix_aux c None p0 p1 in
  (pre, p0, p1)

(** Check if two pattern elements are convertible between each other *)
let pattern_elem_convertible (c : conv_config) (p0 : pattern_elem)
    (p1 : pattern_elem) : bool =
  match pattern_elem_convertible_aux c None p0 p1 with
  | Error _ -> false
  | Ok _ -> true

(*
 * Pattern maps - maps from patterns to values.
 *
 * We implement those as tries.
 *)

module NameMatcherMap = struct
  (* TODO: it would be even better if instead of storing a pattern
     we stored a pattern_elem. The performance would likely be
     better (there would be less backtracking in find_opt) and
     the implementation would be simpler. *)
  type 'a t =
    | Node of 'a option * (pattern * 'a t) list
        (** Nodes are branchings. We do not even attempt to order the branches
            to minimize the number of comparisons - we could do this in later
            updates.

            A node holds a value. All the children patterns must be non-empty,
            and their common prefixes must be pairwise empty. *)

  let empty : 'a t = Node (None, [])

  let rec replace (np : pattern) (nv : 'a) (m : 'a t) : 'a t * 'a option =
    let (Node (node_v, children)) = m in
    (* If the path is empty: stop there *)
    if np = [] then (Node (Some nv, children), node_v)
      (* Otherwise: explore the children *)
    else
      let children, replaced = replace_in_children np nv children in
      (Node (node_v, children), replaced)

  and replace_in_children (np : pattern) (nv : 'a)
      (children : (pattern * 'a t) list) : (pattern * 'a t) list * 'a option =
    let c = { equiv = true } in
    (* The patterns used in the children should have been selected
       such that their common prefixes are pairwise empty.
       We thus just need to check each pattern: if there is one which
       has a non-empty prefix with np, we insert a node there. Otherwise
       we insert a new child at the end.
    *)
    match children with
    | [] ->
        (* We reached the end without finding a pattern which has a non-empty
           prefix with the current children patterns: we simply insert a new child. *)
        ([ (np, Node (Some nv, [])) ], None)
    | (child_pat, child_tree) :: children_tl ->
        (* Check if there is a common prefix *)
        let pre, np_end, child_pat_end = pattern_common_prefix c np child_pat in
        if pre = [] then
          (* Empty prefix: continue *)
          let children_tl, replaced = replace_in_children np nv children_tl in
          ((child_pat, child_tree) :: children_tl, replaced)
        else
          (* Non-empty prefix: insert here *)
          let (nchild, replaced) : (pattern * 'a t) * 'a option =
            match child_pat_end with
            | [] ->
                (* The child path is a prefix of the current path: insert
                   in the child *)
                let child_tree, replaced = replace np_end nv child_tree in
                ((child_pat, child_tree), replaced)
            | _ ->
                (* The child path is not a prefix of the current path:
                   insert a branching.
                   Check if the current path is a prefix of the child path *)
                if np_end = [] then
                  (* Prefix *)
                  ((pre, Node (Some nv, [ (child_pat_end, child_tree) ])), None)
                else
                  (* Not a prefix *)
                  ( ( pre,
                      Node
                        ( None,
                          [
                            (child_pat_end, child_tree);
                            (np_end, Node (Some nv, []));
                          ] ) ),
                    None )
          in
          (nchild :: children_tl, replaced)

  let add (np : pattern) (nv : 'a) (m : 'a t) : 'a t =
    let nm, replaced = replace np nv m in
    (* Because of the way we currently use patterns, we should never override
       a binding: we thuse check it doesn't happen *)
    assert (replaced = None);
    nm

  let match_name_with_generics_prefix (ctx : 'fun_body ctx) (c : match_config)
      (p : pattern) (n : T.name) (g : T.generic_args) :
      (T.name * T.generic_args) option =
    if List.length p = List.length n then
      if match_name_with_generics ctx c p n g then
        Some ([], TypesUtils.empty_generic_args)
      else None
    else if List.length p < List.length n then
      let npre, nend = Collections.List.split_at n (List.length p) in
      if match_name ctx c p npre then Some (nend, g) else None
    else None

  let rec find_with_generics_opt (ctx : 'fun_body ctx) (c : match_config)
      (name : Types.name) (g : Types.generic_args) (m : 'a t) : 'a option =
    let (Node (node_v, children)) = m in
    (* Check if we reached the destination *)
    match name with
    | [] | [ PeMonomorphized _ ] ->
        if g = TypesUtils.empty_generic_args then node_v else None
    | _ ->
        (* Explore the children *)
        find_with_generics_in_children_opt ctx c name g children

  and find_with_generics_in_children_opt (ctx : 'fun_body ctx)
      (c : match_config) (name : Types.name) (g : Types.generic_args)
      (children : (pattern * 'a t) list) : 'a option =
    match children with
    | [] -> None
    | (child_pat, child_tree) :: children -> (
        (* Check if the pattern matches a prefix of the name *)
        match match_name_with_generics_prefix ctx c child_pat name g with
        | None ->
            (* No match: continue.

               Note that because the children patterns are all non-empty and
               pairwise disjoint, there is no point in exploring the other
               children.
            *)
            find_with_generics_in_children_opt ctx c name g children
        | Some (nend, g) ->
            (* Dive into the child *)
            find_with_generics_opt ctx c nend g child_tree)

  let find_opt (ctx : 'fun_body ctx) (c : match_config) (name : Types.name)
      (m : 'a t) : 'a option =
    find_with_generics_opt ctx c name TypesUtils.empty_generic_args m

  let mem (ctx : 'fun_body ctx) (c : match_config) (name : Types.name)
      (m : 'a t) : bool =
    find_opt ctx c name m <> None

  let of_list (ls : (pattern * 'a) list) : 'a t =
    List.fold_left (fun m (pat, v) -> add pat v m) empty ls

  let rec to_string_aux (current_indent : string) (indent : string)
      (v_to_string : 'a -> string) (m : 'a t) : string =
    let (Node (opt_v, children)) = m in
    let opt_v =
      current_indent ^ PrintUtils.option_to_string v_to_string opt_v
    in
    let children =
      String.concat "\n"
        (List.map (child_to_string current_indent indent v_to_string) children)
    in
    opt_v ^ " [\n" ^ children ^ "\n" ^ current_indent ^ "]"

  and child_to_string (current_indent : string) (indent : string)
      (v_to_string : 'a -> string) (child_pat, child) : string =
    let c : print_config = { tgt = TkPattern } in
    current_indent
    ^ pattern_to_string c child_pat
    ^ " ->\n"
    ^ to_string_aux (current_indent ^ indent) indent v_to_string child

  let to_string (v_to_string : 'a -> string) (m : 'a t) : string =
    to_string_aux "" "  " v_to_string m
end
