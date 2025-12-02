open Identifiers
open Meta
open Values
include Generated_Types

type region_db_var = region_id de_bruijn_var [@@deriving show]
type type_db_var = type_var_id de_bruijn_var [@@deriving show]
type const_generic_db_var = const_generic_var_id de_bruijn_var [@@deriving show]
type trait_db_var = trait_clause_id de_bruijn_var [@@deriving show]

let all_signed_int_types = [ Isize; I8; I16; I32; I64; I128 ]
let all_unsigned_int_types = [ Usize; U8; U16; U32; U64; U128 ]

let all_int_types =
  List.append
    (List.map (fun i -> Signed i) all_signed_int_types)
    (List.map (fun u -> Unsigned u) all_unsigned_int_types)

(** The variant id for [Option::None] *)
let option_none_id = VariantId.of_int 0

(** The variant id for [Option::Some] *)
let option_some_id = VariantId.of_int 1

module RegionGroupId = IdGen ()

type region_group_id = RegionGroupId.id [@@deriving show, ord]

(** A group of regions.

    Results from a lifetime analysis: we group the regions with the same
    lifetime together, and compute the hierarchy between the regions. This is
    necessary to introduce the proper abstraction with the proper constraints,
    when evaluating a function call in symbolic mode. *)
type ('rid, 'id) g_region_group = {
  id : 'id;
  regions : 'rid list;
  parents : 'id list;
}
[@@deriving show]

type region_var_group = (RegionId.id, RegionGroupId.id) g_region_group
[@@deriving show]

type region_var_groups = region_var_group list [@@deriving show]

(** Type with erased regions (this only has an informative purpose) *)
type ety = ty

(** Type with non-erased regions (this only has an informative purpose) *)
and rty = ty [@@deriving show, ord]

and region_outlives = (region, region) outlives_pred
and type_outlives = (ty, region) outlives_pred
