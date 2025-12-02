
(** WARNING: this file is partially auto-generated. Do not edit `Types.ml`
    by hand. Edit `Types.template.ml` instead, or improve the code
    generation tool so avoid the need for hand-writing things.

    `Types.template.ml` contains the manual definitions and some `(*
    __REPLACEn__ *)` comments. These comments are replaced by auto-generated
    definitions by running `make generate-ml` in the crate root. The
    code-generation code is in `charon/src/bin/generate-ml`.
 *)

open Identifiers
open Meta
open Values

module TypeVarId = IdGen ()
module TypeDeclId = IdGen ()
module VariantId = IdGen ()
module FieldId = IdGen ()
module GlobalDeclId = IdGen ()
module ConstGenericVarId = IdGen ()
module TraitDeclId = IdGen ()
module TraitImplId = IdGen ()
module TraitClauseId = IdGen ()
module TraitTypeConstraintId = IdGen ()
module UnsolvedTraitId = IdGen ()
module RegionId = IdGen ()
module Disambiguator = IdGen ()
module FunDeclId = IdGen ()
module BodyId = IdGen ()

type ('id, 'x) vector = 'x list [@@deriving show, ord, eq]

type integer_type = Values.integer_type [@@deriving show, ord, eq]
type float_type = Values.float_type [@@deriving show, ord, eq]
type literal_type = Values.literal_type [@@deriving show, ord, eq]

(* Manually implemented because no type uses it (we use plain lists instead of
   vectors in generic_params), which causes visitor inference problems if we
   declare it within a visitor group. *)
type trait_type_constraint_id = TraitTypeConstraintId.id [@@deriving show, ord, eq]

(** We define these types to control the name of the visitor functions *)
type ('id, 'name) indexed_var = {
  index : 'id;  (** Unique index identifying the variable *)
  name : 'name;  (** Variable name *)
}
[@@deriving show, ord, eq]

(* __REPLACE0__ *)

(** Ancestor for iter visitor for {!type: Types.ty} *)
class ['self] iter_ty_base_base =
  object (self : 'self)
    inherit [_] iter_const_generic

    method visit_indexed_var
        : 'id 'name.
          ('env -> 'id -> unit) ->
          ('env -> 'name -> unit) ->
          'env ->
          ('id, 'name) indexed_var ->
          unit =
      fun visit_index visit_name env x ->
        let { index; name } = x in
        visit_index env index;
        visit_name env name
  end

(** Ancestor for map visitor for {!type: Types.ty} *)
class virtual ['self] map_ty_base_base =
  object (self : 'self)
    inherit [_] map_const_generic

    method visit_indexed_var
        : 'id 'name.
          ('env -> 'id -> 'id) ->
          ('env -> 'name -> 'name) ->
          'env ->
          ('id, 'name) indexed_var ->
          ('id, 'name) indexed_var =
      fun visit_index visit_name env x ->
        let { index; name } = x in
        let index = visit_index env index in
        let name = visit_name env name in
        { index; name }
  end

(* __REPLACE1__ *)

(* __REPLACE2__ *)
