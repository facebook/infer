(** WARNING: this file is partially auto-generated. Do not edit `Types.ml` by
    hand. Edit `Types.template.ml` instead, or improve the code generation tool
    so avoid the need for hand-writing things.

    `Types.template.ml` contains the manual definitions and some `(*
    __REPLACEn__ *)` comments. These comments are replaced by auto-generated
    definitions by running `make generate-ml` in the crate root. The
    code-generation code is in `charon/src/bin/generate-ml`. *)

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
type trait_type_constraint_id = TraitTypeConstraintId.id
[@@deriving show, ord, eq]

(** We define these types to control the name of the visitor functions *)
type ('id, 'name) indexed_var = {
  index : 'id;  (** Unique index identifying the variable *)
  name : 'name;  (** Variable name *)
}
[@@deriving show, ord, eq]

(** The id of a translated item. *)
type any_decl_id =
  | IdType of type_decl_id
  | IdFun of fun_decl_id
  | IdGlobal of global_decl_id
  | IdTraitDecl of trait_decl_id
  | IdTraitImpl of trait_impl_id

(** Const Generic Values. Either a primitive value, or a variable corresponding
    to a primitve value *)
and const_generic =
  | CgGlobal of global_decl_id  (** A global constant *)
  | CgVar of const_generic_var_id de_bruijn_var  (** A const generic variable *)
  | CgValue of literal  (** A concrete value *)

and const_generic_var_id = (ConstGenericVarId.id[@visitors.opaque])

(** The index of a binder, counting from the innermost. See [[DeBruijnVar]] for
    details. *)
and de_bruijn_id = int

(** Type-level variable.

    Variables are bound in groups. Each item has a top-level binding group in
    its [generic_params] field, and then inner binders are possible using the
    [RegionBinder<T>] and [Binder<T>] types. Each variable is linked to exactly
    one binder. The [Id] then identifies the specific variable among all those
    bound in that group.

    For instance, we have the following:
    {@rust[
      fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
           ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
             |       inner binder  |       |     inner binder  |        |        |
       top-level binder            |       |                   |        |        |
                             Bound(1, b)   |              Bound(2, b)   |     Bound(0, d)
                                           |                            |
                                       Bound(0, c)                 Bound(1, c)
    ]}

    To make consumption easier for projects that don't do heavy substitution, a
    micro-pass at the end changes the variables bound at the top-level (i.e. in
    the [GenericParams] of items) to be [Free]. This is an optional pass, we may
    add a flag to deactivate it. The example above becomes:
    {@rust[
      fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
           ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
             |       inner binder  |       |     inner binder  |        |        |
       top-level binder            |       |                   |        |        |
                                Free(b)    |                Free(b)     |     Bound(0, d)
                                           |                            |
                                       Bound(0, c)                 Bound(1, c)
    ]}

    At the moment only region variables can be bound in a non-top-level binder.
*)
and 'a0 de_bruijn_var =
  | Bound of de_bruijn_id * 'a0
      (** A variable attached to the nth binder, counting from the innermost. *)
  | Free of 'a0
      (** A variable attached to the outermost binder (the one on the item). As
          explained above, This is not used in charon internals, only as a
          micro-pass before exporting the crate data. *)

and fun_decl_id = (FunDeclId.id[@visitors.opaque])
and global_decl_id = (GlobalDeclId.id[@visitors.opaque])
and trait_clause_id = (TraitClauseId.id[@visitors.opaque])
and trait_decl_id = (TraitDeclId.id[@visitors.opaque])
and trait_impl_id = (TraitImplId.id[@visitors.opaque])
and type_decl_id = (TypeDeclId.id[@visitors.opaque])

and type_var_id = (TypeVarId.id[@visitors.opaque])
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_const_generic";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_const_generic";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "reduce_const_generic";
      monomorphic = [ "env" ];
      variety = "reduce";
      ancestors = [ "reduce_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "mapreduce_const_generic";
      monomorphic = [ "env" ];
      variety = "mapreduce";
      ancestors = [ "mapreduce_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(** Ancestor for iter visitor for {!type: Types.ty} *)
class ['self] iter_ty_base_base =
  object (self : 'self)
    inherit [_] iter_const_generic

    method visit_indexed_var :
        'id 'name.
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

    method visit_indexed_var :
        'id 'name.
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

(* Ancestors for the ty visitors *)
class ['self] iter_ty_base =
  object (self : 'self)
    inherit [_] iter_ty_base_base
    method visit_span : 'env -> span -> unit = fun _ _ -> ()
  end

class ['self] map_ty_base =
  object (self : 'self)
    inherit [_] map_ty_base_base
    method visit_span : 'env -> span -> span = fun _ x -> x
  end

(** A value of type [T] bound by generic parameters. Used in any context where
    we're adding generic parameters that aren't on the top-level item, e.g.
    [for<'a>] clauses (uses [RegionBinder] for now), trait methods, GATs (TODO).
*)
type 'a0 binder = {
  binder_params : generic_params;
  binder_value : 'a0;
      (** Named this way to highlight accesses to the inner value that might be
          handling parameters incorrectly. Prefer using helper methods. *)
}

and binder_kind =
  | BKTraitMethod of trait_decl_id * trait_item_name
      (** The parameters of a trait method. Used in the [methods] lists in trait
          decls and trait impls. *)
  | BKInherentImplBlock
      (** The parameters bound in a non-trait [impl] block. Used in the [Name]s
          of inherent methods. *)
  | BKDyn  (** Binder used for [dyn Trait] existential predicates. *)
  | BKOther  (** Some other use of a binder outside the main Charon ast. *)

(** An built-in function identifier, identifying a function coming from a
    standard library. *)
and builtin_fun_id =
  | BoxNew  (** [alloc::boxed::Box::new] *)
  | ArrayToSliceShared
      (** Cast an array as a slice.

          Converted from [UnOp::ArrayToSlice] *)
  | ArrayToSliceMut
      (** Cast an array as a slice.

          Converted from [UnOp::ArrayToSlice] *)
  | ArrayRepeat
      (** [repeat(n, x)] returns an array where [x] has been replicated [n]
          times.

          We introduce this when desugaring the [ArrayRepeat] rvalue. *)
  | Index of builtin_index_op
      (** Converted from indexing [ProjectionElem]s. The signature depends on
          the parameters. It could look like:
          - [fn ArrayIndexShared<T,N>(&[T;N], usize) -> &T]
          - [fn SliceIndexShared<T>(&[T], usize) -> &T]
          - [fn ArraySubSliceShared<T,N>(&[T;N], usize, usize) -> &[T]]
          - [fn SliceSubSliceMut<T>(&mut [T], usize, usize) -> &mut [T]]
          - etc *)
  | PtrFromParts of ref_kind
      (** Build a raw pointer, from a data pointer and metadata. The metadata
          can be unit, if building a thin pointer.

          Converted from [AggregateKind::RawPtr] *)

(** One of 8 built-in indexing operations. *)
and builtin_index_op = {
  is_array : bool;  (** Whether this is a slice or array. *)
  mutability : ref_kind;
      (** Whether we're indexing mutably or not. Determines the type ofreference
          of the input and output. *)
  is_range : bool;
      (** Whether we're indexing a single element or a subrange. If [true], the
          function takes two indices and the output is a slice; otherwise, the
          function take one index and the output is a reference to a single
          element. *)
}

(** Builtin types identifiers.

    WARNING: for now, all the built-in types are covariant in the generic
    parameters (if there are). Adding types which don't satisfy this will
    require to update the code abstracting the signatures (to properly take into
    account the lifetime constraints).

    TODO: update to not hardcode the types (except [Box] maybe) and be more
    modular. TODO: move to builtins.rs? *)
and builtin_ty =
  | TBox  (** Boxes are de facto a primitive type. *)
  | TArray  (** Primitive type *)
  | TSlice  (** Primitive type *)
  | TStr  (** Primitive type *)

(** A const generic variable in a signature or binder. *)
and const_generic_var = {
  index : const_generic_var_id;
      (** Index identifying the variable among other variables bound at the same
          level. *)
  name : string;  (** Const generic name *)
  ty : literal_type;  (** Type of the const generic *)
}

(** The contents of a [dyn Trait] type. *)
and dyn_predicate = {
  binder : ty binder;
      (** This binder binds a single type [T], which is considered existentially
          quantified. The predicates in the binder apply to [T] and represent
          the [dyn Trait] constraints. E.g. [dyn Iterator<Item=u32> + Send] is
          represented as [exists<T: Iterator<Item=u32> + Send> T].

          Only the first trait clause may have methods. We use the vtable of
          this trait in the [dyn Trait] pointer metadata. *)
}

and fn_ptr = { func : fun_id_or_trait_method_ref; generics : generic_args }

(** Reference to a function declaration. *)
and fun_decl_ref = {
  id : fun_decl_id;
  generics : generic_args;  (** Generic arguments passed to the function. *)
}

(** A function identifier. See [crate::ullbc_ast::Terminator] *)
and fun_id =
  | FRegular of fun_decl_id
      (** A "regular" function (function local to the crate, external function
          not treated as a primitive one). *)
  | FBuiltin of builtin_fun_id
      (** A primitive function, coming from a standard library (for instance:
          [alloc::boxed::Box::new]). TODO: rename to "Primitive" *)

and fun_id_or_trait_method_ref =
  | FunId of fun_id
  | TraitMethod of trait_ref * trait_item_name * fun_decl_id
      (** If a trait: the reference to the trait and the id of the trait method.
          The fun decl id is not really necessary - we put it here for
          convenience purposes. *)

(** A set of generic arguments. *)
and generic_args = {
  regions : region list;
  types : ty list;
  const_generics : const_generic list;
  trait_refs : trait_ref list;
}

(** Generic parameters for a declaration. We group the generics which come from
    the Rust compiler substitutions (the regions, types and const generics) as
    well as the trait clauses. The reason is that we consider that those are
    parameters that need to be filled. We group in a different place the
    predicates which are not trait clauses, because those enforce constraints
    but do not need to be filled with witnesses/instances. *)
and generic_params = {
  regions : region_var list;
  types : type_var list;
  const_generics : const_generic_var list;
  trait_clauses : trait_clause list;
  regions_outlive : (region, region) outlives_pred region_binder list;
      (** The first region in the pair outlives the second region *)
  types_outlive : (ty, region) outlives_pred region_binder list;
      (** The type outlives the region *)
  trait_type_constraints : trait_type_constraint region_binder list;
      (** Constraints over trait associated types *)
}

(** Reference to a global declaration. *)
and global_decl_ref = { id : global_decl_id; generics : generic_args }

(** .0 outlives .1 *)
and ('a0, 'a1) outlives_pred = 'a0 * 'a1

and ref_kind = RMut | RShared

and region =
  | RVar of region_id de_bruijn_var
      (** Region variable. See [DeBruijnVar] for details. *)
  | RStatic  (** Static region *)
  | RErased  (** Erased region *)

(** A value of type [T] bound by regions. We should use [binder] instead but
    this causes name clash issues in the derived ocaml visitors. TODO: merge
    with [binder] *)
and 'a0 region_binder = {
  binder_regions : region_var list;
  binder_value : 'a0;
      (** Named this way to highlight accesses to the inner value that might be
          handling parameters incorrectly. Prefer using helper methods. *)
}

and region_id = (RegionId.id[@visitors.opaque])

(** A region variable in a signature or binder. *)
and region_var = (region_id, string option) indexed_var

(** A trait predicate in a signature, of the form [Type: Trait<Args>]. This
    functions like a variable binder, to which variables of the form
    [TraitRefKind::Clause] can refer to. *)
and trait_clause = {
  clause_id : trait_clause_id;
      (** Index identifying the clause among other clauses bound at the same
          level. *)
  span : span option;
  trait : trait_decl_ref region_binder;  (** The trait that is implemented. *)
}

(** A predicate of the form [Type: Trait<Args>].

    About the generics, if we write:
    {@rust[
      impl Foo<bool> for String { ... }
    ]}

    The substitution is: [[String, bool]]. *)
and trait_decl_ref = { id : trait_decl_id; generics : generic_args }

(** A reference to a tait impl, using the provided arguments. *)
and trait_impl_ref = { id : trait_impl_id; generics : generic_args }

and trait_item_name = string

(** A reference to a trait *)
and trait_ref = {
  trait_id : trait_instance_id;
  trait_decl_ref : trait_decl_ref region_binder;
      (** Not necessary, but useful *)
}

(** Identifier of a trait instance. This is derived from the trait resolution.

    Should be read as a path inside the trait clauses which apply to the current
    definition. Note that every path designated by [TraitInstanceId] refers to a
    *trait instance*, which is why the [[TraitRefKind::Clause]] variant may seem
    redundant with some of the other variants. *)
and trait_instance_id =
  | TraitImpl of trait_impl_ref
      (** A specific top-level implementation item. *)
  | Clause of trait_clause_id de_bruijn_var
      (** One of the local clauses.

          Example:
          {@rust[
            fn f<T>(...) where T : Foo
                               ^^^^^^^
                               Clause(0)
          ]} *)
  | ParentClause of trait_ref * trait_clause_id
      (** A parent clause

          Example:
          {@rust[
            trait Foo1 {}
            trait Foo2 { fn f(); }

            trait Bar : Foo1 + Foo2 {}
                        ^^^^   ^^^^
                               parent clause 1
                parent clause 0

            fn g<T : Bar>(x : T) {
              x.f()
              ^^^^^
              Parent(Clause(0), 1)::f(x)
                                ^
                                parent clause 1 of clause 0
            }
          ]} *)
  | Self
      (** The implicit [Self: Trait] clause. Present inside trait declarations,
          including trait method declarations. Not present in trait
          implementations as we can use [TraitImpl] intead. *)
  | BuiltinOrAuto of
      trait_decl_ref region_binder
      * trait_ref list
      * (trait_item_name * ty * trait_ref list) list
      (** A trait implementation that is computed by the compiler, such as for
          built-in trait [Sized]. This morally points to an invisible [impl]
          block; as such it contains the information we may need from one.

          Fields:
          - [trait_decl_ref]
          - [parent_trait_refs]: Exactly like the same field on [TraitImpl]: the
            [TraitRef]s required to satisfy the implied predicates on the trait
            declaration. E.g. since [FnMut: FnOnce], a built-in [T: FnMut] impl
            would have a [TraitRef] for [T: FnOnce].
          - [types]: The values of the associated types for this trait. *)
  | Dyn of trait_decl_ref region_binder
      (** The automatically-generated implementation for [dyn Trait]. *)
  | UnknownTrait of string  (** For error reporting. *)

(** A constraint over a trait associated type.

    Example:
    {@rust[
      T : Foo<S = String>
              ^^^^^^^^^^
    ]} *)
and trait_type_constraint = {
  trait_ref : trait_ref;
  type_name : trait_item_name;
  ty : ty;
}

and ty =
  | TAdt of type_decl_ref
      (** An ADT. Note that here ADTs are very general. They can be:
          - user-defined ADTs
          - tuples (including [unit], which is a 0-tuple)
          - built-in types (includes some primitive types, e.g., arrays or
            slices) The information on the nature of the ADT is stored in
            ([TypeId])[TypeId]. The last list is used encode const generics,
            e.g., the size of an array

          Note: this is incorrectly named: this can refer to any valid
          [TypeDecl] including extern types. *)
  | TVar of type_var_id de_bruijn_var
  | TLiteral of literal_type
  | TNever
      (** The never type, for computations which don't return. It is sometimes
          necessary for intermediate variables. For instance, if we do (coming
          from the rust documentation):
          {@rust[
            let num: u32 = match get_a_number() {
                Some(num) => num,
                None => break,
            };
          ]}
          the second branch will have type [Never]. Also note that [Never] can
          be coerced to any type.

          Note that we eliminate the variables which have this type in a
          micro-pass. As statements don't have types, this type disappears
          eventually disappears from the AST. *)
  | TRef of region * ty * ref_kind  (** A borrow *)
  | TRawPtr of ty * ref_kind  (** A raw pointer. *)
  | TTraitType of trait_ref * trait_item_name
      (** A trait associated type

          Ex.:
          {@rust[
            trait Foo {
              type Bar; // type associated to the trait Foo
            }
          ]} *)
  | TDynTrait of dyn_predicate  (** [dyn Trait] *)
  | TFnPtr of (ty list * ty) region_binder
      (** Function pointer type. This is a literal pointer to a region of memory
          that contains a callable function. This is a function signature with
          limited generics: it only supports lifetime generics, not other kinds
          of generics. *)
  | TFnDef of fn_ptr region_binder
      (** The unique type associated with each function item. Each function item
          is given a unique generic type that takes as input the function's
          early-bound generics. This type is not generally nameable in Rust;
          it's a ZST (there's a unique value), and a value of that type can be
          cast to a function pointer or passed to functions that expect
          [FnOnce]/[FnMut]/[Fn] parameters. There's a binder here because charon
          function items take both early and late-bound lifetimes as arguments;
          given that the type here is polymorpohic in the late-bound variables
          (those that could appear in a function pointer type like
          [for<'a> fn(&'a u32)]), we need to bind them here. *)
  | TError of string  (** A type that could not be computed or was incorrect. *)

(** Reference to a type declaration or builtin type. *)
and type_decl_ref = { id : type_id; generics : generic_args }

(** Type identifier.

    Allows us to factorize the code for built-in types, adts and tuples *)
and type_id =
  | TAdtId of type_decl_id
      (** A "regular" ADT type.

          Includes transparent ADTs and opaque ADTs (local ADTs marked as
          opaque, and external ADTs). *)
  | TTuple
  | TBuiltin of builtin_ty
      (** Built-in type. Either a primitive type like array or slice, or a
          non-primitive type coming from a standard library and that we handle
          like a primitive type. Types falling into this category include: Box,
          Vec, Cell... The Array and Slice types were initially modelled as
          primitive in the [Ty] type. We decided to move them to built-in types
          as it allows for more uniform treatment throughout the codebase. *)

(** A type variable in a signature or binder. *)
and type_var = (type_var_id, string) indexed_var
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_ty";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_ty_base" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_ty";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_ty_base" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(* Ancestors for the type_decl visitors *)
class ['self] iter_type_decl_base =
  object (self : 'self)
    inherit [_] iter_ty
    method visit_attr_info : 'env -> attr_info -> unit = fun _ _ -> ()
  end

class ['self] map_type_decl_base =
  object (self : 'self)
    inherit [_] map_ty
    method visit_attr_info : 'env -> attr_info -> attr_info = fun _ x -> x
  end

(** (U)LLBC is a language with side-effects: a statement may abort in a way that
    isn't tracked by control-flow. The two kinds of abort are:
    - Panic (may unwind or not depending on compilation setting);
    - Undefined behavior: *)
type abort_kind =
  | Panic of name option  (** A built-in panicking function. *)
  | UndefinedBehavior  (** Undefined behavior in the rust abstract machine. *)
  | UnwindTerminate
      (** Unwind had to stop for Abi reasons or because cleanup code panicked
          again. *)

(** Additional information for closures. *)
and closure_info = {
  kind : closure_kind;
  fn_once_impl : trait_impl_ref region_binder;
      (** The [FnOnce] implementation of this closure -- always exists. *)
  fn_mut_impl : trait_impl_ref region_binder option;
      (** The [FnMut] implementation of this closure, if any. *)
  fn_impl : trait_impl_ref region_binder option;
      (** The [Fn] implementation of this closure, if any. *)
  signature : (ty list * ty) region_binder;
      (** The signature of the function that this closure represents. *)
}

and closure_kind = Fn | FnMut | FnOnce
and disambiguator = (Disambiguator.id[@visitors.opaque])

(** Layout of the discriminant. Describes the offset of the discriminant field
    as well as its encoding as [tag] in memory. *)
and discriminant_layout = {
  offset : int;  (** The offset of the discriminant in bytes. *)
  tag_ty : integer_type;  (** The representation type of the discriminant. *)
  encoding : tag_encoding;  (** How the tag is encoding in memory. *)
}

and field = {
  span : span;
  attr_info : attr_info;
  field_name : string option;
  field_ty : ty;
}

and field_id = (FieldId.id[@visitors.opaque])

(** There are two kinds of [impl] blocks:
    {ul
     {- impl blocks linked to a type ("inherent" impl blocks following Rust
        terminology):
        {@rust[
          impl<T> List<T> { ...}
        ]}
     }
     {- trait impl blocks:
        {@rust[
          impl<T> PartialEq for List<T> { ...}
        ]}
        We distinguish the two.
     }
    } *)
and impl_elem = ImplElemTy of ty binder | ImplElemTrait of trait_impl_id

(** Item kind: whether this function/const is part of a trait declaration, trait
    implementation, or neither.

    Example:
    {@rust[
      trait Foo {
          fn bar(x : u32) -> u32; // trait item decl without default

          fn baz(x : bool) -> bool { x } // trait item decl with default
      }

      impl Foo for ... {
          fn bar(x : u32) -> u32 { x } // trait item implementation
      }

      fn test(...) { ... } // regular

      impl Type {
          fn test(...) { ... } // regular
      }
    ]} *)
and item_kind =
  | TopLevelItem  (** This item stands on its own. *)
  | ClosureItem of closure_info
      (** This is a closure in a function body.

          Fields:
          - [info] *)
  | TraitDeclItem of trait_decl_ref * trait_item_name * bool
      (** This is an associated item in a trait declaration. It has a body if
          and only if the trait provided a default implementation.

          Fields:
          - [trait_ref]: The trait declaration this item belongs to.
          - [item_name]: The name of the item.
          - [has_default]: Whether the trait declaration provides a default
            implementation. *)
  | TraitImplItem of trait_impl_ref * trait_decl_ref * trait_item_name * bool
      (** This is an associated item in a trait implementation.

          Fields:
          - [impl_ref]: The trait implementation the method belongs to.
          - [trait_ref]: The trait declaration that the impl block implements.
          - [item_name]: The name of the item
          - [reuses_default]: True if the trait decl had a default
            implementation for this function/const and this item is a copy of
            the default item. *)
  | VTableTyItem of dyn_predicate
      (** This is a vtable struct for a trait.

          Fields:
          - [dyn_predicate]: The [dyn Trait] predicate implemented by this
            vtable. *)
  | VTableInstanceItem of trait_impl_ref
      (** This is a vtable value for an impl.

          Fields:
          - [impl_ref] *)

(** Meta information about an item (function, trait decl, trait impl, type decl,
    global). *)
and item_meta = {
  name : name;
  span : span;
  source_text : string option;
      (** The source code that corresponds to this item. *)
  attr_info : attr_info;  (** Attributes and visibility. *)
  is_local : bool;
      (** [true] if the type decl is a local type decl, [false] if it comes from
          an external crate. *)
  lang_item : string option;
      (** If the item is built-in, record its internal builtin identifier. *)
}

(** Simplified type layout information.

    Does not include information about niches. If the type does not have a fully
    known layout (e.g. it is ?Sized) some of the layout parts are not available.
*)
and layout = {
  size : int option;  (** The size of the type in bytes. *)
  align : int option;  (** The alignment, in bytes. *)
  discriminant_layout : discriminant_layout option;
      (** The discriminant's layout, if any. Only relevant for types with
          multiple variants. *)
  uninhabited : bool;
      (** Whether the type is uninhabited, i.e. has any valid value at all. Note
          that uninhabited types can have arbitrary layouts: [(u32, !)] has
          space for the [u32] and [enum E2 { A, B(!), C(i32, !) }] may have
          space for a discriminant. *)
  variant_layouts : variant_layout list;
      (** Map from [VariantId] to the corresponding field layouts. Structs are
          modeled as having exactly one variant, unions as having no variant. *)
}

(** An item name/path

    A name really is a list of strings. However, we sometimes need to introduce
    unique indices to disambiguate. This mostly happens because of "impl"
    blocks:
    {@rust[
      impl<T> List<T> {
        ...
      }
    ]}

    A type in Rust can have several "impl" blocks, and those blocks can contain
    items with similar names. For this reason, we need to disambiguate them with
    unique indices. Rustc calls those "disambiguators". In rustc, this gives
    names like this:
    - [betree_main::betree::NodeIdCounter{impl#0}::new]
    - note that impl blocks can be nested, and macros sometimes generate weird
      names (which require disambiguation):
      [betree_main::betree_utils::_#1::{impl#0}::deserialize::{impl#0}]

    Finally, the paths used by rustc are a lot more precise and explicit than
    those we expose in LLBC: for instance, every identifier belongs to a
    specific namespace (value namespace, type namespace, etc.), and is coupled
    with a disambiguator.

    On our side, we want to stay high-level and simple: we use string
    identifiers as much as possible, insert disambiguators only when necessary
    (whenever we find an "impl" block, typically) and check that the
    disambiguator is useless in the other situations (i.e., the disambiguator is
    always equal to 0).

    Moreover, the items are uniquely disambiguated by their (integer) ids
    ([TypeDeclId], etc.), and when extracting the code we have to deal with name
    clashes anyway. Still, we might want to be more precise in the future.

    Also note that the first path element in the name is always the crate name.
*)
and name = (path_elem list[@visitors.opaque])

(** See the comments for [Name] *)
and path_elem =
  | PeIdent of string * disambiguator
  | PeImpl of impl_elem
  | PeMonomorphized of generic_args
      (** This item was obtained by monomorphizing its parent with the given
          args. *)

(** The metadata stored in a pointer. That's the information stored in pointers
    alongside their address. It's empty for [Sized] types, and interesting for
    unsized aka dynamically-sized types. *)
and ptr_metadata =
  | NoMetadata  (** Types that need no metadata, namely [T: Sized] types. *)
  | Length
      (** Metadata for [[T]], [str], and user-defined types that directly or
          indirectly contain one of these two. *)
  | VTable of v_table
      (** Metadata for [dyn Trait] and user-defined types that directly or
          indirectly contain a [dyn Trait]. *)

(** Describes how we represent the active enum variant in memory. *)
and tag_encoding =
  | Direct
      (** Represents the direct encoding of the discriminant as the tag via
          integer casts. *)
  | Niche of variant_id
      (** Represents the encoding of the discriminant in the niche of variant
          [untagged_variant].

          Fields:
          - [untagged_variant] *)

(** A type declaration.

    Types can be opaque or transparent.

    Transparent types are local types not marked as opaque. Opaque types are the
    others: local types marked as opaque, and non-local types (coming from
    external dependencies).

    In case the type is transparent, the declaration also contains the type
    definition (see [TypeDeclKind]).

    A type can only be an ADT (structure or enumeration), as type aliases are
    inlined in MIR. *)
and type_decl = {
  def_id : type_decl_id;
  item_meta : item_meta;  (** Meta information associated with the item. *)
  generics : generic_params;
  src : item_kind;
      (** The context of the type: distinguishes top-level items from
          closure-related items. *)
  kind : type_decl_kind;  (** The type kind: enum, struct, or opaque. *)
  layout : layout option;
      (** The layout of the type. Information may be partial because of generics
          or dynamically- sized types. If rustc cannot compute a layout, it is
          [None]. *)
  ptr_metadata : ptr_metadata option;
      (** The metadata associated with a pointer to the type. This is [None] if
          we could not compute it because of generics. The information is
          *accurate* if it is [Some] while if it is [None], it may still be
          theoretically computable but due to some limitation to be fixed, we
          are unable to obtain the info. See
          [translate_types::{impl ItemTransCtx}::translate_ptr_metadata] for
          more details. *)
}

and type_decl_kind =
  | Struct of field list
  | Enum of variant list
  | Union of field list
  | Opaque
      (** An opaque type.

          Either a local type marked as opaque, or an external type. *)
  | Alias of ty
      (** An alias to another type. This only shows up in the top-level list of
          items, as rustc inlines uses of type aliases everywhere else. *)
  | TDeclError of string
      (** Used if an error happened during the extraction, and we don't panic on
          error. *)

(** A placeholder for the vtable of a trait object. To be implemented in the
    future when [dyn Trait] is fully supported. *)
and v_table = unit

and variant = {
  span : span;
  attr_info : attr_info;
  variant_name : string;
  fields : field list;
  discriminant : scalar_value;
      (** The discriminant value outputted by [std::mem::discriminant] for this
          variant. This is different than the discriminant stored in memory (the
          one controlled by [repr]). That one is described by
          [[DiscriminantLayout]] and [[TagEncoding]]. *)
}

and variant_id = (VariantId.id[@visitors.opaque])

(** Simplified layout of a single variant.

    Maps fields to their offset within the layout. *)
and variant_layout = {
  field_offsets : int list;  (** The offset of each field. *)
  uninhabited : bool;
      (** Whether the variant is uninhabited, i.e. has any valid possible value.
          Note that uninhabited types can have arbitrary layouts. *)
  tag : scalar_value option;
      (** The memory representation of the discriminant corresponding to this
          variant. It must be of the same type as the corresponding
          [[DiscriminantLayout::tag_ty]].

          If it's [None], then this variant is either:
          - the untagged variant (cf. [[TagEncoding::Niche::untagged_variant]])
            of a niched enum;
          - the single variant of a struct;
          - uninhabited. *)
}
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_type_decl";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_type_decl_base" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_type_decl";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_type_decl_base" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]
