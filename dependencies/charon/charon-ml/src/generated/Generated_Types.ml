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

type integer_type = Values.integer_type [@@deriving show, ord, eq]
type float_type = Values.float_type [@@deriving show, ord, eq]
type literal_type = Values.literal_type [@@deriving show, ord, eq]

(* Manually implemented because no type uses it (we use plain lists instead of
   vectors in generic_params), which causes visitor inference problems if we
   declare it within a visitor group. *)
type trait_type_constraint_id = TraitTypeConstraintId.id
[@@deriving show, ord, eq]

(** The index of a binder, counting from the innermost. See [[DeBruijnVar]] for
    details. *)
type de_bruijn_id = int

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

    To make consumption easier for projects that don't do heavy substitution,
    [--unbind-item-vars] changes the variables bound at the top-level (i.e. in
    the [GenericParams] of items) to be [Free]. The example above becomes:
    {@rust[
      fn f<'a, 'b>(x: for<'c> fn(&'b u8, &'c u16, for<'d> fn(&'b u32, &'c u64, &'d u128)) -> u64) {}
           ^^^^^^         ^^       ^       ^          ^^       ^        ^        ^
             |       inner binder  |       |     inner binder  |        |        |
       top-level binder            |       |                   |        |        |
                                Free(b)    |                Free(b)     |     Bound(0, d)
                                           |                            |
                                       Bound(0, c)                 Bound(1, c)
    ]} *)
and 'a0 de_bruijn_var =
  | Bound of de_bruijn_id * 'a0
      (** A variable attached to the nth binder, counting from the innermost. *)
  | Free of 'a0
      (** A variable attached to the outermost binder (the one on the item).
          This is not used within Charon itself, instead ewe insert it at the
          end if [--unbind-item-vars] is set. *)

and fun_decl_id = (FunDeclId.id[@visitors.opaque])
and global_decl_id = (GlobalDeclId.id[@visitors.opaque])

(** The id of a translated item. *)
and item_id =
  | IdType of type_decl_id
  | IdTraitDecl of trait_decl_id
  | IdTraitImpl of trait_impl_id
  | IdFun of fun_decl_id
  | IdGlobal of global_decl_id

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
      name = "iter_type_vars";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_type_vars";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "reduce_type_vars";
      monomorphic = [ "env" ];
      variety = "reduce";
      ancestors = [ "reduce_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "mapreduce_type_vars";
      monomorphic = [ "env" ];
      variety = "mapreduce";
      ancestors = [ "mapreduce_literal" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(* Ancestors for the ty visitors *)
class ['self] iter_ty_base =
  object (self : 'self)
    inherit [_] iter_type_vars
    method visit_span : 'env -> span -> unit = fun _ _ -> ()
  end

class ['self] map_ty_base =
  object (self : 'self)
    inherit [_] map_type_vars
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
  | BKTraitType of trait_decl_id * trait_item_name
      (** The parameters of a generic associated type. *)
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
  | BoxNew
      (** Used instead of [alloc::boxed::Box::new] when [--treat-box-as-builtin]
          is set. *)
  | ArrayToSliceShared
      (** Cast [&[T; N]] to [&[T]].

          This is used instead of unsizing coercions when
          [--ops-to-function-calls] is set. *)
  | ArrayToSliceMut
      (** Cast [&mut [T; N]] to [&mut [T]].

          This is used instead of unsizing coercions when
          [--ops-to-function-calls] is set. *)
  | ArrayRepeat
      (** [repeat(n, x)] returns an array where [x] has been replicated [n]
          times.

          This is used instead of [Rvalue::ArrayRepeat] when
          [--ops-to-function-calls] is set. *)
  | Index of builtin_index_op
      (** A built-in funciton introduced instead of array/slice place indexing
          when [--index-to-function-calls] is set. The signature depends on the
          parameters. It could look like:
          - [fn ArrayIndexShared<T,N>(&[T;N], usize) -> &T]
          - [fn SliceIndexShared<T>(&[T], usize) -> &T]
          - [fn ArraySubSliceShared<T,N>(&[T;N], usize, usize) -> &[T]]
          - [fn SliceSubSliceMut<T>(&mut [T], usize, usize) -> &mut [T]]
          - etc *)
  | PtrFromParts of ref_kind
      (** Build a raw pointer, from a data pointer and metadata. The metadata
          can be unit, if building a thin pointer.

          This is used instead of [AggregateKind::RawPtr] when
          [--ops-to-function-calls] is set. *)

(** Describes a built-in impl. Mostly lists the implemented trait, sometimes
    with more details about the contents of the implementation. *)
and builtin_impl_data =
  | BuiltinSized
  | BuiltinMetaSized
  | BuiltinTuple
  | BuiltinPointee
  | BuiltinDiscriminantKind
  | BuiltinAuto
  | BuiltinNoopDestruct
      (** An impl of [Destruct] for a type with no drop glue. *)
  | BuiltinUntrackedDestruct
      (** An impl of [Destruct] for a type parameter, which we could not resolve
          because [--add-drop-bounds] was not set. *)
  | BuiltinFn
  | BuiltinFnMut
  | BuiltinFnOnce
  | BuiltinCopy
  | BuiltinClone

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
  | TStr  (** Primitive type *)

(** A byte, in the MiniRust sense: it can either be uninitialized, a concrete u8
    value, or part of a pointer with provenance (e.g. to a global or a function)
*)
and byte =
  | Uninit  (** An uninitialized byte *)
  | Value of int  (** A concrete byte value *)
  | Provenance of provenance * int
      (** A byte that is part of a pointer with provenance. The u8 is the offset
          within the pointer. Note that we do not have an actual value for this
          pointer byte, unlike MiniRust, as that is non-deterministic. *)

(** A const generic variable in a signature or binder. *)
and const_generic_param = {
  index : const_generic_var_id;
      (** Index identifying the variable among other variables bound at the same
          level. *)
  name : string;  (** Const generic name *)
  ty : ty;  (** Type of the const generic *)
}

and const_generic_var_id = (ConstGenericVarId.id[@visitors.opaque])
and constant_expr = { kind : constant_expr_kind; ty : ty }

(** A constant expression.

    Only the [[ConstantExprKind::Literal]] and [[ConstantExprKind::Var]] cases
    are left in the final LLBC.

    The other cases come from a straight translation from the MIR:

    [[ConstantExprKind::Adt]] case: It is a bit annoying, but rustc treats some
    ADT and tuple instances as constants when generating MIR:
    - an enumeration with one variant and no fields is a constant.
    - a structure with no field is a constant.
    - sometimes, Rust stores the initialization of an ADT as a constant (if all
      the fields are constant) rather than as an aggregated value We later
      desugar those to regular ADTs, see [regularize_constant_adts.rs].

    [[ConstantExprKind::Global]] case: access to a global variable. We later
    desugar it to a copy of a place global.

    [[ConstantExprKind::Ref]] case: reference to a constant value. We later
    desugar it to a separate statement.

    [[ConstantExprKind::FnPtr]] case: a function pointer (to a top-level
    function).

    Remark: MIR seems to forbid more complex expressions like paths. For
    instance, reading the constant [a.b] is translated to
    [{ _1 = const a; _2 = (_1.0) }]. *)
and constant_expr_kind =
  | CLiteral of literal
  | CAdt of variant_id option * constant_expr list
      (** In most situations: Enumeration with one variant with no fields,
          structure with no fields, unit (encoded as a 0-tuple).

          Less frequently: arbitrary ADT values.

          We eliminate this case in a micro-pass. *)
  | CArray of constant_expr list
  | CGlobal of global_decl_ref
      (** The value is a top-level constant/static.

          We eliminate this case in a micro-pass.

          Remark: constants can actually have generic parameters.
          {@rust[
            struct V<const N: usize, T> {
              x: [T; N],
            }

            impl<const N: usize, T> V<N, T> {
              const LEN: usize = N; // This has generics <N, T>
            }

            fn use_v<const N: usize, T>(v: V<N, T>) {
              let l = V::<N, T>::LEN; // We need to provided a substitution here
            }
          ]} *)
  | CTraitConst of trait_ref * trait_item_name
      (** A trait associated constant.

          Ex.:
          {@rust[
            impl Foo for Bar {
              const C : usize = 32; // <-
            }
          ]} *)
  | CVTableRef of trait_ref
      (** A reference to the vtable [static] item for this trait ref. This can
          be normalized for cases where we do emit a vtable item. That's not
          always the case for builtin traits, e.g. for [MetaSized]. *)
  | CRef of constant_expr * unsizing_metadata option
      (** A shared reference to a constant value.

          We eliminate this case in a micro-pass. *)
  | CPtr of ref_kind * constant_expr * unsizing_metadata option
      (** A pointer to a mutable static.

          We eliminate this case in a micro-pass. *)
  | CVar of const_generic_var_id de_bruijn_var  (** A const generic var *)
  | CFnDef of fn_ptr  (** Function definition -- this is a ZST constant *)
  | CFnPtr of fn_ptr
      (** A function pointer to a function item; this is an actual pointer to
          that function item.

          We eliminate this case in a micro-pass. *)
  | CPtrNoProvenance of big_int
      (** A pointer with no provenance (e.g. 0 for the null pointer)

          We eliminate this case in a micro-pass. *)
  | CRawMemory of byte list
      (** Raw memory value obtained from constant evaluation. Used when a more
          structured representation isn't possible (e.g. for unions) or just
          isn't implemented yet. *)
  | COpaque of string
      (** A constant expression that Charon still doesn't handle, along with the
          reason why. *)

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

and field_id = (FieldId.id[@visitors.opaque])
and fn_ptr = { kind : fn_ptr_kind; generics : generic_args }

and fn_ptr_kind =
  | FunId of fun_id
  | TraitMethod of trait_ref * trait_item_name * fun_decl_id
      (** If a trait: the reference to the trait and the id of the trait method.
          The fun decl id is not really necessary - we put it here for
          convenience purposes. *)

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

(** A function signature. *)
and fun_sig = {
  is_unsafe : bool;  (** Is the function unsafe or not *)
  inputs : ty list;
  output : ty;
}

(** A set of generic arguments. *)
and generic_args = {
  regions : region list;
  types : ty list;
  const_generics : constant_expr list;
  trait_refs : trait_ref list;
}

(** Generic parameters for a declaration. We group the generics which come from
    the Rust compiler substitutions (the regions, types and const generics) as
    well as the trait clauses. The reason is that we consider that those are
    parameters that need to be filled. We group in a different place the
    predicates which are not trait clauses, because those enforce constraints
    but do not need to be filled with witnesses/instances. *)
and generic_params = {
  regions : region_param list;
  types : type_param list;
  const_generics : const_generic_param list;
  trait_clauses : trait_param list;
  regions_outlive : (region, region) outlives_pred region_binder list;
      (** The first region in the pair outlives the second region *)
  types_outlive : (ty, region) outlives_pred region_binder list;
      (** The type outlives the region *)
  trait_type_constraints : trait_type_constraint region_binder list;
      (** Constraints over trait associated types *)
}

(** Reference to a global declaration. *)
and global_decl_ref = { id : global_decl_id; generics : generic_args }

(** Hash-consed data structure: a reference-counted wrapper that guarantees that
    two equal value will be stored at the same address. This makes it possible
    to use the pointer address as a hash value. *)
and 'a0 hash_consed = 'a0 (* Not actually hash-consed on the OCaml side *)

(** The nature of locations where a given lifetime parameter is used. If this
    lifetime ever flows to be used as the lifetime of a mutable reference
    [&'a mut] then we consider it mutable. *)
and lifetime_mutability =
  | LtMutable  (** A lifetime that is used for a mutable reference. *)
  | LtShared  (** A lifetime used only in shared references. *)
  | LtUnknown
      (** A lifetime for which we couldn't/didn't compute mutability. *)

(** .0 outlives .1 *)
and ('a0, 'a1) outlives_pred = 'a0 * 'a1

and provenance =
  | ProvGlobal of global_decl_ref
  | ProvFunction of fun_decl_ref
  | ProvUnknown

and ref_kind = RMut | RShared

and region =
  | RVar of region_id de_bruijn_var
      (** Region variable. See [DeBruijnVar] for details. *)
  | RStatic  (** Static region *)
  | RBody of region_id
      (** Body-local region, considered existentially-bound at the level of a
          body. *)
  | RErased  (** Erased region *)

(** A value of type [T] bound by regions. We should use [binder] instead but
    this causes name clash issues in the derived ocaml visitors. *)
and 'a0 region_binder = {
  binder_regions : region_param list;
  binder_value : 'a0;
      (** Named this way to highlight accesses to the inner value that might be
          handling parameters incorrectly. Prefer using helper methods. *)
}

and region_id = (RegionId.id[@visitors.opaque])

(** A region variable in a signature or binder. *)
and region_param = {
  index : region_id;
      (** Index identifying the variable among other variables bound at the same
          level. *)
  name : string option;  (** Region name *)
  mutability : lifetime_mutability;
      (** Whether this lifetime is (recursively) used in a [&'a mut T] type.
          Only [true] if this lifetime parameter belongs to an ADT. This is a
          global analysis that looks even into opaque items. When unsure, err on
          the side of assuming mutability. *)
}

(** The value of a trait associated type. *)
and trait_assoc_ty_impl = { value : ty }

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

(** A trait predicate in a signature, of the form [Type: Trait<Args>]. This
    functions like a variable binder, to which variables of the form
    [TraitRefKind::Clause] can refer to. *)
and trait_param = {
  clause_id : trait_clause_id;
      (** Index identifying the clause among other clauses bound at the same
          level. *)
  span : span option;
  trait : trait_decl_ref region_binder;  (** The trait that is implemented. *)
}

(** A reference to a trait.

    This type is hash-consed, [TraitRefContents] contains the actual data. *)
and trait_ref = trait_ref_contents hash_consed

and trait_ref_contents = {
  kind : trait_ref_kind;
  trait_decl_ref : trait_decl_ref region_binder;
      (** Not necessary, but useful *)
}

(** Identifier of a trait instance. This is derived from the trait resolution.

    Should be read as a path inside the trait clauses which apply to the current
    definition. Note that every path designated by [TraitInstanceId] refers to a
    *trait instance*, which is why the [[TraitRefKind::Clause]] variant may seem
    redundant with some of the other variants. *)
and trait_ref_kind =
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
  | ItemClause of trait_ref * trait_item_name * trait_clause_id
      (** A clause defined on an associated type. This variant is only used
          during translation; after the [lift_associated_item_clauses] pass,
          clauses on items become [ParentClause]s.

          Example:
          {@rust[
            trait Foo {
              type W: Bar0 + Bar1 // Bar1 contains a method bar1
                             ^^^^
                          this is the clause 1 applying to W
            }

            fn f<T : Foo>(x : T::W) {
              x.bar1();
              ^^^^^^^
              ItemClause(Clause(0), W, 1)
                                    ^^^^
                                    clause 1 from item W (from local clause 0)
            }
          ]} *)
  | Self
      (** The implicit [Self: Trait] clause. Present inside trait declarations,
          including trait method declarations. Not present in trait
          implementations as we can use [TraitImpl] intead. *)
  | BuiltinOrAuto of
      builtin_impl_data
      * trait_ref list
      * (trait_item_name * trait_assoc_ty_impl) list
      (** A trait implementation that is computed by the compiler, such as for
          built-in trait [Sized]. This morally points to an invisible [impl]
          block; as such it contains the information we may need from one.

          Fields:
          - [builtin_data]
          - [parent_trait_refs]: Exactly like the same field on [TraitImpl]: the
            [TraitRef]s required to satisfy the implied predicates on the trait
            declaration. E.g. since [FnMut: FnOnce], a built-in [T: FnMut] impl
            would have a [TraitRef] for [T: FnOnce].
          - [types]: The values of the associated types for this trait. *)
  | Dyn  (** The automatically-generated implementation for [dyn Trait]. *)
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

(** A type.

    Warning: the [DriveMut] impls of [Ty] needs to clone and re-hash the
    modified type to maintain the hash-consing invariant. This is expensive,
    avoid visiting types mutably when not needed. *)
and ty = ty_kind hash_consed

and ty_kind =
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
  | TFnPtr of fun_sig region_binder
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
  | TPtrMetadata of ty
      (** As a marker of taking out metadata from a given type The internal type
          is assumed to be a type variable *)
  | TArray of ty * constant_expr  (** An array type [[T; N]] *)
  | TSlice of ty  (** A slice type [[T]] *)
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
and type_param = {
  index : type_var_id;
      (** Index identifying the variable among other variables bound at the same
          level. *)
  name : string;  (** Variable name *)
}

and unsizing_metadata =
  | MetaLength of constant_expr  (** Cast from [[T; N]] to [[T]]. *)
  | MetaVTable of trait_ref * constant_expr
      (** Cast from a sized value to a [dyn Trait] value. The [TraitRef] is the
          proof of the [dyn Trait] predicate; the constant expression is a
          reference to the vtable [static] value. *)
  | MetaVTableUpcast of field_id list
      (** Cast from [dyn Trait] to [dyn OtherTrait]. The fields indicate how to
          retreive the vtable: it's always either the same we already had, or
          the vtable for a (possibly nested) supertrait.

          Note that we cheat in one case: when upcasting to a marker trait (e.g.
          [dyn Trait -> dyn Sized]), we keep the current vtable. *)
  | MetaUnknown

and variant_id = (VariantId.id[@visitors.opaque])
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

(** Describes modifiers to the alignment and packing of the corresponding type.
    Represents [repr(align(n))] and [repr(packed(n))]. *)
type alignment_modifier = Align of int | Pack of int

(** Additional information for closures. *)
and closure_info = {
  kind : closure_kind;
  fn_once_impl : trait_impl_ref region_binder;
      (** The [FnOnce] implementation of this closure -- always exists. *)
  fn_mut_impl : trait_impl_ref region_binder option;
      (** The [FnMut] implementation of this closure, if any. *)
  fn_impl : trait_impl_ref region_binder option;
      (** The [Fn] implementation of this closure, if any. *)
  signature : fun_sig region_binder;
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
and item_source =
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
  | VTableTyItem of dyn_predicate * v_table_field list * field_id option list
      (** This is a vtable struct for a trait.

          Fields:
          - [dyn_predicate]: The [dyn Trait] predicate implemented by this
            vtable.
          - [field_map]: Record what each vtable field means.
          - [supertrait_map]: For each implied clause that is also a supertrait
            clause, reords which field id corresponds to it. *)
  | VTableInstanceItem of trait_impl_ref
      (** This is a vtable value for an impl.

          Fields:
          - [impl_ref] *)
  | VTableMethodShimItem
      (** The method shim wraps a concrete implementation of a method into a
          function that takes [dyn Trait] as its [Self] type. This shim casts
          the receiver to the known concrete type and calls the real method. *)
  | VTableInstanceMonoItem
  | VTableMethodPreShimItem of trait_decl_id * trait_item_name * ty list

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
  | PeInstantiated of generic_args binder
      (** This item was obtained by instantiating its parent with the given
          args. The binder binds the parameters of the new items. If the binder
          binds nothing then this is a monomorphization. *)

(** The metadata stored in a pointer. That's the information stored in pointers
    alongside their address. It's empty for [Sized] types, and interesting for
    unsized aka dynamically-sized types. *)
and ptr_metadata =
  | NoMetadata  (** Types that need no metadata, namely [T: Sized] types. *)
  | Length
      (** Metadata for [[T]] and [str], and user-defined types that directly or
          indirectly contain one of the two. Of type [usize]. Notably, length
          for [[T]] denotes the number of elements in the slice. While for [str]
          it denotes the number of bytes in the string. *)
  | VTable of type_decl_ref
      (** Metadata for [dyn Trait], referring to the vtable struct, also for
          user-defined types that directly or indirectly contain a [dyn Trait].
          Of type [&'static vtable] *)
  | InheritFrom of ty
      (** Unknown due to generics, but will inherit from the given type. This is
          consistent with [<Ty as Pointee>::Metadata]. Of type
          [TyKind::Metadata(Ty)]. *)

(** Describes which layout algorithm is used for representing the corresponding
    type. Depends on the [#[repr(...)]] used. *)
and repr_algorithm =
  | Rust
      (** The default layout algorithm. Used without an explicit [ŗepr] or for
          [repr(Rust)]. *)
  | C  (** The C layout algorithm as enforced by [repr(C)]. *)

(** The representation options as annotated by the user.

    NOTE: This does not include less common/unstable representations such as
    [#[repr(simd)]] or the compiler internal [#[repr(linear)]]. Similarly, enum
    discriminant representations are encoded in [[Variant::discriminant]] and
    [[DiscriminantLayout]] instead. This only stores whether the discriminant
    type was derived from an explicit annotation. *)
and repr_options = {
  repr_algo : repr_algorithm;
  align_modif : alignment_modifier option;
  transparent : bool;
  explicit_discr_type : bool;
}

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
  src : item_source;
      (** The context of the type: distinguishes top-level items from
          closure-related items. *)
  kind : type_decl_kind;  (** The type kind: enum, struct, or opaque. *)
  layout : layout option;
      (** The layout of the type. Information may be partial because of generics
          or dynamically- sized types. If rustc cannot compute a layout, it is
          [None]. *)
  ptr_metadata : ptr_metadata;
      (** The metadata associated with a pointer to the type. *)
  repr : repr_options option;
      (** The representation options of this type declaration as annotated by
          the user. Is [None] for foreign type declarations. *)
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

and v_table_field =
  | VTableSize
  | VTableAlign
  | VTableDrop
  | VTableMethod of trait_item_name
  | VTableSuperTrait of trait_clause_id

and variant = {
  span : span;
  attr_info : attr_info;
  variant_name : string;
  fields : field list;
  discriminant : literal;
      (** The discriminant value outputted by [std::mem::discriminant] for this
          variant. This can be different than the value stored in memory (called
          [tag]). That one is described by [[DiscriminantLayout]] and
          [[TagEncoding]]. *)
}

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
