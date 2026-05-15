(** WARNING: this file is partially auto-generated. Do not edit `GAst.ml` by
    hand. Edit `GAst.template.ml` instead, or improve the code generation tool
    so avoid the need for hand-writing things.

    `GAst.template.ml` contains the manual definitions and some `(* __REPLACEn__
    *)` comments. These comments are replaced by auto-generated definitions by
    running `make generate-ml` in the crate root. The code-generation code is in
    `charon/src/bin/generate-ml`. *)

open Types
open Meta
open Expressions
module FunDeclId = Expressions.FunDeclId
module GlobalDeclId = Expressions.GlobalDeclId
module TraitDeclId = Types.TraitDeclId
module TraitImplId = Types.TraitImplId
module TraitClauseId = Types.TraitClauseId

(* Imports *)
type builtin_fun_id = Types.builtin_fun_id [@@deriving show, ord]
type fun_id = Types.fun_id [@@deriving show, ord]
type fn_ptr_kind = Types.fn_ptr_kind [@@deriving show, ord]
type fun_decl_id = Types.fun_decl_id [@@deriving show, ord]

(** (U)LLBC is a language with side-effects: a statement may abort in a way that
    isn't tracked by control-flow. The three kinds of abort are:
    - Panic
    - Undefined behavior (caused by an "assume")
    - Unwind termination *)
type abort_kind =
  | Panic of name option
      (** A built-in panicking function, or a panic due to a failed built-in
          check (e.g. for out-of-bounds accesses). *)
  | UndefinedBehavior  (** Undefined behavior in the rust abstract machine. *)
  | UnwindTerminate
      (** Unwind had to stop for ABI reasons or because cleanup code panicked
          again. *)

(** Check the value of an operand and abort if the value is not expected. This
    is introduced to avoid a lot of small branches.

    We translate MIR asserts (introduced for out-of-bounds accesses or divisions
    by zero for instance) to this. We then eliminate them in
    [crate::transform::resugar::reconstruct_fallible_operations], because
    they're implicit in the semantics of our array accesses etc. Finally we
    introduce new asserts in [crate::transform::resugar::reconstruct_asserts].
*)
and assertion = {
  cond : operand;
  expected : bool;
      (** The value that the operand should evaluate to for the assert to
          succeed. *)
  check_kind : builtin_assert_kind option;
      (** The kind of check performed by this assert. This is only used for
          error reporting, as the check is actually performed by the
          instructions preceding the assert. *)
}

(** The kind of a built-in assertion, which may panic and unwind. These are
    removed by [reconstruct_fallible_operations] because they're implicit in the
    semantics of (U)LLBC. This kind should only be used for error-reporting
    purposes, as the check itself is performed in the instructions preceding the
    assert. *)
and builtin_assert_kind =
  | BoundsCheck of operand * operand
      (** Fields:
          - [len]
          - [index] *)
  | Overflow of binop * operand * operand
  | OverflowNeg of operand
  | DivisionByZero of operand
  | RemainderByZero of operand
  | MisalignedPointerDereference of operand * operand
      (** Fields:
          - [required]
          - [found] *)
  | NullPointerDereference
  | InvalidEnumConstruction of operand

and call = { func : fn_operand; args : operand list; dest : place }
and copy_non_overlapping = { src : operand; dst : operand; count : operand }

(** A [Drop] statement/terminator can mean two things, depending on what MIR
    phase we retrieved from rustc: it could be a real drop, or it could be a
    "conditional drop", which is where drop may happen depending on whether the
    borrow-checker determines a drop is needed. *)
and drop_kind =
  | Precise
      (** A real drop. This calls
          [<T as Destruct>::drop_in_place(&raw mut place)] and marks the place
          as moved-out-of. Use [--desugar-drops] to transform all such drops to
          an actual function call.

          The [drop_in_place] method is added by Charon to the [Destruct] trait
          to make it possible to track drop code in polymorphic code. It
          contains the same code as the [core::ptr::drop_in_place<T>] builtin
          would.

          Drop are precise in MIR [elaborated] and [optimized]. *)
  | Conditional
      (** A conditional drop, which may or may not end up running drop code
          depending on the code path that led to it. A conditional drop may also
          become a partial drop (dropping only the subplaces that haven't been
          moved out of), may be conditional on the code path that led to it, or
          become an async drop. The exact semantics are left intentionally
          unspecified by rustc developers. To elaborate such drops into precise
          drops, pass [--precise-drops] to Charon.

          A conditional drop may also be passed an unaligned place when dropping
          fields of packed structs. Such a thing is UB for a precise drop.

          Drop are conditional in MIR [built] and [promoted]. *)

(** Common error used during the translation. *)
and error = { span : span; msg : string }

(** A function operand is used in function calls. It either designates a
    top-level function, or a place in case we are using function pointers stored
    in local variables. *)
and fn_operand =
  | FnOpRegular of fn_ptr
      (** Regular case: call to a top-level function, trait method, etc. *)
  | FnOpDynamic of operand  (** Use of a function pointer. *)

(** A variable *)
and local = {
  index : local_id;  (** Unique index identifying the variable *)
  name : string option;
      (** Variable name - may be [None] if the variable was introduced by Rust
          through desugaring. *)
  span : span;  (** Span of the variable declaration. *)
  local_ty : ty;  (** The variable type *)
}

(** The local variables of a body. *)
and locals = {
  arg_count : int;
      (** The number of local variables used for the input arguments. *)
  locals : local list;
      (** The local variables. We always have, in the following order:
          - the local used for the return value (index 0)
          - the [arg_count] input arguments
          - the remaining locals, used for the intermediate computations *)
}
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_fun_sig";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_rvalue" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_fun_sig";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_rvalue" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(** A global variable definition (constant or static). *)
type global_decl = {
  def_id : global_decl_id;
  item_meta : item_meta;  (** The meta data associated with the declaration. *)
  generics : generic_params;
  ty : ty;
  src : item_source;
      (** The context of the global: distinguishes top-level items from
          trait-associated items. *)
  global_kind : global_kind;  (** The kind of global (static or const). *)
  init : fun_decl_id;
      (** The initializer function used to compute the initial value for this
          constant/static. It uses the same generic parameters as the global. *)
}

and global_kind =
  | Static  (** A static. *)
  | NamedConst
      (** A const with a name (either top-level or an associated const in a
          trait). *)
  | AnonConst
      (** A const without a name:
          - An inline const expression ([const { 1 + 1 }]);
          - A const expression in a type ([[u8; sizeof::<T>()]]);
          - A promoted constant, automatically lifted from a body ([&0]). *)
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_global_decl";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_fun_sig" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_global_decl";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_fun_sig" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(** An associated constant in a trait. *)
type trait_assoc_const = {
  name : trait_item_name;
  attr_info : attr_info;
  ty : ty;
  default : global_decl_ref option;
}

(** An associated type in a trait. *)
and trait_assoc_ty = {
  name : trait_item_name;
  attr_info : attr_info;
  default : trait_assoc_ty_impl option;
  implied_clauses : trait_param list;
      (** List of trait clauses that apply to this type. *)
}

(** A trait **declaration**.

    For instance:
    {@rust[
      trait Foo {
        type Bar;

        fn baz(...); // required method (see below)

        fn test() -> bool { true } // provided method (see below)
      }
    ]}

    In case of a trait declaration, we don't include the provided methods (the
    methods with a default implementation): they will be translated on a
    per-need basis. This is important for two reasons:
    - this makes the trait definitions a lot smaller (the Iterator trait has
      *one* declared function and more than 70 provided functions)
    - this is important for the external traits, whose provided methods often
      use features we don't support yet

    Remark: In Aeneas, we still translate the provided methods on an individual
    basis, and in such a way thay they take as input a trait instance. This
    means that we can use default methods *but*:
    - implementations of required methods shoudln't call default methods
    - trait implementations shouldn't redefine required methods The use case we
      have in mind is [std::iter::Iterator]: it declares one required method
      ([next]) that should be implemented for every iterator, and defines many
      helpers like [all], [map], etc. that shouldn't be re-implemented. Of
      course, this forbids other useful use cases such as visitors implemented
      by means of traits. *)
and trait_decl = {
  def_id : trait_decl_id;
  item_meta : item_meta;
  generics : generic_params;
  implied_clauses : trait_param list;
      (** The "parent" clauses: the supertraits.

          Supertraits are actually regular where clauses, but we decided to have
          a custom treatment.
          {@rust[
            trait Foo : Bar {
                        ^^^
                    supertrait, that we treat as a parent predicate
            }
          ]}
          TODO: actually, as of today, we consider that all trait clauses of
          trait declarations are parent clauses. *)
  consts : trait_assoc_const list;
      (** The associated constants declared in the trait. *)
  types : trait_assoc_ty binder list;
      (** The associated types declared in the trait. The binder binds the
          generic parameters of the type if it is a GAT (Generic Associated
          Type). For a plain associated type the binder binds nothing. *)
  methods : trait_method binder list;
      (** The methods declared by the trait. The binder binds the generic
          parameters of the method.

          {@rust[
            rust
            trait Trait<T> {
              // The [Binder] for this method binds ['a] and [U].
              fn method<'a, U>(x: &'a U);
            }
          ]} *)
  vtable : type_decl_ref option;
      (** The virtual table struct for this trait, if it has one. It is
          guaranteed that the trait has a vtable iff it is dyn-compatible. *)
}

(** A trait method. *)
and trait_method = {
  name : trait_item_name;
  attr_info : attr_info;
  item : fun_decl_ref;
      (** Each method declaration is represented by a function item. That
          function contains the signature of the method as well as information
          like attributes. It has a body iff the method declaration has a
          default implementation; otherwise it has an [Opaque] body. *)
}
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_trait_decl";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_global_decl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_trait_decl";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_global_decl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

(** A trait **implementation**.

    For instance:
    {@rust[
      impl Foo for List {
        type Bar = ...

        fn baz(...) { ... }
      }
    ]} *)
type trait_impl = {
  def_id : trait_impl_id;
  item_meta : item_meta;
  impl_trait : trait_decl_ref;
      (** The information about the implemented trait. Note that this contains
          the instantiation of the "parent" clauses. *)
  generics : generic_params;
  implied_trait_refs : trait_ref list;
      (** The trait references for the parent clauses (see [TraitDecl]). *)
  consts : (trait_item_name * global_decl_ref) list;
      (** The implemented associated constants. *)
  types : (trait_item_name * trait_assoc_ty_impl binder) list;
      (** The implemented associated types. *)
  methods : (trait_item_name * fun_decl_ref binder) list;
      (** The implemented methods *)
  vtable : global_decl_ref option;
      (** The virtual table instance for this trait implementation. This is
          [Some] iff the trait is dyn-compatible. *)
}
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_trait_impl";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_trait_decl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_trait_impl";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_trait_decl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]

type cli_options = {
  ullbc : bool;
      (** Extract the unstructured LLBC (i.e., don't reconstruct the
          control-flow) *)
  precise_drops : bool;
      (** Whether to precisely translate drops and drop-related code. For this,
          we add explicit [Destruct] bounds to all generic parameters, set the
          MIR level to at least [elaborated], and attempt to retrieve drop glue
          for all types.

          This option is known to cause panics inside rustc, because their drop
          handling is not design to work on polymorphic types. To silence the
          warning, pass appropriate
          [--opaque '{impl core::marker::Destruct for some::Type}'] options.

          Without this option, drops may be "conditional" and we may lack
          information about what code is run on drop in a given polymorphic
          function body. *)
  skip_borrowck : bool;
      (** If activated, this skips borrow-checking of the crate. *)
  mir : mir_level option;
      (** The MIR stage to extract. This is only relevant for the current crate;
          for dpendencies only MIR optimized is available. *)
  rustc_args : string list;  (** Extra flags to pass to rustc. *)
  monomorphize : bool;
      (** Monomorphize the items encountered when possible. Generic items found
          in the crate are skipped. To only translate a particular call graph,
          use [--start-from]. Note: this doesn't currently support [dyn Trait].
      *)
  monomorphize_mut : monomorphize_mut option;
      (** Partially monomorphize items to make it so that no item is ever
          monomorphized with a mutable reference (or type containing one); said
          differently, so that the presence of mutable references in a type is
          independent of its generics. This is used by Aeneas. *)
  start_from : string list;
      (** A list of item paths to use as starting points for the translation. We
          will translate these items and any items they refer to, according to
          the opacity rules. When absent, we start from the path [crate] (which
          translates the whole crate). *)
  start_from_if_exists : string list;
      (** Same as --start-from, but won't raise an error if a pattern doesn't
          match any item. This is useful when the patterns are generated by a
          build script and may be out of sync with the code. *)
  start_from_attribute : string option;
      (** Use all the items annotated with the given attribute as starting
          points for translation (except modules). If an attribute name is not
          specified, [verify::start_from] is used. *)
  start_from_pub : bool;
      (** Use all the [pub] items as starting points for translation (except
          modules). *)
  included : string list;
      (** Whitelist of items to translate. These use the name-matcher syntax. *)
  opaque : string list;
      (** Blacklist of items to keep opaque. Works just like [--include], see
          the doc there. *)
  exclude : string list;
      (** Blacklist of items to not translate at all. Works just like
          [--include], see the doc there. *)
  extract_opaque_bodies : bool;
      (** Usually we skip the bodies of foreign methods and structs with private
          fields. When this flag is on, we don't. *)
  translate_all_methods : bool;
      (** Usually we skip the provided methods that aren't used. When this flag
          is on, we translate them all. *)
  lift_associated_types : string list;
      (** Transform the associate types of traits to be type parameters instead.
          This takes a list of name patterns of the traits to transform, using
          the same syntax as [--include]. *)
  hide_marker_traits : bool;
      (** Whether to hide various marker traits such as [Sized], [Sync], [Send]
          and [Destruct] anywhere they show up. This can considerably speed up
          translation. *)
  remove_adt_clauses : bool;
      (** Remove trait clauses from type declarations. Must be combined with
          [--remove-associated-types] for type declarations that use trait
          associated types in their fields, otherwise this will result in
          errors. *)
  hide_allocator : bool;
      (** Hide the [A] type parameter on standard library containers ([Box],
          [Vec], etc). *)
  remove_unused_self_clauses : bool;
      (** Trait method declarations take a [Self: Trait] clause as parameter, so
          that they can be reused by multiple trait impls. This however causes
          trait definitions to be mutually recursive with their method
          declarations. This flag removes [Self] clauses that aren't used to
          break this mutual recursion when possible. *)
  desugar_drops : bool;
      (** Transform precise drops to the equivalent [drop_in_place(&raw mut p)]
          call. *)
  ops_to_function_calls : bool;
      (** Transform array-to-slice unsizing, repeat expressions, and raw pointer
          construction into builtin functions in ULLBC. *)
  index_to_function_calls : bool;
      (** Transform array/slice indexing into builtin functions in ULLBC. Note
          that this may introduce UB since it creates references that were not
          normally created, including when indexing behind a raw pointer. *)
  treat_box_as_builtin : bool;
      (** Treat [Box<T>] as if it was a built-in type. *)
  raw_consts : bool;  (** Do not inline or evaluate constants. *)
  unsized_strings : bool;
      (** Replace string literal constants with a constant u8 array that gets
          unsized, expliciting the fact a string constant has a hidden
          reference. *)
  reconstruct_fallible_operations : bool;
      (** Replace "bound checks followed by UB-on-overflow operation" with the
          corresponding panic-on-overflow operation. This loses unwinding
          information. *)
  reconstruct_asserts : bool;
      (** Replace "if x { panic() }" with "assert(x)". *)
  unbind_item_vars : bool;
      (** Use [DeBruijnVar::Free] for the variables bound in item signatures,
          instead of [DeBruijnVar::Bound] everywhere. This simplifies the
          management of generics for projects that don't intend to manipulate
          them too much. *)
  print_original_ullbc : bool;
      (** Pretty-print the ULLBC immediately after extraction from MIR. *)
  print_ullbc : bool;
      (** Pretty-print the ULLBC after applying the micro-passes (before
          serialization/control-flow reconstruction). *)
  print_built_llbc : bool;
      (** Pretty-print the LLBC just after we built it (i.e., immediately after
          loop reconstruction). *)
  print_llbc : bool;
      (** Pretty-print the final LLBC (after all the cleaning micro-passes). *)
  dest_dir : path_buf option;
      (** The destination directory. Files will be generated as
          [<dest_dir>/<crate_name>.{u}llbc], unless [dest_file] is set.
          [dest_dir] defaults to the current directory. *)
  dest_file : path_buf option;
      (** The destination file. By default [<dest_dir>/<crate_name>.llbc]. If
          this is set we ignore [dest_dir]. *)
  no_dedup_serialized_ast : bool;
      (** Don't deduplicate values (types, trait refs) in the .(u)llbc file.
          This makes the file easier to inspect. *)
  no_serialize : bool;  (** Don't serialize the final (U)LLBC to a file. *)
  abort_on_error : bool;
      (** Panic on the first error. This is useful for debugging. *)
  error_on_warnings : bool;  (** Consider any warnings to be errors. *)
  preset : preset option;  (** Named builtin sets of options. *)
}

(** A (group of) top-level declaration(s), properly reordered. *)
and declaration_group =
  | TypeGroup of type_decl_id g_declaration_group
      (** A type declaration group *)
  | FunGroup of fun_decl_id g_declaration_group
      (** A function declaration group *)
  | GlobalGroup of global_decl_id g_declaration_group
      (** A global declaration group *)
  | TraitDeclGroup of trait_decl_id g_declaration_group
  | TraitImplGroup of trait_impl_id g_declaration_group
  | MixedGroup of item_id g_declaration_group
      (** Anything that doesn't fit into these categories. *)

(** A (group of) top-level declaration(s), properly reordered. "G" stands for
    "generic" *)
and 'a0 g_declaration_group =
  | NonRecGroup of 'a0  (** A non-recursive declaration *)
  | RecGroup of 'a0 list  (** A (group of mutually) recursive declaration(s) *)

(** An expression body. TODO: arg_count should be stored in GFunDecl below. But
    then, the print is obfuscated and Aeneas may need some refactoring. *)
and 'a0 gexpr_body = {
  span : span;
  bound_body_regions : int;
      (** The number of regions existentially bound in this body. We introduce
          fresh such regions during translation instead of the erased regions
          that rustc gives us. *)
  locals : locals;  (** The local variables. *)
  body : 'a0;  (** The statements and blocks that compose this body. *)
}

(** The MIR stage to use. This is only relevant for the current crate: for
    dependencies, only mir optimized is available (or mir elaborated for
    consts). *)
and mir_level =
  | Built  (** The MIR just after MIR lowering. *)
  | Promoted
      (** The MIR after const promotion. This is the MIR used by the
          borrow-checker. *)
  | Elaborated
      (** The MIR after drop elaboration. This is the first MIR to include all
          the runtime information. *)
  | Optimized
      (** The MIR after optimizations. Charon disables all the optimizations it
          can, so this is sensibly the same MIR as the elaborated MIR. *)

and monomorphize_mut =
  | All  (** Monomorphize any item instantiated with [&mut]. *)
  | ExceptTypes
      (** Monomorphize all non-typedecl items instantiated with [&mut]. *)

(** Presets to make it easier to tweak options without breaking dependent
    projects. Eventually we should define semantically-meaningful presets
    instead of project-specific ones. *)
and preset =
  | OldDefaults
      (** The default translation used before May 2025. After that, many passes
          were made optional and disabled by default. *)
  | RawMir
      (** Emit the MIR as unmodified as possible. This is very imperfect for
          now, we should make more passes optional. *)
  | Aeneas
  | Eurydice
  | Soteria
  | Tests
[@@deriving show]
