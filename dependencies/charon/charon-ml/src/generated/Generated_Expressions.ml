(** WARNING: this file is partially auto-generated. Do not edit
    `src/Expressions.ml` by hand. Edit `templates/Expressions.ml` instead, or
    improve the code generation tool so avoid the need for hand-writing things.

    `templates/Expressions.ml` contains the manual definitions and some `(*
    __REPLACEn__ *)` comments. These comments are replaced by auto-generated
    definitions by running `make generate-ml` in the crate root. The
    code-generation code is in `charon/src/bin/generate-ml`. *)
open Identifiers

open Types
open Values
module LocalId = IdGen ()
module GlobalDeclId = Types.GlobalDeclId
module FunDeclId = Types.FunDeclId

(** An aggregated ADT.

    Note that ADTs are desaggregated at some point in MIR. For instance, if we
    have in Rust:
    {@rust[
      ignore
      let ls = Cons(hd, tl);
    ]}

    In MIR we have (yes, the discriminant update happens *at the end* for some
    reason):
    {@rust[
      (ls as Cons).0 = move hd;
      (ls as Cons).1 = move tl;
      discriminant(ls) = 0; // assuming [Cons] is the variant of index 0
    ]}

    Rem.: in the Aeneas semantics, both cases are handled (in case of
    desaggregated initialization, [ls] is initialized to [⊥], then this [⊥] is
    expanded to [Cons (⊥, ⊥)] upon the first assignment, at which point we can
    initialize the field 0, etc.). *)
type aggregate_kind =
  | AggregatedAdt of type_decl_ref * variant_id option * field_id option
      (** A struct, enum or union aggregate. The [VariantId], if present,
          indicates this is an enum and the aggregate uses that variant. The
          [FieldId], if present, indicates this is a union and the aggregate
          writes into that field. Otherwise this is a struct. *)
  | AggregatedArray of ty * const_generic
      (** We don't put this with the ADT cas because this is the only built-in
          type with aggregates, and it is a primitive type. In particular, it
          makes sense to treat it differently because it has a variable number
          of fields. *)
  | AggregatedRawPtr of ty * ref_kind
      (** Construct a raw pointer from a pointer value, and its metadata (can be
          unit, if building a thin pointer). The type is the type of the
          pointee. We lower this to a builtin function call for LLBC in
          [crate::transform::ops_to_function_calls]. *)

(** Binary operations. *)
and binop =
  | BitXor
  | BitAnd
  | BitOr
  | Eq
  | Lt
  | Le
  | Ne
  | Ge
  | Gt
  | Add of overflow_mode
  | Sub of overflow_mode
  | Mul of overflow_mode
  | Div of overflow_mode
  | Rem of overflow_mode
  | AddChecked
      (** Returns [(result, did_overflow)], where [result] is the result of the
          operation with wrapping semantics, and [did_overflow] is a boolean
          that indicates whether the operation overflowed. This operation does
          not fail. *)
  | SubChecked  (** Like [AddChecked]. *)
  | MulChecked  (** Like [AddChecked]. *)
  | Shl of overflow_mode
      (** Fails if the shift is bigger than the bit-size of the type. *)
  | Shr of overflow_mode
      (** Fails if the shift is bigger than the bit-size of the type. *)
  | Offset
      (** [BinOp(Offset, ptr, n)] for [ptr] a pointer to type [T] offsets [ptr]
          by [n * size_of::<T>()]. *)
  | Cmp
      (** [BinOp(Cmp, a, b)] returns [-1u8] if [a < b], [0u8] if [a == b], and
          [1u8] if [a > b]. *)

and borrow_kind =
  | BShared
  | BMut
  | BTwoPhaseMut
      (** See
          <https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.MutBorrowKind.html#variant.TwoPhaseBorrow>
          and
          <https://rustc-dev-guide.rust-lang.org/borrow_check/two_phase_borrows.html>
      *)
  | BShallow
      (** Those are typically introduced when using guards in matches, to make
          sure guards don't change the variant of an enum value while me match
          over it.

          See
          <https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.FakeBorrowKind.html#variant.Shallow>.
      *)
  | BUniqueImmutable
      (** Data must be immutable but not aliasable. In other words you can't
          mutate the data but you can mutate *through it*, e.g. if it points to
          a [&mut T]. This is only used in closure captures, e.g.
          {@rust[
            let mut z = 3;
            let x: &mut isize = &mut z;
            let y = || *x += 5;
          ]}
          Here the captured variable can't be [&mut &mut x] since the [x]
          binding is not mutable, yet we must be able to mutate what it points
          to.

          See
          <https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.MutBorrowKind.html#variant.ClosureCapture>.
      *)

(** For all the variants: the first type gives the source type, the second one
    gives the destination type. *)
and cast_kind =
  | CastScalar of literal_type * literal_type
      (** Conversion between types in [{Integer, Bool}] Remark: for now we don't
          support conversions with Char. *)
  | CastRawPtr of ty * ty
  | CastFnPtr of ty * ty
  | CastUnsize of ty * ty * unsizing_metadata
      (** [Unsize coercion](https://doc.rust-lang.org/std/ops/trait.CoerceUnsized.html).
          This is either [[T; N]] -> [[T]] or [T: Trait] -> [dyn Trait]
          coercions, behind a pointer (reference, [Box], or other type that
          implements [CoerceUnsized]).

          The special case of [&[T; N]] -> [&[T]] coercion is caught by
          [UnOp::ArrayToSlice]. *)
  | CastTransmute of ty * ty
      (** Reinterprets the bits of a value of one type as another type, i.e.
          exactly what [[std::mem::transmute]] does. *)

and constant_expr = { value : raw_constant_expr; ty : ty }

and field_proj_kind =
  | ProjAdt of type_decl_id * variant_id option
  | ProjTuple of int
      (** If we project from a tuple, the projection kind gives the arity of the
          tuple. *)

and local_id = (LocalId.id[@visitors.opaque])

(** Nullary operation *)
and nullop = SizeOf | AlignOf | OffsetOf of (int * field_id) list | UbChecks

and operand =
  | Copy of place
  | Move of place
  | Constant of constant_expr
      (** Constant value (including constant and static variables) *)

and overflow_mode =
  | OPanic
      (** If this operation overflows, it panics. Only exists in debug mode, for
          instance in [a + b], and is introduced by the [remove_dynamic_checks]
          pass. *)
  | OUB
      (** If this operation overflows, it UBs, for instance in
          [core::num::unchecked_add]. *)
  | OWrap
      (** If this operation overflows, it wraps around, for instance in
          [core::num::wrapping_add], or [a + b] in release mode. *)

and place = { kind : place_kind; ty : ty }

and place_kind =
  | PlaceLocal of local_id  (** A local variable in a function body. *)
  | PlaceProjection of place * projection_elem  (** A subplace of a place. *)
  | PlaceGlobal of global_decl_ref
      (** A global (const or static). Not present in MIR; introduced in
          [simplify_constants.rs]. *)

(** Note that we don't have the equivalent of "downcasts". Downcasts are
    actually necessary, for instance when initializing enumeration values: the
    value is initially [Bottom], and we need a way of knowing the variant. For
    example: [((_0 as Right).0: T2) = move _1;] In MIR, downcasts always happen
    before field projections: in our internal language, we thus merge downcasts
    and field projections. *)
and projection_elem =
  | Deref
      (** Dereference a shared/mutable reference, a box, or a raw pointer. *)
  | Field of field_proj_kind * field_id
      (** Projection from ADTs (variants, structures). We allow projections to
          be used as left-values and right-values. We should never have
          projections to fields of symbolic variants (they should have been
          expanded before through a match). *)
  | ProjIndex of operand * bool
      (** MIR imposes that the argument to an index projection be a local
          variable, meaning that even constant indices into arrays are let-bound
          as separate variables. We **eliminate** this variant in a micro-pass
          for LLBC.

          Fields:
          - [offset]
          - [from_end] *)
  | Subslice of operand * operand * bool
      (** Take a subslice of a slice or array. If [from_end] is [true] this is
          [slice[from..slice.len() - to]], otherwise this is [slice[from..to]].
          We **eliminate** this variant in a micro-pass for LLBC.

          Fields:
          - [from]
          - [to]
          - [from_end] *)

(** A constant expression.

    Only the [[RawConstantExpr::Literal]] and [[RawConstantExpr::Var]] cases are
    left in the final LLBC.

    The other cases come from a straight translation from the MIR:

    [[RawConstantExpr::Adt]] case: It is a bit annoying, but rustc treats some
    ADT and tuple instances as constants when generating MIR:
    - an enumeration with one variant and no fields is a constant.
    - a structure with no field is a constant.
    - sometimes, Rust stores the initialization of an ADT as a constant (if all
      the fields are constant) rather than as an aggregated value We later
      desugar those to regular ADTs, see [regularize_constant_adts.rs].

    [[RawConstantExpr::Global]] case: access to a global variable. We later
    desugar it to a copy of a place global.

    [[RawConstantExpr::Ref]] case: reference to a constant value. We later
    desugar it to a separate statement.

    [[RawConstantExpr::FnPtr]] case: a function pointer (to a top-level
    function).

    Remark: MIR seems to forbid more complex expressions like paths. For
    instance, reading the constant [a.b] is translated to
    [{ _1 = const a; _2 = (_1.0) }]. *)
and raw_constant_expr =
  | CLiteral of literal
  | CTraitConst of trait_ref * trait_item_name
      (** A trait constant.

          Ex.:
          {@rust[
            impl Foo for Bar {
              const C : usize = 32; // <-
            }
          ]}

          Remark: trait constants can not be used in types, they are necessarily
          values. *)
  | CVar of const_generic_var_id de_bruijn_var  (** A const generic var *)
  | CFnPtr of fn_ptr  (** Function pointer *)
  | CRawMemory of int list
      (** Raw memory value obtained from constant evaluation. Used when a more
          structured representation isn't possible (e.g. for unions) or just
          isn't implemented yet. *)
  | COpaque of string
      (** A constant expression that Charon still doesn't handle, along with the
          reason why. *)

(** TODO: we could factor out [Rvalue] and function calls (for LLBC, not ULLBC).
    We can also factor out the unops, binops with the function calls. TODO: move
    the aggregate kind to operands TODO: we should prefix the type variants with
    "R" or "Rv", this would avoid collisions *)
and rvalue =
  | Use of operand  (** Lifts an operand as an rvalue. *)
  | RvRef of place * borrow_kind  (** Takes a reference to the given place. *)
  | RawPtr of place * ref_kind
      (** Takes a raw pointer with the given mutability to the given place. This
          is generated by pointer casts like [&v as *const _] or raw borrow
          expressions like [&raw const v.] *)
  | BinaryOp of binop * operand * operand
      (** Binary operations (note that we merge "checked" and "unchecked"
          binops) *)
  | UnaryOp of unop * operand  (** Unary operation (e.g. not, neg) *)
  | NullaryOp of nullop * ty  (** Nullary operation (e.g. [size_of]) *)
  | Discriminant of place
      (** Discriminant read. Reads the discriminant value of an enum. The place
          must have the type of an enum.

          This case is filtered in [crate::transform::remove_read_discriminant]
      *)
  | Aggregate of aggregate_kind * operand list
      (** Creates an aggregate value, like a tuple, a struct or an enum:
          {@rust[
            l = List::Cons { value:x, tail:tl };
          ]}
          Note that in some MIR passes (like optimized MIR), aggregate values
          are decomposed, like below:
          {@rust[
            (l as List::Cons).value = x;
            (l as List::Cons).tail = tl;
          ]}
          Because we may want to plug our translation mechanism at various
          places, we need to take both into accounts in the translation and in
          our semantics. Aggregate value initialization is easy, you might want
          to have a look at expansion of [Bottom] values for explanations about
          the other case.

          Remark: in case of closures, the aggregated value groups the closure
          id together with its state. *)
  | Len of place * ty * const_generic option
      (** Length of a memory location. The run-time length of e.g. a vector or a
          slice is represented differently (but pretty-prints the same, FIXME).
          Should be seen as a function of signature:
          - [fn<T;N>(&[T;N]) -> usize]
          - [fn<T>(&[T]) -> usize]

          We store the type argument and the const generic (the latter only for
          arrays).

          [Len] is automatically introduced by rustc, notably for the bound
          checks: we eliminate it together with the bounds checks whenever
          possible. There are however occurrences that we don't eliminate (yet).
          For instance, for the following Rust code:
          {@rust[
            fn slice_pattern_4(x: &[()]) {
                match x {
                    [_named] => (),
                    _ => (),
                }
            }
          ]}
          rustc introduces a check that the length of the slice is exactly equal
          to 1 and that we preserve. *)
  | Repeat of operand * ty * const_generic
      (** [Repeat(x, n)] creates an array where [x] is copied [n] times.

          We translate this to a function call for LLBC. *)
  | ShallowInitBox of operand * ty
      (** Transmutes a [*mut u8] (obtained from [malloc]) into
          shallow-initialized [Box<T>]. This only appears as part of lowering
          [Box::new()] in some cases. We reconstruct the original [Box::new()]
          call, but sometimes may fail to do so, leaking the expression. *)

(** Unary operation *)
and unop =
  | Not
  | Neg of overflow_mode  (** This can overflow, for [-i::MIN]. *)
  | PtrMetadata
      (** Retreive the metadata part of a fat pointer. For slices, this
          retreives their length. *)
  | Cast of cast_kind
      (** Casts are rvalues in MIR, but we treat them as unops. *)

and unsizing_metadata =
  | MetaLength of const_generic
  | MetaVTablePtr of trait_ref
  | MetaUnknown
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_rvalue";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_type_decl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_rvalue";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_type_decl" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]
