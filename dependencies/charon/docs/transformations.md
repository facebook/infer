# Charon transformations

Charon has options to make the generated code easier to use. Some are currently not optional but
could be made so on request.

## Control-flow reconstruction

The MIR we get from rustc is a control-flow graph (CFG), where control-flow happens with gotos that
can jump arbitrarily around the code.

Charon by default transforms this CFG into structured control-flow, i.e. with nested
`match`/`if`/`loop` constructs.

To turn this off, pass `--ullbc` to `charon` (ULLBC stands for Unstructured LLBC).

## `--hide-marker-traits`

This option removes mentions of the following built-in traits from the output crate:
- `core::marker::Sized`
- `core::marker::Tuple`
- `core::marker::Send`
- `core::marker::Sync`
- `core::marker::Unpin`

This is a convenience option, meant to reduce noise for Charon consumers that output user-readable
things.

Note that for historical reasons Charon removes mentions of the `Allocator` trait regardless of this
setting. This should eventually be changed, open an issue if you have a need for it.

Note that today this is implemented by calling `Vector::remove` to remove the entry in the
`trait_clauses`/`trait_refs` vectors. This abuses the confusion between newtype-indexed vectors and
newtype-indexed maps we have in Charon, which can cause annoying footguns. See
https://github.com/AeneasVerif/charon/issues/490.

## `--remove-associated-types`

This option takes a pattern, and transforms the associated types of all the traits that match the
pattern. This transforms code like:
```rust
trait Iterator {
  type Item;
  fn next(&mut self) -> Option<Self::Item>;
}
fn use_iterator<I: Iterator>(it: I) { ... }
```
into:
```rust
trait Iterator<Item> {
  fn next(&mut self) -> Option<Item>;
}
fn use_iterator<I: Iterator<Clause0Item>, Clause0Item>(it: I) { ... }
```

This transformation has limitations:
- GATs cannot be transformed this way;
- Some recursive traits cannot be transformed this way, e.g.:
```rust
trait Bar {
    type BarTy;
}
trait Foo {
    type FooTy: Foo + Bar;
}
// becomes:
trait Bar<BarTy> {}
trait Foo {
    type FooTy: Foo + Bar<Self::FooTy_BarTy>;
    // We need to supply an argument to `Bar` but we can't add a type parameter, so we add a
    // new associated type.
    type FooTy_BarTy;
}
```
- We're currently missing assoc type information to transform `dyn Trait` (https://github.com/AeneasVerif/charon/issues/123).
- We currently don't track bound lifetimes in quantified clauses properly (https://github.com/AeneasVerif/charon/issues/534).
- Type aliases don't have the correct clauses in scope (https://github.com/AeneasVerif/charon/issues/531).
- We don't take into account unicity of trait implementations. This means we won't detect type
  equalities due to the same trait predicate appearing twice, or a trait predicate coinciding with
  an existing trait impl. See the `dictionary_passing_style_woes.rs` test file for an example.

## Make implied bounds explicit

WIP: https://github.com/AeneasVerif/charon/issues/585
