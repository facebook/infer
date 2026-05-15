# hax adt into

This crate provides the `adt_into` procedural macro, allowing for
mirroring data types with small variations.

This crate is used by the frontend of hax, where we need to mirror a
big part of the data types defined by the Rust compiler. While the
abstract syntax trees (ASTs) from the Rust compiler expose a lot of
indirections (identifiers one should lookup, additional informations
reachable only via interactive queries), hax exposes the same ASTs,
removing indirections and inlining additional informations.

The `adt_into` derive macro can be used on `struct`s and `enum`s. `adt_into` then looks for another `#[args(<GENERICS>, from: FROM_TYPE, state: STATE_TYPE as SOME_NAME)]` attribute. Such an attribute means that the `struct` or `enum` mirrors the type `FROM_TYPE`, and that the transformation is carried along with a state of type `STATE_TYPE` that will be accessible via the name `SOME_NAME`.

An example is available in the `tests` folder.
