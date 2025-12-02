# What Charon Does For You

The purpose of Charon is to centralize the efforts of extracting information from rustc internals
and turning them into a uniform and usable shape. Here are some things that Charon does once so you
don't have to do it yourself:

TODO: explain each item

- Trait resolution (explicitly track how each trait bound was proven to hold);
- Reconstruct expressions from all the various constant representations;
- Hide the distinction between early- and late-bound lifetime variables;
- Make non-overriden default methods in impl blocks appear as normal methods;
- Handle trait method implementations that have a more general signature than as declared in the trait (WIP: https://github.com/AeneasVerif/charon/issues/513);
- Represent closures as normal structs that implement the `Fn*` traits;
- Represent VTables as normal structs stored in statics (WIP: see https://github.com/AeneasVerif/charon/issues/123);
- Many useful post-processing transformations to make the output more usable; see [the dedicated file](./transformations.md).
