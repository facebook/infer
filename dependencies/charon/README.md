<p><div style="text-align: center">
<img src="static/Charon.jpg"
     alt="Landscape with Charon crossing the Styx" title="Landscape with Charon crossing the Styx"
     style=""/>
<figcaption>
Patinir, E., 1515-1524, <i>Landscape with Charon crossing the Styx</i> [Oil on wood].
Museo del Prado, Madrid.
<a href="https://en.wikipedia.org/wiki/Landscape_with_Charon_Crossing_the_Styx">Source</a>
</figcaption>
</div></p>

# Charon

Charon (pronounced "Ka-ron") extracts the complete contents of a Rust crate (and its dependencies) into a JSON file:

```json
{
  "crate_name": "...",
  "type_decls": [ ... ],
  "fun_decls": [ ... ],
  "global_decls": [ ... ],
  "trait_decls": [ ... ],
  "trait_impls": [ ... ],
}
```

The output contains the types, functions and traits of the crate and its dependencies, the
simplified MIR bodies of functions, source information for each item, and other semantic information described below.

We are **open to contributions**! Please contact us so that we can coordinate ourselves, if you are
willing to contribute. Discussions happen on [Zulip](https://aeneas-verif.zulipchat.com/).

## Usage

Run `charon` inside the crate of interest, much like you would call `cargo build`. It will produce
a `crate_name.llbc` file.

The `charon-lib` crate can read this file and let you manipulate its contents. Parse the file using
`serde_json::from_reader::<charon_lib::export::CrateData>(file)`. OCaml bindings are also available
in the `charon-ml` folder.

For more detailed usage instructions, see the [documentation](./docs/usage.md).

## Why Charon?

The output of Charon looks simple but constructing the relevant information is hard. The purpose of
Charon is to centralize the efforts of extracting information from rustc internals and turning them
into a uniform and usable shape. See the [documentation](./docs/what_charon_does_for_you.md) for an overview of what
Charon has to do today.

If you're writing a rustc driver, chances are you could use Charon instead. Charon is however geared
towards semantic analyses; it likely won't help you for syntactic analyses like many lints.

Charon is, in Greek mythology, an old man carrying the souls of the deceased accross the Styx,
a river separating the world of the living from the world of the dead. In the present context,
Charon allows us to cross the river of compiler internals to go from the world of Rust programs to
the world of code analysis and verification.

## Limitations

Charon is alpha software. While it works quite well for a large number of crates, it has not reached
the full set of features we intend, incorrectly translates code in some edge cases, and a number of
breaking changes in its API are planned. See the [limitations](./docs/limitations.md) for details.

## Installation & Build

If you use nix, you can directly run `nix run github:AeneasVerif/charon`. Otherwise, read on.

You first need to install [`rustup`](https://www.rust-lang.org/tools/install).

As Charon is set up with cargo, rustup will automatically download and install the proper packages
upon building the project. If you only want to build the Rust project (in `./charon`), run `make
build-charon-rust` in the root directory. The resulting `charon` binary can be found in `bin/charon`,
from where you can use it.

If you also want to build the ML library (in `./charon-ml`), you will need to
install OCaml and the proper dependencies.

We suggest you to follow those [instructions](https://ocaml.org/docs/install.html),
and install OPAM on the way (same instructions).

For Charon-ML, we use **OCaml 4.14.0**: `opam switch create 4.14.0+options`

The dependencies can be installed with `opam install . --deps-only`.

You can then run `make build-charon-ml` to build the ML library, or even simply
`make` to build the whole project (Rust and OCaml). Finally, you can run the
tests with `make test`.

Alternatively, you can use Nix and do `nix develop` and all dependencies should be made available.

## Documentation

You can access the (work-in-progress) Rust documentation
[online](https://aeneasverif.github.io/charon/charon_lib/index.html).

You can also run `make doc` to generate the documentation locally.
It will generate a documentation accessible from
[`doc-rust.html`](./doc-rust.html) (for the Rust project) and
[`doc-ml.html`](./doc-ml.html) (for the ML library).
