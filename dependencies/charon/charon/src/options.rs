//! The options that control charon behavior.
use annotate_snippets::Level;
use clap::ValueEnum;
use indoc::indoc;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use crate::{
    ast::*,
    errors::{ErrorCtx, display_unspanned_error},
    name_matcher::NamePattern,
    raise_error, register_error,
};

/// The name of the environment variable we use to save the serialized Cli options
/// when calling charon-driver from cargo-charon.
pub const CHARON_ARGS: &str = "CHARON_ARGS";

// This structure is used to store the command-line instructions.
// We automatically derive a command-line parser based on this structure.
// Note that the doc comments are used to generate the help message when using
// `--help`.
//
// Note that because we need to transmit the options to the charon driver,
// we store them in a file before calling this driver (hence the `Serialize`,
// `Deserialize` options).
#[derive(Debug, Default, Clone, clap::Args, PartialEq, Eq, Serialize, Deserialize)]
#[clap(name = "Charon")]
#[charon::rename("cli_options")]
pub struct CliOpts {
    /// Extract the unstructured LLBC (i.e., don't reconstruct the control-flow)
    #[clap(long = "ullbc")]
    #[serde(default)]
    pub ullbc: bool,
    /// Compile the package's library
    #[clap(long = "lib")]
    #[serde(default)]
    pub lib: bool,
    /// Compile the specified binary
    #[clap(long = "bin")]
    #[serde(default)]
    pub bin: Option<String>,
    /// Deprecated: use `--mir promoted` instead.
    #[clap(long = "mir_promoted")]
    #[serde(default)]
    pub mir_promoted: bool,
    /// Deprecated: use `--mir optimized` instead.
    #[clap(long = "mir_optimized")]
    #[serde(default)]
    pub mir_optimized: bool,
    /// The MIR stage to extract. This is only relevant for the current crate; for dpendencies only
    /// MIR optimized is available.
    #[arg(long)]
    pub mir: Option<MirLevel>,
    /// The input file (the entry point of the crate to extract).
    /// This is needed if you want to define a custom entry point (to only
    /// extract part of a crate for instance).
    #[clap(long = "input", value_parser)]
    #[serde(default)]
    pub input_file: Option<PathBuf>,
    /// Read an llbc file and pretty-print it. This is a terrible API, we should use subcommands.
    #[clap(long = "read-llbc", value_parser)]
    #[serde(default)]
    pub read_llbc: Option<PathBuf>,
    /// The destination directory. Files will be generated as `<dest_dir>/<crate_name>.{u}llbc`,
    /// unless `dest_file` is set. `dest_dir` defaults to the current directory.
    #[clap(long = "dest", value_parser)]
    #[serde(default)]
    pub dest_dir: Option<PathBuf>,
    /// The destination file. By default `<dest_dir>/<crate_name>.llbc`. If this is set we ignore
    /// `dest_dir`.
    #[clap(long = "dest-file", value_parser)]
    #[serde(default)]
    pub dest_file: Option<PathBuf>,
    /// If activated, use Polonius' non-lexical lifetimes (NLL) analysis.
    /// Otherwise, use the standard borrow checker.
    #[clap(long = "polonius")]
    #[serde(default)]
    pub use_polonius: bool,
    /// If activated, this skips borrow-checking of the crate.
    #[clap(long = "skip-borrowck")]
    #[serde(default)]
    pub skip_borrowck: bool,
    /// Monomorphize the items encountered when possible. Generic items found in the crate are
    /// translated as normal. To only translate a particular call graph, use `--start-from`. This
    /// uses a different mechanism than `--monomorphize-conservative` which should be a lot more
    /// complete, but doesn't currently support `dyn Trait`.
    #[clap(long, visible_alias = "mono")]
    #[serde(default)]
    pub monomorphize: bool,
    /// Monomorphize the code, replacing generics with their concrete types. This is less complete
    /// than `--monomorphize` but at least doesn't crash on `dyn Trait`. This will eventually be
    /// fully replaced with `--monomorphized`.
    #[clap(long)]
    #[serde(default)]
    pub monomorphize_conservative: bool,
    /// Usually we skip the bodies of foreign methods and structs with private fields. When this
    /// flag is on, we don't.
    #[clap(long = "extract-opaque-bodies")]
    #[serde(default)]
    pub extract_opaque_bodies: bool,
    /// Usually we skip the provided methods that aren't used. When this flag is on, we translate
    /// them all.
    #[clap(long = "translate-all-methods")]
    #[serde(default)]
    pub translate_all_methods: bool,
    /// Whitelist of items to translate. These use the name-matcher syntax.
    #[clap(
        long = "include",
        help = indoc!("
            Whitelist of items to translate. These use the name-matcher syntax (note: this differs
            a bit from the ocaml NameMatcher).

            Note: This is very rough at the moment. E.g. this parses `u64` as a path instead of the
            built-in type. It is also not possible to filter a trait impl (this will only filter
            its methods). Please report bugs or missing features.

            Examples:
              - `crate::module1::module2::item`: refers to this item and all its subitems (e.g.
                  submodules or trait methods);
              - `crate::module1::module2::item::_`: refers only to the subitems of this item;
              - `core::convert::{impl core::convert::Into<_> for _}`: retrieve the body of this
                  very useful impl;

            When multiple patterns in the `--include` and `--opaque` options match the same item,
            the most precise pattern wins. E.g.: `charon --opaque crate::module --include
            crate::module::_` makes the `module` opaque (we won't explore its contents), but the
            items in it transparent (we will translate them if we encounter them.)
    "))]
    #[serde(default)]
    #[charon::rename("included")]
    pub include: Vec<String>,
    /// Blacklist of items to keep opaque. These use the name-matcher syntax.
    #[clap(
        long = "opaque",
        help = "Blacklist of items to keep opaque. Works just like `--include`, see the doc there."
    )]
    #[serde(default)]
    pub opaque: Vec<String>,
    /// Blacklist of items to not translate at all. These use the name-matcher syntax.
    #[clap(
        long = "exclude",
        help = "Blacklist of items to not translate at all. Works just like `--include`, see the doc there."
    )]
    #[serde(default)]
    pub exclude: Vec<String>,
    /// List of traits for which we transform associated types to type parameters.
    #[clap(
        long = "remove-associated-types",
        help = "List of traits for which we transform associated types to type parameters. \
        The syntax is like `--include`, see the doc there."
    )]
    #[serde(default)]
    pub remove_associated_types: Vec<String>,
    /// Whether to hide the `Sized`, `Sync`, `Send` and `Unpin` marker traits anywhere they show
    /// up.
    #[clap(long = "hide-marker-traits")]
    #[serde(default)]
    pub hide_marker_traits: bool,
    /// Hide the `A` type parameter on standard library containers (`Box`, `Vec`, etc).
    #[clap(long)]
    #[serde(default)]
    pub hide_allocator: bool,
    /// Trait method declarations take a `Self: Trait` clause as parameter, so that they can be
    /// reused by multiple trait impls. This however causes trait definitions to be mutually
    /// recursive with their method declarations. This flag removes `Self` clauses that aren't used
    /// to break this mutual recursion.
    #[clap(long)]
    #[serde(default)]
    pub remove_unused_self_clauses: bool,
    /// Whether to add `Drop` bounds everywhere to enable proper tracking of what code runs on a
    /// given `drop` call.
    #[clap(long)]
    #[serde(default)]
    pub add_drop_bounds: bool,
    /// A list of item paths to use as starting points for the translation. We will translate these
    /// items and any items they refer to, according to the opacity rules. When absent, we start
    /// from the path `crate` (which translates the whole crate).
    #[clap(long = "start-from")]
    #[serde(default)]
    pub start_from: Vec<String>,
    /// Do not run cargo; instead, run the driver directly.
    #[clap(long = "no-cargo")]
    #[serde(default)]
    pub no_cargo: bool,
    /// Extra flags to pass to rustc.
    #[clap(long = "rustc-flag", alias = "rustc-arg")]
    #[serde(default)]
    pub rustc_args: Vec<String>,
    /// Extra flags to pass to cargo. Incompatible with `--no-cargo`.
    #[clap(long = "cargo-arg")]
    #[serde(default)]
    pub cargo_args: Vec<String>,
    /// Panic on the first error. This is useful for debugging.
    #[clap(long = "abort-on-error")]
    #[serde(default)]
    pub abort_on_error: bool,
    /// Print the errors as warnings
    #[clap(long = "error-on-warnings", help = "Consider any warnings as errors")]
    #[serde(default)]
    pub error_on_warnings: bool,
    #[clap(
        long = "no-serialize",
        help = "Don't serialize the final (U)LLBC to a file."
    )]
    #[serde(default)]
    pub no_serialize: bool,
    #[clap(
        long = "print-original-ullbc",
        help = "Print the ULLBC immediately after extraction from MIR."
    )]
    #[serde(default)]
    pub print_original_ullbc: bool,
    #[clap(
        long = "print-ullbc",
        help = "Print the ULLBC after applying the micro-passes (before serialization/control-flow reconstruction)."
    )]
    #[serde(default)]
    pub print_ullbc: bool,
    #[clap(
        long = "print-built-llbc",
        help = "Print the LLBC just after we built it (i.e., immediately after loop reconstruction)."
    )]
    #[serde(default)]
    pub print_built_llbc: bool,
    #[clap(
        long = "print-llbc",
        help = "Print the final LLBC (after all the cleaning micro-passes)."
    )]
    #[serde(default)]
    pub print_llbc: bool,
    #[clap(
        long = "no-merge-goto-chains",
        help = "Do not merge the chains of gotos in the ULLBC control-flow graph."
    )]
    #[serde(default)]
    pub no_merge_goto_chains: bool,

    #[clap(
        long = "no-ops-to-function-calls",
        help = "Do not transform ArrayToSlice, Repeat, and RawPtr aggregates to builtin function calls for ULLBC"
    )]
    #[serde(default)]
    pub no_ops_to_function_calls: bool,

    #[clap(
        long = "raw-boxes",
        help = "Do not special-case the translation of `Box<T>` into a builtin ADT."
    )]
    #[serde(default)]
    pub raw_boxes: bool,

    /// Named builtin sets of options. Currently used only for dependent projects, eveentually
    /// should be replaced with semantically-meaningful presets.
    #[clap(long = "preset")]
    #[arg(value_enum)]
    pub preset: Option<Preset>,
}

/// The MIR stage to use. This is only relevant for the current crate: for dependencies, only mir
/// optimized is available (or mir elaborated for consts).
#[derive(
    Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Serialize, Deserialize,
)]
pub enum MirLevel {
    /// The MIR just after MIR lowering.
    #[default]
    Built,
    /// The MIR after const promotion. This is the MIR used by the borrow-checker.
    Promoted,
    /// The MIR after drop elaboration. This is the first MIR to include all the runtime
    /// information.
    Elaborated,
    /// The MIR after optimizations. Charon disables all the optimizations it can, so this is
    /// sensibly the same MIR as the elaborated MIR.
    Optimized,
}

/// Presets to make it easier to tweak options without breaking dependent projects. Eventually we
/// should define semantically-meaningful presets instead of project-specific ones.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Serialize, Deserialize)]
#[non_exhaustive]
pub enum Preset {
    /// The default translation used before May 2025. After that, many passes were made optional
    /// and disabled by default.
    OldDefaults,
    Aeneas,
    Eurydice,
    Soteria,
    Tests,
}

impl CliOpts {
    pub fn apply_preset(&mut self) {
        if let Some(preset) = self.preset {
            match preset {
                Preset::OldDefaults => {
                    self.hide_allocator = !self.raw_boxes;
                }
                Preset::Aeneas => {
                    self.remove_associated_types.push("*".to_owned());
                    self.hide_marker_traits = true;
                    self.hide_allocator = true;
                    self.remove_unused_self_clauses = true;
                    // Hide drop impls because they often involve nested borrows. which aeneas
                    // doesn't handle yet.
                    self.exclude.push("core::ops::drop::Drop".to_owned());
                    self.exclude
                        .push("{impl core::ops::drop::Drop for _}".to_owned());
                }
                Preset::Eurydice => {
                    self.hide_allocator = true;
                    self.remove_associated_types.push("*".to_owned());
                }
                Preset::Soteria => {
                    self.extract_opaque_bodies = true;
                    self.monomorphize = true;
                    self.raw_boxes = true;
                    self.mir = Some(MirLevel::Elaborated);
                    self.ullbc = true;
                }
                Preset::Tests => {
                    self.hide_allocator = !self.raw_boxes;
                    self.rustc_args.push("--edition=2021".to_owned());
                    self.exclude.push("core::fmt::Formatter".to_owned());
                }
            }
        }
    }

    /// Check that the options are meaningful
    pub fn validate(&self) {
        assert!(
            !self.lib || self.bin.is_none(),
            "Can't use --lib and --bin at the same time"
        );

        assert!(
            !self.mir_promoted || !self.mir_optimized,
            "Can't use --mir_promoted and --mir_optimized at the same time"
        );

        if self.input_file.is_some() {
            display_unspanned_error(
                Level::WARNING,
                "`--input` is deprecated, use `charon rustc [charon options] -- [rustc options] <input file>` instead",
            )
        }
        if self.no_cargo {
            display_unspanned_error(
                Level::WARNING,
                "`--no-cargo` is deprecated, use `charon rustc [charon options] -- [rustc options] <input file>` instead",
            )
        }
        if self.read_llbc.is_some() {
            display_unspanned_error(
                Level::WARNING,
                "`--read_llbc` is deprecated, use `charon pretty-print <input file>` instead",
            )
        }
        if self.use_polonius {
            display_unspanned_error(
                Level::WARNING,
                "`--polonius` is deprecated, use `--rustc-arg=-Zpolonius` instead",
            )
        }
        if self.mir_optimized {
            display_unspanned_error(
                Level::WARNING,
                "`--mir_optimized` is deprecated, use `--mir optimized` instead",
            )
        }
        if self.mir_promoted {
            display_unspanned_error(
                Level::WARNING,
                "`--mir_promoted` is deprecated, use `--mir promoted` instead",
            )
        }
        if self.lib {
            display_unspanned_error(
                Level::WARNING,
                "`--lib` is deprecated, use `charon cargo -- --lib` instead",
            )
        }
        if self.bin.is_some() {
            display_unspanned_error(
                Level::WARNING,
                "`--bin` is deprecated, use `charon cargo -- --bin <name>` instead",
            )
        }
        if self.dest_dir.is_some() {
            display_unspanned_error(
                Level::WARNING,
                "`--dest` is deprecated, use `--dest-file` instead",
            )
        }
    }
}

/// The options that control translation and transformation.
pub struct TranslateOptions {
    /// The level at which to extract the MIR
    pub mir_level: MirLevel,
    /// Usually we skip the provided methods that aren't used. When this flag is on, we translate
    /// them all.
    pub translate_all_methods: bool,
    /// Whether to hide the `Sized`, `Sync`, `Send` and `Unpin` marker traits anywhere they show
    /// up.
    pub hide_marker_traits: bool,
    /// Hide the `A` type parameter on standard library containers (`Box`, `Vec`, etc).
    pub hide_allocator: bool,
    /// Remove unused `Self: Trait` clauses on method declarations.
    pub remove_unused_self_clauses: bool,
    /// Monomorphize functions as a post-processing pass.
    pub monomorphize_as_pass: bool,
    /// Monomorphize code using hax's instantiation mechanism.
    pub monomorphize_with_hax: bool,
    /// Transforms ArrayToSlice, Repeat, and RawPtr aggregates to builtin function calls.
    pub no_ops_to_function_calls: bool,
    /// Do not merge the chains of gotos.
    pub no_merge_goto_chains: bool,
    /// Print the llbc just after control-flow reconstruction.
    pub print_built_llbc: bool,
    /// Don't special-case the translation of `Box<T>`
    pub raw_boxes: bool,
    /// List of patterns to assign a given opacity to. Same as the corresponding `TranslateOptions`
    /// field.
    pub item_opacities: Vec<(NamePattern, ItemOpacity)>,
    /// List of traits for which we transform associated types to type parameters.
    pub remove_associated_types: Vec<NamePattern>,
}

impl TranslateOptions {
    pub fn new(error_ctx: &mut ErrorCtx, options: &CliOpts) -> Self {
        let mut parse_pattern = |s: &str| match NamePattern::parse(s) {
            Ok(p) => Ok(p),
            Err(e) => {
                raise_error!(
                    error_ctx,
                    crate(&TranslatedCrate::default()),
                    Span::dummy(),
                    "failed to parse pattern `{s}` ({e})"
                )
            }
        };

        let mir_level = if options.mir_optimized {
            MirLevel::Optimized
        } else if options.mir_promoted {
            MirLevel::Promoted
        } else {
            options.mir.unwrap_or_default()
        };

        let item_opacities = {
            use ItemOpacity::*;
            let mut opacities = vec![];

            // This is how to treat items that don't match any other pattern.
            if options.extract_opaque_bodies {
                opacities.push(("_".to_string(), Transparent));
            } else {
                opacities.push(("_".to_string(), Foreign));
            }

            // We always include the items from the crate.
            opacities.push(("crate".to_owned(), Transparent));

            for pat in options.include.iter() {
                opacities.push((pat.to_string(), Transparent));
            }
            for pat in options.opaque.iter() {
                opacities.push((pat.to_string(), Opaque));
            }
            for pat in options.exclude.iter() {
                opacities.push((pat.to_string(), Invisible));
            }

            if options.hide_allocator {
                opacities.push((format!("core::alloc::Allocator"), Invisible));
                opacities.push((
                    format!("alloc::alloc::{{impl core::alloc::Allocator for _}}"),
                    Invisible,
                ));
            }

            opacities
                .into_iter()
                .filter_map(|(s, opacity)| parse_pattern(&s).ok().map(|pat| (pat, opacity)))
                .collect()
        };

        let remove_associated_types = options
            .remove_associated_types
            .iter()
            .filter_map(|s| parse_pattern(&s).ok())
            .collect();

        TranslateOptions {
            mir_level,
            hide_marker_traits: options.hide_marker_traits,
            hide_allocator: options.hide_allocator,
            remove_unused_self_clauses: options.remove_unused_self_clauses,
            monomorphize_as_pass: options.monomorphize_conservative,
            monomorphize_with_hax: options.monomorphize,
            no_merge_goto_chains: options.no_merge_goto_chains,
            no_ops_to_function_calls: options.no_ops_to_function_calls,
            print_built_llbc: options.print_built_llbc,
            item_opacities,
            raw_boxes: options.raw_boxes,
            remove_associated_types,
            translate_all_methods: options.translate_all_methods,
        }
    }

    /// Find the opacity requested for the given name. This does not take into account
    /// `#[charon::opaque]` annotations, only cli parameters.
    #[tracing::instrument(skip(self, krate), ret)]
    pub fn opacity_for_name(&self, krate: &TranslatedCrate, name: &Name) -> ItemOpacity {
        // Find the most precise pattern that matches this name. There is always one since
        // the list contains the `_` pattern. If there are conflicting settings for this item, we
        // err on the side of being more opaque.
        let (_, opacity) = self
            .item_opacities
            .iter()
            .filter(|(pat, _)| pat.matches(krate, name))
            .max()
            .unwrap();
        *opacity
    }
}
