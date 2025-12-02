//! Run the rustc compiler with our custom options and hooks.
use crate::CharonFailure;
use crate::translate::translate_crate;
use charon_lib::options::CliOpts;
use charon_lib::transform::TransformCtx;
use rustc_driver::{Callbacks, Compilation};
use rustc_interface::Config;
use rustc_interface::interface::Compiler;
use rustc_middle::ty::TyCtxt;
use rustc_middle::util::Providers;
use rustc_session::config::{OutputType, OutputTypes, Polonius};
use std::ops::Deref;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{env, fmt};

/// Helper that runs the compiler and catches its fatal errors.
fn run_compiler_with_callbacks(
    args: Vec<String>,
    callbacks: &mut (dyn Callbacks + Send),
) -> Result<(), CharonFailure> {
    rustc_driver::catch_fatal_errors(|| rustc_driver::run_compiler(&args, callbacks))
        .map_err(|_| CharonFailure::RustcError)
}

/// Tweak options to get usable MIR even for foreign crates.
fn set_mir_options(config: &mut Config) {
    config.opts.unstable_opts.always_encode_mir = true;
    config.opts.unstable_opts.mir_opt_level = Some(0);
    config.opts.unstable_opts.mir_emit_retag = true;
    let disabled_mir_passes = ["CheckAlignment"];
    for pass in disabled_mir_passes {
        config
            .opts
            .unstable_opts
            .mir_enable_passes
            .push((pass.to_owned(), false));
    }
}

/// Don't even try to codegen. This avoids errors due to checking if the output filename is
/// available (despite the fact that we won't emit it because we stop compilation early).
fn set_no_codegen(config: &mut Config) {
    config.opts.unstable_opts.no_codegen = true;
    // Only emit metadata.
    config.opts.output_types = OutputTypes::new(&[(OutputType::Metadata, None)]);
}

// We use a static to be able to pass data to `override_queries`.
static SKIP_BORROWCK: AtomicBool = AtomicBool::new(false);
fn set_skip_borrowck() {
    SKIP_BORROWCK.store(true, Ordering::SeqCst);
}
fn skip_borrowck_if_set(providers: &mut Providers) {
    if SKIP_BORROWCK.load(Ordering::SeqCst) {
        providers.mir_borrowck = |tcx, _def_id| {
            // Empty result, which is what is used for custom_mir bodies.
            Ok(tcx.arena.alloc(Default::default()))
        }
    }
}

fn setup_compiler(config: &mut Config, options: &CliOpts, do_translate: bool) {
    if do_translate {
        if options.skip_borrowck {
            // We use a static to be able to pass data to `override_queries`.
            set_skip_borrowck();
        }

        config.override_queries = Some(|_sess, providers| {
            skip_borrowck_if_set(providers);

            // TODO: catch the MIR in-flight to avoid stealing issues?
            // providers.mir_built = |tcx, def_id| {
            //     let mir = (rustc_interface::DEFAULT_QUERY_PROVIDERS.mir_built)(tcx, def_id);
            //     let mut mir = mir.steal();
            //     // use the mir
            //     tcx.alloc_steal_mir(mir)
            // };
        });

        set_no_codegen(config);
        if options.use_polonius {
            config.opts.unstable_opts.polonius = Polonius::Legacy;
        }
    }
    set_mir_options(config);
}

/// Run the rustc driver with our custom hooks. Returns `None` if the crate was not compiled with
/// charon (e.g. because it was a dependency). Otherwise returns the translated crate, ready for
/// post-processing transformations.
pub fn run_rustc_driver(options: &CliOpts) -> Result<Option<TransformCtx>, CharonFailure> {
    // Retreive the command-line arguments pased to `charon_driver`. The first arg is the path to
    // the current executable, we skip it.
    let mut compiler_args: Vec<String> = env::args().skip(1).collect();

    // When called using cargo, we tell cargo to use `charon-driver` by setting the `RUSTC_WRAPPER`
    // env var. This uses `charon-driver` for all the crates being compiled.
    // We may however not want to be calling charon on all crates; `CARGO_PRIMARY_PACKAGE` tells us
    // whether the crate was specifically selected or is a dependency.
    let is_workspace_dependency =
        env::var("CHARON_USING_CARGO").is_ok() && !env::var("CARGO_PRIMARY_PACKAGE").is_ok();
    // Determines if we are being invoked to build a crate for the "target" architecture, in
    // contrast to the "host" architecture. Host crates are for build scripts and proc macros and
    // still need to be built like normal; target crates need to be processed by Charon.
    //
    // Currently, we detect this by checking for "--target=", which is never set for host crates.
    // This matches what Miri does, which hopefully makes it reliable enough. This relies on us
    // always invoking cargo itself with `--target`, which `charon` ensures.
    let is_target = arg_values(&compiler_args, "--target").next().is_some();
    // Whether this is the crate we want to translate.
    let is_selected_crate = !is_workspace_dependency && is_target;

    let output = if !is_selected_crate {
        trace!("Skipping charon; running compiler normally instead.");
        // Run the compiler normally.
        run_compiler_with_callbacks(compiler_args, &mut RunCompilerNormallyCallbacks { options })?;
        None
    } else {
        for extra_flag in options.rustc_args.iter().cloned() {
            compiler_args.push(extra_flag);
        }

        // Call the Rust compiler with our custom callback.
        let mut callback = CharonCallbacks {
            options,
            transform_ctx: None,
        };
        run_compiler_with_callbacks(compiler_args, &mut callback)?;
        // If `transform_ctx` is not set here, there was a fatal error.
        let ctx = callback.transform_ctx.ok_or(CharonFailure::RustcError)?;
        Some(ctx)
    };
    Ok(output)
}

/// The callbacks for Charon
pub struct CharonCallbacks<'a> {
    options: &'a CliOpts,
    /// This is to be filled during the extraction; it contains the translated crate.
    transform_ctx: Option<TransformCtx>,
}
impl<'a> Callbacks for CharonCallbacks<'a> {
    fn config(&mut self, config: &mut Config) {
        setup_compiler(config, self.options, true);
    }

    /// The MIR is modified in place: borrow-checking requires the "promoted" MIR, which causes the
    /// "built" MIR (which results from the conversion to HIR to MIR) to become unaccessible.
    /// Because we require built MIR at the moment, we hook ourselves before MIR-based analysis
    /// passes.
    fn after_expansion<'tcx>(&mut self, compiler: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        // Set up our own `DefId` debug routine.
        rustc_hir::def_id::DEF_ID_DEBUG
            .swap(&(def_id_debug as fn(_, &mut fmt::Formatter<'_>) -> _));

        let transform_ctx = translate_crate::translate(
            &self.options,
            tcx,
            compiler.sess.opts.sysroot.path().to_owned(),
        );
        self.transform_ctx = Some(transform_ctx);
        Compilation::Continue
    }
    fn after_analysis<'tcx>(&mut self, _: &Compiler, _: TyCtxt<'tcx>) -> Compilation {
        // Don't continue to codegen etc.
        Compilation::Stop
    }
}

/// Dummy callbacks used to run the compiler normally when we shouldn't be analyzing the crate.
pub struct RunCompilerNormallyCallbacks<'a> {
    options: &'a CliOpts,
}
impl<'a> Callbacks for RunCompilerNormallyCallbacks<'a> {
    fn config(&mut self, config: &mut Config) {
        setup_compiler(config, self.options, false);
    }
}

/// Returns the values of the command-line options that match `find_arg`. The options are built-in
/// to be of the form `--arg=value` or `--arg value`.
fn arg_values<'a, T: Deref<Target = str>>(
    args: &'a [T],
    needle: &'a str,
) -> impl Iterator<Item = &'a str> {
    struct ArgFilter<'a, T> {
        args: std::slice::Iter<'a, T>,
        needle: &'a str,
    }
    impl<'a, T: Deref<Target = str>> Iterator for ArgFilter<'a, T> {
        type Item = &'a str;
        fn next(&mut self) -> Option<Self::Item> {
            while let Some(arg) = self.args.next() {
                let mut split_arg = arg.splitn(2, '=');
                if split_arg.next() == Some(self.needle) {
                    return match split_arg.next() {
                        // `--arg=value` form
                        arg @ Some(_) => arg,
                        // `--arg value` form
                        None => self.args.next().map(|x| x.deref()),
                    };
                }
            }
            None
        }
    }
    ArgFilter {
        args: args.iter(),
        needle,
    }
}

/// Custom `DefId` debug routine that doesn't print unstable values like ids and hashes.
fn def_id_debug(def_id: rustc_hir::def_id::DefId, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    rustc_middle::ty::tls::with_opt(|opt_tcx| {
        if let Some(tcx) = opt_tcx {
            let crate_name = if def_id.is_local() {
                tcx.crate_name(rustc_hir::def_id::LOCAL_CRATE)
            } else {
                tcx.cstore_untracked().crate_name(def_id.krate)
            };
            write!(
                f,
                "{}{}",
                crate_name,
                tcx.def_path(def_id).to_string_no_crate_verbose()
            )?;
        } else {
            write!(f, "<can't access `tcx` to print `DefId` path>")?;
        }
        Ok(())
    })
}
