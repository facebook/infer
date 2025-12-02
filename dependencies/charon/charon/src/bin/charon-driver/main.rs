//! The Charon driver, which calls Rustc with callbacks to compile some Rust
//! crate to LLBC.
#![feature(rustc_private)]
#![expect(incomplete_features)]
#![feature(box_patterns)]
#![feature(deref_patterns)]
#![feature(if_let_guard)]
#![feature(iter_array_chunks)]
#![feature(iterator_try_collect)]

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_driver;
extern crate rustc_error_messages;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

#[macro_use]
extern crate charon_lib;

mod driver;
mod translate;

use charon_lib::{
    export, logger,
    options::{self, CliOpts},
    transform::{
        FINAL_CLEANUP_PASSES, INITIAL_CLEANUP_PASSES, LLBC_PASSES, Pass, PrintCtxPass,
        SHARED_FINALIZING_PASSES, ULLBC_PASSES,
    },
};
use std::{env, fmt, panic};

pub enum CharonFailure {
    /// The usize is the number of errors.
    CharonError(usize),
    RustcError,
    Panic,
    Serialize,
}

impl fmt::Display for CharonFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CharonFailure::RustcError => write!(f, "Code failed to compile")?,
            CharonFailure::CharonError(err_count) => write!(
                f,
                "Charon failed to translate this code ({err_count} errors)"
            )?,
            CharonFailure::Panic => write!(f, "Compilation panicked")?,
            CharonFailure::Serialize => write!(f, "Could not serialize output file")?,
        }
        Ok(())
    }
}

/// Calculate the list of passes we will run on the crate before outputting it.
pub fn transformation_passes(options: &CliOpts) -> Vec<Pass> {
    let mut passes: Vec<Pass> = vec![];

    passes.push(Pass::NonBody(PrintCtxPass::new(
        options.print_original_ullbc,
        format!("# ULLBC after translation from MIR"),
    )));

    passes.extend(INITIAL_CLEANUP_PASSES);
    passes.extend(ULLBC_PASSES);

    if !options.ullbc {
        // If we're reconstructing control-flow, print the ullbc here.
        passes.push(Pass::NonBody(PrintCtxPass::new(
            options.print_ullbc,
            format!("# Final ULLBC before control-flow reconstruction"),
        )));
    }

    if !options.ullbc {
        passes.extend(LLBC_PASSES);
    }
    passes.extend(SHARED_FINALIZING_PASSES);

    if options.ullbc {
        // If we're not reconstructing control-flow, print the ullbc after finalizing passes.
        passes.push(Pass::NonBody(PrintCtxPass::new(
            options.print_ullbc,
            format!("# Final ULLBC before serialization"),
        )));
    } else {
        passes.push(Pass::NonBody(PrintCtxPass::new(
            options.print_llbc,
            format!("# Final LLBC before serialization"),
        )));
    }

    // Run the final passes after pretty-printing so that we get some output even if check_generics
    // fails.
    passes.extend(FINAL_CLEANUP_PASSES);
    passes
}

/// Run charon. Returns the number of warnings generated.
fn run_charon(options: CliOpts) -> Result<usize, CharonFailure> {
    // Run the driver machinery.
    let Some(mut ctx) = driver::run_rustc_driver(&options)? else {
        // We didn't run charon.
        return Ok(0);
    };

    // The bulk of the translation is done, we no longer need to interact with rustc internals. We
    // run several passes that simplify the items and cleanup the bodies.
    for pass in transformation_passes(&options) {
        trace!("# Starting pass {}", pass.name());
        pass.run(&mut ctx);
    }

    let error_count = ctx.errors.borrow().error_count;

    // # Final step: generate the files.
    if !options.no_serialize {
        let crate_data = export::CrateData::new(ctx);
        let dest_file = match options.dest_file.clone() {
            Some(f) => f,
            None => {
                let mut target_filename = options.dest_dir.clone().unwrap_or_default();
                let crate_name = &crate_data.translated.crate_name;
                let extension = if options.ullbc { "ullbc" } else { "llbc" };
                target_filename.push(format!("{crate_name}.{extension}"));
                target_filename
            }
        };
        trace!("Target file: {:?}", dest_file);
        crate_data
            .serialize_to_file(&dest_file)
            .map_err(|()| CharonFailure::Serialize)?;
    }

    if options.error_on_warnings && error_count != 0 {
        return Err(CharonFailure::CharonError(error_count));
    }

    Ok(error_count)
}

fn main() {
    // Initialize the logger
    logger::initialize_logger();

    // Retrieve the Charon options by deserializing them from the environment variable
    // (cargo-charon serialized the arguments and stored them in a specific environment
    // variable before calling cargo with RUSTC_WRAPPER=charon-driver).
    let mut options: options::CliOpts = match env::var(options::CHARON_ARGS) {
        Ok(opts) => serde_json::from_str(opts.as_str()).unwrap(),
        Err(_) => Default::default(),
    };
    options.apply_preset();

    // Catch any and all panics coming from charon to display a clear error.
    let res = panic::catch_unwind(move || run_charon(options))
        .map_err(|_| CharonFailure::Panic)
        .and_then(|x| x);

    match res {
        Ok(warn_count) => {
            if warn_count != 0 {
                let msg = format!("The extraction generated {} warnings", warn_count);
                eprintln!("warning: {}", msg);
            }
        }
        Err(err) => {
            log::error!("{err}");
            let exit_code = match err {
                CharonFailure::CharonError(_) | CharonFailure::Serialize => 1,
                CharonFailure::RustcError => 2,
                // This is a real panic, exit with the standard rust panic error code.
                CharonFailure::Panic => 101,
            };
            std::process::exit(exit_code);
        }
    }
}
