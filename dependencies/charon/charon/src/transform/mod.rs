pub mod ctx;
pub mod typecheck_and_unify;
pub mod utils;

/// Passes that finish translation, i.e. required for the output to be a valid output.
pub mod finish_translation {
    pub mod filter_invisible_trait_impls;
    pub mod insert_assign_return_unit;
    pub mod insert_ptr_metadata;
    pub mod insert_storage_lives;
}

/// Passes that compute extra info to be stored in the crate.
pub mod add_missing_info {
    pub mod compute_short_names;
    pub mod recover_body_comments;
    pub mod reorder_decls;
    pub mod sccs;
}

/// Passes that effect some kind of normalization on the crate.
pub mod normalize {
    pub mod desugar_drops;
    pub mod expand_associated_types;
    pub mod filter_unreachable_blocks;
    pub mod partial_monomorphization;
    pub mod skip_trait_refs_when_known;
    pub mod transform_dyn_trait_calls;
}

/// Passes that undo some lowering done by rustc to recover an operation closer to what the user
/// wrote.
pub mod resugar {
    pub mod inline_local_panic_functions;
    pub mod move_asserts_to_statements;
    pub mod reconstruct_asserts;
    pub mod reconstruct_boxes;
    pub mod reconstruct_fallible_operations;
    pub mod reconstruct_intrinsics;
    pub mod reconstruct_matches;
}

/// Passes that make the output simpler/easier to consume.
pub mod simplify_output {
    pub mod filter_trivial_drops;
    pub mod hide_allocator_param;
    pub mod hide_marker_traits;
    pub mod index_intermediate_assigns;
    pub mod index_to_function_calls;
    pub mod inline_anon_consts;
    pub mod lift_associated_item_clauses;
    pub mod ops_to_function_calls;
    pub mod remove_adt_clauses;
    pub mod remove_nops;
    pub mod remove_unit_locals;
    pub mod remove_unused_locals;
    pub mod remove_unused_self_clause;
    pub mod simplify_constants;
    pub mod unbind_item_vars;
    pub mod update_block_indices;
}

/// Passes that manipulate the control flow and reconstruct its structure.
pub mod control_flow {
    pub mod duplicate_return;
    pub mod merge_goto_chains;
    pub mod prettify_cfg;
    pub mod ullbc_to_llbc;
}

use Pass::*;
pub use ctx::TransformCtx;
use ctx::{LlbcPass, TransformPass, UllbcPass};

/// Item and type cleanup passes.
pub static INITIAL_CLEANUP_PASSES: &[Pass] = &[
    // Compute short names. We do it early to make pretty-printed output more legible in traces.
    NonBody(&add_missing_info::compute_short_names::Transform),
    // Check that translation emitted consistent types, and unify body lifetimes (best-effort).
    NonBody(&typecheck_and_unify::Check::PostTranslation),
    // # Micro-pass: filter the trait impls that were marked invisible since we couldn't filter
    // them out earlier.
    NonBody(&finish_translation::filter_invisible_trait_impls::Transform),
    // Compute the metadata & insert for Rvalue
    UnstructuredBody(&finish_translation::insert_ptr_metadata::Transform),
    // # Micro-pass: add the missing assignments to the return value.
    // When the function return type is unit, the generated MIR doesn't
    // set the return value to `()`. This can be a concern: in the case
    // of Aeneas, it means the return variable contains ⊥ upon returning.
    // For this reason, when the function has return type unit, we insert
    // an extra assignment just before returning.
    UnstructuredBody(&finish_translation::insert_assign_return_unit::Transform),
    // Insert `StorageLive` for locals that don't have one (that's allowed in MIR).
    NonBody(&finish_translation::insert_storage_lives::Transform),
    // Move clauses on associated types to be implied clauses of the trait.
    NonBody(&simplify_output::lift_associated_item_clauses::Transform),
    // # Micro-pass: hide some overly-common traits we don't need: Sized, Sync, Allocator, etc..
    NonBody(&simplify_output::hide_marker_traits::Transform),
    // Hide the `A` type parameter on standard library containers (`Box`, `Vec`, etc).
    NonBody(&simplify_output::hide_allocator_param::Transform),
    // # Micro-pass: remove the explicit `Self: Trait` clause of methods/assoc const declaration
    // items if they're not used. This simplifies the graph of dependencies between definitions.
    NonBody(&simplify_output::remove_unused_self_clause::Transform),
    // Transform Drops into Calls to drop_in_place.
    UnstructuredBody(&normalize::desugar_drops::Transform),
    // # Micro-pass: whenever we reference a trait method on a known type, refer to the method
    // `FunDecl` directly instead of going via a `TraitRef`. This is done before `reorder_decls` to
    // remove some sources of mutual recursion.
    NonBody(&normalize::skip_trait_refs_when_known::Transform),
    // Transform dyn trait method calls to vtable function pointer calls
    // This should be early to handle the calls before other transformations
    UnstructuredBody(&normalize::transform_dyn_trait_calls::Transform),
    // Change trait associated types to be type parameters instead. See the module for details.
    // This also normalizes any use of an associated type that we can resolve.
    NonBody(&normalize::expand_associated_types::Transform),
    // `--remove-adt-clauses`: Remove all trait clauses from type declarations.
    NonBody(&simplify_output::remove_adt_clauses::Transform),
];

/// Body cleanup passes on the ullbc.
pub static ULLBC_PASSES: &[Pass] = &[
    // Inline promoted and inline consts into their parent bodies.
    UnstructuredBody(&simplify_output::inline_anon_consts::Transform),
    // Remove drop statements that are noops.
    UnstructuredBody(&simplify_output::filter_trivial_drops::Transform),
    // Inline all asserts that correspond to dynamic checks into statements.
    // The following pass will then merge the generated gotos as part of this substitution,
    // and [reconstruct_fallible_operations] can then use the inlined asserts to reconstruct
    // the fallible operations.
    UnstructuredBody(&resugar::move_asserts_to_statements::Transform),
    // # Micro-pass: merge single-origin gotos into their parent. This drastically reduces the
    // graph size of the CFG.
    // This must be done early as some resugaring passes depend on it.
    UnstructuredBody(&control_flow::merge_goto_chains::Transform),
    // # Micro-pass: Remove overflow/div-by-zero/bounds checks since they are already part of the
    // arithmetic/array operation in the semantics of (U)LLBC.
    // **WARNING**: this pass uses the fact that the dynamic checks introduced by Rustc use a
    // special "assert" construct. Because of this, it must happen *before* the
    // [reconstruct_asserts] pass. See the comments in [crate::remove_dynamic_checks].
    // **WARNING**: this pass relies on a precise structure of the MIR statements. Because of this,
    // it must happen before passes that insert statements like [simplify_constants].
    UnstructuredBody(&resugar::reconstruct_fallible_operations::Transform),
    // Recognize calls to the `offset_of` intrinsics and replace them with the corresponding
    // `NullOp`.
    UnstructuredBody(&resugar::reconstruct_intrinsics::Transform),
    // # Micro-pass: reconstruct the special `Box::new` operations inserted e.g. in the `vec![]`
    // macro.
    // **WARNING**: this pass relies on a precise structure of the MIR statements. Because of this,
    // it must happen before passes that insert statements like [simplify_constants].
    // **WARNING**: this pass works across calls, hence must happen after `merge_goto_chains`,
    UnstructuredBody(&resugar::reconstruct_boxes::Transform),
    // # Micro-pass: reconstruct the asserts
    UnstructuredBody(&resugar::reconstruct_asserts::Transform),
    // # Micro-pass: `panic!()` expands to a new function definition each time. This pass cleans
    // those up.
    UnstructuredBody(&resugar::inline_local_panic_functions::Transform),
    // # Micro-pass: desugar the constants to other values/operands as much
    // as possible.
    UnstructuredBody(&simplify_output::simplify_constants::Transform),
    // # Micro-pass: introduce intermediate assignments in preparation of the
    // [`index_to_function_calls`] pass.
    UnstructuredBody(&simplify_output::index_intermediate_assigns::Transform),
    // # Micro-pass: remove locals of type `()` which show up a lot.
    UnstructuredBody(&simplify_output::remove_unit_locals::Transform),
    // # Micro-pass: duplicate the return blocks
    UnstructuredBody(&control_flow::duplicate_return::Transform),
    // Remove the locals which are never used.
    NonBody(&simplify_output::remove_unused_locals::Transform),
    // Another round.
    UnstructuredBody(&control_flow::merge_goto_chains::Transform),
    // # Micro-pass: filter the "dangling" blocks. Those might have been introduced by,
    // for instance, [`merge_goto_chains`].
    UnstructuredBody(&normalize::filter_unreachable_blocks::Transform),
    // # Micro-pass: make sure the block ids used in the ULLBC are consecutive
    UnstructuredBody(&simplify_output::update_block_indices::Transform),
];

/// Body cleanup passes after control flow reconstruction.
pub static LLBC_PASSES: &[Pass] = &[
    // # Go from ULLBC to LLBC (Low-Level Borrow Calculus) by reconstructing the control flow.
    NonBody(&control_flow::ullbc_to_llbc::Transform),
    // Reconstruct matches on enum variants.
    StructuredBody(&resugar::reconstruct_matches::Transform),
    // Cleanup the cfg.
    StructuredBody(&control_flow::prettify_cfg::Transform),
    // # Micro-pass: replace some unops/binops and the array aggregates with
    // function calls (introduces: ArrayToSlice, etc.)
    StructuredBody(&simplify_output::ops_to_function_calls::Transform),
    // # Micro-pass: replace the arrays/slices index operations with function
    // calls.
    // (introduces: ArrayIndexShared, ArrayIndexMut, etc.)
    StructuredBody(&simplify_output::index_to_function_calls::Transform),
];

/// Cleanup passes useful for both llbc and ullbc.
pub static SHARED_FINALIZING_PASSES: &[Pass] = &[
    // # Micro-pass: remove the locals which are never used.
    NonBody(&simplify_output::remove_unused_locals::Transform),
    // # Micro-pass: remove the useless `StatementKind::Nop`s.
    NonBody(&simplify_output::remove_nops::Transform),
    // # Micro-pass: take all the comments found in the original body and assign them to
    // statements. This must be last after all the statement-affecting passes to avoid losing
    // comments.
    NonBody(&add_missing_info::recover_body_comments::Transform),
    // Partially monomorphize items so that no item is ever instanciated with a mutable reference
    // or a type containing one.
    NonBody(&normalize::partial_monomorphization::Transform),
    // # Reorder the graph of dependencies and compute the strictly connex components to:
    // - compute the order in which to extract the definitions
    // - find the recursive definitions
    // - group the mutually recursive definitions
    // This is done last to account for the final item graph, not the initial one.
    NonBody(&add_missing_info::reorder_decls::Transform),
];

/// Final passes to run at the end, after pretty-printing the llbc if applicable. These are only
/// split from the above list to get test outputs even when generics fail to match.
pub static FINAL_CLEANUP_PASSES: &[Pass] = &[
    // Check that types are still consistent after the transformation passes.
    NonBody(&typecheck_and_unify::Check::PostTransformation),
    // Use `DeBruijnVar::Free` for the variables bound in item signatures.
    NonBody(&simplify_output::unbind_item_vars::Check),
];

#[derive(Clone, Copy)]
pub enum Pass {
    NonBody(&'static dyn TransformPass),
    UnstructuredBody(&'static dyn UllbcPass),
    StructuredBody(&'static dyn LlbcPass),
}

impl Pass {
    pub fn run(self, ctx: &mut TransformCtx) {
        match self {
            NonBody(pass) => {
                if pass.should_run(&ctx.options) {
                    pass.transform_ctx(ctx)
                }
            }
            UnstructuredBody(pass) => {
                if pass.should_run(&ctx.options) {
                    pass.transform_ctx(ctx)
                }
            }
            StructuredBody(pass) => {
                if pass.should_run(&ctx.options) {
                    pass.transform_ctx(ctx)
                }
            }
        }
    }

    pub fn name(&self) -> &str {
        match self {
            NonBody(pass) => pass.name(),
            UnstructuredBody(pass) => pass.name(),
            StructuredBody(pass) => pass.name(),
        }
    }
}

pub struct PrintCtxPass {
    pub message: String,
    /// Whether we're printing to stdout or only logging.
    pub to_stdout: bool,
}

impl PrintCtxPass {
    pub fn new(to_stdout: bool, message: String) -> &'static Self {
        let ret = Self { message, to_stdout };
        Box::leak(Box::new(ret))
    }
}

impl TransformPass for PrintCtxPass {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let message = &self.message;
        if self.to_stdout {
            println!("{message}:\n\n{ctx}\n");
        } else {
            trace!("{message}:\n\n{ctx}\n");
        }
    }
}
