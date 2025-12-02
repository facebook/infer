pub mod check_generics;
pub mod compute_short_names;
pub mod ctx;
pub mod duplicate_defaulted_methods;
pub mod duplicate_return;
pub mod expand_associated_types;
pub mod filter_invisible_trait_impls;
pub mod filter_unreachable_blocks;
pub mod graphs;
pub mod hide_allocator_param;
pub mod hide_marker_traits;
pub mod index_intermediate_assigns;
pub mod index_to_function_calls;
pub mod inline_local_panic_functions;
pub mod inline_promoted_consts;
pub mod insert_assign_return_unit;
pub mod insert_storage_lives;
pub mod lift_associated_item_clauses;
pub mod merge_goto_chains;
pub mod monomorphize;
pub mod ops_to_function_calls;
pub mod prettify_cfg;
pub mod reconstruct_asserts;
pub mod reconstruct_boxes;
pub mod recover_body_comments;
pub mod remove_drop_never;
pub mod remove_dynamic_checks;
pub mod remove_nops;
pub mod remove_read_discriminant;
pub mod remove_unit_locals;
pub mod remove_unused_locals;
pub mod remove_unused_methods;
pub mod remove_unused_self_clause;
pub mod reorder_decls;
pub mod simplify_constants;
pub mod skip_trait_refs_when_known;
pub mod ullbc_to_llbc;
pub mod unbind_item_vars;
pub mod update_block_indices;
pub mod utils;

use Pass::*;
pub use ctx::TransformCtx;
use ctx::{LlbcPass, TransformPass, UllbcPass};

/// Item and type cleanup passes.
pub static INITIAL_CLEANUP_PASSES: &[Pass] = &[
    // Compute short names
    NonBody(&compute_short_names::Transform),
    // Remove the trait/impl methods that were not translated (because not used).
    NonBody(&remove_unused_methods::Transform),
    // Move clauses on associated types to be parent clauses
    NonBody(&lift_associated_item_clauses::Transform),
    // Check that all supplied generic types match the corresponding generic parameters.
    // Needs `lift_associated_item_clauses`.
    NonBody(&check_generics::Check("after translation")),
    // # Micro-pass: hide some overly-common traits we don't need: Sized, Sync, Allocator, etc..
    NonBody(&hide_marker_traits::Transform),
    // Hide the `A` type parameter on standard library containers (`Box`, `Vec`, etc).
    NonBody(&hide_allocator_param::Transform),
    // # Micro-pass: filter the trait impls that were marked invisible since we couldn't filter
    // them out earlier.
    NonBody(&filter_invisible_trait_impls::Transform),
    // # Micro-pass: remove the explicit `Self: Trait` clause of methods/assoc const declaration
    // items if they're not used. This simplifies the graph of dependencies between definitions.
    NonBody(&remove_unused_self_clause::Transform),
    // Add missing methods to trait impls by duplicating the default method.
    NonBody(&duplicate_defaulted_methods::Transform),
    // # Micro-pass: whenever we call a trait method on a known type, refer to the method `FunDecl`
    // directly instead of going via a `TraitRef`. This is done before `reorder_decls` to remove
    // some sources of mutual recursion.
    UnstructuredBody(&skip_trait_refs_when_known::Transform),
    // Change trait associated types to be type parameters instead. See the module for details.
    NonBody(&expand_associated_types::Transform),
];

/// Body cleanup passes on the ullbc.
pub static ULLBC_PASSES: &[Pass] = &[
    // Inline promoted consts into their parent bodies.
    UnstructuredBody(&inline_promoted_consts::Transform),
    // # Micro-pass: merge single-origin gotos into their parent. This drastically reduces the
    // graph size of the CFG.
    UnstructuredBody(&merge_goto_chains::Transform),
    // # Micro-pass: Remove overflow/div-by-zero/bounds checks since they are already part of the
    // arithmetic/array operation in the semantics of (U)LLBC.
    // **WARNING**: this pass uses the fact that the dynamic checks introduced by Rustc use a
    // special "assert" construct. Because of this, it must happen *before* the
    // [reconstruct_asserts] pass. See the comments in [crate::remove_dynamic_checks].
    // **WARNING**: this pass relies on a precise structure of the MIR statements. Because of this,
    // it must happen before passes that insert statements like [simplify_constants].
    UnstructuredBody(&remove_dynamic_checks::Transform),
    // # Micro-pass: reconstruct the special `Box::new` operations inserted e.g. in the `vec![]`
    // macro.
    // **WARNING**: this pass relies on a precise structure of the MIR statements. Because of this,
    // it must happen before passes that insert statements like [simplify_constants].
    // **WARNING**: this pass works across calls, hence must happen after `merge_goto_chains`,
    UnstructuredBody(&reconstruct_boxes::Transform),
    // # Micro-pass: desugar the constants to other values/operands as much
    // as possible.
    UnstructuredBody(&simplify_constants::Transform),
    // # Micro-pass: make sure the block ids used in the ULLBC are consecutive
    UnstructuredBody(&update_block_indices::Transform),
    // # Micro-pass: reconstruct the asserts
    UnstructuredBody(&reconstruct_asserts::Transform),
    // # Micro-pass: duplicate the return blocks
    UnstructuredBody(&duplicate_return::Transform),
    // # Micro-pass: filter the "dangling" blocks. Those might have been introduced by,
    // for instance, [`reconstruct_asserts`].
    UnstructuredBody(&filter_unreachable_blocks::Transform),
    // # Micro-pass: `panic!()` expands to a new function definition each time. This pass cleans
    // those up.
    UnstructuredBody(&inline_local_panic_functions::Transform),
    // # Micro-pass: introduce intermediate assignments in preparation of the
    // [`index_to_function_calls`] pass.
    UnstructuredBody(&index_intermediate_assigns::Transform),
    // # Micro-pass: add the missing assignments to the return value.
    // When the function return type is unit, the generated MIR doesn't
    // set the return value to `()`. This can be a concern: in the case
    // of Aeneas, it means the return variable contains ⊥ upon returning.
    // For this reason, when the function has return type unit, we insert
    // an extra assignment just before returning.
    UnstructuredBody(&insert_assign_return_unit::Transform),
    // # Micro-pass: remove locals of type `()` which show up a lot.
    UnstructuredBody(&remove_unit_locals::Transform),
    // # Micro-pass: remove the drops of locals whose type is `Never` (`!`). This
    // is in preparation of the next transformation.
    UnstructuredBody(&remove_drop_never::Transform),
];

/// Body cleanup passes after control flow reconstruction.
pub static LLBC_PASSES: &[Pass] = &[
    // # Go from ULLBC to LLBC (Low-Level Borrow Calculus) by reconstructing the control flow.
    NonBody(&ullbc_to_llbc::Transform),
    // # Micro-pass: replace some unops/binops and the array aggregates with
    // function calls (introduces: ArrayToSlice, etc.)
    StructuredBody(&ops_to_function_calls::Transform),
    // # Micro-pass: replace the arrays/slices index operations with function
    // calls.
    // (introduces: ArrayIndexShared, ArrayIndexMut, etc.)
    StructuredBody(&index_to_function_calls::Transform),
    // # Micro-pass: Remove the discriminant reads (merge them with the switches)
    StructuredBody(&remove_read_discriminant::Transform),
    // Cleanup the cfg.
    StructuredBody(&prettify_cfg::Transform),
];

/// Cleanup passes useful for both llbc and ullbc.
pub static SHARED_FINALIZING_PASSES: &[Pass] = &[
    // # Micro-pass: remove the locals which are never used.
    NonBody(&remove_unused_locals::Transform),
    // Insert storage lives for locals that are always allocated at the beginning of the function.
    NonBody(&insert_storage_lives::Transform),
    // Monomorphize the functions and types.
    NonBody(&monomorphize::Transform),
    // # Micro-pass: remove the useless `StatementKind::Nop`s.
    NonBody(&remove_nops::Transform),
    // # Micro-pass: take all the comments found in the original body and assign them to
    // statements. This must be last after all the statement-affecting passes to avoid losing
    // comments.
    NonBody(&recover_body_comments::Transform),
    // # Reorder the graph of dependencies and compute the strictly connex components to:
    // - compute the order in which to extract the definitions
    // - find the recursive definitions
    // - group the mutually recursive definitions
    NonBody(&reorder_decls::Transform),
];

/// Final passes to run at the end, after pretty-printing the llbc if applicable. These are only
/// split from the above list to get test outputs even when generics fail to match.
pub static FINAL_CLEANUP_PASSES: &[Pass] = &[
    // Check that all supplied generic types match the corresponding generic parameters.
    NonBody(&check_generics::Check("after transformations")),
    // Use `DeBruijnVar::Free` for the variables bound in item signatures.
    NonBody(&unbind_item_vars::Check),
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
            NonBody(pass) => pass.transform_ctx(ctx),
            UnstructuredBody(pass) => pass.transform_ctx(ctx),
            StructuredBody(pass) => pass.transform_ctx(ctx),
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
