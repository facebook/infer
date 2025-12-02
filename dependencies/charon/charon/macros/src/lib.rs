//! This module contains some macros for Charon. Due to technical reasons, Rust
//! forces users to define such macros in a separate, dedicated library. Note
//! that this doesn't apply to `macro_rules`.
#![feature(non_exhaustive_omitted_patterns_lint)]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

mod enum_helpers;

use enum_helpers::EnumMethodKind;

#[proc_macro_derive(VariantName)]
pub fn derive_variant_name(item: TokenStream) -> TokenStream {
    enum_helpers::derive_variant_name(item.into()).into()
}

/// Macro to derive a function `fn variant_index_arity(&self) -> (u32, usize)`
/// the pair (variant index, variant arity).
/// Only works on enumerations, of course.
#[proc_macro_derive(VariantIndexArity)]
pub fn derive_variant_index_arity(item: TokenStream) -> TokenStream {
    enum_helpers::derive_variant_index_arity(item.into()).into()
}

/// Macro `EnumIsA`
///
/// Derives functions of the form `fn is_{variant_name}(&self) -> bool` returning true
/// if an enumeration instance is of some variant. For lists, it would generate
/// `is_cons` and `is_nil`.
/// Note that there already exists a crate implementing such macros,
/// [`enum_methods`](https://docs.rs/enum-methods/0.0.8/enum_methods/), but
/// it doesn't work when the enumeration has generic parameters and it seems
/// dead (a PR from 2019 has never been merged), so it seems better to maintain
/// our own code here (which is small) rather than doing PRs for this crate.
#[proc_macro_derive(EnumIsA)]
pub fn derive_enum_is_a(item: TokenStream) -> TokenStream {
    enum_helpers::derive_enum_variant_method(item.into(), EnumMethodKind::EnumIsA).into()
}

/// Macro `EnumAsGetters`
///
/// Derives functions of the form `fn as_{variant_name}(&self) -> ...` checking
/// that an enumeration instance is of the proper variant and returning shared
/// borrows to its fields.
/// Also see the comments for [crate::derive_enum_is_a]
#[proc_macro_derive(EnumAsGetters)]
pub fn derive_enum_as_getters(item: TokenStream) -> TokenStream {
    let by_ref = enum_helpers::derive_enum_variant_method(
        item.clone().into(),
        EnumMethodKind::EnumAsGetters,
    );
    let by_ref_mut =
        enum_helpers::derive_enum_variant_method(item.into(), EnumMethodKind::EnumAsMutGetters);
    quote! {
        #by_ref #by_ref_mut
    }
    .into()
}

/// Macro `EnumToGetters`
///
/// Derives functions of the form `fn to_{variant_name}(self) -> ...` checking
/// that an enumeration instance is of the proper variant and returning its
/// fields (while consuming the instance).
/// Also see the comments for [crate::derive_enum_is_a]
#[proc_macro_derive(EnumToGetters)]
pub fn derive_enum_to_getters(item: TokenStream) -> TokenStream {
    enum_helpers::derive_enum_variant_method(item.into(), EnumMethodKind::EnumToGetters).into()
}
