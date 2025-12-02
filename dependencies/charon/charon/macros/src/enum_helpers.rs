use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::vec::Vec;
use syn::{parse2, Data, DataEnum, DeriveInput, Fields, Ident, Type};

use self::EnumMethodKind::*;

/// We initially used the convert-case crate, but it converts names like "I32"
/// to "i_32", while we want to get "i32". We thus reimplemented our own converter
/// (which removes one dependency at the same time).
fn to_snake_case(s: &str) -> String {
    let mut snake_case = String::new();

    // We need to keep track of whether the last treated character was
    // lowercase (or not) to prevent this kind of transformations:
    // "VARIANT" -> "v_a_r_i_a_n_t"
    // Note that if we remember whether the last character was uppercase instead,
    // we get things like this:
    // "I32" -> "I3_2"
    let mut last_is_lowercase = false;

    for c in s.chars() {
        if c.is_uppercase() {
            if last_is_lowercase {
                snake_case.push('_');
            }
            last_is_lowercase = false;
            snake_case.push(c.to_lowercase().next().unwrap());
        } else {
            last_is_lowercase = true;
            snake_case.push(c);
        }
    }

    snake_case
}

struct MatchPattern {
    /// The variant name, as a string.
    variant_name: String,
    /// The match pattern.
    /// For instance: `List::Cons(x0, x1)`
    pattern: TokenStream,
    /// The number of arguments in the match pattern (including anonymous
    /// arguments).
    num_args: usize,
    /// The variables we introduced in the match pattern.
    /// `["x0", "x1"]` if the pattern is `List::Cons(hd, tl)`.
    pattern_vars: Vec<Ident>,
    /// The types of the variables introduced in the match pattern
    arg_types: Vec<Type>,
}

/// Generate matching patterns for an enumeration.
fn generate_variant_match_patterns(enum_name: &Ident, data: &DataEnum) -> Vec<MatchPattern> {
    let mut patterns: Vec<MatchPattern> = vec![];
    for variant in &data.variants {
        // Compute the pattern (without the variant constructor), the list
        // of introduced arguments and the list of field types.
        let fields = &variant.fields;
        let num_vars = fields.len();
        let vars: Vec<Ident> = (0..num_vars)
            .map(|i| Ident::new(&format!("_x{i}"), Span::mixed_site()))
            .collect();
        let vartypes: Vec<_> = fields.iter().map(|f| f.ty.clone()).collect();

        let pattern_vars = match fields {
            Fields::Named(_) => {
                let field_names: Vec<_> = fields.iter().map(|f| &f.ident).collect();
                quote!({ #(#field_names : #vars,)* })
            }
            Fields::Unnamed(_) => {
                quote!((#(#vars,)*))
            }
            Fields::Unit => quote!(),
        };
        let variant_name = &variant.ident;
        let pattern = quote!(#enum_name :: #variant_name #pattern_vars);

        patterns.push(MatchPattern {
            variant_name: variant.ident.to_string(),
            pattern,
            num_args: num_vars,
            pattern_vars: vars,
            arg_types: vartypes,
        });
    }

    patterns
}

/// Macro to derive a function `fn variant_name(&self) -> String` printing the
/// constructor of an enumeration. Only works on enumerations, of course.
pub fn derive_variant_name(item: TokenStream) -> TokenStream {
    // Parse the input
    let ast: DeriveInput = parse2(item).unwrap();

    // Generate the code for the matches
    let Data::Enum(data) = &ast.data else {
        panic!("VariantName macro can only be called on enums");
    };
    let patterns = generate_variant_match_patterns(&ast.ident, data);
    let match_branches: Vec<TokenStream> = patterns
        .into_iter()
        .map(|mp| {
            let pattern = &mp.pattern;
            let name = &mp.variant_name;
            quote!( #pattern => #name )
        })
        .collect();

    let adt_name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    quote!(
        impl #impl_generics #adt_name #ty_generics #where_clause {
            pub fn variant_name(&self) -> &'static str {
                match self {
                    #(#match_branches,)*
                }
            }
        }
    )
}

/// Macro to derive a function `fn variant_index_arity(&self) -> (u32, usize)`
/// the pair (variant index, variant arity).
/// Only works on enumerations, of course.
pub fn derive_variant_index_arity(item: TokenStream) -> TokenStream {
    // Parse the input
    let ast: DeriveInput = parse2(item).unwrap();

    // Generate the code for the matches
    let Data::Enum(data) = &ast.data else {
        panic!("VariantIndex macro can only be called on enums");
    };
    let patterns = generate_variant_match_patterns(&ast.ident, data);
    let match_branches: Vec<TokenStream> = patterns
        .into_iter()
        .enumerate()
        .map(|(i, mp)| {
            let pattern = &mp.pattern;
            let i = i as u32;
            let arity = mp.num_args;
            quote!( #pattern => (#i, #arity) )
        })
        .collect();

    let adt_name = &ast.ident;
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    quote!(
        impl #impl_generics #adt_name #ty_generics #where_clause {
            pub fn variant_index_arity(&self) -> (u32, usize) {
                match self {
                    #(#match_branches,)*
                }
            }
        }
    )
}

#[derive(PartialEq, Eq)]
pub enum EnumMethodKind {
    EnumIsA,
    EnumAsGetters,
    EnumAsMutGetters,
    EnumToGetters,
}

impl EnumMethodKind {
    /// We have to write this by hand: we can't use the macros defined above on
    /// the declarations of this file...
    fn variant_name(&self) -> &'static str {
        match self {
            EnumIsA => "EnumIsA",
            EnumAsGetters => "EnumAsGetters",
            EnumAsMutGetters => "EnumAsMutGetters",
            EnumToGetters => "EnumToGetters",
        }
    }
}

/// Generic helper for `EnumIsA` and `EnumAsGetters`.
/// This generates one function per variant.
pub fn derive_enum_variant_method(item: TokenStream, method_kind: EnumMethodKind) -> TokenStream {
    // Parse the input
    let ast: DeriveInput = parse2(item).unwrap();

    // Generate the code
    let adt_name = &ast.ident;

    // Generate the code for all the functions in the impl block
    let Data::Enum(data) = &ast.data else {
        panic!(
            "{} macro can only be called on enums",
            method_kind.variant_name()
        );
    };
    let patterns = generate_variant_match_patterns(&ast.ident, data);
    let methods: Vec<TokenStream> = patterns
        .into_iter()
        .map(|mp| {
            let pattern = &mp.pattern;
            let name_prefix = match method_kind {
                EnumIsA => "is_",
                EnumAsGetters | EnumAsMutGetters => "as_",
                EnumToGetters => "to_",
            };
            let name_suffix = match method_kind {
                EnumAsMutGetters => "_mut",
                _ => "",
            };
            let ref_kind = match method_kind {
                EnumAsGetters | EnumIsA => quote!(&),
                EnumAsMutGetters => quote!(&mut),
                EnumToGetters => quote!(),
            };
            // TODO: write our own to_snake_case function:
            // names like "i32" become "i_32" with this one.
            let variant_name = to_snake_case(&mp.variant_name);
            let method_name = format!("{name_prefix}{variant_name}{name_suffix}");
            let method_name = Ident::new(&method_name, Span::call_site());
            match method_kind {
                EnumIsA => {
                    quote!(
                        pub fn #method_name(#ref_kind self) -> bool {
                            #[allow(unreachable)]
                            match self {
                                #pattern => true,
                                _ => false,
                            }
                        }
                    )
                }
                EnumAsGetters | EnumAsMutGetters | EnumToGetters => {
                    let vars = &mp.pattern_vars;
                    let tys = &mp.arg_types;
                    quote!(
                        pub fn #method_name(#ref_kind self) -> Option<( #(#ref_kind #tys),* )> {
                            #[allow(unreachable)]
                            match self {
                                #pattern => Some(( #(#vars),* )),
                                _ => None,
                            }
                        }
                    )
                }
            }
        })
        .collect();

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    quote!(
        impl #impl_generics #adt_name #ty_generics #where_clause {
            #(#methods)*
        }
    )
}

#[test]
fn test_snake_case() {
    let s = to_snake_case("ConstantValue");
    println!("{}", s);
    assert!(s == "constant_value");
}

#[test]
fn test_generics() {
    let s = quote!(
        enum Foo<T: Clone>
        where
            T: Copy,
        {
            Variant1(T),
            Variant2 { x: u32 },
            Variant3,
        }
    );
    assert_tokens_eq::assert_tokens_eq!(
        derive_variant_index_arity(s.clone()),
        quote! {
            impl<T: Clone,> Foo<T,>
            where
                T: Copy,
            {
                pub fn variant_index_arity(&self) -> (u32, usize) {
                    match self {
                        Foo::Variant1(_x0) => (0u32, 1usize),
                        Foo::Variant2 { x: _x0 } => (1u32, 1usize),
                        Foo::Variant3 => (2u32, 0usize),
                    }
                }
            }
        }
    );
    assert_tokens_eq::assert_tokens_eq!(
        derive_enum_variant_method(s, EnumAsMutGetters),
        quote! {
            impl<T: Clone,> Foo<T,>
            where
                T: Copy,
            {
                pub fn as_variant1_mut(&mut self) -> (&mut T) {
                    #[allow(unreachable)]
                    match self {
                        Foo::Variant1(_x0) => (_x0),
                        _ => unreachable!("Foo::as_variant1_mut: Not the proper variant"),
                    }
                }
                pub fn as_variant2_mut(&mut self) -> (&mut u32) {
                    #[allow(unreachable)]
                    match self {
                        Foo::Variant2 { x: _x0 } => (_x0),
                        _ => unreachable!("Foo::as_variant2_mut: Not the proper variant"),
                    }
                }
                pub fn as_variant3_mut(&mut self) -> () {
                    #[allow(unreachable)]
                    match self {
                        Foo::Variant3 => (),
                        _ => unreachable!("Foo::as_variant3_mut: Not the proper variant"),
                    }
                }
            }
        }
    );
}
