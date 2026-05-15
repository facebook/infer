use quote::quote;
use quote::quote_spanned;
use syn::Token;
use syn::parse::ParseStream;
use syn::{Data, DeriveInput, Generics, parse_macro_input};
use syn::{PathArguments, PathSegment, spanned::Spanned};

fn strip_parenthesis(tokens: proc_macro::TokenStream) -> Option<proc_macro::TokenStream> {
    match tokens.into_iter().collect::<Vec<_>>().as_slice() {
        [proc_macro::TokenTree::Group(token)] => Some(token.stream()),
        _ => None,
    }
}

#[derive(Debug)]
struct Options {
    generics: Generics,
    from: syn::TypePath,
    state: syn::Ident,
    state_type: syn::Type,
    where_clause: Option<syn::WhereClause>,
}
mod option_parse {
    use super::*;
    mod kw {
        syn::custom_keyword!(from);
        syn::custom_keyword!(state);
    }
    impl syn::parse::Parse for Options {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let generics = input.parse()?;
            input.parse::<Token![,]>()?;

            input.parse::<kw::from>()?;
            input.parse::<Token![:]>()?;
            let from = input.parse()?;
            input.parse::<Token![,]>()?;

            input.parse::<kw::state>()?;
            input.parse::<Token![:]>()?;
            let state_type = input.parse()?;
            input.parse::<Token![as]>()?;
            let state = input.parse()?;

            let mut where_clause = None;
            if input.peek(Token![,]) && input.peek2(Token![where]) {
                input.parse::<Token![,]>()?;
                where_clause = Some(input.parse()?);
            }

            Ok(Options {
                generics,
                from,
                state,
                state_type,
                where_clause,
            })
        }
    }
}

/// Returns the token stream corresponding to an attribute (if it
/// exists), stripping parenthesis already.
fn tokens_of_attrs<'a>(
    attr_name: &'a str,
    attrs: &'a Vec<syn::Attribute>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    attrs
        .iter()
        .filter(|attr| attr.path.is_ident(attr_name))
        .map(|attr| attr.clone().tokens.into())
        .flat_map(strip_parenthesis)
        .map(|x| x.into())
}

fn parse_attrs<'a, T: syn::parse::Parse>(
    attr_name: &'a str,
    attrs: &'a Vec<syn::Attribute>,
) -> impl Iterator<Item = T> + 'a {
    tokens_of_attrs(attr_name, attrs).map(move |x| {
        syn::parse::<T>(x.clone().into())
            .expect(format!("expected attribtue {}", attr_name).as_str())
    })
}

/// Parse an attribute as a T if it exists.
fn parse_attr<T: syn::parse::Parse>(attr_name: &str, attrs: &Vec<syn::Attribute>) -> Option<T> {
    parse_attrs(attr_name, attrs).next()
}

/*
TODO: add `ensure_no_attr` calls to forbid meaningless attributes
fn ensure_no_attr(context: &str, attr: &str, attrs: &Vec<syn::Attribute>) {
    if attrs.iter().any(|a| a.path.is_ident(attr)) {
        panic!("Illegal attribute {} {}", attr, context)
    }
}
*/

/// Create a match arm that corresponds to a given set of fields.
/// This can be used for named fields as well as unnamed ones.
fn fields_to_arm(
    from_record_name: proc_macro2::TokenStream,
    to_record_name: proc_macro2::TokenStream,
    fields: Vec<syn::Field>,
    full_span: proc_macro2::Span,
    prepend: proc_macro2::TokenStream,
    used_fields: Vec<syn::Ident>,
    state: syn::Ident,
) -> proc_macro2::TokenStream {
    if fields.is_empty() {
        return quote_spanned! {full_span=> #from_record_name => #to_record_name, };
    }

    let is_struct = fields.iter().any(|f| f.ident.is_some());
    let is_tuple = fields.iter().any(|f| f.ident.is_none());
    if is_tuple && is_struct {
        panic!("Impossibe: variant with both named and unamed fields")
    }

    let data = fields.iter().enumerate().map(|(i, field)| {
        let attrs = &field.attrs;
        let name_destination = field.ident.clone().unwrap_or(syn::Ident::new(
            format!("value_{}", i).as_str(),
            field.span(),
        ));
        let span = field.span();
        let field_name_span = field.clone().ident.map(|x| x.span()).unwrap_or(span);
        let name_source =
            parse_attr::<syn::Ident>("from", attrs).unwrap_or(name_destination.clone());
        let value = parse_attr::<syn::Expr>("value", attrs);
        let not_in_source =
            value.is_some() ||
            attrs.iter().any(|attr| attr.path.is_ident("not_in_source"));
        let typ = &field.ty;
        let point = syn::Ident::new("x", field_name_span);

        let translation = parse_attr::<syn::Expr>("map", attrs).or(value).unwrap_or(
            syn::parse::<syn::Expr>((quote_spanned! {typ.span()=> #point.sinto(#state)}).into())
                .expect("Could not default [translation]")
        );
        let mapped_value = if not_in_source {
            quote_spanned! {span=> {#translation}}
        } else {
            quote_spanned! {span=> {#[allow(unused_variables)] let #point = #name_source; #translation}}
        };

        let prefix = if is_struct {
            quote_spanned! {field_name_span=> #name_destination:}
        } else {
            quote! {}
        };
        (
            if not_in_source {
                quote! {}
            } else {
                quote_spanned! {span=> #name_source, }
            },
            quote_spanned! {span=> #prefix #mapped_value, },
        )
    });

    let bindings: proc_macro2::TokenStream = data
        .clone()
        .map(|(x, _)| x)
        .chain(used_fields.iter().map(|f| quote! {#f,}))
        .collect();
    let fields: proc_macro2::TokenStream = data.clone().map(|(_, x)| x).collect();

    if is_struct {
        quote_spanned! {full_span=> #from_record_name { #bindings .. } => {#prepend #to_record_name { #fields }}, }
    } else {
        quote_spanned! {full_span=> #from_record_name ( #bindings ) => {#prepend #to_record_name ( #fields )}, }
    }
}

/// Extracts a vector of Field out of a Fields.
/// This function discard the Unnamed / Named variants.
fn field_vec_of_fields(fields: syn::Fields) -> Vec<syn::Field> {
    match fields {
        syn::Fields::Unit => vec![],
        syn::Fields::Named(syn::FieldsNamed { named: fields, .. })
        | syn::Fields::Unnamed(syn::FieldsUnnamed {
            unnamed: fields, ..
        }) => fields.into_iter().collect(),
    }
}

/// Given a variant, produce a match arm.
fn variant_to_arm(
    typ_from: proc_macro2::TokenStream,
    typ_to: proc_macro2::TokenStream,
    variant: syn::Variant,
    state: syn::Ident,
) -> proc_macro2::TokenStream {
    let attrs = &variant.attrs;
    let to_variant = variant.clone().ident;
    if attrs.iter().any(|attr| attr.path.is_ident("todo")) {
        return quote!();
    }

    let disable_mapping = attrs
        .iter()
        .any(|attr| attr.path.is_ident("disable_mapping"));
    let custom_arm = tokens_of_attrs("custom_arm", attrs).next();
    // TODO: either complete map or drop it
    let map = parse_attr::<syn::Expr>("map", attrs);
    // ensure_no_attr(
    //     format!("on the variant {}::{}", typ_to, to_variant).as_str(),
    //     "map",
    //     attrs,
    // );
    let from_variant = parse_attr::<syn::Ident>("from", attrs);

    if disable_mapping && (map.is_some() || custom_arm.is_some() || from_variant.is_some()) {
        println!("Warning: `disable_mapping` makes `map`, `custom_arm` and `from_variant` inert")
    }
    if custom_arm.is_some() && (map.is_some() || from_variant.is_some()) {
        println!("Warning: `custom_arm` makes `map` and `from` inert")
    }

    if disable_mapping {
        return quote! {};
    }
    if let Some(custom_arm) = custom_arm {
        return custom_arm.into();
    }

    let from_variant = from_variant.unwrap_or(to_variant.clone());

    let to_variant = quote! { #typ_to::#to_variant };
    let from_variant = quote! { #typ_from::#from_variant };

    let fields = field_vec_of_fields(variant.clone().fields);

    if let Some(map) = map {
        let names: proc_macro2::TokenStream = fields
            .iter()
            .filter(|f| {
                let attrs = &f.attrs;
                !(parse_attr::<syn::Expr>("value", attrs).is_some()
                    || attrs.iter().any(|attr| attr.path.is_ident("not_in_source")))
            })
            .enumerate()
            .map(|(nth, f)| {
                f.clone()
                    .ident
                    .unwrap_or(syn::Ident::new(format!("x{}", nth).as_str(), f.span()))
            })
            .map(|name| quote! {#name, })
            .collect();
        if fields.iter().any(|f| f.ident.is_some()) {
            quote_spanned!(variant.span()=> #from_variant {#names ..} => #map,)
        } else {
            quote_spanned!(variant.span()=> #from_variant (#names) => #map,)
        }
    } else {
        fields_to_arm(
            from_variant,
            to_variant,
            fields,
            variant.span(),
            tokens_of_attrs("prepend", attrs).collect(),
            parse_attrs("use_field", attrs).collect(),
            state,
        )
    }
}

/// [`AdtInto`] derives a
/// [`SInto`](../hax_frontend_exporter/trait.SInto.html)
/// instance. This helps at transporting a algebraic data type `A` to
/// another ADT `B` when `A` and `B` shares a lot of structure.
#[proc_macro_derive(
    AdtInto,
    attributes(
        map,
        from,
        custom_arm,
        disable_mapping,
        use_field,
        prepend,
        append,
        args,
        todo,
        not_in_source,
        value,
    )
)]
pub fn adt_into(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let dinput = {
        let input = input.clone();
        parse_macro_input!(input as DeriveInput)
    };
    let attrs = &dinput.attrs;
    let span = dinput.clone().span().clone();
    let to = dinput.ident;
    let to_generics = dinput.generics;

    let Options {
        generics,
        from: from_with_generics,
        state,
        state_type,
        where_clause,
    } = parse_attr("args", attrs).expect("An [args] attribute was expected");

    let generics = {
        let mut generics = generics;
        generics.params = merge_generic_params(
            to_generics.params.clone().into_iter(),
            generics.params.into_iter(),
        )
        .collect();
        generics
    };

    trait DropBounds {
        fn drop_bounds(&mut self);
    }

    impl DropBounds for syn::GenericParam {
        fn drop_bounds(&mut self) {
            use syn::GenericParam::*;
            match self {
                Lifetime(lf) => {
                    lf.colon_token = None;
                    lf.bounds.clear()
                }
                Type(t) => {
                    t.colon_token = None;
                    t.bounds.clear();
                    t.eq_token = None;
                    t.default = None;
                }
                Const(c) => {
                    c.eq_token = None;
                    c.default = None;
                }
            }
        }
    }
    impl DropBounds for syn::Generics {
        fn drop_bounds(&mut self) {
            self.params.iter_mut().for_each(DropBounds::drop_bounds);
        }
    }
    let to_generics = {
        let mut to_generics = to_generics;
        to_generics.drop_bounds();
        to_generics
    };

    let from = drop_generics(from_with_generics.clone());

    let append: proc_macro2::TokenStream = tokens_of_attrs("append", &dinput.attrs)
        .next()
        .unwrap_or((quote! {}).into())
        .into();

    let body = match &dinput.data {
        Data::Union(..) => panic!("Union types are not supported"),
        Data::Struct(syn::DataStruct { fields, .. }) => {
            let arm = fields_to_arm(
                quote! {#from},
                quote! {#to},
                field_vec_of_fields(fields.clone()),
                span,
                tokens_of_attrs("prepend", attrs).collect(),
                parse_attrs("use_field", attrs).collect(),
                state.clone(),
            );
            quote! { match self { #arm #append } }
        }
        Data::Enum(syn::DataEnum { variants, .. }) => {
            let arms: proc_macro2::TokenStream = variants
                .iter()
                .cloned()
                .map(|variant| variant_to_arm(quote! {#from}, quote! {#to}, variant, state.clone()))
                .collect();
            let todo = variants.iter().find_map(|variant| {
                let attrs = &variant.attrs;
                let to_variant = variant.clone().ident;
                if attrs.iter().any(|attr| attr.path.is_ident("todo")) {
                    Some (quote_spanned! {variant.span()=> x => TO_TYPE::#to_variant(format!("{:?}", x)),})
                } else {
                    None
                }
            }).unwrap_or(quote!{});
            let append = quote! {
                #append
                #todo
            };
            quote! { match self { #arms #append } }
        }
    };

    quote! {
        const _ : () = {
            use #from as FROM_TYPE;
            use #to as TO_TYPE;
            impl #generics SInto<#state_type, #to #to_generics> for #from_with_generics #where_clause {
                #[tracing::instrument(level = "trace", skip(#state))]
                fn sinto(&self, #state: &#state_type) -> #to #to_generics {
                    tracing::trace!("Enters sinto ({})", stringify!(#from_with_generics));
                    #body
                }
            }
        };
    }
    .into()
}

/// Merge two collections of generic params, with params from [a]
/// before the ones from [b]. This function ensures lifetimes
/// appear before anything else.
fn merge_generic_params(
    a: impl Iterator<Item = syn::GenericParam>,
    b: impl Iterator<Item = syn::GenericParam>,
) -> impl Iterator<Item = syn::GenericParam> {
    fn partition(
        a: impl Iterator<Item = syn::GenericParam>,
    ) -> (Vec<syn::GenericParam>, Vec<syn::GenericParam>) {
        a.partition(|g| matches!(g, syn::GenericParam::Lifetime(_)))
    }
    let (a_lt, a_others) = partition(a);
    let (b_lt, b_others) = partition(b);
    let h = |x: Vec<_>, y: Vec<_>| x.into_iter().chain(y.into_iter());
    h(a_lt, b_lt).chain(h(a_others, b_others))
}

fn drop_generics(type_path: syn::TypePath) -> syn::TypePath {
    syn::TypePath {
        path: syn::Path {
            segments: type_path
                .path
                .segments
                .into_iter()
                .map(|s| PathSegment {
                    ident: s.ident,
                    arguments: match s.arguments {
                        PathArguments::AngleBracketed(_) => PathArguments::None,
                        _ => s.arguments,
                    },
                })
                .collect(),
            ..type_path.path
        },
        ..type_path
    }
}
