#![feature(box_patterns)]

use charon_lib::llbc_ast::*;
use itertools::Itertools;
use std::collections::HashMap;

mod util;
use util::*;

fn translate_with_options(
    code: impl std::fmt::Display,
    options: &[&str],
) -> anyhow::Result<TranslatedCrate> {
    util::translate_rust_text(code, options)
}

fn translate(code: impl std::fmt::Display) -> anyhow::Result<TranslatedCrate> {
    util::translate_rust_text(code, &[])
}

/// A general item, with information shared by all items.
struct Item<'c> {
    name_str: String,
    // Not a ref because we do a little hack.
    generics: GenericParams,
    #[expect(dead_code)]
    kind: ItemRef<'c>,
}

/// Get all the items for this crate.
fn items_by_name<'c>(crate_data: &'c TranslatedCrate) -> HashMap<String, Item<'c>> {
    crate_data
        .all_items()
        .map(|item| {
            let mut generics = item.generic_params().clone();
            if let ItemRef::TraitDecl(tdecl) = &item {
                // We do a little hack.
                assert!(generics.trait_clauses.is_empty());
                generics.trait_clauses = tdecl.implied_clauses.clone();
            }
            Item {
                name_str: repr_name(crate_data, &item.item_meta().name),
                generics,
                kind: item,
            }
        })
        .map(|item| (item.name_str.clone(), item))
        .collect()
}

#[test]
fn type_decl() -> anyhow::Result<()> {
    let crate_data = translate(
        "
        struct Struct;
        fn main() {}
        ",
    )?;
    assert_eq!(
        repr_name(&crate_data, &crate_data.type_decls[0].item_meta.name),
        "test_crate::Struct"
    );
    Ok(())
}

#[test]
fn file_name() -> anyhow::Result<()> {
    let crate_data = translate(
        "
        type Foo = Option<()>;
        ",
    )?;
    assert_eq!(
        repr_name(&crate_data, &crate_data.type_decls[0].item_meta.name),
        "test_crate::Foo"
    );
    assert_eq!(
        repr_name(&crate_data, &crate_data.type_decls[1].item_meta.name),
        "core::option::Option"
    );
    let file_id = crate_data.type_decls[1].item_meta.span.data.file_id;
    let file = &crate_data.files[file_id];
    assert_eq!(file.name.to_string(), "/rustc/library/core/src/option.rs");
    Ok(())
}

#[test]
fn spans() -> anyhow::Result<()> {
    let crate_data = translate_with_options(
        "
        pub fn sum(s: &[u32]) -> u32 {
            let mut sum = 0;
            let mut i = 0;
            while i < s.len() {
                sum += s[i];
                i += 1;
            }
            sum
        }
        ",
        &["--reconstruct-fallible-operations"],
    )?;
    let function = &crate_data.fun_decls[1];
    // Span of the whole function.
    assert_eq!(repr_span(function.item_meta.span), "2:8-10:9");

    let body = &function.body.as_structured().unwrap();
    // Span of the function body
    assert_eq!(repr_span(body.body.span), "3:16-10:9");

    let sum_var = &body.locals.locals[2];
    assert_eq!(sum_var.name.as_deref(), Some("sum"));
    assert_eq!(repr_span(sum_var.span), "3:16-3:23");

    let the_loop = body
        .body
        .statements
        .iter()
        .find(|st| st.kind.is_loop())
        .unwrap();
    assert_eq!(repr_span(the_loop.span), "5:12-8:13");

    Ok(())
}

#[test]
fn predicate_origins() -> anyhow::Result<()> {
    use PredicateOrigin::*;
    let crate_data = translate(
        "
        fn top_level_function<T: Clone>() where T: Default {}

        #[derive(Clone)]
        struct Struct<T: Clone> where T: Default { x: T }

        type TypeAlias<T: Clone> where T: Default = Struct<T>;

        impl<T: Clone> Struct<T> where T: Default {
            fn inherent_method<U: From<T>>() where T: From<U> {}
        }

        trait Trait<T: Copy>: Clone where T: Default {
            type AssocType: Default;
            fn trait_method<U: From<T>>() where T: From<U>;
        }

        impl<T: Copy> Trait<T> for Struct<T> where T: Default {
            type AssocType = ();
            fn trait_method<U: From<T>>() where T: From<U> {}
        }
        ",
    )?;
    let expected_function_clause_origins: Vec<(&str, Vec<_>)> = vec![
        (
            "test_crate::top_level_function",
            vec![
                (WhereClauseOnFn, "Sized"),
                (WhereClauseOnFn, "Clone"),
                (WhereClauseOnFn, "Default"),
            ],
        ),
        (
            "test_crate::Struct",
            vec![
                (WhereClauseOnType, "Sized"),
                (WhereClauseOnType, "Clone"),
                (WhereClauseOnType, "Default"),
            ],
        ),
        (
            "test_crate::TypeAlias",
            vec![
                (WhereClauseOnType, "Sized"),
                (WhereClauseOnType, "Clone"),
                (WhereClauseOnType, "Default"),
            ],
        ),
        (
            "test_crate::<inherent impl>::inherent_method",
            vec![
                (WhereClauseOnImpl, "Sized"),
                (WhereClauseOnImpl, "Clone"),
                (WhereClauseOnImpl, "Default"),
                (WhereClauseOnFn, "Sized"),
                (WhereClauseOnFn, "From"),
                (WhereClauseOnFn, "From"),
            ],
        ),
        (
            "test_crate::Trait",
            vec![
                (WhereClauseOnTrait, "MetaSized"),
                (WhereClauseOnTrait, "Clone"),
                (WhereClauseOnTrait, "Sized"),
                (WhereClauseOnTrait, "Copy"),
                (WhereClauseOnTrait, "Default"),
                (TraitItem(TraitItemName("AssocType".into())), "Sized"),
                (TraitItem(TraitItemName("AssocType".into())), "Default"),
            ],
        ),
        // Interesting note: the method definition does not mention the clauses on the trait.
        (
            "test_crate::Trait::trait_method",
            vec![
                (WhereClauseOnFn, "Trait"),
                (WhereClauseOnFn, "Sized"),
                (WhereClauseOnFn, "From"),
                (WhereClauseOnFn, "From"),
            ],
        ),
        (
            "test_crate::<impl Trait for ??>",
            vec![
                (WhereClauseOnImpl, "Sized"),
                (WhereClauseOnImpl, "Copy"),
                (WhereClauseOnImpl, "Default"),
            ],
        ),
        (
            "test_crate::<impl Trait for ??>::trait_method",
            vec![
                (WhereClauseOnImpl, "Sized"),
                (WhereClauseOnImpl, "Copy"),
                (WhereClauseOnImpl, "Default"),
                (WhereClauseOnFn, "Sized"),
                (WhereClauseOnFn, "From"),
                (WhereClauseOnFn, "From"),
            ],
        ),
    ];
    let items_by_name = items_by_name(&crate_data);
    for (item_name, origins) in expected_function_clause_origins {
        let Some(item) = items_by_name.get(item_name) else {
            let keys = items_by_name
                .keys()
                .sorted()
                .map(|k| format!("- `{k}`"))
                .join("\n");
            panic!("Item `{item_name}` not found. Available items: \n{keys}")
        };
        let clauses = &item.generics.trait_clauses;
        assert_eq!(
            origins.len(),
            clauses.elem_count(),
            "failed for {item_name}"
        );
        for (clause, (expected_origin, expected_trait_name)) in clauses.iter().zip(origins) {
            let trait_name = trait_name(&crate_data, clause.trait_.skip_binder.id);
            assert_eq!(trait_name, expected_trait_name, "failed for {item_name}");
            assert_eq!(&clause.origin, &expected_origin, "failed for {item_name}");
        }
    }

    Ok(())
}

#[test]
fn attributes() -> anyhow::Result<()> {
    // Use the `clippy::` prefix because it's ignored by rustc.
    let unknown_attrs = |item_meta: &ItemMeta| {
        item_meta
            .attr_info
            .attributes
            .iter()
            .filter_map(|a| a.as_unknown())
            .map(|a| a.to_string())
            .collect_vec()
    };
    let crate_data = translate(
        r#"
        #![feature(stmt_expr_attributes)]

        #[clippy::foo]
        #[clippy::foo(arg)]
        #[clippy::foo = "arg"]
        struct Struct;

        #[non_exhaustive]
        enum Enum {}

        #[clippy::foo]
        trait Trait {}

        #[clippy::foo]
        impl Trait for Struct {}

        #[clippy::foo]
        const FOO: () = ();

        #[clippy::foo]
        static BAR: () = ();

        #[inline(never)]
        /// This is a doc comment.
        fn main() {
            // Attribute on a closure.
            let _f = #[inline(always)] || 42;
        }
        "#,
    )?;
    assert_eq!(
        unknown_attrs(&crate_data.type_decls[0].item_meta),
        vec!["clippy::foo", "clippy::foo(arg)", "clippy::foo(\"arg\")"]
    );
    assert!(unknown_attrs(&crate_data.type_decls[1].item_meta).is_empty());
    assert_eq!(
        unknown_attrs(&crate_data.trait_decls[0].item_meta),
        vec!["clippy::foo"]
    );
    assert_eq!(
        unknown_attrs(&crate_data.trait_impls[0].item_meta),
        vec!["clippy::foo"]
    );
    assert_eq!(
        unknown_attrs(&crate_data.global_decls[1].item_meta),
        vec!["clippy::foo"]
    );
    assert_eq!(
        unknown_attrs(&crate_data.global_decls[2].item_meta),
        vec!["clippy::foo"]
    );
    assert!(unknown_attrs(&crate_data.fun_decls[1].item_meta).is_empty());
    assert_eq!(
        crate_data.fun_decls[1].item_meta.attr_info.inline,
        Some(InlineAttr::Never)
    );
    assert_eq!(
        crate_data.fun_decls[1]
            .item_meta
            .attr_info
            .attributes
            .last()
            .unwrap(),
        &Attribute::DocComment(" This is a doc comment.".to_owned())
    );
    // Check that the `inline` attribute on closures gets picked up.
    let any_inline_always = crate_data
        .fun_decls
        .iter()
        .any(|decl| matches!(decl.item_meta.attr_info.inline, Some(InlineAttr::Always)));
    assert!(any_inline_always);
    Ok(())
}

#[test]
fn visibility() -> anyhow::Result<()> {
    let crate_data = translate(
        r#"
        pub struct Pub;
        struct Priv;

        mod private {
            pub struct PubInPriv;
        }
        "#,
    )?;
    assert_eq!(
        repr_name(&crate_data, &crate_data.type_decls[0].item_meta.name),
        "test_crate::Pub"
    );
    assert!(crate_data.type_decls[0].item_meta.attr_info.public);
    assert_eq!(
        repr_name(&crate_data, &crate_data.type_decls[1].item_meta.name),
        "test_crate::Priv"
    );
    assert!(!crate_data.type_decls[1].item_meta.attr_info.public);
    // Note how we think `PubInPriv` is public. It kind of is but there is no path to it. This is
    // probably fine.
    assert_eq!(
        repr_name(&crate_data, &crate_data.type_decls[2].item_meta.name),
        "test_crate::private::PubInPriv"
    );
    assert!(crate_data.type_decls[2].item_meta.attr_info.public);
    Ok(())
}

#[test]
fn discriminants() -> anyhow::Result<()> {
    let crate_data = translate(
        r#"
        enum Foo {
            Variant1,
            Variant2,
        }
        #[repr(u32)]
        enum Bar {
            Variant1 = 3,
            Variant2 = 42,
        }
        "#,
    )?;
    fn get_enum_discriminants(ty: &TypeDecl) -> Vec<Literal> {
        ty.kind
            .as_enum()
            .unwrap()
            .iter()
            .map(|v| v.discriminant.clone())
            .collect()
    }
    assert_eq!(
        get_enum_discriminants(&crate_data.type_decls[0]),
        vec![
            Literal::Scalar(ScalarValue::Signed(IntTy::Isize, 0)),
            Literal::Scalar(ScalarValue::Signed(IntTy::Isize, 1))
        ]
    );
    assert_eq!(
        get_enum_discriminants(&crate_data.type_decls[1]),
        vec![
            Literal::Scalar(ScalarValue::Unsigned(UIntTy::U32, 3)),
            Literal::Scalar(ScalarValue::Unsigned(UIntTy::U32, 42))
        ]
    );
    Ok(())
}

#[test]
fn rename_attribute() -> anyhow::Result<()> {
    let crate_data = translate(
        r#"
        #![feature(register_tool)]
        #![register_tool(charon)]
        #![register_tool(aeneas)]

        #[charon::rename("BoolTest")]
        pub trait BoolTrait {
            #[charon::rename("AsSoCtY")]
            type AssocTy;

            #[charon::rename("konst")]
            const ASSOC_CONST: u32 = 42;

            // Required method
            #[charon::rename("getTest")]
            fn get_bool(&self) -> bool;

            // Provided method
            #[charon::rename("retTest")]
            fn ret_true(&self) -> bool {
                true
            }
        }

        #[charon::rename("BoolImpl")]
        impl BoolTrait for bool {
            type AssocTy = ();

            fn get_bool(&self) -> bool {
                *self
            }
        }

        #[charon::rename("BoolFn")]
        fn test_bool_trait<T>(x: bool) -> bool {
            x.get_bool() && x.ret_true()
        }

        #[charon::rename("TypeTest")]
        type Test = i32;

        #[charon::rename("VariantsTest")]
        #[charon::variants_prefix("Simple")]
        #[charon::variants_suffix("_")]
        enum SimpleEnum {
            #[charon::rename("Variant1")]
            FirstVariant,
            SecondVariant,
            ThirdVariant,
        }

        #[charon::rename("StructTest")]
        struct Foo {
            #[charon::rename("FieldTest")]
            field1: u32,
        }

        #[charon::rename("Const_Test")]
        const C: u32 = 100 + 10 + 1;

        #[aeneas::rename("_TypeAeneas36")]
        type Test2 = u32;
        "#,
    )?;

    assert_eq!(
        crate_data.trait_decls[0]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("BoolTest")
    );

    assert_eq!(
        crate_data.trait_decls[0].types[0]
            .skip_binder
            .attr_info
            .rename
            .as_deref(),
        Some("AsSoCtY")
    );

    assert_eq!(
        crate_data.trait_decls[0].consts[0]
            .attr_info
            .rename
            .as_deref(),
        Some("konst")
    );

    assert_eq!(
        crate_data.trait_decls[0].methods[0]
            .skip_binder
            .attr_info
            .rename
            .as_deref(),
        Some("getTest")
    );

    assert_eq!(
        crate_data.fun_decls[1]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("BoolFn")
    );

    assert_eq!(
        crate_data.fun_decls[2]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("getTest")
    );

    assert_eq!(
        crate_data.fun_decls[3]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("retTest")
    );

    assert_eq!(
        crate_data.fun_decls[5]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("retTest")
    );

    assert_eq!(
        crate_data.fun_decls[6]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("Const_Test")
    );

    assert_eq!(
        crate_data.trait_impls[0]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("BoolImpl")
    );

    assert_eq!(
        crate_data.type_decls[0]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("TypeTest")
    );

    assert_eq!(
        crate_data.type_decls[1]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("VariantsTest")
    );

    assert_eq!(
        crate_data.type_decls[1].kind.as_enum().unwrap()[0].renamed_name(),
        "Variant1"
    );
    assert_eq!(
        crate_data.type_decls[1].kind.as_enum().unwrap()[1].renamed_name(),
        "SimpleSecondVariant_"
    );

    assert_eq!(
        crate_data.type_decls[2]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("StructTest")
    );

    assert_eq!(
        crate_data.global_decls[1]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("Const_Test")
    );

    assert_eq!(
        crate_data.type_decls[3]
            .item_meta
            .attr_info
            .rename
            .as_deref(),
        Some("_TypeAeneas36")
    );

    assert_eq!(
        crate_data.type_decls[2].kind.as_struct().unwrap()[0]
            .attr_info
            .rename
            .as_deref(),
        Some("FieldTest")
    );
    Ok(())
}

#[test]
fn declaration_groups() -> anyhow::Result<()> {
    let crate_data = translate(
        r#"
        fn foo() {
            panic!()
        }
        trait Foo {
            const FOO: usize = 42;
        }
        impl Foo for () {}
        "#,
    )?;

    // There are 3 function items: one for `foo`, one for the initializer of `Trait::FOO`, and
    // one for the initializer of UNIT_METADATA (always included).
    assert_eq!(crate_data.fun_decls.iter().count(), 3);
    let decl_groups = crate_data.ordered_decls.unwrap();
    assert_eq!(decl_groups.len(), 6);

    Ok(())
}

#[test]
fn source_text() -> anyhow::Result<()> {
    let crate_data = translate(
        r#"
        fn foo() {
            panic!()
        }
        mod bar {
            fn baz( x : usize )  ->() { 
            let _ = x;
                } fn quux () {}
        }
        struct Foo { x: usize }
        trait Trait {
            fn method() {}
        }
        impl Trait for () {}
        "#,
    )?;

    let sources = crate_data
        .all_items()
        .filter_map(|i| i.item_meta().source_text.as_ref())
        .collect_vec();
    assert_eq!(sources[0], "struct Foo { x: usize }");
    assert_eq!(
        sources[1],
        "trait Trait {\n            fn method() {}\n        }"
    );
    assert_eq!(sources[2], "impl Trait for () {}");
    assert_eq!(sources[3], "fn foo() {\n            panic!()\n        }");
    assert_eq!(
        sources[4],
        "fn baz( x : usize )  ->() { \n            let _ = x;\n                }"
    );
    assert_eq!(sources[5], "fn quux () {}");
    Ok(())
}

#[test]
fn known_trait_method_call() -> anyhow::Result<()> {
    let crate_data = translate(
        r#"
        #[derive(Default)]
        struct Struct;
        fn use_default() -> Struct {
            Struct::default()
        }
        "#,
    )?;
    let function = &crate_data.fun_decls[1];
    assert_eq!(
        repr_name(&crate_data, &function.item_meta.name),
        "test_crate::use_default"
    );
    let body = &function.body.as_structured().unwrap().body;
    let [first_stmt, ..] = body.statements.as_slice() else {
        panic!()
    };
    let StatementKind::Call(call) = &first_stmt.kind else {
        panic!()
    };
    let FnOperand::Regular(fn_ptr) = &call.func else {
        panic!()
    };
    // Assert that this call referes to the method directly, without using a trait ref.
    let FnPtrKind::Fun(FunId::Regular(id)) = fn_ptr.kind.as_ref() else {
        panic!()
    };
    // This is the function that gets called.
    let function = &crate_data.fun_decls[id.index()];
    assert_eq!(
        repr_name(&crate_data, &function.item_meta.name),
        "test_crate::<impl Default for ??>::default"
    );
    let ItemSource::TraitImpl { .. } = &function.src else {
        panic!()
    };
    Ok(())
}

#[test]
fn multiple_deserialize() -> anyhow::Result<()> {
    // Test that deserializing deduplicated values from two different invocations of Charon works
    // correctly. This is non-obvious because `HashConsId`s will overlap between two invocations,
    // yet the deserialization side-table is not reset between deserializations. It's however ok
    // because the second deserialization simply overrides the `HashConsId -> HashCons<T>` mapping
    // from the first one. This all breaks down if someone tries to deserialize a sub-value of the
    // whole crate of course.
    let krate1 = translate(
        "
        fn foo(_: bool) {}
        fn bar(_: bool) {}
        ",
    )?;
    let krate2 = translate(
        "
        fn foo(_: u32) {}
        fn bar(_: u32) {}
        ",
    )?;
    let ty1_1 = krate1.fun_decls[1].signature.inputs[0].clone();
    let ty1_2 = krate1.fun_decls[2].signature.inputs[0].clone();
    let ty2_1 = krate2.fun_decls[1].signature.inputs[0].clone();
    let ty2_2 = krate2.fun_decls[2].signature.inputs[0].clone();
    assert_eq!(ty1_1.kind().as_literal(), Some(&LiteralTy::Bool));
    assert_eq!(ty1_2, ty1_1);
    assert_eq!(
        ty2_1.kind().as_literal(),
        Some(&LiteralTy::UInt(UIntTy::U32))
    );
    assert_eq!(ty2_2, ty2_1);
    Ok(())
}
