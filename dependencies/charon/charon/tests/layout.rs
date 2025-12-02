#![feature(box_patterns)]
use std::path::PathBuf;

use indexmap::IndexMap;

use charon_lib::ast::*;

mod util;
use util::*;

#[test]
fn type_layout() -> anyhow::Result<()> {
    let crate_data = translate_rust_text(
        r#"
        #![feature(never_type)]
        use std::num::NonZero;
        use std::fmt::Debug;

        struct SimpleStruct {
            x: u32,
            y: u32,
            z: u32
        }

        struct GenericStruct<T> {
            a: usize,
            b: T
        }

        struct UnsizedStruct {
            x: usize,
            y: [usize]
        }

        enum SimpleEnum {
            Var1,
            Other,
        }

        enum SimpleAdt {
            EmptyVar,
            StructVar { x: usize, y: usize },
            TupleVar(u32, u32),
        }

        enum NicheAdt {
            None,
            Some(NonZero<u32>)
        }

        struct IsAZST;

        struct UnsizedStruct2 {
            x: usize,
            y: dyn Debug
        }

        struct GenericWithKnownLayout<T> {
            x: usize,
            y: Box<T>,
        }

        // Rust reorders the fields to save space.
        struct Reordered {
            x: u8,
            y: u32,
            z: u8,
        }

        // `repr(C)` prevents reordering the fields.
        #[repr(C)]
        struct NotReordered {
            x: u8,
            y: u32,
            z: u8,
        }

        #[repr(packed)]
        struct Packed {
            x: u8,
            y: u32,
            z: u8,
        }

        enum UninhabitedVariant {
            A(!, u32),
            B(u32),
        }

        struct Uninhabited(!);

        enum DiscriminantInNicheOfField<'a,T> {
            None,
            Some((usize, &'a T))
        }

        union PackIntsUnion {
            x: (u32, u32),
            y: u64,
        }

        enum NonZeroNiche {
            A(char),
            B,
            C,
        }

        #[repr(i32)]
        enum ArbitraryDiscriminants {
            A(String) = 12,
            B(u32) = 43,
            C = 123456,
        }

        #[repr(i8)]
        enum MyOrder {
            Less = -1,
            Equal = 0,
            Greater = 1,
        }

        enum WithNicheAndUninhabited {
            First,
            Second(!),
            Third(NonZero<u32>)
        }

        enum GenericUnsized<'a, T: ?Sized> {
            First,
            Second(&'a T),
        }

        enum GenericButFixedSize<'a, T: Sized> {
            First,
            Second(&'a T),
        }

        #[repr(u128)]
        enum MaxBitsDiscr {
            First = 42,
            Second = 18446744073709551615,
        }
        "#,
        &[],
    )?;

    // Check whether niche discriminant computations are correct, i.e. reversible.
    for tdecl in crate_data.type_decls.iter() {
        if let Some(layout) = tdecl.layout.as_ref() {
            if layout.discriminant_layout.is_some() {
                let name = repr_name(&crate_data, &tdecl.item_meta.name);
                for (var_id, variant) in layout.variant_layouts.iter_indexed() {
                    let tag = variant.tag;
                    if layout.is_variant_uninhabited(var_id) {
                        assert_eq!(
                            None, tag,
                            "For type {} with uninhabited variant {} something went wrong!",
                            name, var_id
                        );
                    } else {
                        match tag {
                            None => (), // Must be the untagged variant
                            Some(tag) => {
                                let roundtrip_var_id = tdecl.get_variant_from_tag(tag.clone());
                                assert_eq!(
                                    Some(var_id),
                                    roundtrip_var_id,
                                    "For type {} something went wrong, tag = {:?}",
                                    name,
                                    tag
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    let layouts: IndexMap<String, Option<Layout>> = crate_data
        .type_decls
        .iter()
        .filter_map(|tdecl| {
            if tdecl.item_meta.name.name[0].as_ident().unwrap().0 != "test_crate" {
                return None;
            }
            let name = repr_name(&crate_data, &tdecl.item_meta.name);
            Some((name, tdecl.layout.clone()))
        })
        .collect();
    let layouts_str = serde_json::to_string_pretty(&layouts)?;

    let action = if std::env::var("IN_CI").as_deref() == Ok("1") {
        Action::Verify
    } else {
        Action::Overwrite
    };
    compare_or_overwrite(action, layouts_str, &PathBuf::from("./tests/layout.json"))?;
    Ok(())
}
