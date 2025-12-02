use std::path::PathBuf;

use charon_lib::ast::*;

mod util;
use indexmap::IndexMap;
use util::*;

#[test]
fn ptr_metadata() -> anyhow::Result<()> {
    let crate_data = translate_rust_text(
        r#"
    #![feature(ptr_metadata)]
    use std::ptr::Thin;

    type Alias = str;
    struct StrDst {
      meta : i32,
      str : str
    }

    struct SliceDst {
      meta : u64,
      slice : [u8]
    }

    struct Point { x : i32, y : u32, z : i128 }
    struct MoreSliceDst {
      meta : u128,
      slice : [Point]
    }

    struct Embedded {
      meta : i32,
      more : MoreSliceDst
    }

    trait Showable { fn show(&self) -> &str; }
    struct DynTrait {
      meta : u32,
      dynt : dyn Showable
    }

    struct GenericInLastField<T> {
        x: u32,
        y: T,
    }
    struct GenericNotLastField<T> {
        x: u32,
        y: T,
        z: u32,
    }
    struct GenericBehindIndirection<T> {
        x: u32,
        y: Box<T>,
    }
    // Charon doesn't recognize that we know the metadata in this case. That's ok.
    struct ThinGeneric<T: Thin> {
        x: u32,
        y: T,
    }
    "#,
        &[],
    )?;
    let meta_kinds: IndexMap<String, Option<&PtrMetadata>> = crate_data
        .type_decls
        .iter()
        .map(|td| {
            let name = repr_name(&crate_data, &td.item_meta.name);
            (name, td.ptr_metadata.as_ref())
        })
        .collect();
    let str = serde_json::to_string_pretty(&meta_kinds)?;

    let action = if std::env::var("IN_CI").as_deref() == Ok("1") {
        Action::Verify
    } else {
        Action::Overwrite
    };
    compare_or_overwrite(action, str, &PathBuf::from("./tests/ptr-metadata.json"))?;
    Ok(())
}
