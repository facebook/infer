use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;
use itertools::Itertools;

use charon_lib::ast::*;
use charon_lib::name_matcher::Pattern;

mod util;

static TEST_FILE: &str = "tests/ui/rust-name-matcher-tests.rs";

fn parse_pattern_attr(a: &Attribute) -> Option<(bool, Pattern)> {
    let Attribute::Unknown(a) = a else {
        return None;
    };
    let (pass, a) = if a.path == "pattern::pass" {
        (true, a)
    } else if a.path == "pattern::fail" {
        (false, a)
    } else {
        return None;
    };
    let a = a.args.as_ref()?;
    let a = a.strip_prefix("\"")?.strip_suffix("\"")?;
    match Pattern::parse(a) {
        Ok(pat) => Some((pass, pat)),
        Err(e) => {
            panic!("Failed to parse pattern `{a}` ({e})")
        }
    }
}

#[test]
fn test_name_matcher() -> anyhow::Result<()> {
    let crate_data = util::translate_rust_text(&std::fs::read_to_string(TEST_FILE)?, &[])?;
    let fmt_ctx = &crate_data.into_fmt();

    for item in crate_data.all_items() {
        let name = &item.item_meta().name;
        let patterns = item
            .item_meta()
            .attr_info
            .attributes
            .iter()
            .filter_map(|a| parse_pattern_attr(a))
            .collect_vec();
        for (pass, pat) in patterns {
            let passes = pat.matches_item(&crate_data, item);
            if passes != pass {
                if passes {
                    panic!(
                        "Pattern `{pat}` passes on `{}` but shouldn't",
                        name.with_ctx(fmt_ctx)
                    );
                } else {
                    panic!(
                        "Pattern `{pat}` doesn't pass on `{}` but should",
                        name.with_ctx(fmt_ctx)
                    );
                }
            }
        }
    }

    Ok(())
}
