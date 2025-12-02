#![allow(unexpected_cfgs)]
// The `cfg` is here to test that we correctly passed rustc flags from the `Charon.toml`. The call
// to `is_some` is here to exercice the `extract_opaque_bodies` option.
#[cfg(abc)]
fn main() {
    let _ = Some(false).is_some();
}
