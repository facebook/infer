#![allow(unexpected_cfgs)]
#[cfg(abc)]
pub const FOO: u8 = 42;

// Ensure compilation fails if the build script is not used correctly.
#[cfg(not(abc))]
const _: () = false;

fn main() {}
