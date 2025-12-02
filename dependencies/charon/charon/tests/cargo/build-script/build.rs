fn main() {
    // Set something that we can detect in `main.rs` to be sure the build script was used
    // correctly.
    println!("cargo:rustc-cfg=abc");
}
