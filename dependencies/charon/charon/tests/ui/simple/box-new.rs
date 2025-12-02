//@ charon-args=--mir=optimized
//@ no-default-options
fn main() {
    let _ = Box::new(42);
}
