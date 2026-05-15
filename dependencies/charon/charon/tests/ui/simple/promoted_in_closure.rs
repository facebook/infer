//@ charon-args=--mir=optimized
// If we're not careful, the promoted uses the wrong generic params.
fn main() {
    let f = || {
        let _ = &42;
    };
}
