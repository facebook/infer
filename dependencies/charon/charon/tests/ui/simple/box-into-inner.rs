//@ charon-args=--mir_optimized
fn into_inner(b: Box<String>) {
    let _x = *b;
}
