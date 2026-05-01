//@ charon-args=--mir optimized
fn into_inner(b: Box<String>) {
    let _x = *b;
}
