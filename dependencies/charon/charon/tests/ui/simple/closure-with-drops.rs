//@ charon-args=--precise-drops
fn foo<T>(x: T) {
    let _ = || drop(x);
}

fn bar() {
    let x = || {};
}
