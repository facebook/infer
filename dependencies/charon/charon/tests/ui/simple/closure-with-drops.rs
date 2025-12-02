//@ charon-args=--add-drop-bounds
fn foo<T>(x: T) {
    let _ = || drop(x);
}

fn bar() {
    let x = || {};
}
