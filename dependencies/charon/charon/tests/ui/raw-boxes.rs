//@ charon-args=--extract-opaque-bodies
//@ charon-args=--raw-boxes
//@ charon-args=--mir elaborated

unsafe fn foo() {
    let b = Box::new(42);
    let p = Box::into_raw(b);
    let _ = Box::leak(Box::new(42));
    let b = Box::from_raw(p);
    let i = *b;
}
