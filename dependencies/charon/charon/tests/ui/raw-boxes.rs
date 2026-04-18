//@ no-default-options
//@ charon-args=--extract-opaque-bodies
//@ charon-args=--mir elaborated
//@ charon-args=--exclude core::fmt::Formatter

unsafe fn foo() {
    let b = Box::new(42);
    let p = Box::into_raw(b);
    let _ = Box::leak(Box::new(42));
    let b = Box::from_raw(p);
    let i = *b;
}
