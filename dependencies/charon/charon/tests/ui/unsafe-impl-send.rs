//! Regression test for something that caused a panic in charon.

struct Foo(*const ());

unsafe impl Send for Foo {}
