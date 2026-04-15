//@ charon-args=--start-from-if-exists crate::foo
//@ charon-args=--start-from-if-exists crate::bar

// no foo here!
// fn foo() {}

fn bar() {}
