//! Tests that we correctly translate opaque traits.
//!
//! Constraints:
//! - an impl must have the same set of methods as the trait it implements;
//! - trait methods that are never used are considered not to exist.
//!
//! A method use is:
//! - A call to the trait method via a `TraitMethod` kind of call;
//! - The implementation of a trait method in an impl block.
//!
//! We keep all const and const initializers for now.
#![feature(register_tool)]
#![register_tool(charon)]

#[charon::opaque]
trait Trait {
    // Used in a function body.
    const CONST1: usize = 0;
    // Used in an implementation.
    const CONST2: usize = 0;
    const CONST3: usize = 0;
    // Used in a function body.
    fn method1() {}
    // Used in an implementation.
    fn method2() {}
    fn method3() {}
}

#[charon::opaque]
impl Trait for () {
    const CONST1: usize = 1;
    const CONST2: usize = 1;
    const CONST3: usize = 1;
    fn method1() {}
    fn method2() {}
    fn method3() {}
}

impl Trait for u8 {
    const CONST2: usize = 2;
    // TODO: don't trigger translation of defaulted methods.
    fn method2() {}
}

fn use_method<T: Trait>() {
    T::method1();
    T::CONST1;
}
