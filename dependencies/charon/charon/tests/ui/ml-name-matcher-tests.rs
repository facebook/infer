//@ no-default-options
//@ charon-args=--hide-allocator
//@ charon-args=--treat-box-as-builtin
//@ charon-args=--ops-to-function-calls
//@ charon-args=--index-to-function-calls
//@ charon-args=--unbind-item-vars
#![feature(register_tool)]
#![register_tool(pattern)]
//! Tests for the ml name matcher. This is in the rust test suite so that the llbc file gets
//! generated. Tests on the ml side will then inspect the file and check that each item matches the
//! specified pattern.

mod foo {
    #[pattern::pass("test_crate::foo::bar")]
    #[pattern::fail("crate::foo::bar")]
    #[pattern::fail("foo::bar")]
    fn bar() {}
}

trait Trait<T> {
    fn method<U>();
}

impl<T> Trait<Option<T>> for Box<T> {
    #[pattern::pass(
        "test_crate::{test_crate::Trait<alloc::boxed::Box<@T>, core::option::Option<@T>>}::method<@T, @U>"
    )]
    // `Box` is special: it can be abbreviated.
    #[pattern::pass("test_crate::{test_crate::Trait<Box<@T>, core::option::Option<@T>>}::method<@T, @U>")]
    // Can't abbreviate `Option`, only `Box` is special like that.
    #[pattern::fail("test_crate::{test_crate::Trait<Box<@T>, Option<@T>>}::method<@T, @U>")]
    // More general patterns work too.
    #[pattern::pass(
        "test_crate::{test_crate::Trait<alloc::boxed::Box<@T>, core::option::Option<@U>>}::method<@T, @U>"
    )]
    // TODO: the trait reference in the pattern below is wrong: it should fail
    #[pattern::pass("test_crate::{test_crate::Trait<@T, @U>}::method<@T, @U>")]
    // TODO: the pattern below should work
    #[pattern::fail("test_crate::Trait<@T>::method")]
    fn method<U>() {}
}

impl<T, U> Trait<Box<T>> for Option<U> {
    // Using the same variable name twice means they must match. This is not the case here.
    // TODO: this should not pass!
    #[pattern::pass(
        "test_crate::{test_crate::Trait<core::option::Option<@T>, alloc::boxed::Box<@T>>}::method<@T, @U, @V>"
    )]
    #[pattern::pass(
        "test_crate::{test_crate::Trait<core::option::Option<@U>, alloc::boxed::Box<@T>>}::method<@T, @U, @V>"
    )]
    fn method<V>() {}
}

// `call[i]` is a hack to be able to refer to `Call` statements inside the function body.
#[pattern::pass(call[0], "core::option::{core::option::Option<@T>}::is_some<'_, i32>")]
#[pattern::fail(call[0], "core::option::{core::option::Option<@T>}::is_some<_, @T>")]
#[pattern::pass(call[0], "core::option::{core::option::Option<@T>}::is_some<'_, @U>")]
#[pattern::fail(call[0], "core::option::{core::option::Option<i32>}::is_some<_, i32>")]
#[pattern::pass(call[1], "ArrayToSliceShared<'_, bool, 1>")]
// This is a trait instance call.
#[pattern::pass(call[2], "core::ops::index::Index<[bool], core::ops::range::RangeFrom<usize>>::index")]
#[pattern::pass(call[2], "core::ops::index::Index<[@T], @I>::index")]
// We can reference the method directly.
// TODO: this should pass!
#[pattern::fail(call[2], "core::slice::index::{core::ops::index::Index<[@T], @I>}::index<bool, core::ops::range::RangeFrom<usize>>")]
fn foo() {
    let _ = Some(0).is_some();
    let slice: &[bool] = &[false];
    let _ = &slice[1..];
}

#[pattern::pass(call[0], "test_crate::funs_with_disambiguator::f")]
#[pattern::pass(call[0], "test_crate::funs_with_disambiguator::f#0")]
#[pattern::fail(call[0], "test_crate::funs_with_disambiguator::f#1")]
#[pattern::pass(call[1], "test_crate::funs_with_disambiguator::f#1")]
#[pattern::fail(call[1], "test_crate::funs_with_disambiguator::f#0")]
#[pattern::fail(call[1], "test_crate::funs_with_disambiguator::f")]
fn funs_with_disambiguator(b: bool) -> u32 {
    if b {
        fn f() -> u32 {
            0
        }
        f()
    } else {
        fn f() -> u32 {
            1
        }
        f()
    }
}

struct MonoContainer<T> {
    item: T,
}

impl<T> MonoContainer<T> {
    #[pattern::pass("test_crate::{test_crate::MonoContainer<@T>}::create<@T>")]
    #[pattern::pass("test_crate::_::create<@T>")]
    fn create(item: T) -> Self {
        Self { item }
    }
}

#[pattern::pass("test_crate::mono_usage")]
fn mono_usage() {
    let _container1 = MonoContainer::create(42i32);
    let _container2 = MonoContainer::create("test");
}

trait Get<'a, T> {
    fn get(self) -> &'a T;
}

impl<'a, T> Get<'a, T> for &'a MonoContainer<T> {
    #[pattern::to_name("test_cratetest_crateGetASharedAtest_crateMonoContainerTTget")]
    fn get(self) -> &'a T {
        &self.item
    }
}
