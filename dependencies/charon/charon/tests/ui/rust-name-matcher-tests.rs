#![feature(register_tool)]
#![register_tool(pattern)]
mod foo {
    #[pattern::fail("test_crate::foo::bar::_")]
    #[pattern::pass("test_crate::foo::bar")]
    #[pattern::pass("test_crate::foo::_")]
    #[pattern::pass("test_crate::foo")]
    #[pattern::pass("test_crate")]
    #[pattern::pass("test_crate::_")]
    #[pattern::pass("test_crate::_::bar")]
    #[pattern::fail("test_crate::_::lsdkfjs")]
    #[pattern::pass("crate::foo::bar")]
    #[pattern::fail("foo::bar")]
    fn bar() {}
}

#[pattern::pass("test_crate::Trait")]
#[pattern::fail("test_crate::Trait<_>")]
#[pattern::pass("test_crate::Trait<_, _>")]
trait Trait<T> {
    #[pattern::pass("test_crate::Trait::method")]
    #[pattern::pass("test_crate::Trait<_>::method")]
    fn method<U>();
}

#[pattern::pass("test_crate::{impl test_crate::Trait<_> for _}")]
#[pattern::pass("_::{impl test_crate::Trait<_> for _}")]
#[pattern::pass("*::{impl test_crate::Trait<_> for _}")]
#[pattern::pass("{impl test_crate::Trait<_> for _}")]
#[pattern::pass("test_crate::{impl test_crate::Trait<_, _>}")]
#[pattern::fail("test_crate::{impl test_crate::Trait<_>}")]
#[pattern::fail("test_crate::{impl test_crate::Trait<_, _> for _}")]
#[pattern::pass(
    "test_crate::{impl test_crate::Trait<core::option::Option<_>> for alloc::boxed::Box<_, _>}"
)]
#[pattern::pass(
    "test_crate::{impl test_crate::Trait<alloc::boxed::Box<_, _>, core::option::Option<_>>}"
)]
#[pattern::pass(
    "test_crate::{impl test_crate::Trait<core::option::Option<_>> for alloc::boxed::Box<_, _>}"
)]
#[pattern::fail("test_crate::{impl test_crate::Trait<Option<_>> for alloc::boxed::Box<_, _>}")]
#[pattern::fail("test_crate::{impl test_crate::Trait<FooBar<_>> for alloc::boxed::Box<_, _>}")]
#[pattern::fail("test_crate::{impl test_crate::Trait<core::option::Option<_>> for FooBar<_>}")]
#[pattern::fail("test_crate::{impl Trait<_> for _}")]
#[pattern::fail("test_crate::{impl test_crate::OtherTrait<_> for _}")]
#[pattern::fail("test_crate::Trait<_>")]
impl<T> Trait<Option<T>> for Box<T> {
    #[pattern::pass("test_crate::{impl test_crate::Trait<_> for _}::method")]
    #[pattern::pass("{impl test_crate::Trait<_> for _}::method")]
    #[pattern::pass("test_crate::{impl test_crate::Trait<_> for _}::_")]
    #[pattern::pass("test_crate::{impl test_crate::Trait<_> for _}")]
    #[pattern::pass("test_crate::{impl test_crate::Trait<_, _>}"::method)]
    #[pattern::fail("test_crate::Trait<_>::method")]
    fn method<U>() {}
}

#[pattern::pass("test_crate::{impl test_crate::Trait<_> for Slice<_>}")]
impl<T> Trait<T> for [T] {
    fn method<U>() {}
}

#[pattern::pass("test_crate::{impl test_crate::Trait<_> for &Slice<_>}")]
impl<T> Trait<T> for &[T] {
    fn method<U>() {}
}
