#![feature(register_tool)]
#![register_tool(charon)]
//! Test that we scope things properly.

trait Trait<'a> {
    fn method<'b>(&'b self) -> &'b ();
}

impl<'a> Trait<'a> for &'a () {
    #[charon::opaque]
    fn method<'b>(&'b self) -> &'b () {
        self
    }
}

#[charon::opaque]
struct Foo<'a>(&'a ());

impl Foo<'_> {
    #[charon::opaque]
    fn bar(&self) -> &() {
        self.0
    }
}

#[charon::opaque]
fn foo(_: &fn(&u32) -> u32) {}

#[charon::opaque]
fn bar(_: &fn(&fn(&u32) -> u32)) {}

#[charon::opaque]
fn baz<'a>(_: &'a for<'b> fn(&'a u32, &'b for<'c> fn(&'a u32, &'b u32, &'c u32) -> u32)) {}
