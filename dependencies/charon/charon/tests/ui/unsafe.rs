//@ charon-args=--exclude=core::ptr::metadata::Thin

//! The 5 "unsafe superpowers" (https://doc.rust-lang.org/book/ch19-01-unsafe-rust.html#unsafe-superpowers).
#![feature(core_intrinsics)]
#![allow(internal_features)]

fn call_unsafe_fn() {
    let x: *const _ = std::ptr::null::<u32>();
    let _ = unsafe { x.read() };
}

fn deref_raw_ptr() {
    let x: *const _ = std::ptr::null::<u32>();
    let _ = unsafe { *x };
}

unsafe trait Trait {}

unsafe impl Trait for () {}

static mut COUNTER: usize = 0;

fn access_mutable_static() {
    unsafe {
        COUNTER += 1;
    }
}

union Foo {
    one: u64,
    two: [u32; 2],
}

fn access_union_field() {
    let one = Foo { one: 42 };
    let _two = unsafe { one.two };
}

fn assume() {
    unsafe { std::intrinsics::assume(true) }
}
