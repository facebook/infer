#![allow(unused)]

#[derive(Clone, Copy)]
struct Foo;
fn copy_foo(x: Foo) {
    let y = x;
    let z = x;
}

fn copy_generic<T: Copy>(x: T) {
    let y = x;
    let z = x;
}

trait Trait {
    type Ty: Copy;
}
fn copy_assoc_ty<T: Trait>(x: T::Ty) {
    let y = x;
    let z = x;
}
