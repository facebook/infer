//@ known-failure
#![feature(generic_const_exprs)]
#![feature(adt_const_params)]
#![allow(incomplete_features)]
#![allow(dead_code)]
use std::marker::ConstParamTy;

#[derive(ConstParamTy, PartialEq, Eq, Debug)]
enum Foo {
    A,
    B,
}

fn foo<const X: Foo>() -> Foo {
    X
}

fn bar<const N: usize>()
where
    [(); N + 1]:,
{
    let _: [(); N + 1] = [(); N + 1];
}
