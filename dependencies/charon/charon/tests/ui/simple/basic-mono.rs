//@ charon-args=--monomorphize

fn foo<const K: usize>() {}

fn bar<T>() {}

fn main() {
    foo::<10>();
    bar::<u32>();
}
