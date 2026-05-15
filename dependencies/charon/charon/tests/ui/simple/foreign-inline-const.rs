//@ aux-crate=closure-inside-inline-const.rs
//@ charon-args=--include=closure_inside_inline_const
fn bar<T>() {
    closure_inside_inline_const::foo::<T>();
}
