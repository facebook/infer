//@aux-crate=trait-default-const.rs
//@charon-args=--extract-opaque-bodies
fn bar<T>() {
    trait_default_const::foo::<T>();
}
