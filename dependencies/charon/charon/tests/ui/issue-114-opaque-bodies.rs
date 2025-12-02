//@ charon-args=--extract-opaque-bodies
//@ aux-crate=issue-114-opaque-bodies-aux.rs

fn use_inlines() -> u32 {
    use issue_114_opaque_bodies_aux::*;
    inline_always() + inline_sometimes() + inline_never() + inline_generic::<bool>()
}

fn bool_to_opt(b: bool) -> Option<()> {
    b.then_some(())
}

fn convert(x: i32) -> i64 {
    i64::from(x)
}

fn vec(_x: Vec<u32>) {}

fn max() -> usize {
    usize::MAX
}

fn partial_eq<T: PartialEq>(_x: T) {}
