//@ charon-args=--monomorphize
/// Reproducer for a bug whereby a bound lifetime was not allowed in monomorphic context.
fn foo(x: &u32) -> Option<&u32> {
    Some(x)
}

fn main() {
    let _ = foo(&42);
}
