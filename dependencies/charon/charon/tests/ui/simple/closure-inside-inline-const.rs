// This tests a closure inside a non-evaluable inline constant. This is tricky because both these
// items get fake generic params in rustc, for type inference purposes. Yet the generic args for
// the closure don't include the extra arg for its parent inline constant. Generally speaking we
// must manage these extra args by hand in hax. Thankfully they don't show up inside the MIR
// bodies, only in signatures.
pub fn foo<T>() -> usize {
    const {
        let _f = || 42;
        std::mem::size_of::<T>()
    }
}
