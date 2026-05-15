fn do_something() {}
fn do_something_else() {}
fn do_something_at_the_end() {}

// This has a cfg in the form of a diamond with a diagonal: can't be mapped to simple nested `if`s.
// This may require duplicating a node or adding intermediate branching flags.
fn foo(opt: Option<u32>) {
    match opt {
        Some(x) if x >= 42 => do_something(),
        _ => do_something_else(),
    }
    do_something_at_the_end()
}
