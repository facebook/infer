// This makes the arg lifetime early-bound wrt the closure signature.
fn make_early<'a, T>(_: fn(&'a T)) {}

fn main() {
    let _ = |_: &u8| ();
    make_early(|_: &u16| ());
}
