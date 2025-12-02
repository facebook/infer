use std::fmt::Debug;

fn debug_slice<T: Debug>(slice: &[T]) {
    for x in slice {
        eprintln!("- {x:?}");
    }
}

fn main() {
    debug_slice(&[0, 1, 2]);
}
