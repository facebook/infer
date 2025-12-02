//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main

fn apply_to_zero_once(f: impl FnOnce(u8) -> u8) -> u8 {
    f(0)
}

struct NotCopy;

fn main() {
    let z = NotCopy {};
    apply_to_zero_once(|x| {
        drop(z);
        x + 1
    });
}
