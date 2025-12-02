//@ charon-args=--monomorphize
//@ charon-args=--start-from=crate::main
// Ensures closures are monomorphized and replaced with static function calls

struct Thing;

fn apply_to_zero(f: impl Fn(u8) -> u8) -> u8 {
    f(0)
}

fn apply_to_zero_mut(mut f: impl FnMut(u8) -> u8) -> u8 {
    f(0)
}

fn apply_to_zero_once(f: impl FnOnce(u8) -> u8) -> u8 {
    f(0)
}

fn main() {
    let z = 1;
    apply_to_zero(|x| x + z);

    let mut z = 3;
    apply_to_zero_mut(|x| {
        z += 1;
        x + z
    });

    let z = Thing {};
    apply_to_zero_once(|x| {
        drop(z);
        x + 1
    });
}
