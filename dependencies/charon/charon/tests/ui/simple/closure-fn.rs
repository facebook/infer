fn apply_to(f: &impl Fn(u8, u8) -> u8) -> u8 {
    f(10, 20)
}

fn apply_to_mut(f: &mut impl FnMut(u8, u8) -> u8) -> u8 {
    f(10, 20)
}

fn apply_to_once(f: impl FnOnce(u8, u8) -> u8) -> u8 {
    f(10, 20)
}

fn main() {
    let v = 5;
    let z = 1;
    let mut f = |x, y| v + x + y + z;
    f(10, 20);
    apply_to(&f);
    apply_to_mut(&mut f);
    apply_to_once(f);
}
