fn apply_to_zero_mut(mut f: impl FnMut(u8) -> u8) -> u8 {
    f(0)
}

fn main() {
    let mut z = 3;
    apply_to_zero_mut(|x| {
        z += 1;
        x + z
    });
}
