//@ charon-args=--skip-borrowck
fn choose<'a, T>(b: bool, x: &'a mut T, y: &'a mut T) -> &'a mut T {
    if b {
        x
    } else {
        y
    }
}

pub fn choose_test() {
    let mut x = 0;
    let mut y = 0;
    let z = choose(true, &mut x, &mut y);
    *z += 1;
    assert!(*z == 1);
    // drop(z)
    assert!(x == 1);
    assert!(y == 0);
    assert!(*z == 1); // z is not valid anymore
}
