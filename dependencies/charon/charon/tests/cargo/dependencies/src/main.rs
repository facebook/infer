//! This tests the invocation of `charon` on a crate with external dependencies.

#[cfg(not(feature = "test_feature"))]
fn main() {}

#[cfg(feature = "test_feature")]
fn main() {
    fn silly_incr(x: &mut u32) {
        take_mut::take(x, |y| y + 1);
    }

    let mut x = 0;
    silly_incr(&mut x);
    assert_eq!(x, 1);
}
