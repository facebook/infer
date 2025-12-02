//@ charon-args=--include core::cmp::impls::_::partial_cmp

fn main() {
    let x: i32 = 11;
    let y: i32 = 22;
    let _ = x.cmp(&y);
}
