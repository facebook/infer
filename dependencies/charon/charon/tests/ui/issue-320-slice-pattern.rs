fn slice_pat1() {
    let array: [_; 4] = [0; 4];
    let [_a, _b @ .., _c] = array;
}

fn slice_pat2() {
    let array_ref: &[_; 4] = &[0; 4];
    let [_a, _b @ .., _c] = array_ref;
}

fn slice_pat3() {
    let slice: &[_] = &[0; 4];
    let [_a, _b @ .., _c] = slice else { panic!() };
}

fn slice_pat4(x: &[u32]) {
    match x {
        [_y] => {}
        _ => {}
    }
}

/// This test generates a `RValue::Len` on a place that isn't `Deref`. It's a regression test to
/// avoid the mistaken assumption that `Len` only applies to `Deref` places.
struct Unsized([u32]);
fn slice_pat5(x: &Unsized) {
    match x.0 {
        [_y] => {}
        _ => {}
    }
}
