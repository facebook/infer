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
