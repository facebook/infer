fn slice_pattern_end(x: &[()]) {
    match x {
        [.., _named] => (),
        _ => (),
    }
}
