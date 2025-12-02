//@ known-failure
pub fn next(b: bool) -> Option<Vec<u8>> {
    let vec = vec![if b { 42 } else { return None }];
    Some(vec)
}
