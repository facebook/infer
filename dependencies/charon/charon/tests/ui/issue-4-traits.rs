use std::convert::TryInto;
fn trait_error(s: &[u8]) {
    let _array: [u8; 4] = s.try_into().unwrap();
}
