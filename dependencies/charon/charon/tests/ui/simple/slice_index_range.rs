//@ charon-args=--extract-opaque-bodies
pub fn slice_index_range(slice: &[u8]) -> &[u8] {
    &slice[0..=10]
}
