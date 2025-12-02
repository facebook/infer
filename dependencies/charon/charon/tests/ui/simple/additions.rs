//@ charon-args=--extract-opaque-bodies
//! Tests the various ways to handle overflow are correctly translated.
fn main() {
    let _ = 255u8.wrapping_add(1);
    let _ = 255u8.overflowing_add(1);
    // This one is meant to be implemented by codegen backends, hence has no useful MIR.
    let _ = 255u8.saturating_add(1);
    // FIXME(#543): this causes a panic in hax
    // unsafe {
    //     let _ = 255u8.unchecked_add(1);
    // }
}
