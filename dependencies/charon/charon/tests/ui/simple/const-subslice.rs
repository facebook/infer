//@ charon-args=--mir_optimized
fn main() {
    let y: &[u8] = const {
        let x: &[u8] = &[0, 1, 2, 3, 4];
        unsafe { std::slice::from_raw_parts(x as *const [u8] as *const u8, 3) }
    };
    let z: *const u8 = y as *const [u8] as *const u8;
    let _ = z as usize;
}
