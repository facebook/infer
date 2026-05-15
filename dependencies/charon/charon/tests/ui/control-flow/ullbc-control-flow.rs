//@ charon-args=--ullbc --print-ullbc --no-serialize
//! Test the pre-reconstruction output so we don't accidentally regress passes in ways that are
//! invisible post-reconstruction.
pub fn nested_loops_enum(step_out: usize, step_in: usize) -> usize {
    let mut s = 0;

    for _ in 0..128 {
        s += 1;
    }

    for _ in 0..(step_out) {
        // Test comment
        for _ in 0..(step_in) {
            s += 1;
            assert!(s >= 1);
        }
    }

    s
}
