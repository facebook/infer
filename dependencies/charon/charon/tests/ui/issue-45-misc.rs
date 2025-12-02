pub fn map(x: [i32; 256]) -> [i32; 256] {
    x.map(|v| v)
}

pub fn array<const LEN: usize>() -> [u8; LEN] {
    [0u8; LEN]
}

fn cbd(mut prf_input: [u8; 33]) {
    for i in 0..3 {
        prf_input[0] = i;
    }
}

pub(crate) fn select(lhs: &[u8], rhs: &[u8]) {
    debug_assert_eq!(lhs.len(), rhs.len());
}
