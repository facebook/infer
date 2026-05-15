fn rej_sample(a: &[u8]) -> usize {
    let mut sampled = 0;
    if a[0] < 42 && a[1] < 16 {
        sampled += 100;
    } else {
        // This statement is duplicated.
        sampled += 101;
    }
    sampled
}
