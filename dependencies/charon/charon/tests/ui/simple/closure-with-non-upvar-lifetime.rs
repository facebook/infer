fn call_fn(a: &[u8], i: usize) -> u8 {
    let read = |i: usize| -> u8 { (&a)[i] };
    read(i)
}
