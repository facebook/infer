//@ charon-args=--extract-opaque-bodies

fn unwrap(res: Result<u32, u32>) -> u32 {
    res.unwrap()
}
