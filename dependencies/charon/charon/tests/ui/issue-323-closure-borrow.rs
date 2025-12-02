struct Rng;

impl Rng {
    fn next_u64(&mut self) {}
}

fn new(rng: &mut Rng) {
    let _ = || {
        rng.next_u64();
    };
}
