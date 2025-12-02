trait Ops {
    fn ZERO() -> Self;
    fn ntt_multiply() -> Self;
}

struct Portable;

fn ntt_multiply() -> Portable {
    Portable::ZERO()
}

impl Ops for Portable {
    fn ZERO() -> Self {
        Portable
    }

    fn ntt_multiply() -> Self {
        ntt_multiply()
    }
}
