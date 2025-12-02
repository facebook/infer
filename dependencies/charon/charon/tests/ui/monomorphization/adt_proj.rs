//@ charon-args=--monomorphize
// Ensures monomorphization happens when projections on discriminants of generic enums are
// involved -- in this case the discriminant of Result<u32, u32>

fn main() {
    let res: Result<u32, u32> = Ok(0);
    let Ok(n) = res else { panic!() };
}
