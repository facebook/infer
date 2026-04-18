//@ charon-args=--monomorphize
fn foo<O>(x: O) -> O {
    let f: fn(O) -> O = const { |x| x };
    f(x)
}

pub fn main() {
    foo::<()>(());
}
