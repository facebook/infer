//@ known-failure
fn foo<'a, T: Clone>(x: &'a T) -> impl Fn() -> &'a T {
    let f = move || move || x;
    f()
}
