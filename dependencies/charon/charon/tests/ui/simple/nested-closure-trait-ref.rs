fn foo<T: Clone>(x: T) -> impl Fn() -> T {
    let f = move || move || x.clone();
    f()
}
