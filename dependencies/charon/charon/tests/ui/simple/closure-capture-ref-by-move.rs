fn foo() {
    let mut x = 0;
    let rx = &mut x;
    let mut closure = move || {
        *rx += 1;
    };
    closure();
}
