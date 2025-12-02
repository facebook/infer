fn takes_fn(f: &dyn for<'a> Fn(&'a mut u32) -> bool) {
    let mut counter = 0;
    if f(&mut counter) {}
}

fn gives_fn() {
    takes_fn(&|counter| {
        *counter += 1;
        true
    })
}
