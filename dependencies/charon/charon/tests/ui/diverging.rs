fn my_panic(_x: u32) -> ! {
    panic!()
}

fn do_something_else() {}

fn call_my_panic() {
    my_panic(0);
    do_something_else();
}
