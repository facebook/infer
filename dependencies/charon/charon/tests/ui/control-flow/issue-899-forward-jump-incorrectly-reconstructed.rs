fn condition(_x: u32) -> bool {
    true
}
fn foo() {}
fn bar() {}

fn absorb_full() {
    // bb0
    loop {
        // bb1
        if condition(1) {
            // bb3
            if condition(2) {
                // bb7
                foo()
            } else {
                // bb8
                break;
            }
            // bb10
        }

        // bb6
        if condition(3) {
            // bb12
            return;
        }

        // unconditional break, the `loop` is only for fwd jumping
        break;
    }
    // bb13
    bar()
}
