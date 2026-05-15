fn foo() -> bool {
    false
}
fn bar() -> bool {
    true
}
fn do_something() {}
fn do_something_at_the_end() {}

fn main() {
    // Needed a few convolutions to make it emit a `break` instead of moving the `return`
    // inside the loop.
    if foo() {
        loop {
            do_something();
            if foo() || bar() {
                break;
            }
        }
    }
    do_something_at_the_end()
}
