fn foo() {}

fn main() {
    let f: fn() = foo;
    f();
}
