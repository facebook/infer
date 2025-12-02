fn foo() {
    let _v = vec![1];
    let _v2 = vec![1; 10];
}

pub struct Foo;
pub fn bar() {
    let _ = vec![Foo];
}
