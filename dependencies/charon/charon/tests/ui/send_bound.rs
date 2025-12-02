fn foo<M: Send>(_msg: M) {}

fn main() {
    foo(());
}
