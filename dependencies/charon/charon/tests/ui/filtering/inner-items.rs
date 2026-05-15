//@ charon-args=--start-from=crate::foo
//@ charon-args=--start-from=crate::_::bar
//@ charon-args=--start-from=crate::module
fn foo() {
    fn inner1() {}

    trait Trait {}
    impl Trait for () {}
}

struct Bar;
impl Bar {
    fn bar(&self) {
        fn inner2() {}
    }
}

mod module {
    const _: () = {
        fn inner3() {}
    };

    fn baz() {
        fn inner4() {}
    }

    #[charon::opaque]
    fn opaque() {
        fn dont_translate() {}
    }
}
