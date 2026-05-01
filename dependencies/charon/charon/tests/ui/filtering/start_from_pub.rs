//@ charon-arg=--start-from-pub
fn dont_translate() {}

mod module1 {
    pub fn do_translate() {}
}

pub mod module2 {
    fn dont_translate() {}

    pub fn do_translate() {}

    pub struct Type1;
    struct Type2;

    pub trait Trait1 {
        fn method();
    }
    impl Trait1 for Type1 {
        fn method() {}
    }
    impl Trait1 for Type2 {
        fn method() {
            println!("don't translate this!")
        }
    }

    trait Trait2 {}
    impl Trait2 for Type1 {}
}
