//@ charon-arg=--start-from=crate::module1
//@ charon-arg=--start-from=test_crate::module2
//@ charon-arg=--start-from=std::iter::once
//@ charon-arg=--start-from=std::clone::Clone::clone_from
//@ charon-arg=--start-from=std::slice::Iter::as_slice
//@ charon-arg=--start-from=u32::wrapping_add
//@ charon-arg=--start-from=crate::*::do_translate_glob
//@ charon-arg=--start-from={impl crate::hidden_module::Trait1 for crate::hidden_module::Type1}::method
//@ charon-arg=--start-from={impl crate::hidden_module::Trait2 for _}

fn dont_translate() {}

mod module1 {
    fn do_translate() {}
    fn do_translate_glob() {}
}
mod module2 {
    fn do_translate() {}
    fn do_translate_glob() {}
}
mod hidden_module {
    fn dont_translate() {}
    fn do_translate_glob() {}

    struct Type1;
    struct Type2;
    trait Trait1 {
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
