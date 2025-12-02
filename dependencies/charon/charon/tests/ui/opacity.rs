//@ charon-args=--include core::option
//@ charon-args=--exclude core::slice::raw::from_ref
//@ charon-args=--include test_crate::module::dont_translate_body
//@ charon-args=--opaque test_crate::module::dont_translate_body
//@ charon-args=--opaque crate::module::other_mod
//@ charon-args=--include crate::module::other_mod::_
//@ charon-args=--include core::convert::{core::convert::Into<_,_>}::into
//@ charon-args=--include core::convert::num::{core::convert::From<_,_>}
//@ charon-args=--opaque _::exclude_me
//@ charon-args=--exclude crate::invisible_mod
//@ charon-args=--exclude crate::keep_names_of_excluded
//@ charon-args=--include crate::keep_names_of_excluded::{crate::keep_names_of_excluded::Trait<_>}
// Note: we don't use the `impl Trait for T` syntax above because we can't have spaces in these
// options.
#![feature(register_tool)]
#![register_tool(charon)]

fn foo() {
    let _ = Some(0).is_some();
    let _: u64 = 42u32.into();
    let _ = std::slice::from_ref(&0);
}

mod module {
    fn dont_translate_body() {
        println!("aaa")
    }
    mod other_mod {
        fn dont_even_see_me() {}
    }
}

fn exclude_me() {}

mod invisible_mod {
    fn invisible() {}
}

mod keep_names_of_excluded {
    // We exclude this trait
    trait Trait {
        fn method();
    }

    // We want to include this impl, so we need to keep the name of the trait around.
    impl Trait for () {
        fn method() {
            let _ = 0;
        }
    }
}

struct Struct;

#[charon::opaque]
impl Struct {
    fn method() {
        let _ = 0;
    }
}

// Foreign modules can't be named or have attributes, so we can't mark them opaque.
#[charon::opaque]
extern "C" {
    fn extern_fn(x: i32);
}
