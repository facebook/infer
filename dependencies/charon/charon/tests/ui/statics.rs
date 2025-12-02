#![feature(raw_ref_op)]

fn constant() {
    const CONST: usize = 0;

    let _val = CONST;
    let _ref = &CONST;
    let _ref_mut = &mut CONST;
}

fn shared_static() {
    static SHARED_STATIC: usize = 0;

    let _val = SHARED_STATIC;
    let _ref = &SHARED_STATIC;
    let _ptr = &raw const SHARED_STATIC;
}

fn mut_static() {
    static mut MUT_STATIC: usize = 0;

    unsafe {
        let _val = MUT_STATIC;
        let _ref = &MUT_STATIC;
        let _ref_mut = &mut MUT_STATIC;
        let _ptr = &raw const MUT_STATIC;
        let _ptr_mut = &raw mut MUT_STATIC;
    }
}

fn non_copy_static() {
    struct Foo;
    impl Foo {
        fn method(&self) {}
    }

    static FOO: Foo = Foo;
    FOO.method()
}
