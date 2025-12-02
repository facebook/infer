union Foo {
    one: u64,
    two: [u32; 2],
}

fn use_union() {
    let mut one = Foo { one: 42 };
    unsafe {
        one.one = 43;
    }
    let _two = unsafe { one.two };
}
