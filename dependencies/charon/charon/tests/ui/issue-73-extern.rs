#![feature(extern_types)]
extern "C" {
    fn foo(x: i32);
    static CONST: u8;
    type Type;
}

fn use_type(_x: &Type) {}
