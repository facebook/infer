//@ no-default-options
//@ charon-args=--ullbc --print-ullbc

struct Foo {}

impl Drop for Foo {
    fn drop(&mut self) {
        println!("Dropping Foo");
    }
}

fn main() {
    let f = Foo {};
    let _ = 255u8 + 1;
    let f_ref = &f; // force Foo to be dropped after the overflow
}
