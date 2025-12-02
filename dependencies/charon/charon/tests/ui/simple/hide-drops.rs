//@ charon-arg=--exclude={impl core::ops::drop::Drop for _}
fn use_string(_: String) {}

fn main() {
    let _s = String::new();
}
