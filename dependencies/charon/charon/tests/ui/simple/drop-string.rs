//@ charon-args=--precise-drops
//@ charon-args=--include=alloc::string::String
fn use_string(_: String) {}

fn main() {
    let mut s = String::new();
    s = String::new();
}
