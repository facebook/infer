//@ charon-args=--precise-drops
//@ charon-args=--desugar-drops
//@ charon-args=--include=alloc::string::String
fn use_string(_: String) {}

fn main() {
    let _s = String::new();
}
