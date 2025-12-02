//@ charon-args=--mir=elaborated
//@ charon-args=--add-drop-bounds
//@ charon-args=--include=alloc::string::String
fn use_string(_: String) {}

fn main() {
    let _s = String::new();
}
