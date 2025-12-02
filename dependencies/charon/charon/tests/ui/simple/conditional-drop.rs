//@ charon-args=--mir=elaborated
//@ charon-args=--add-drop-bounds
fn use_box(_: Box<u32>) {}

fn main() {
    let b = Box::new(1u32);
    if false {
        use_box(b);
    }
    // `b` is dropped implicitly here
}
