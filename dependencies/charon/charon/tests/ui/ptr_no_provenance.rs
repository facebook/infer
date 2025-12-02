//@ charon-args=--include=core::ptr::null

fn main() {
    let ptr: *const () = core::ptr::null();
}
