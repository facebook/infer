fn main() {
    let b = Box::new(42);
    std::mem::forget(b);
}