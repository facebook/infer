pub fn drop_implicit() -> i32 {
    let s = String::from("hello");
    let b = Box::new(42);
    
    *b
}

fn main() {
    let _ = drop_implicit();
}