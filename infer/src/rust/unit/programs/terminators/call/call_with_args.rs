fn call_with_args(x: i32, y: i32) -> i32 {
    add(x, y)
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = call_with_args(x, y);
}