pub fn compare(x: i32, y: i32) -> i32 {
    if x > y { x } else { y }
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = compare(x, y);
}