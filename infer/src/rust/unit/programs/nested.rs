pub fn swi_nested(a: i32, b: i32) -> i32 {
    if a >= 0 {
        if b == 0 { 1 } else { 2 }
    } else {
        3
    }
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = swi_nested(x, y);
}