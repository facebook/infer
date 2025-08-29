pub fn goto_with_continue(mut x: i32, y: i32) -> i32 {
    loop {
        if x < y {
            x += 1;
            continue;
        }
        break;
    }
    x
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let _ = goto_with_continue(x, y);
}