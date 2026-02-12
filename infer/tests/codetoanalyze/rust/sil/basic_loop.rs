pub fn goto_loop(mut n: i32) -> i32 {
    loop {
        n += 1;
        if n > 5 {
            break;
        }
    }
    n
}

fn main() {
    let x: i32 = 0;
    let _ = goto_loop(x);
}