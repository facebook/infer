fn main() {
    let x = ifelse(true,10,20);
}

fn ifelse(b : bool, x : i32, y : i32) -> i32 {
    if b {
        x
    } else {
        y
    }
}