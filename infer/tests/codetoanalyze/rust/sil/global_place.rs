const X : i32 = 5;
const Y : i32 = succ(X);


const fn succ(i : i32) -> i32 {
    i + 1
}

#[allow(unused)]
fn main() {
    let eleven = X + Y;
}