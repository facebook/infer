pub fn calls_and_temp() -> i32 {
    let k = callee(3);
    k
}

fn callee(n: i32) -> i32 { n * 2 }

fn main() {
    let _ = calls_and_temp();
}