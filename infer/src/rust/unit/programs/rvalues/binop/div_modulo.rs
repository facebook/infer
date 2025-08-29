fn div_and_modulo() {
    let a: u32 = 7;
    let b: u32 = 3;
    let _ = a / b;
    let _ = a % b;

    let c: i32 = 7;
    let d: i32 = -3;
    let _ = c / d;
    let _ = c % d;

    let e: i32 = -7;
    let f: i32 = 3;
    let _ = e / f;
    let _ = e % f;
}

fn main() {
    div_and_modulo();
}