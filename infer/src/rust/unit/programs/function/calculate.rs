pub fn calculate(a: i32, b: i32) -> i32 {
    let result1 = square(a);
    let result2 = square(result1);
    subtract(result1, result2)
}

fn square(x: i32) -> i32 {
    x * x
}

fn subtract(x: i32, y: i32) -> i32 {
    x - y
}

fn main() {
    let x: i32 = 0;
    let y: i32 = 1;
    let result = calculate(x, y);
    let _ = result;
}