//@ charon-args=--precise-drops
//@ charon-args=--desugar-drops

fn use_string(_: String) {}

fn use_vec(_: Vec<i32>) {}

fn drop_unknown<T>(_: T) {}

fn skip_drop_scalar(_: i32) {}

struct Point {
    x: Box<i32>,
    y: Box<i32>,
}

impl Drop for Point {
    fn drop(&mut self) {
        println!("Dropping Point");
    }
}

fn main() {
    let mut p = Point {
        x: Box::new(1),
        y: Box::new(2),
    };
}
