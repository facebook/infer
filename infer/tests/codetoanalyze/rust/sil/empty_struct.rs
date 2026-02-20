struct Adder;

impl Adder {
    fn add(&self, a : i32, b : i32) -> i32 {
        a + b
    }
}

fn main() {
    let adder = Adder; // no fields
    let three = adder.add(1,2);
}
