fn main() {
    let mut x = (1, 2);
    let one = x.0;
    let two = x.1;
    x.0 = two;
    x.1 = one;
}