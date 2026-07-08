fn main() {
    let mut x = 0;
    let r = &mut x;
    *r = 1;
    let _v = *r;
}
