fn main() {
    let a = mydup1::hello();
    let b = mydup2::hello();
    assert_eq!(a + b, 3);
}
