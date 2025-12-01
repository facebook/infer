fn main() {
    let x = 1;
    {
        let x = 2;
        let _ = x;
    }
    let _ = x;
}