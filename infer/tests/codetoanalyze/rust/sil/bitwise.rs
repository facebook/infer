fn main() {
    let _ = 0b1010u8 & 0b1100u8;
    let _ = 0b1010u8 | 0b0101u8;
    let _ = 0b1111u8 ^ 0b0101u8;

    let a: i8 = -1;
    let _ = a & 0b0000_1111i8;
}