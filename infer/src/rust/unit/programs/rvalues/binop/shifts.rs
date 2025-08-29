fn shifts() {
    let _ : u8 = 1 << 3;
    let _ : u8 = 0b1000_0000 >> 7;

    let x: i8 = -2; 
    let _ : i8 = x >> 1; 
}

fn main() {
    shifts();
}