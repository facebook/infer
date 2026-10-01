fn write(a: &mut i32) {
    *a = 1;
}

fn main() {
    let mut x = 0;
    let mut y = 0;
    write(&mut x);
    write(&mut y);
}
