fn deref() {
    let x = 42;
    let ref_x = &x;
    let y = *ref_x; 
}

fn main() {
    deref();
}
