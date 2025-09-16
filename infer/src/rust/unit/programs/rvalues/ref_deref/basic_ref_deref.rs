fn ref_deref() {
    let x = 42;
    
    let y = &*&*&x; 
}

fn main() {
    ref_deref();
}
