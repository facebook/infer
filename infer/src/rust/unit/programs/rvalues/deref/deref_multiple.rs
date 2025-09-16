fn deref_multiple() {
    let x = 42;
    let ref_1 = &x;           
    let ref_2 = &ref_1;       
    let ref_3 = &ref_2;       
    let y = ***ref_3;
}

fn main() {
    deref_multiple();
}