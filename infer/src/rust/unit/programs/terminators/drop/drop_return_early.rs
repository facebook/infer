pub fn drop_return_early(flag: bool) -> i32 {
    let a = String::from("A");
    let b = Box::new(1);
    if flag {
        return 1;
    }
    drop(a);
    drop(b);
    0
}

fn main() {
    let x: bool = false;
    let _ = drop_return_early(x);
}