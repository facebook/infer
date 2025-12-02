enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
}

fn main() {
    match Ordering::Less {
        Ordering::Less => {}
        Ordering::Equal => {}
        Ordering::Greater => {}
    }
}
