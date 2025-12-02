//@ known-failure
fn main() {
    match 'x' {
        'a' => {}
        _ => {}
    }
}
