pub struct Ref<'a, B>(&'a B);
fn main() {
    let _ = &Ref(&());
}
