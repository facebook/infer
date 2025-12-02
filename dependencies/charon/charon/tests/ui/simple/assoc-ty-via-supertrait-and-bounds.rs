//@ charon-args=--remove-associated-types=*
pub trait HasOutput {
    type Output;
}
impl HasOutput for () {
    type Output = ();
}

pub trait HasOutput2: HasOutput {}
impl HasOutput2 for () {}

struct Wrapper<T>(T);
impl<T: HasOutput> HasOutput for Wrapper<T> {
    type Output = <T as HasOutput>::Output;
}
impl<T: HasOutput2> HasOutput2 for Wrapper<T> {}

fn take<T: HasOutput2>() {}
fn main() {
    take::<Wrapper<()>>()
}
