//@ charon-args=--monomorphize
pub trait SizedTypeProperties: Sized {
    const IS_ZST: bool = size_of::<Self>() == 0;
}

impl<T> SizedTypeProperties for T {}

fn main() {
    let _ = <() as SizedTypeProperties>::IS_ZST;
}
