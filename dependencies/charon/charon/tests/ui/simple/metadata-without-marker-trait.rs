//@ charon-args=--hide-marker-traits
pub fn wrap_shared_in_option<'a, T>(x: &'a T) -> Option<&'a T> {
    Option::Some(x)
}
