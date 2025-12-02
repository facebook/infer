//@ charon-args=--remove-associated-types=*
pub fn flabada<'a>(x: &'a ()) -> &'a () {
    x
}

pub fn call<'b, F: Fn(&'b ()) -> &'b ()>(_: F) {}

pub fn flibidi() {
    call(flabada);
}
