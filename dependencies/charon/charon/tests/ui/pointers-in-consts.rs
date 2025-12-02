//@ known-failure
const DISGUISED_INT: *const () = 42 as _;

pub fn bar() {
    match 43 as *const () {
        DISGUISED_INT => {}
        _ => {}
    }
}
