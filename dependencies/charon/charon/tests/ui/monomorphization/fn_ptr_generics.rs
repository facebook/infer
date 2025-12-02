//@ charon-args=--monomorphize
// Ensures monomorphization happens for the generic arguments of function pointers -- in this
// case, the generic args are in the index function, with Option<u8>

fn init_option() {
    let a = [Some(4u8); 6];
    let b = a[0];
}
