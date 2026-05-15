//@ charon-args=--remove-associated-types=*
#![feature(ptr_metadata)]

fn empty_metadata() {
    let _ = (&0u32 as *const u32).to_raw_parts();
}

fn slice_metadata() {
    let _ = (&[0u32, 1u32] as *const [u32]).to_raw_parts();
}
