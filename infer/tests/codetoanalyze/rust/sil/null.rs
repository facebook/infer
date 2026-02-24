fn main() {
    let ptr: *const i32 = std::ptr::null(); 
    let x = unsafe {*ptr};
}