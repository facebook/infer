fn main() {
    let ptr: *const i32;
    {
        let x = Box::new(50);
        ptr = &*x; // Convert box to raw pointer
        // Box is freed here since borrowchecker does not keep track of raw pointer
    }
    #[allow(unused)]
    let ub = unsafe {*ptr}; // Error Occurs Here
}